/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2013, 2015, Oracle and/or its affiliates. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file Type.cpp
 *
 * Implements a collection of type analyses operating on AST constructs.
 *
 ***********************************************************************/

#include "ast/analysis/Type.h"
#include "AggregateOp.h"
#include "ConstraintSystem.h"
#include "FunctorOps.h"
#include "Global.h"
#include "ast/Aggregator.h"
#include "ast/Argument.h"
#include "ast/Atom.h"
#include "ast/Attribute.h"
#include "ast/BinaryConstraint.h"
#include "ast/BranchInit.h"
#include "ast/Clause.h"
#include "ast/Constant.h"
#include "ast/Counter.h"
#include "ast/Functor.h"
#include "ast/IntrinsicFunctor.h"
#include "ast/Negation.h"
#include "ast/Node.h"
#include "ast/NumericConstant.h"
#include "ast/Program.h"
#include "ast/QualifiedName.h"
#include "ast/RecordInit.h"
#include "ast/Relation.h"
#include "ast/StringConstant.h"
#include "ast/TranslationUnit.h"
#include "ast/TypeCast.h"
#include "ast/UnnamedVariable.h"
#include "ast/UserDefinedFunctor.h"
#include "ast/Variable.h"
#include "ast/analysis/Constraint.h"
#include "ast/analysis/Functor.h"
#include "ast/analysis/SumTypeBranches.h"
#include "ast/analysis/TypeEnvironment.h"
#include "ast/analysis/TypeSystem.h"
#include "ast/utility/NodeMapper.h"
#include "ast/utility/Utils.h"
#include "souffle/TypeAttribute.h"
#include "souffle/utility/ContainerUtil.h"
#include "souffle/utility/FunctionalUtil.h"
#include "souffle/utility/MiscUtil.h"
#include "souffle/utility/StringUtil.h"
#include <algorithm>
#include <cassert>
#include <cstddef>
#include <functional>
#include <map>
#include <memory>
#include <optional>
#include <set>
#include <sstream>
#include <stdexcept>
#include <string>
#include <utility>

namespace souffle::ast::analysis {

namespace {

// -----------------------------------------------------------------------------
//                          Type Deduction Lattice
// -----------------------------------------------------------------------------

/**
 * An implementation of a meet operation between sets of types computing
 * the set of pair-wise greatest common subtypes.
 */
struct sub_type {
    bool operator()(TypeSet& a, const TypeSet& b) const {
        // compute result set
        TypeSet greatestCommonSubtypes = getGreatestCommonSubtypes(a, b);

        // check whether a should change
        if (greatestCommonSubtypes == a) {
            return false;
        }

        // update a
        a = greatestCommonSubtypes;
        return true;
    }
};

/**
 * A factory for computing sets of types covering all potential types.
 */
struct all_type_factory {
    TypeSet operator()() const {
        return TypeSet(true);
    }
};

/**
 * The type lattice forming the property space for the Type analysis. The
 * value set is given by sets of types and the meet operator is based on the
 * pair-wise computation of greatest common subtypes. Correspondingly, the
 * bottom element has to be the set of all types.
 */
struct type_lattice : public property_space<TypeSet, sub_type, all_type_factory> {};

/** The definition of the type of variable to be utilized in the type analysis */
using TypeVar = ConstraintAnalysisVar<type_lattice>;

/** The definition of the type of constraint to be utilized in the type analysis */
using TypeConstraint = std::shared_ptr<Constraint<TypeVar>>;

/**
 * A constraint factory ensuring that all the types associated to the variable
 * a are subtypes of the variable b.
 */
TypeConstraint isSubtypeOf(const TypeVar& a, const TypeVar& b) {
    return sub(a, b, "<:");
}

/**
 * A constraint factory ensuring that all the types associated to the variable
 * a are subtypes of type b.
 */
TypeConstraint isSubtypeOf(const TypeVar& variable, const Type& type) {
    struct C : public Constraint<TypeVar> {
        TypeVar variable;
        const Type& type;

        C(TypeVar variable, const Type& type) : variable(std::move(variable)), type(type) {}

        bool update(Assignment<TypeVar>& assignments) const override {
            TypeSet& assignment = assignments[variable];

            if (assignment.isAll()) {
                assignment = TypeSet(type);
                return true;
            }

            TypeSet newAssignment;
            for (const Type& t : assignment) {
                newAssignment.insert(getGreatestCommonSubtypes(t, type));
            }

            // check whether there was a change
            if (assignment == newAssignment) {
                return false;
            }
            assignment = newAssignment;
            return true;
        }

        void print(std::ostream& out) const override {
            out << variable << " <: " << type.getName();
        }
    };

    return std::make_shared<C>(variable, type);
}

/**
 * A constraint factory ensuring that all the types associated to the variable
 * are subtypes of some type in the provided set (values)
 *
 * Values can't be all.
 */
TypeConstraint hasSuperTypeInSet(const TypeVar& var, TypeSet values) {
    struct C : public Constraint<TypeVar> {
        TypeVar var;
        TypeSet values;

        C(TypeVar var, TypeSet values) : var(std::move(var)), values(std::move(values)) {}

        bool update(Assignment<TypeVar>& assigment) const override {
            // get current value of variable a
            TypeSet& assigments = assigment[var];

            // remove all types that are not sub-types of b
            if (assigments.isAll()) {
                assigments = values;
                return true;
            }

            TypeSet newAssigments;
            for (const Type& type : assigments) {
                bool existsSuperTypeInValues =
                        any_of(values, [&type](const Type& value) { return isSubtypeOf(type, value); });
                if (existsSuperTypeInValues) {
                    newAssigments.insert(type);
                }
            }
            // check whether there was a change
            if (newAssigments == assigments) {
                return false;
            }
            assigments = newAssigments;
            return true;
        }

        void print(std::ostream& out) const override {
            out << "∃ t ∈ " << values << ": " << var << " <: t";
        }
    };

    return std::make_shared<C>(var, values);
}

const Type& getBaseType(const Type* type) {
    while (auto subset = dynamic_cast<const SubsetType*>(type)) {
        type = &subset->getBaseType();
    };
    assert((isA<ConstantType>(type) || isA<RecordType>(type)) &&
            "Root must be a constant type or a record type");
    return *type;
}

/**
 * Ensure that types of left and right have the same base types.
 */
TypeConstraint subtypesOfTheSameBaseType(const TypeVar& left, const TypeVar& right) {
    struct C : public Constraint<TypeVar> {
        TypeVar left;
        TypeVar right;

        C(TypeVar left, TypeVar right) : left(std::move(left)), right(std::move(right)) {}

        bool update(Assignment<TypeVar>& assigment) const override {
            // get current value of variable a
            TypeSet& assigmentsLeft = assigment[left];
            TypeSet& assigmentsRight = assigment[right];

            // Base types common to left and right variables.
            TypeSet baseTypes;

            // Base types present in left/right variable.
            TypeSet baseTypesLeft;
            TypeSet baseTypesRight;

            // Iterate over possible types extracting base types.
            // Left
            if (!assigmentsLeft.isAll()) {
                for (const Type& type : assigmentsLeft) {
                    if (isA<SubsetType>(type) || isA<ConstantType>(type)) {
                        baseTypesLeft.insert(getBaseType(&type));
                    }
                }
            }
            // Right
            if (!assigmentsRight.isAll()) {
                for (const Type& type : assigmentsRight) {
                    if (isA<SubsetType>(type) || isA<ConstantType>(type)) {
                        baseTypesRight.insert(getBaseType(&type));
                    }
                }
            }

            TypeSet resultLeft;
            TypeSet resultRight;

            // Handle all
            if (assigmentsLeft.isAll() && assigmentsRight.isAll()) {
                return false;
            }

            // If left xor right is all, assign base types of the other side as possible values.
            if (assigmentsLeft.isAll()) {
                assigmentsLeft = baseTypesRight;
                return true;
            }
            if (assigmentsRight.isAll()) {
                assigmentsRight = baseTypesLeft;
                return true;
            }

            baseTypes = TypeSet::intersection(baseTypesLeft, baseTypesRight);

            // Allow types if they are subtypes of any of the common base types.
            for (const Type& type : assigmentsLeft) {
                bool isSubtypeOfCommonBaseType = any_of(baseTypes.begin(), baseTypes.end(),
                        [&type](const Type& baseType) { return isSubtypeOf(type, baseType); });
                if (isSubtypeOfCommonBaseType) {
                    resultLeft.insert(type);
                }
            }

            for (const Type& type : assigmentsRight) {
                bool isSubtypeOfCommonBaseType = any_of(baseTypes.begin(), baseTypes.end(),
                        [&type](const Type& baseType) { return isSubtypeOf(type, baseType); });
                if (isSubtypeOfCommonBaseType) {
                    resultRight.insert(type);
                }
            }

            // check whether there was a change
            if (resultLeft == assigmentsLeft && resultRight == assigmentsRight) {
                return false;
            }
            assigmentsLeft = resultLeft;
            assigmentsRight = resultRight;
            return true;
        }
        //
        void print(std::ostream& out) const override {
            out << "∃ t : (" << left << " <: t)"
                << " ∧ "
                << "(" << right << " <: t)"
                << " where t is a base type";
        }
    };

    return std::make_shared<C>(left, right);
}

/**
 * Given a set of overloads, wait the list of candidates to reduce to one and then apply its constraints.
 * NOTE:  `subtypeResult` implies that `func <: overload-return-type`, rather than
 *        `func = overload-return-type`. This is required for old type semantics.
 *        See #1296 and tests/semantic/type_system4
 */
TypeConstraint satisfiesOverload(const TypeEnvironment& typeEnv, IntrinsicFunctors overloads, TypeVar result,
        std::vector<TypeVar> args, bool subtypeResult) {
    struct C : public Constraint<TypeVar> {
        // Check if there already was a non-monotonic update
        mutable bool nonMonotonicUpdate = false;

        const TypeEnvironment& typeEnv;
        mutable IntrinsicFunctors overloads;
        TypeVar result;
        std::vector<TypeVar> args;
        bool subtypeResult;

        C(const TypeEnvironment& typeEnv, IntrinsicFunctors overloads, TypeVar result,
                std::vector<TypeVar> args, bool subtypeResult)
                : typeEnv(typeEnv), overloads(std::move(overloads)), result(std::move(result)),
                  args(std::move(args)), subtypeResult(subtypeResult) {}

        bool update(Assignment<TypeVar>& assigment) const override {
            auto subtypesOf = [&](const TypeSet& src, TypeAttribute tyAttr) {
                auto& ty = typeEnv.getConstantType(tyAttr);
                return src.filter(TypeSet(true), [&](auto&& x) { return isSubtypeOf(x, ty); });
            };

            auto possible = [&](TypeAttribute ty, const TypeVar& var) {
                auto& curr = assigment[var];
                return curr.isAll() || any_of(curr, [&](auto&& t) { return getTypeAttribute(t) == ty; });
            };

            overloads = filterNot(std::move(overloads), [&](const IntrinsicFunctorInfo& x) -> bool {
                if (!x.variadic && args.size() != x.params.size()) return true;  // arity mismatch?

                for (size_t i = 0; i < args.size(); ++i)
                    if (!possible(x.params[x.variadic ? 0 : i], args[i])) return true;

                return !possible(x.result, result);
            });

            bool changed = false;
            auto newResult = [&]() -> std::optional<TypeSet> {
                if (0 == overloads.size()) return TypeSet();
                if (1 < overloads.size()) return {};

                auto& overload = overloads.front().get();
                // `ord` is freakin' magical: it has the signature `a -> Int`.
                // As a consequence, we might be given non-primitive arguments (i.e. types for which
                // `TypeEnv::getConstantType` is undefined).
                // Handle this by not imposing constraints on the arguments.
                if (overload.op != FunctorOp::ORD) {
                    for (size_t i = 0; i < args.size(); ++i) {
                        auto argTy = overload.params[overload.variadic ? 0 : i];
                        auto& currArg = assigment[args[i]];
                        auto newArg = subtypesOf(currArg, argTy);
                        changed |= currArg != newArg;
                        // 2020-05-09: CI linter says to remove `std::move`, but clang-tidy-10 is happy.
                        currArg = std::move(newArg);  // NOLINT
                    }
                }

                if (nonMonotonicUpdate || subtypeResult) {
                    return subtypesOf(assigment[result], overload.result);
                } else {
                    nonMonotonicUpdate = true;
                    return TypeSet{typeEnv.getConstantType(overload.result)};
                }
            }();

            if (newResult) {
                auto& curr = assigment[result];
                changed |= curr != *newResult;
                // 2020-05-09: CI linter says to remove `std::move`, but clang-tidy-10 is happy.
                curr = std::move(*newResult);  // NOLINT
            }

            return changed;
        }

        void print(std::ostream& out) const override {
            // TODO (darth_tytus): is this description correct?
            out << "∃ t : " << result << " <: t where t is a base type";
        }
    };

    return std::make_shared<C>(
            typeEnv, std::move(overloads), std::move(result), std::move(args), subtypeResult);
}

/**
 * Constraint on record type and its elements.
 */
TypeConstraint isSubtypeOfComponent(
        const TypeVar& elementVariable, const TypeVar& recordVariable, size_t index) {
    struct C : public Constraint<TypeVar> {
        TypeVar elementVariable;
        TypeVar recordVariable;
        unsigned index;

        C(TypeVar elementVariable, TypeVar recordVariable, int index)
                : elementVariable(std::move(elementVariable)), recordVariable(std::move(recordVariable)),
                  index(index) {}

        bool update(Assignment<TypeVar>& assignment) const override {
            // get list of types for b
            const TypeSet& recordTypes = assignment[recordVariable];

            // if it is (not yet) constrainted => skip
            if (recordTypes.isAll()) {
                return false;
            }

            // compute new types for element and record
            TypeSet newElementTypes;
            TypeSet newRecordTypes;

            for (const Type& type : recordTypes) {
                // A type must be either a record type or a subset of a record type
                if (!isOfKind(type, TypeAttribute::Record)) {
                    continue;
                }

                const auto& typeAsRecord = *as<RecordType>(type);

                // Wrong size => skip.
                if (typeAsRecord.getFields().size() <= index) {
                    continue;
                }

                // Valid type for record.
                newRecordTypes.insert(type);

                // and its corresponding field.
                newElementTypes.insert(*typeAsRecord.getFields()[index]);
            }

            // combine with current types assigned to element
            newElementTypes = getGreatestCommonSubtypes(assignment[elementVariable], newElementTypes);

            // update values
            bool changed = false;
            if (newRecordTypes != recordTypes) {
                assignment[recordVariable] = newRecordTypes;
                changed = true;
            }

            if (assignment[elementVariable] != newElementTypes) {
                assignment[elementVariable] = newElementTypes;
                changed = true;
            }

            return changed;
        }

        void print(std::ostream& out) const override {
            out << elementVariable << " <: " << recordVariable << "::" << index;
        }
    };

    return std::make_shared<C>(elementVariable, recordVariable, index);
}
}  // namespace

/* Return a new clause with type-annotated variables */
Own<Clause> createAnnotatedClause(
        const Clause* clause, const std::map<const Argument*, TypeSet> argumentTypes) {
    // Annotates each variable with its type based on a given type analysis result
    struct TypeAnnotator : public NodeMapper {
        const std::map<const Argument*, TypeSet>& types;

        TypeAnnotator(const std::map<const Argument*, TypeSet>& types) : types(types) {}

        Own<Node> operator()(Own<Node> node) const override {
            if (auto* var = dynamic_cast<ast::Variable*>(node.get())) {
                std::stringstream newVarName;
                newVarName << var->getName() << "&isin;" << types.find(var)->second;
                return mk<ast::Variable>(newVarName.str());
            } else if (auto* var = dynamic_cast<UnnamedVariable*>(node.get())) {
                std::stringstream newVarName;
                newVarName << "_"
                           << "&isin;" << types.find(var)->second;
                return mk<ast::Variable>(newVarName.str());
            }
            node->apply(*this);
            return node;
        }
    };

    /* Note:
     * Because the type of each argument is stored in the form [address -> type-set],
     * the type-analysis result does not immediately apply to the clone due to differing
     * addresses.
     * Two ways around this:
     *  (1) Perform the type-analysis again for the cloned clause
     *  (2) Keep track of the addresses of equivalent arguments in the cloned clause
     * Method (2) was chosen to avoid having to recompute the analysis each time.
     */
    auto annotatedClause = souffle::clone(clause);

    // Maps x -> y, where x is the address of an argument in the original clause, and y
    // is the address of the equivalent argument in the clone.
    std::map<const Argument*, const Argument*> memoryMap;

    std::vector<const Argument*> originalAddresses;
    visitDepthFirst(*clause, [&](const Argument& arg) { originalAddresses.push_back(&arg); });

    std::vector<const Argument*> cloneAddresses;
    visitDepthFirst(*annotatedClause, [&](const Argument& arg) { cloneAddresses.push_back(&arg); });

    assert(cloneAddresses.size() == originalAddresses.size());

    for (size_t i = 0; i < originalAddresses.size(); i++) {
        memoryMap[originalAddresses[i]] = cloneAddresses[i];
    }

    // Map the types to the clause clone
    std::map<const Argument*, TypeSet> cloneArgumentTypes;
    for (auto& pair : argumentTypes) {
        cloneArgumentTypes[memoryMap[pair.first]] = pair.second;
    }

    // Create the type-annotated clause
    TypeAnnotator annotator(cloneArgumentTypes);
    annotatedClause->apply(annotator);
    return annotatedClause;
}

/**
 * Constraint analysis framework for types.
 *
 * The analysis operates on the concept of sinks and sources.
 * If the atom is negated or is a head then it's a sink,
 * and we can only extract the kind constraint from it
 * Otherwise it is a source, and the type of the element must
 * be a subtype of source attribute.
 */
class TypeConstraintsAnalysis : public ConstraintAnalysis<TypeVar> {
public:
    TypeConstraintsAnalysis(const TranslationUnit& tu) : tu(tu) {}

private:
    const TranslationUnit& tu;
    const TypeEnvironment& typeEnv = tu.getAnalysis<TypeEnvironmentAnalysis>()->getTypeEnvironment();
    const Program& program = tu.getProgram();
    const SumTypeBranchesAnalysis& sumTypesBranches = *tu.getAnalysis<SumTypeBranchesAnalysis>();
    const TypeAnalysis& typeAnalysis = *tu.getAnalysis<TypeAnalysis>();

    // Sinks = {head} ∪ {negated atoms}
    std::set<const Atom*> sinks;

    void collectConstraints(const Clause& clause) override {
        sinks.insert(clause.getHead());
        visitDepthFirstPreOrder(clause, *this);
    }

    void visitSink(const Atom& atom) {
        iterateOverAtom(atom, [&](const Argument& argument, const Type& attributeType) {
            if (isA<RecordType>(attributeType)) {
                addConstraint(isSubtypeOf(getVar(argument), getBaseType(&attributeType)));
                return;
            }
            for (auto& constantType : typeEnv.getConstantTypes()) {
                if (isSubtypeOf(attributeType, constantType)) {
                    addConstraint(isSubtypeOf(getVar(argument), constantType));
                }
            }
        });
    }

    void visitAtom(const Atom& atom) override {
        if (contains(sinks, &atom)) {
            visitSink(atom);
            return;
        }

        iterateOverAtom(atom, [&](const Argument& argument, const Type& attributeType) {
            addConstraint(isSubtypeOf(getVar(argument), attributeType));
        });
    }

    void visitNegation(const Negation& cur) override {
        sinks.insert(cur.getAtom());
    }

    void visitStringConstant(const StringConstant& cnst) override {
        addConstraint(isSubtypeOf(getVar(cnst), typeEnv.getConstantType(TypeAttribute::Symbol)));
    }

    void visitNumericConstant(const NumericConstant& constant) override {
        TypeSet possibleTypes;

        // Check if the type is given.
        if (constant.getFixedType().has_value()) {
            switch (constant.getFixedType().value()) {
                // Insert a type, but only after checking that parsing is possible.
                case NumericConstant::Type::Int:
                    if (canBeParsedAsRamSigned(constant.getConstant())) {
                        possibleTypes.insert(typeEnv.getConstantType(TypeAttribute::Signed));
                    }
                    break;
                case NumericConstant::Type::Uint:
                    if (canBeParsedAsRamUnsigned(constant.getConstant())) {
                        possibleTypes.insert(typeEnv.getConstantType(TypeAttribute::Unsigned));
                    }
                    break;
                case NumericConstant::Type::Float:
                    if (canBeParsedAsRamFloat(constant.getConstant())) {
                        possibleTypes.insert(typeEnv.getConstantType(TypeAttribute::Float));
                    }
                    break;
            }
        } else if (contains(typeAnalysis.getNumericConstantTypes(), &constant)) {
            switch (typeAnalysis.getNumericConstantTypes().at(&constant)) {
                // Insert a type, but only after checking that parsing is possible.
                case NumericConstant::Type::Int:
                    if (canBeParsedAsRamSigned(constant.getConstant())) {
                        possibleTypes.insert(typeEnv.getConstantType(TypeAttribute::Signed));
                    }
                    break;
                case NumericConstant::Type::Uint:
                    if (canBeParsedAsRamUnsigned(constant.getConstant())) {
                        possibleTypes.insert(typeEnv.getConstantType(TypeAttribute::Unsigned));
                    }
                    break;
                case NumericConstant::Type::Float:
                    if (canBeParsedAsRamFloat(constant.getConstant())) {
                        possibleTypes.insert(typeEnv.getConstantType(TypeAttribute::Float));
                    }
                    break;
            }
        } else {
            // Else: all numeric types that can be parsed are valid.
            if (canBeParsedAsRamSigned(constant.getConstant())) {
                possibleTypes.insert(typeEnv.getConstantType(TypeAttribute::Signed));
            }

            if (canBeParsedAsRamUnsigned(constant.getConstant())) {
                possibleTypes.insert(typeEnv.getConstantType(TypeAttribute::Unsigned));
            }

            if (canBeParsedAsRamFloat(constant.getConstant())) {
                possibleTypes.insert(typeEnv.getConstantType(TypeAttribute::Float));
            }
        }

        addConstraint(hasSuperTypeInSet(getVar(constant), possibleTypes));
    }

    void visitBinaryConstraint(const BinaryConstraint& rel) override {
        auto lhs = getVar(rel.getLHS());
        auto rhs = getVar(rel.getRHS());
        addConstraint(isSubtypeOf(lhs, rhs));
        addConstraint(isSubtypeOf(rhs, lhs));
    }

    void visitFunctor(const Functor& fun) override {
        auto functorVar = getVar(fun);

        auto intrFun = as<IntrinsicFunctor>(fun);
        if (intrFun) {
            auto argVars = map(intrFun->getArguments(), [&](auto&& x) { return getVar(x); });
            // The type of the user-defined function might not be set at this stage.
            // If so then add overloads as alternatives
            if (!typeAnalysis.hasValidTypeInfo(intrFun))
                addConstraint(satisfiesOverload(typeEnv, functorBuiltIn(intrFun->getBaseFunctionOp()),
                        functorVar, argVars, isInfixFunctorOp(intrFun->getBaseFunctionOp())));

            // In polymorphic case
            // We only require arguments to share a base type with a return type.
            // (instead of, for example, requiring them to be of the same type)
            // This approach is related to old type semantics
            // See #1296 and tests/semantic/type_system4
            if (isInfixFunctorOp(intrFun->getBaseFunctionOp())) {
                for (auto&& var : argVars)
                    addConstraint(subtypesOfTheSameBaseType(var, functorVar));

                return;
            }

            if (!typeAnalysis.hasValidTypeInfo(intrFun)) return;
        }

        // Skip constraint adding if type info is not available
        if (!typeAnalysis.hasValidTypeInfo(&fun)) return;

        // add a constraint for the return type of the functor
        TypeAttribute returnType = typeAnalysis.getFunctorReturnType(&fun);
        addConstraint(isSubtypeOf(functorVar, typeEnv.getConstantType(returnType)));
        // Special case. Ord returns the ram representation of any object.
        if (intrFun && typeAnalysis.getPolymorphicOperator(intrFun) == FunctorOp::ORD) return;

        // Add constraints on arguments
        auto arguments = fun.getArguments();
        for (size_t i = 0; i < arguments.size(); ++i) {
            TypeAttribute argType = typeAnalysis.getFunctorArgType(&fun, i);
            addConstraint(isSubtypeOf(getVar(arguments[i]), typeEnv.getConstantType(argType)));
        }
    }

    void visitCounter(const Counter& counter) override {
        addConstraint(isSubtypeOf(getVar(counter), typeEnv.getConstantType(TypeAttribute::Signed)));
    }

    void visitTypeCast(const ast::TypeCast& typeCast) override {
        auto& typeName = typeCast.getType();
        if (!typeEnv.isType(typeName)) {
            return;
        }

        addConstraint(isSubtypeOf(getVar(typeCast), typeEnv.getType(typeName)));

        // If we are dealing with a constant then its type must be deduced from the cast
        // Otherwise, expression like: to_string(as(2, float)) couldn't be typed.
        auto* value = typeCast.getValue();

        if (isA<Constant>(value)) {
            addConstraint(isSubtypeOf(getVar(*value), typeEnv.getType(typeName)));
        }
    }

    void visitRecordInit(const RecordInit& record) override {
        auto arguments = record.getArguments();
        for (size_t i = 0; i < arguments.size(); ++i) {
            addConstraint(isSubtypeOfComponent(getVar(arguments[i]), getVar(record), i));
        }
    }

    void visitBranchInit(const BranchInit& adt) override {
        auto* correspondingType = sumTypesBranches.getType(adt.getConstructor());

        if (correspondingType == nullptr) {
            return;  // malformed program.
        }

        // Sanity check
        assert(isA<AlgebraicDataType>(correspondingType));

        // Constraint on the whole branch. $Branch(...) <: ADTtype
        addConstraint(isSubtypeOf(getVar(adt), *correspondingType));

        // Constraints on arguments
        try {
            auto branchTypes = as<AlgebraicDataType>(correspondingType)->getBranchTypes(adt.getConstructor());
            auto branchArgs = adt.getArguments();

            if (branchTypes.size() != branchArgs.size()) {
                // handled by semantic checker later.
                throw std::invalid_argument("Invalid arity");
            }

            // Add constraints for each of the branch arguments.
            for (size_t i = 0; i < branchArgs.size(); ++i) {
                auto argVar = getVar(branchArgs[i]);
                addConstraint(isSubtypeOf(argVar, *branchTypes[i]));
            }
        } catch (...) {
            return;  // Invalid program.
        }
    }

    void visitAggregator(const Aggregator& agg) override {
        if (agg.getBaseOperator() == AggregateOp::COUNT) {
            addConstraint(isSubtypeOf(getVar(agg), typeEnv.getConstantType(TypeAttribute::Signed)));
        } else if (agg.getBaseOperator() == AggregateOp::MEAN) {
            addConstraint(isSubtypeOf(getVar(agg), typeEnv.getConstantType(TypeAttribute::Float)));
        } else {
            addConstraint(hasSuperTypeInSet(getVar(agg), typeEnv.getConstantNumericTypes()));
        }

        // If there is a target expression - it should be of the same type as the aggregator.
        if (auto expr = agg.getTargetExpression()) {
            addConstraint(isSubtypeOf(getVar(expr), getVar(agg)));
            addConstraint(isSubtypeOf(getVar(agg), getVar(expr)));
        }
    }

    /**
     * Utility function.
     * Iterate over atoms valid pairs of (argument, type-attribute) and apply procedure `map` for its
     * side-effects.
     */
    void iterateOverAtom(const Atom& atom, std::function<void(const Argument&, const Type&)> map) {
        // get relation
        auto rel = getAtomRelation(&atom, &program);
        if (rel == nullptr) {
            return;  // error in input program
        }

        auto atts = rel->getAttributes();
        auto args = atom.getArguments();
        if (atts.size() != args.size()) {
            return;  // error in input program
        }

        for (size_t i = 0; i < atts.size(); i++) {
            const auto& typeName = atts[i]->getTypeName();
            if (typeEnv.isType(typeName)) {
                map(*args[i], typeEnv.getType(typeName));
            }
        }
    }
};

std::map<const Argument*, TypeSet> TypeAnalysis::analyseTypes(
        const TranslationUnit& tu, const Clause& clause, std::ostream* logs) {
    return TypeConstraintsAnalysis(tu).analyse(clause, logs);
}

void TypeAnalysis::print(std::ostream& os) const {
    os << "-- Analysis logs --" << std::endl;
    os << analysisLogs.str() << std::endl;
    os << "-- Result --" << std::endl;
    for (auto& cur : annotatedClauses) {
        os << *cur << std::endl;
    }
}

TypeAttribute TypeAnalysis::getFunctorReturnType(const Functor* functor) const {
    assert(hasValidTypeInfo(functor) && "functor not yet processed");
    if (auto* intrinsic = as<IntrinsicFunctor>(functor)) {
        return functorInfo.at(intrinsic)->result;
    } else if (const auto* udf = as<UserDefinedFunctor>(functor)) {
        return udfDeclaration.at(udf->getName())->getReturnType();
    }
    fatal("Missing functor type.");
}

TypeAttribute TypeAnalysis::getFunctorArgType(const Functor* functor, const size_t idx) const {
    assert(hasValidTypeInfo(functor) && "functor not yet processed");
    if (auto* intrinsic = as<IntrinsicFunctor>(functor)) {
        auto* info = functorInfo.at(intrinsic);
        return info->params.at(info->variadic ? 0 : idx);
    } else if (auto* udf = as<UserDefinedFunctor>(functor)) {
        return udfDeclaration.at(udf->getName())->getArgsTypes().at(idx);
    }
    fatal("Missing functor type.");
}

const std::vector<TypeAttribute>& TypeAnalysis::getFunctorArgTypes(const UserDefinedFunctor& udf) const {
    return udfDeclaration.at(udf.getName())->getArgsTypes();
}

bool TypeAnalysis::isStatefulFunctor(const UserDefinedFunctor* udf) const {
    return udfDeclaration.at(udf->getName())->isStateful();
}

const std::map<const NumericConstant*, NumericConstant::Type>& TypeAnalysis::getNumericConstantTypes() const {
    return numericConstantType;
}

bool TypeAnalysis::isMultiResultFunctor(const Functor& functor) {
    if (isA<UserDefinedFunctor>(functor)) {
        return false;
    } else if (auto* intrinsic = as<IntrinsicFunctor>(functor)) {
        auto candidates = functorBuiltIn(intrinsic->getBaseFunctionOp());
        assert(!candidates.empty() && "at least one op should match");
        return candidates[0].get().multipleResults;
    }
    fatal("Missing functor type.");
}

std::set<TypeAttribute> TypeAnalysis::getTypeAttributes(const Argument* arg) const {
    std::set<TypeAttribute> typeAttributes;

    if (const auto* inf = dynamic_cast<const IntrinsicFunctor*>(arg)) {
        // intrinsic functor type is its return type if its set
        if (hasValidTypeInfo(inf)) {
            typeAttributes.insert(getFunctorReturnType(inf));
            return typeAttributes;
        }
    }

    const auto& types = getTypes(arg);
    if (types.isAll()) {
        return {TypeAttribute::Signed, TypeAttribute::Unsigned, TypeAttribute::Float, TypeAttribute::Symbol,
                TypeAttribute::Record};
    }
    for (const auto& type : types) {
        typeAttributes.insert(getTypeAttribute(type));
    }
    return typeAttributes;
}

IntrinsicFunctors TypeAnalysis::getValidIntrinsicFunctorOverloads(const IntrinsicFunctor& inf) const {
    // Get the info of all possible functors which can be used here
    IntrinsicFunctors functorInfos = contains(functorInfo, &inf) ? functorBuiltIn(functorInfo.at(&inf)->op)
                                                                 : functorBuiltIn(inf.getBaseFunctionOp());

    // Filter out the ones which don't fit in with the current knowledge
    auto returnTypes = getTypeAttributes(&inf);
    auto argTypes = map(inf.getArguments(), [&](const Argument* arg) { return getTypeAttributes(arg); });
    auto isValidOverload = [&](const IntrinsicFunctorInfo& candidate) {
        // Check for arity mismatch
        if (!candidate.variadic && argTypes.size() != candidate.params.size()) {
            return false;
        }

        // Check that argument types match
        for (size_t i = 0; i < argTypes.size(); ++i) {
            const auto& expectedType = candidate.params[candidate.variadic ? 0 : i];
            if (!contains(argTypes[i], expectedType)) {
                return false;
            }
        }

        // Check that the return type matches
        return contains(returnTypes, candidate.result);
    };
    auto candidates = filter(functorInfos, isValidOverload);

    // Sort them in a standardised way (so order is deterministic)
    auto comparator = [&](const IntrinsicFunctorInfo& a, const IntrinsicFunctorInfo& b) {
        if (a.result != b.result) return a.result < b.result;
        if (a.variadic != b.variadic) return a.variadic < b.variadic;
        return std::lexicographical_compare(
                a.params.begin(), a.params.end(), b.params.begin(), b.params.end());
    };
    std::sort(candidates.begin(), candidates.end(), comparator);

    return candidates;
}

bool TypeAnalysis::hasValidTypeInfo(const Argument* argument) const {
    if (auto* inf = as<IntrinsicFunctor>(argument)) {
        return contains(functorInfo, inf);
    } else if (auto* udf = as<UserDefinedFunctor>(argument)) {
        return contains(udfDeclaration, udf->getName());
    } else if (auto* nc = as<NumericConstant>(argument)) {
        return contains(numericConstantType, nc);
    } else if (auto* agg = as<Aggregator>(argument)) {
        return contains(aggregatorType, agg);
    }
    return true;
}

NumericConstant::Type TypeAnalysis::getPolymorphicNumericConstantType(const NumericConstant* nc) const {
    assert(hasValidTypeInfo(nc) && "numeric constant type not set");
    return numericConstantType.at(nc);
}

BinaryConstraintOp TypeAnalysis::getPolymorphicOperator(const BinaryConstraint* bc) const {
    assert(contains(constraintType, bc) && "binary constraint operator not set");
    return constraintType.at(bc);
}

AggregateOp TypeAnalysis::getPolymorphicOperator(const Aggregator* agg) const {
    assert(hasValidTypeInfo(agg) && "aggregator operator not set");
    return aggregatorType.at(agg);
}

FunctorOp TypeAnalysis::getPolymorphicOperator(const IntrinsicFunctor* inf) const {
    assert(hasValidTypeInfo(inf) && "functor type not set");
    return functorInfo.at(inf)->op;
}

bool TypeAnalysis::analyseIntrinsicFunctors(const TranslationUnit& translationUnit) {
    bool changed = false;
    const auto& program = translationUnit.getProgram();
    visitDepthFirst(program, [&](const IntrinsicFunctor& functor) {
        auto candidates = getValidIntrinsicFunctorOverloads(functor);
        if (candidates.empty()) {
            // No valid overloads - mark it as an invalid functor
            if (contains(functorInfo, &functor)) {
                functorInfo.erase(&functor);
                changed = true;
            }
            return;
        }

        // Update to the canonic representation if different
        const auto* curInfo = &candidates.front().get();
        if (contains(functorInfo, &functor) && functorInfo.at(&functor) == curInfo) return;
        functorInfo[&functor] = curInfo;
        changed = true;
    });
    return changed;
}

bool TypeAnalysis::analyseNumericConstants(const TranslationUnit& translationUnit) {
    bool changed = false;
    const auto& program = translationUnit.getProgram();

    auto setNumericConstantType = [&](const NumericConstant& nc, NumericConstant::Type ncType) {
        if (contains(numericConstantType, &nc) && numericConstantType.at(&nc) == ncType) return;
        changed = true;
        numericConstantType[&nc] = ncType;
    };

    visitDepthFirst(program, [&](const NumericConstant& numericConstant) {
        // Constant has a fixed type
        if (numericConstant.getFixedType().has_value()) {
            setNumericConstantType(numericConstant, numericConstant.getFixedType().value());
            return;
        }

        // Otherwise, type should be inferred
        TypeSet types = getTypes(&numericConstant);
        auto hasOfKind = [&](TypeAttribute kind) -> bool {
            return any_of(types, [&](const analysis::Type& type) { return isOfKind(type, kind); });
        };
        if (hasOfKind(TypeAttribute::Signed)) {
            setNumericConstantType(numericConstant, NumericConstant::Type::Int);
        } else if (hasOfKind(TypeAttribute::Unsigned)) {
            setNumericConstantType(numericConstant, NumericConstant::Type::Uint);
        } else if (hasOfKind(TypeAttribute::Float)) {
            setNumericConstantType(numericConstant, NumericConstant::Type::Float);
        } else {
            // Type information no longer valid
            if (contains(numericConstantType, &numericConstant)) {
                numericConstantType.erase(&numericConstant);
                changed = true;
            }
        }
    });

    return changed;
}

bool TypeAnalysis::analyseAggregators(const TranslationUnit& translationUnit) {
    bool changed = false;
    const auto& program = translationUnit.getProgram();

    auto setAggregatorType = [&](const Aggregator& agg, TypeAttribute attr) {
        auto overloadedType = convertOverloadedAggregator(agg.getBaseOperator(), attr);
        if (contains(aggregatorType, &agg) && aggregatorType.at(&agg) == overloadedType) return;
        changed = true;
        aggregatorType[&agg] = overloadedType;
    };

    visitDepthFirst(program, [&](const Aggregator& agg) {
        if (isOverloadedAggregator(agg.getBaseOperator())) {
            auto* targetExpression = agg.getTargetExpression();
            if (isFloat(targetExpression)) {
                setAggregatorType(agg, TypeAttribute::Float);
            } else if (isUnsigned(targetExpression)) {
                setAggregatorType(agg, TypeAttribute::Unsigned);
            } else {
                setAggregatorType(agg, TypeAttribute::Signed);
            }
        } else {
            if (contains(aggregatorType, &agg)) {
                assert(aggregatorType.at(&agg) == agg.getBaseOperator() &&
                        "non-overloaded aggr types should always be the base operator");
                return;
            }
            changed = true;
            aggregatorType[&agg] = agg.getBaseOperator();
        }
    });

    return changed;
}

bool TypeAnalysis::analyseBinaryConstraints(const TranslationUnit& translationUnit) {
    bool changed = false;
    const auto& program = translationUnit.getProgram();

    auto setConstraintType = [&](const BinaryConstraint& bc, TypeAttribute attr) {
        auto overloadedType = convertOverloadedConstraint(bc.getBaseOperator(), attr);
        if (contains(constraintType, &bc) && constraintType.at(&bc) == overloadedType) return;
        changed = true;
        constraintType[&bc] = overloadedType;
    };

    visitDepthFirst(program, [&](const BinaryConstraint& binaryConstraint) {
        if (isOverloaded(binaryConstraint.getBaseOperator())) {
            // Get arguments
            auto* leftArg = binaryConstraint.getLHS();
            auto* rightArg = binaryConstraint.getRHS();

            // Both args must be of the same type
            if (isFloat(leftArg) && isFloat(rightArg)) {
                setConstraintType(binaryConstraint, TypeAttribute::Float);
            } else if (isUnsigned(leftArg) && isUnsigned(rightArg)) {
                setConstraintType(binaryConstraint, TypeAttribute::Unsigned);
            } else if (isSymbol(leftArg) && isSymbol(rightArg)) {
                setConstraintType(binaryConstraint, TypeAttribute::Symbol);
            } else {
                setConstraintType(binaryConstraint, TypeAttribute::Signed);
            }
        } else {
            if (contains(constraintType, &binaryConstraint)) {
                assert(constraintType.at(&binaryConstraint) == binaryConstraint.getBaseOperator() &&
                        "unexpected constraint type");
                return;
            }
            changed = true;
            constraintType[&binaryConstraint] = binaryConstraint.getBaseOperator();
        }
    });

    return changed;
}

bool TypeAnalysis::isFloat(const Argument* argument) const {
    return isOfKind(getTypes(argument), TypeAttribute::Float);
}

bool TypeAnalysis::isUnsigned(const Argument* argument) const {
    return isOfKind(getTypes(argument), TypeAttribute::Unsigned);
}

bool TypeAnalysis::isSymbol(const Argument* argument) const {
    return isOfKind(getTypes(argument), TypeAttribute::Symbol);
}

void TypeAnalysis::run(const TranslationUnit& translationUnit) {
    // Check if debugging information is being generated
    std::ostream* debugStream = nullptr;
    if (Global::config().has("debug-report") || Global::config().has("show", "type-analysis")) {
        debugStream = &analysisLogs;
    }

    // Analyse user-defined functor types
    const Program& program = translationUnit.getProgram();
    visitDepthFirst(
            program, [&](const FunctorDeclaration& fdecl) { udfDeclaration[fdecl.getName()] = &fdecl; });

    // Rest of the analysis done until fixpoint reached
    bool changed = true;
    while (changed) {
        changed = false;
        argumentTypes.clear();

        // Analyse general argument types, clause by clause.
        for (const Clause* clause : program.getClauses()) {
            auto clauseArgumentTypes = analyseTypes(translationUnit, *clause, debugStream);
            argumentTypes.insert(clauseArgumentTypes.begin(), clauseArgumentTypes.end());

            if (debugStream != nullptr) {
                // Store an annotated clause for printing purposes
                annotatedClauses.emplace_back(createAnnotatedClause(clause, clauseArgumentTypes));
            }
        }

        // Analyse intrinsic-functor types
        changed |= analyseIntrinsicFunctors(translationUnit);

        // Deduce numeric-constant polymorphism
        changed |= analyseNumericConstants(translationUnit);

        // Deduce aggregator polymorphism
        changed |= analyseAggregators(translationUnit);

        // Deduce binary-constraint polymorphism
        changed |= analyseBinaryConstraints(translationUnit);
    }
}

}  // namespace souffle::ast::analysis
