/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2013, 2015, Oracle and/or its affiliates. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file ClauseTranslator.h
 *
 * Translator for clauses from AST to RAM
 *
 ***********************************************************************/

#include "ast2ram/ClauseTranslator.h"
#include "Global.h"
#include "ast/Aggregator.h"
#include "ast/Clause.h"
#include "ast/Constant.h"
#include "ast/IntrinsicFunctor.h"
#include "ast/NilConstant.h"
#include "ast/NumericConstant.h"
#include "ast/RecordInit.h"
#include "ast/StringConstant.h"
#include "ast/UnnamedVariable.h"
#include "ast/analysis/Functor.h"
#include "ast/analysis/PolymorphicObjects.h"
#include "ast/transform/ReorderLiterals.h"
#include "ast/utility/Utils.h"
#include "ast/utility/Visitor.h"
#include "ast2ram/AstToRamTranslator.h"
#include "ast2ram/ConstraintTranslator.h"
#include "ast2ram/ValueTranslator.h"
#include "ast2ram/utility/Location.h"
#include "ast2ram/utility/TranslatorContext.h"
#include "ast2ram/utility/Utils.h"
#include "ast2ram/utility/ValueIndex.h"
#include "ram/Aggregate.h"
#include "ram/Break.h"
#include "ram/Conjunction.h"
#include "ram/Constraint.h"
#include "ram/EmptinessCheck.h"
#include "ram/ExistenceCheck.h"
#include "ram/Filter.h"
#include "ram/FloatConstant.h"
#include "ram/GuardedProject.h"
#include "ram/Negation.h"
#include "ram/NestedIntrinsicOperator.h"
#include "ram/Project.h"
#include "ram/Query.h"
#include "ram/Relation.h"
#include "ram/Scan.h"
#include "ram/SignedConstant.h"
#include "ram/TupleElement.h"
#include "ram/UnpackRecord.h"
#include "ram/UnsignedConstant.h"
#include "ram/utility/Utils.h"
#include "souffle/SymbolTable.h"
#include "souffle/utility/StringUtil.h"
#include <map>
#include <vector>

namespace souffle::ast2ram {

Own<ram::Statement> ClauseTranslator::translateClause(
        const ast::Clause& clause, const ast::Clause& originalClause, int version) {
    if (auto reorderedClause = getReorderedClause(clause, version)) {
        // translate reordered clause instead
        return translateClause(*reorderedClause, originalClause, version);
    }

    // Handle facts
    if (isFact(clause)) {
        const auto* head = clause.getHead();

        // Translate arguments
        VecOwn<ram::Expression> values;
        for (auto& arg : head->getArguments()) {
            values.push_back(ValueTranslator::translate(context, symbolTable, ValueIndex(), arg));
        }

        // Create a fact statement
        return mk<ram::Query>(
                mk<ram::Project>(getConcreteRelationName(head->getQualifiedName()), std::move(values)));
    }

    // the rest should be rules
    assert(isRule(clause));

    indexClause(clause);

    /* -- create RAM statement -- */

    // Create the projection statement
    Own<ram::Operation> op = createProjection(clause, originalClause);

    // Set up the main operations in the clause
    op = addVariableBindingConstraints(std::move(op));
    op = addBodyLiteralConstraints(clause, std::move(op));
    op = addGeneratorLevels(std::move(op));
    op = buildFinalOperation(clause, originalClause, version, std::move(op));

    // Generate the final RAM insert statement
    Own<ram::Condition> cond = createCondition(originalClause);
    if (cond != nullptr) {
        return mk<ram::Query>(mk<ram::Filter>(std::move(cond), std::move(op)));
    } else {
        return mk<ram::Query>(std::move(op));
    }
}

Own<ram::Operation> ClauseTranslator::addVariableBindingConstraints(Own<ram::Operation> op) {
    for (const auto& [_, references] : valueIndex->getVariableReferences()) {
        // Equate the first appearance to all other appearances
        assert(!references.empty() && "variable should appear at least once");
        const auto& first = *references.begin();
        for (const auto& reference : references) {
            if (first != reference && !valueIndex->isGenerator(reference.identifier)) {
                // FIXME: equiv' for float types (`FEQ`)
                op = mk<ram::Filter>(mk<ram::Constraint>(BinaryConstraintOp::EQ, makeRamTupleElement(first),
                                             makeRamTupleElement(reference)),
                        std::move(op));
            }
        }
    }
    return op;
}

Own<ram::Operation> ClauseTranslator::createProjection(
        const ast::Clause& clause, const ast::Clause& originalClause) {
    const auto head = clause.getHead();
    auto headRelationName = getConcreteRelationName(head->getQualifiedName());

    VecOwn<ram::Expression> values;
    for (const auto* arg : head->getArguments()) {
        values.push_back(ValueTranslator::translate(context, symbolTable, *valueIndex, arg));
    }

    VecOwn<ram::Condition> dependencies;
    // Impose the functional dependencies of the relation on each PROJECT
    const auto relInfo = context.getRelation(originalClause.getHead()->getQualifiedName());
    if (!relInfo->getFunctionalDependencies().empty()) {
        const auto attributes = relInfo->getAttributes();
        std::vector<const ast::FunctionalConstraint*> addedConstraints;
        for (const auto& fd : relInfo->getFunctionalDependencies()) {
            // skip if already has an equivalent constraints added.
            bool added = false;
            for (const auto other : addedConstraints) {
                if (other->equivalentConstraint(*fd)) {
                    added = true;
                    break;
                }
            }
            if (added) {
                continue;
            } else {
                addedConstraints.push_back(fd);
            }
            // Remove redunatnt attributes within same key.
            std::set<std::string> keys;
            for (auto key : fd->getKeys()) {
                keys.insert(key->getName());
            }
            VecOwn<ram::Expression> vals;
            VecOwn<ram::Expression> valsCopy;
            for (size_t i = 0; i < attributes.size(); ++i) {
                const auto attribute = attributes[i];
                auto found = keys.find(attribute->getName());

                // If this particular source argument matches the head argument, insert it.
                if (found != keys.end()) {
                    /* vals.push_back(ast2ram::translateValue(head->getArguments()[i], *valueIndex)); */
                    vals.push_back(ValueTranslator::translate(
                            context, symbolTable, *valueIndex, head->getArguments()[i]));
                    valsCopy.push_back(ValueTranslator::translate(
                            context, symbolTable, *valueIndex, head->getArguments()[i]));
                    // Otherwise insert ⊥
                } else {
                    vals.push_back(mk<ram::UndefValue>());
                    valsCopy.push_back(mk<ram::UndefValue>());
                }
            }

            // if we are in a recursive clause, need to guard both new and original relation.
            if (isPrefix("@new_", head->getQualifiedName().toString())) {
                dependencies.push_back(
                        mk<ram::Negation>(mk<ram::ExistenceCheck>(headRelationName, std::move(vals))));
                dependencies.push_back(mk<ram::Negation>(mk<ram::ExistenceCheck>(
                        relInfo->getQualifiedName().toString(), std::move(valsCopy))));
            } else {
                dependencies.push_back(
                        mk<ram::Negation>(mk<ram::ExistenceCheck>(headRelationName, std::move(vals))));
            }
        }
    }

    if (head->getArity() == 0) {
        // TODO: need double check.
        /* project = mk<ram::Filter>(mk<ram::EmptinessCheck>(headRelationName), std::move(project)); */
        return mk<ram::Project>(headRelationName, std::move(values));
    } else if (dependencies.empty()) {
        return mk<ram::Project>(headRelationName, std::move(values));
    } else {
        return mk<ram::GuardedProject>(headRelationName, std::move(values), ram::toCondition(dependencies));
    }
}

Own<ram::Operation> ClauseTranslator::buildFinalOperation(
        const ast::Clause& clause, const ast::Clause& originalClause, int version, Own<ram::Operation> op) {
    const ast::Atom* head = clause.getHead();

    // build operation bottom-up
    while (!op_nesting.empty()) {
        // get next operator
        const auto* cur = op_nesting.back();
        op_nesting.pop_back();

        // get current nesting level
        auto level = op_nesting.size();

        if (const auto* atom = dynamic_cast<const ast::Atom*>(cur)) {
            // add constraints
            // TODO: do we wish to enable constraints by header functor? record inits do so...
            op = filterByConstraints(level, atom->getArguments(), std::move(op), false);

            // check whether all arguments are unnamed variables
            bool isAllArgsUnnamed = true;
            for (auto* argument : atom->getArguments()) {
                if (!isA<ast::UnnamedVariable>(argument)) {
                    isAllArgsUnnamed = false;
                }
            }

            // add check for emptiness for an atom
            op = mk<ram::Filter>(mk<ram::Negation>(mk<ram::EmptinessCheck>(
                                         getConcreteRelationName(atom->getQualifiedName()))),
                    std::move(op));

            // add a scan level
            if (atom->getArity() != 0 && !isAllArgsUnnamed) {
                if (head->getArity() == 0) {
                    op = mk<ram::Break>(mk<ram::Negation>(mk<ram::EmptinessCheck>(
                                                getConcreteRelationName(head->getQualifiedName()))),
                            std::move(op));
                }
                if (Global::config().has("profile")) {
                    std::stringstream ss;
                    ss << head->getQualifiedName();
                    ss.str("");
                    ss << "@frequency-atom" << ';';
                    ss << originalClause.getHead()->getQualifiedName() << ';';
                    ss << version << ';';
                    ss << stringify(toString(clause)) << ';';
                    ss << stringify(toString(*atom)) << ';';
                    ss << stringify(toString(originalClause)) << ';';
                    ss << level << ';';
                    op = mk<ram::Scan>(getConcreteRelationName(atom->getQualifiedName()), level,
                            std::move(op), ss.str());
                } else {
                    op = mk<ram::Scan>(
                            getConcreteRelationName(atom->getQualifiedName()), level, std::move(op));
                }
            }

            // TODO: support constants in nested records!
        } else if (const auto* rec = dynamic_cast<const ast::RecordInit*>(cur)) {
            // add constant constraints
            op = filterByConstraints(level, rec->getArguments(), std::move(op));

            // add an unpack level
            const Location& loc = valueIndex->getDefinitionPoint(*rec);
            op = mk<ram::UnpackRecord>(
                    std::move(op), level, makeRamTupleElement(loc), rec->getArguments().size());
        } else {
            fatal("Unsupported AST node for creation of scan-level!");
        }
    }

    return op;
}

Own<ram::Operation> ClauseTranslator::instantiateAggregator(
        Own<ram::Operation> op, const ast::Aggregator* agg) {
    auto addAggEqCondition = [&](Own<ram::Condition> aggr, Own<ram::Expression> value, size_t pos) {
        if (isUndefValue(value.get())) return aggr;

        // FIXME: equiv' for float types (`FEQ`)
        return addConjunctiveTerm(
                std::move(aggr), mk<ram::Constraint>(BinaryConstraintOp::EQ,
                                         mk<ram::TupleElement>(level, pos), std::move(value)));
    };

    Own<ram::Condition> aggCond;

    // translate constraints of sub-clause
    for (const auto* lit : agg->getBodyLiterals()) {
        // literal becomes a constraint
        if (auto condition = ConstraintTranslator::translate(context, symbolTable, *valueIndex, lit)) {
            aggCond = addConjunctiveTerm(std::move(aggCond), std::move(condition));
        }
    }

    // translate arguments of atom to conditions
    const auto& aggBodyAtoms =
            filter(agg->getBodyLiterals(), [&](const ast::Literal* lit) { return isA<ast::Atom>(lit); });
    assert(aggBodyAtoms.size() == 1 && "exactly one atom should exist per aggregator body");
    const auto* aggAtom = static_cast<const ast::Atom*>(aggBodyAtoms.at(0));

    const auto& aggAtomArgs = aggAtom->getArguments();
    for (size_t i = 0; i < aggAtomArgs.size(); i++) {
        const auto* arg = aggAtomArgs.at(i);

        // variable bindings are issued differently since we don't want self
        // referential variable bindings
        if (auto* var = dynamic_cast<const ast::Variable*>(arg)) {
            for (auto&& loc : valueIndex->getVariableReferences(var->getName())) {
                if (level != loc.identifier || (int)i != loc.element) {
                    aggCond = addAggEqCondition(std::move(aggCond), makeRamTupleElement(loc), i);
                    break;
                }
            }
        } else {
            assert(arg != nullptr && "aggregator argument cannot be nullptr");
            auto value = ValueTranslator::translate(context, symbolTable, *valueIndex, arg);
            aggCond = addAggEqCondition(std::move(aggCond), std::move(value), i);
        }
    }

    // translate aggregate expression
    const auto* aggExpr = agg->getTargetExpression();
    auto expr = aggExpr ? ValueTranslator::translate(context, symbolTable, *valueIndex, aggExpr) : nullptr;

    // add Ram-Aggregation layer
    return mk<ram::Aggregate>(std::move(op), agg->getFinalType().value(),
            getConcreteRelationName(aggAtom->getQualifiedName()),
            expr ? std::move(expr) : mk<ram::UndefValue>(), aggCond ? std::move(aggCond) : mk<ram::True>(),
            level);
}

Own<ram::Operation> ClauseTranslator::instantiateMultiResultFunctor(
        Own<ram::Operation> op, const ast::IntrinsicFunctor* inf) {
    VecOwn<ram::Expression> args;
    for (auto&& x : inf->getArguments()) {
        args.push_back(ValueTranslator::translate(context, symbolTable, *valueIndex, x));
    }

    auto func_op = [&]() -> ram::NestedIntrinsicOp {
        switch (inf->getFinalOpType().value()) {
            case FunctorOp::RANGE: return ram::NestedIntrinsicOp::RANGE;
            case FunctorOp::URANGE: return ram::NestedIntrinsicOp::URANGE;
            case FunctorOp::FRANGE: return ram::NestedIntrinsicOp::FRANGE;

            default: fatal("missing case handler or bad code-gen");
        }
    };

    return mk<ram::NestedIntrinsicOperator>(func_op(), std::move(args), std::move(op), level);
}

Own<ram::Operation> ClauseTranslator::addGeneratorLevels(Own<ram::Operation> op) {
    level--;
    for (auto* cur : reverse(generators)) {
        if (auto agg = dynamic_cast<const ast::Aggregator*>(cur)) {
            op = instantiateAggregator(std::move(op), agg);
        } else if (const auto* inf = dynamic_cast<const ast::IntrinsicFunctor*>(cur)) {
            op = instantiateMultiResultFunctor(std::move(op), inf);
        } else {
            assert(false && "unhandled generator");
        }
        level--;
    }
    return op;
}

Own<ram::Operation> ClauseTranslator::addBodyLiteralConstraints(
        const ast::Clause& clause, Own<ram::Operation> op) {
    for (const auto* lit : clause.getBodyLiterals()) {
        // constraints become literals
        if (auto condition = ConstraintTranslator::translate(context, symbolTable, *valueIndex, lit)) {
            op = mk<ram::Filter>(std::move(condition), std::move(op));
        }
    }
    return op;
}

Own<ram::Condition> ClauseTranslator::createCondition(const ast::Clause& originalClause) {
    const auto head = originalClause.getHead();

    // add stopping criteria for nullary relations
    // (if it contains already the null tuple, don't re-compute)
    if (head->getArity() == 0) {
        return mk<ram::EmptinessCheck>(getConcreteRelationName(head->getQualifiedName()));
    }
    return nullptr;
}

RamDomain ClauseTranslator::getConstantRamRepresentation(
        SymbolTable& symbolTable, const ast::Constant& constant) {
    if (auto strConstant = dynamic_cast<const ast::StringConstant*>(&constant)) {
        return symbolTable.lookup(strConstant->getConstant());
    } else if (isA<ast::NilConstant>(&constant)) {
        return 0;
    } else if (auto* numConstant = dynamic_cast<const ast::NumericConstant*>(&constant)) {
        assert(numConstant->getFinalType().has_value() && "constant should have valid type");
        switch (numConstant->getFinalType().value()) {
            case ast::NumericConstant::Type::Int:
                return RamSignedFromString(numConstant->getConstant(), nullptr, 0);
            case ast::NumericConstant::Type::Uint:
                return RamUnsignedFromString(numConstant->getConstant(), nullptr, 0);
            case ast::NumericConstant::Type::Float: return RamFloatFromString(numConstant->getConstant());
        }
    }

    fatal("unaccounted-for constant");
}

Own<ram::Expression> ClauseTranslator::translateConstant(
        SymbolTable& symbolTable, const ast::Constant& constant) {
    auto rawConstant = getConstantRamRepresentation(symbolTable, constant);
    if (const auto* numericConstant = dynamic_cast<const ast::NumericConstant*>(&constant)) {
        switch (numericConstant->getFinalType().value()) {
            case ast::NumericConstant::Type::Int: return mk<ram::SignedConstant>(rawConstant);
            case ast::NumericConstant::Type::Uint: return mk<ram::UnsignedConstant>(rawConstant);
            case ast::NumericConstant::Type::Float: return mk<ram::FloatConstant>(rawConstant);
        }
        fatal("unaccounted-for constant");
    }
    return mk<ram::SignedConstant>(rawConstant);
}

Own<ram::Operation> ClauseTranslator::filterByConstraints(size_t const level,
        const std::vector<ast::Argument*>& arguments, Own<ram::Operation> op, bool constrainByFunctors) {
    auto mkFilter = [&](bool isFloatArg, Own<ram::Expression> rhs, size_t pos) {
        return mk<ram::Filter>(
                mk<ram::Constraint>(isFloatArg ? BinaryConstraintOp::FEQ : BinaryConstraintOp::EQ,
                        mk<ram::TupleElement>(level, pos), std::move(rhs)),
                std::move(op));
    };

    for (size_t pos = 0; pos < arguments.size(); pos++) {
        const auto* argument = arguments.at(pos);
        if (const auto* constant = dynamic_cast<const ast::Constant*>(argument)) {
            const auto* numericConstant = dynamic_cast<const ast::NumericConstant*>(constant);
            assert((!numericConstant || numericConstant->getFinalType().has_value()) &&
                    "numeric constant not bound to a type");
            op = mkFilter(numericConstant && numericConstant->getFinalType().value() ==
                                                     ast::NumericConstant::Type::Float,
                    translateConstant(symbolTable, *constant), pos);
        } else if (const auto* functor = dynamic_cast<const ast::Functor*>(argument)) {
            if (constrainByFunctors) {
                TypeAttribute returnType = context.getFunctorReturnType(functor);
                op = mkFilter(returnType == TypeAttribute::Float,
                        ValueTranslator::translate(context, symbolTable, *valueIndex, functor), pos);
            }
        }
    }

    return op;
}

Own<ast::Clause> ClauseTranslator::getReorderedClause(const ast::Clause& clause, const int version) const {
    const auto& plan = clause.getExecutionPlan();
    if (plan == nullptr) {
        // no plan, so reorder it according to the internal heuristic
        if (auto* reorderedClause = ast::transform::ReorderLiteralsTransformer::reorderClauseWithSips(
                    *context.getSipsMetric(), &clause)) {
            return Own<ast::Clause>(reorderedClause);
        }
        return nullptr;
    }

    // check if there's a plan for the current version
    auto orders = plan->getOrders();
    if (!contains(orders, version)) {
        return nullptr;
    }

    // get the imposed order, and change it to start at zero
    const auto& order = orders.at(version);
    std::vector<unsigned int> newOrder(order->getOrder().size());
    std::transform(order->getOrder().begin(), order->getOrder().end(), newOrder.begin(),
            [](unsigned int i) -> unsigned int { return i - 1; });

    // create a copy and fix order
    auto reorderedClause = souffle::clone(&clause);
    reorderedClause.reset(reorderAtoms(reorderedClause.get(), newOrder));

    // clear other order to fix plan
    reorderedClause->clearExecutionPlan();
    return reorderedClause;
}

void ClauseTranslator::indexNodeArguments(int nodeLevel, const std::vector<ast::Argument*>& nodeArgs) {
    for (size_t i = 0; i < nodeArgs.size(); i++) {
        const auto& arg = nodeArgs.at(i);

        // check for variable references
        if (const auto* var = dynamic_cast<const ast::Variable*>(arg)) {
            valueIndex->addVarReference(*var, nodeLevel, i);
        }

        // check for nested records
        if (auto rec = dynamic_cast<const ast::RecordInit*>(arg)) {
            // introduce new nesting level for unpack
            op_nesting.push_back(rec);
            valueIndex->setRecordDefinition(*rec, nodeLevel, i);
            indexNodeArguments(level++, rec->getArguments());
        }
    }
}

std::optional<int> ClauseTranslator::addGenerator(const ast::Argument& arg) {
    int aggLoc = level++;
    generators.push_back(&arg);
    valueIndex->setGeneratorLoc(arg, Location({aggLoc, 0}));
    return aggLoc;
}

void ClauseTranslator::indexAtoms(const ast::Clause& clause) {
    for (const auto* atom : ast::getBodyLiterals<ast::Atom>(clause)) {
        // give the atom the current level
        op_nesting.push_back(atom);
        indexNodeArguments(level++, atom->getArguments());
    }
}

void ClauseTranslator::indexAggregator(const ast::Aggregator& agg) {
    auto aggLoc = valueIndex->getGeneratorLoc(agg);

    // Get the single body atom inside the aggregator
    const auto& aggBodyAtoms =
            filter(agg.getBodyLiterals(), [&](const ast::Literal* lit) { return isA<ast::Atom>(lit); });
    assert(aggBodyAtoms.size() == 1 && "exactly one atom should exist per aggregator body");
    const auto* aggAtom = static_cast<const ast::Atom*>(aggBodyAtoms.at(0));

    // Add the variable references inside this atom
    const auto& aggAtomArgs = aggAtom->getArguments();
    for (size_t i = 0; i < aggAtomArgs.size(); i++) {
        const auto* arg = aggAtomArgs.at(i);
        if (const auto* var = dynamic_cast<const ast::Variable*>(arg)) {
            valueIndex->addVarReference(*var, aggLoc.identifier, (int)i);
        }
    }
}

void ClauseTranslator::indexAggregators(const ast::Clause& clause) {
    visitDepthFirst(clause, [&](const ast::Aggregator& agg) { addGenerator(agg); });

    // index aggregator atoms
    visitDepthFirst(clause, [&](const ast::Aggregator& agg) { indexAggregator(agg); });

    // add aggregator introductions
    visitDepthFirst(clause, [&](const ast::BinaryConstraint& bc) {
        if (!isEqConstraint(bc.getBaseOperator())) return;
        const auto* lhs = dynamic_cast<const ast::Variable*>(bc.getLHS());
        const auto* rhs = dynamic_cast<const ast::Aggregator*>(bc.getRHS());
        if (lhs == nullptr || rhs == nullptr) return;
        valueIndex->addVarReference(*lhs, valueIndex->getGeneratorLoc(*rhs));
    });
}

void ClauseTranslator::indexMultiResultFunctors(const ast::Clause& clause) {
    visitDepthFirst(clause, [&](const ast::IntrinsicFunctor& func) {
        if (ast::analysis::FunctorAnalysis::isMultiResult(func)) {
            addGenerator(func);
        }
    });

    // add multi-result functor introductions
    visitDepthFirst(clause, [&](const ast::BinaryConstraint& bc) {
        if (!isEqConstraint(bc.getBaseOperator())) return;
        const auto* lhs = dynamic_cast<const ast::Variable*>(bc.getLHS());
        const auto* rhs = dynamic_cast<const ast::IntrinsicFunctor*>(bc.getRHS());
        if (lhs == nullptr || rhs == nullptr) return;
        if (!ast::analysis::FunctorAnalysis::isMultiResult(*rhs)) return;
        valueIndex->addVarReference(*lhs, valueIndex->getGeneratorLoc(*rhs));
    });
}

void ClauseTranslator::indexClause(const ast::Clause& clause) {
    indexAtoms(clause);
    indexAggregators(clause);
    indexMultiResultFunctors(clause);
}

}  // namespace souffle::ast2ram
