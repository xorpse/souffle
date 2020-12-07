/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2018 The Souffle Developers. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file ProvenanceTranslator.h
 *
 ***********************************************************************/

#include "ast2ram/ProvenanceTranslator.h"
#include "ast/BinaryConstraint.h"
#include "ast/Clause.h"
#include "ast/Constraint.h"
#include "ast/SubroutineArgument.h"
#include "ast/analysis/AuxArity.h"
#include "ast/analysis/RelationDetailCache.h"
#include "ast/utility/Utils.h"
#include "ast/utility/Visitor.h"
#include "ast2ram/ConstraintTranslator.h"
#include "ast2ram/ProvenanceClauseTranslator.h"
#include "ast2ram/ValueTranslator.h"
#include "ast2ram/utility/TranslatorContext.h"
#include "ast2ram/utility/Utils.h"
#include "ast2ram/utility/ValueIndex.h"
#include "ram/ExistenceCheck.h"
#include "ram/Filter.h"
#include "ram/Negation.h"
#include "ram/Query.h"
#include "ram/Sequence.h"
#include "ram/SignedConstant.h"
#include "ram/SubroutineReturn.h"
#include "ram/UndefValue.h"
#include "souffle/BinaryConstraintOps.h"
#include "souffle/utility/StringUtil.h"
#include <sstream>

namespace souffle::ast2ram {

Own<ram::Sequence> ProvenanceTranslator::generateProgram(const ast::TranslationUnit& translationUnit) {
    // do the regular translation
    auto ramProgram = AstToRamTranslator::generateProgram(translationUnit);

    // add subroutines for each clause
    addProvenanceClauseSubroutines(context->getProgram());

    return ramProgram;
}

Own<ram::Statement> ProvenanceTranslator::generateClearExpiredRelations(
        const std::set<const ast::Relation*>& /* expiredRelations */) const {
    // relations should be preserved if provenance is enabled
    return mk<ram::Sequence>();
}

void ProvenanceTranslator::addProvenanceClauseSubroutines(const ast::Program* program) {
    visitDepthFirst(*program, [&](const ast::Clause& clause) {
        std::stringstream relName;
        relName << clause.getHead()->getQualifiedName();

        // do not add subroutines for info relations or facts
        if (isPrefix("info", relName.str()) || clause.getBodyLiterals().empty()) {
            return;
        }

        std::string subroutineLabel =
                relName.str() + "_" + std::to_string(getClauseNum(program, &clause)) + "_subproof";
        addRamSubroutine(subroutineLabel, makeSubproofSubroutine(clause));

        std::string negationSubroutineLabel =
                relName.str() + "_" + std::to_string(getClauseNum(program, &clause)) + "_negation_subproof";
        addRamSubroutine(negationSubroutineLabel, makeNegationSubproofSubroutine(clause));
    });
}

/** make a subroutine to search for subproofs */
Own<ram::Statement> ProvenanceTranslator::makeSubproofSubroutine(const ast::Clause& clause) {
    auto intermediateClause = mk<ast::Clause>(souffle::clone(clause.getHead()));

    // create a clone where all the constraints are moved to the end
    for (auto bodyLit : clause.getBodyLiterals()) {
        // first add all the things that are not constraints
        if (!isA<ast::Constraint>(bodyLit)) {
            intermediateClause->addToBody(souffle::clone(bodyLit));
        }
    }

    // now add all constraints
    for (auto bodyLit : ast::getBodyLiterals<ast::Constraint>(clause)) {
        intermediateClause->addToBody(souffle::clone(bodyLit));
    }

    // name unnamed variables
    nameUnnamedVariables(intermediateClause.get());

    // add constraint for each argument in head of atom
    ast::Atom* head = intermediateClause->getHead();
    size_t auxiliaryArity = context->getAuxiliaryArity(head);
    auto args = head->getArguments();
    for (size_t i = 0; i < head->getArity() - auxiliaryArity; i++) {
        auto arg = args[i];

        if (auto var = dynamic_cast<ast::Variable*>(arg)) {
            // FIXME: float equiv (`FEQ`)
            auto constraint = mk<ast::BinaryConstraint>(
                    BinaryConstraintOp::EQ, souffle::clone(var), mk<ast::SubroutineArgument>(i));
            constraint->setFinalType(BinaryConstraintOp::EQ);
            intermediateClause->addToBody(std::move(constraint));
        } else if (auto func = dynamic_cast<ast::Functor*>(arg)) {
            TypeAttribute returnType;
            if (auto* inf = dynamic_cast<ast::IntrinsicFunctor*>(func)) {
                assert(inf->getFinalReturnType().has_value() && "functor has missing return type");
                returnType = inf->getFinalReturnType().value();
            } else if (auto* udf = dynamic_cast<ast::UserDefinedFunctor*>(func)) {
                assert(udf->getFinalReturnType().has_value() && "functor has missing return type");
                returnType = udf->getFinalReturnType().value();
            } else {
                assert(false && "unexpected functor type");
            }
            auto opEq = returnType == TypeAttribute::Float ? BinaryConstraintOp::FEQ : BinaryConstraintOp::EQ;
            auto constraint =
                    mk<ast::BinaryConstraint>(opEq, souffle::clone(func), mk<ast::SubroutineArgument>(i));
            constraint->setFinalType(opEq);
            intermediateClause->addToBody(std::move(constraint));
        } else if (auto rec = dynamic_cast<ast::RecordInit*>(arg)) {
            auto constraint = mk<ast::BinaryConstraint>(
                    BinaryConstraintOp::EQ, souffle::clone(rec), mk<ast::SubroutineArgument>(i));
            constraint->setFinalType(BinaryConstraintOp::EQ);
            intermediateClause->addToBody(std::move(constraint));
        } else if (auto adt = dynamic_cast<ast::BranchInit*>(arg)) {
            // TODO: fill this out like record arguments
        }
    }

    // index of level argument in argument list
    size_t levelIndex = head->getArguments().size() - auxiliaryArity;

    // add level constraints, i.e., that each body literal has height less than that of the head atom
    const auto& bodyLiterals = intermediateClause->getBodyLiterals();
    for (auto lit : bodyLiterals) {
        if (auto atom = dynamic_cast<ast::Atom*>(lit)) {
            auto arity = atom->getArity();
            auto atomArgs = atom->getArguments();
            // arity - 1 is the level number in body atoms
            auto constraint = mk<ast::BinaryConstraint>(BinaryConstraintOp::LT,
                    souffle::clone(atomArgs[arity - 1]), mk<ast::SubroutineArgument>(levelIndex));
            constraint->setFinalType(BinaryConstraintOp::LT);
            intermediateClause->addToBody(std::move(constraint));
        }
    }
    return ProvenanceClauseTranslator::generateClause(*context, *symbolTable, *intermediateClause, clause);
}

/** make a subroutine to search for subproofs for the non-existence of a tuple */
Own<ram::Statement> ProvenanceTranslator::makeNegationSubproofSubroutine(const ast::Clause& clause) {
    // TODO (taipan-snake): Currently we only deal with atoms (no constraints or negations or aggregates
    // or anything else...)
    //
    // The resulting subroutine looks something like this:
    // IF (arg(0), arg(1), _, _) IN rel_1:
    //   return 1
    // IF (arg(0), arg(1), _ ,_) NOT IN rel_1:
    //   return 0
    // ...

    // clone clause for mutation, rearranging constraints to be at the end
    auto clauseReplacedAggregates = mk<ast::Clause>(souffle::clone(clause.getHead()));

    // create a clone where all the constraints are moved to the end
    for (auto bodyLit : clause.getBodyLiterals()) {
        // first add all the things that are not constraints
        if (!isA<ast::Constraint>(bodyLit)) {
            clauseReplacedAggregates->addToBody(souffle::clone(bodyLit));
        }
    }

    // now add all constraints
    for (auto bodyLit : ast::getBodyLiterals<ast::Constraint>(clause)) {
        clauseReplacedAggregates->addToBody(souffle::clone(bodyLit));
    }

    struct AggregatesToVariables : public ast::NodeMapper {
        mutable int aggNumber{0};

        AggregatesToVariables() = default;

        Own<ast::Node> operator()(Own<ast::Node> node) const override {
            if (dynamic_cast<ast::Aggregator*>(node.get()) != nullptr) {
                return mk<ast::Variable>("agg_" + std::to_string(aggNumber++));
            }

            node->apply(*this);
            return node;
        }
    };

    AggregatesToVariables aggToVar;
    clauseReplacedAggregates->apply(aggToVar);

    // build a vector of unique variables
    std::vector<const ast::Variable*> uniqueVariables;

    visitDepthFirst(*clauseReplacedAggregates, [&](const ast::Variable& var) {
        if (var.getName().find("@level_num") == std::string::npos) {
            // use find_if since uniqueVariables stores pointers, and we need to dereference the pointer to
            // check equality
            if (std::find_if(uniqueVariables.begin(), uniqueVariables.end(),
                        [&](const ast::Variable* v) { return *v == var; }) == uniqueVariables.end()) {
                uniqueVariables.push_back(&var);
            }
        }
    });

    // a mapper to replace variables with subroutine arguments
    struct VariablesToArguments : public ast::NodeMapper {
        const std::vector<const ast::Variable*>& uniqueVariables;

        VariablesToArguments(const std::vector<const ast::Variable*>& uniqueVariables)
                : uniqueVariables(uniqueVariables) {}

        Own<ast::Node> operator()(Own<ast::Node> node) const override {
            // replace unknown variables
            if (auto varPtr = dynamic_cast<const ast::Variable*>(node.get())) {
                if (varPtr->getName().find("@level_num") == std::string::npos) {
                    size_t argNum = std::find_if(uniqueVariables.begin(), uniqueVariables.end(),
                                            [&](const ast::Variable* v) { return *v == *varPtr; }) -
                                    uniqueVariables.begin();

                    return mk<ast::SubroutineArgument>(argNum);
                } else {
                    return mk<ast::UnnamedVariable>();
                }
            }

            // apply recursive
            node->apply(*this);

            // otherwise nothing
            return node;
        }
    };

    auto makeRamAtomExistenceCheck = [&](ast::Atom* atom) {
        auto relName = getConcreteRelationName(atom->getQualifiedName());
        size_t auxiliaryArity = context->getAuxiliaryArity(atom);

        // translate variables to subroutine arguments
        VariablesToArguments varsToArgs(uniqueVariables);
        atom->apply(varsToArgs);

        // construct a query
        VecOwn<ram::Expression> query;
        auto atomArgs = atom->getArguments();

        // add each value (subroutine argument) to the search query
        for (size_t i = 0; i < atom->getArity() - auxiliaryArity; i++) {
            auto arg = atomArgs[i];
            query.push_back(ValueTranslator::translate(*context, *symbolTable, ValueIndex(), arg));
        }

        // fill up query with nullptrs for the provenance columns
        for (size_t i = 0; i < auxiliaryArity; i++) {
            query.push_back(mk<ram::UndefValue>());
        }

        // ensure the length of query tuple is correct
        assert(query.size() == atom->getArity() && "wrong query tuple size");

        // create existence checks to check if the tuple exists or not
        return mk<ram::ExistenceCheck>(relName, std::move(query));
    };

    auto makeRamReturnTrue = [&]() {
        VecOwn<ram::Expression> returnTrue;
        returnTrue.push_back(mk<ram::SignedConstant>(1));
        return mk<ram::SubroutineReturn>(std::move(returnTrue));
    };

    auto makeRamReturnFalse = [&]() {
        VecOwn<ram::Expression> returnFalse;
        returnFalse.push_back(mk<ram::SignedConstant>(0));
        return mk<ram::SubroutineReturn>(std::move(returnFalse));
    };

    // the structure of this subroutine is a sequence where each nested statement is a search in each
    // relation
    VecOwn<ram::Statement> searchSequence;

    // make a copy so that when we mutate clause, pointers to objects in newClause are not affected
    auto newClause = souffle::clone(clauseReplacedAggregates);

    // go through each body atom and create a return
    size_t litNumber = 0;
    for (const auto& lit : newClause->getBodyLiterals()) {
        if (auto atom = dynamic_cast<ast::Atom*>(lit)) {
            auto existenceCheck = makeRamAtomExistenceCheck(atom);
            auto negativeExistenceCheck = mk<ram::Negation>(souffle::clone(existenceCheck));

            // create a ram::Query to return true/false
            appendStmt(searchSequence,
                    mk<ram::Query>(mk<ram::Filter>(std::move(existenceCheck), makeRamReturnTrue())));
            appendStmt(searchSequence,
                    mk<ram::Query>(mk<ram::Filter>(std::move(negativeExistenceCheck), makeRamReturnFalse())));
        } else if (auto neg = dynamic_cast<ast::Negation*>(lit)) {
            auto atom = neg->getAtom();
            auto existenceCheck = makeRamAtomExistenceCheck(atom);
            auto negativeExistenceCheck = mk<ram::Negation>(souffle::clone(existenceCheck));

            // create a ram::Query to return true/false
            appendStmt(searchSequence,
                    mk<ram::Query>(mk<ram::Filter>(std::move(existenceCheck), makeRamReturnFalse())));
            appendStmt(searchSequence,
                    mk<ram::Query>(mk<ram::Filter>(std::move(negativeExistenceCheck), makeRamReturnTrue())));
        } else if (auto con = dynamic_cast<ast::Constraint*>(lit)) {
            VariablesToArguments varsToArgs(uniqueVariables);
            con->apply(varsToArgs);

            // translate to a ram::Condition
            auto condition = ConstraintTranslator::translate(*context, *symbolTable, ValueIndex(), con);
            auto negativeCondition = mk<ram::Negation>(souffle::clone(condition));

            appendStmt(searchSequence,
                    mk<ram::Query>(mk<ram::Filter>(std::move(condition), makeRamReturnTrue())));
            appendStmt(searchSequence,
                    mk<ram::Query>(mk<ram::Filter>(std::move(negativeCondition), makeRamReturnFalse())));
        }

        litNumber++;
    }

    return mk<ram::Sequence>(std::move(searchSequence));
}

}  // namespace souffle::ast2ram
