/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2018 The Souffle Developers. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file SubproofGenerator.cpp
 *
 ***********************************************************************/

#include "ast2ram/provenance/SubproofGenerator.h"
#include "Global.h"
#include "ast/Atom.h"
#include "ast/BinaryConstraint.h"
#include "ast/Clause.h"
#include "ast/utility/Utils.h"
#include "ast2ram/ConstraintTranslator.h"
#include "ast2ram/ValueTranslator.h"
#include "ast2ram/provenance/ClauseTranslator.h"
#include "ast2ram/utility/TranslatorContext.h"
#include "ast2ram/utility/Utils.h"
#include "ast2ram/utility/ValueIndex.h"
#include "ram/Condition.h"
#include "ram/Constraint.h"
#include "ram/Filter.h"
#include "ram/Negation.h"
#include "ram/ProvenanceExistenceCheck.h"
#include "ram/Query.h"
#include "ram/Relation.h"
#include "ram/SignedConstant.h"
#include "ram/Statement.h"
#include "ram/SubroutineArgument.h"
#include "ram/SubroutineReturn.h"
#include "ram/UndefValue.h"

namespace souffle::ast2ram::provenance {

SubproofGenerator::SubproofGenerator(const TranslatorContext& context, SymbolTable& symbolTable)
        : ast2ram::provenance::ClauseTranslator(context, symbolTable) {}

SubproofGenerator::~SubproofGenerator() = default;

Own<ram::Operation> SubproofGenerator::addNegatedAtom(Own<ram::Operation> op, const ast::Atom* atom) const {
    // Add direct values
    VecOwn<ram::Expression> values;
    for (const auto* arg : atom->getArguments()) {
        values.push_back(context.translateValue(symbolTable, *valueIndex, arg));
    }

    // Undefined value for rule number
    values.push_back(mk<ram::UndefValue>());

    // Height annotation for provenanceNotExists
    // TODO: get the correct height here this is not correct
    values.push_back(mk<ram::UndefValue>());

    return mk<ram::Filter>(mk<ram::Negation>(mk<ram::ProvenanceExistenceCheck>(
                                   getConcreteRelationName(atom->getQualifiedName()), std::move(values))),
            std::move(op));
}

Own<ram::Statement> SubproofGenerator::createRamFactQuery(const ast::Clause& clause) const {
    assert(isFact(clause) && "clause should be fact");
    assert(!isRecursive() && "recursive clauses cannot have facts");
    return mk<ram::Query>(generateReturnInstantiatedValues(clause));
}

Own<ram::Statement> SubproofGenerator::createRamRuleQuery(const ast::Clause& clause) {
    assert(isRule(clause) && "clause should be rule");

    // Index all variables and generators in the clause
    valueIndex = mk<ValueIndex>();
    indexClause(clause);

    // Set up the RAM statement bottom-up
    auto op = generateReturnInstantiatedValues(clause);
    op = addVariableBindingConstraints(std::move(op));
    op = addBodyLiteralConstraints(clause, std::move(op));
    op = addGeneratorLevels(std::move(op), clause);
    op = addVariableIntroductions(clause, std::move(op));
    return mk<ram::Query>(std::move(op));
}

Own<ram::Operation> SubproofGenerator::addBodyLiteralConstraints(
        const ast::Clause& clause, Own<ram::Operation> op) const {
    // Add all non-constraints, and then constraints
    std::vector<const ast::Constraint*> constraints;
    for (const auto* lit : clause.getBodyLiterals()) {
        if (const auto* constraint = dynamic_cast<const ast::Constraint*>(lit)) {
            constraints.push_back(constraint);
            continue;
        }

        if (auto condition = context.translateConstraint(symbolTable, *valueIndex, lit)) {
            op = mk<ram::Filter>(std::move(condition), std::move(op));
        }
    }

    for (const auto* constraint : constraints) {
        if (auto condition = context.translateConstraint(symbolTable, *valueIndex, constraint)) {
            op = mk<ram::Filter>(std::move(condition), std::move(op));
        }
    }

    // index of level argument in argument list
    const auto* head = clause.getHead();
    const auto& headArgs = head->getArguments();
    size_t levelIndex = clause.getHead()->getArguments().size();
    for (size_t i = 0; i < head->getArity(); i++) {
        auto arg = headArgs.at(i);
        if (const auto* var = dynamic_cast<const ast::Variable*>(arg)) {
            // FIXME: float equiv (`FEQ`)
            auto lhs = context.translateValue(symbolTable, *valueIndex, var);
            auto constraint = mk<ram::Constraint>(
                    BinaryConstraintOp::EQ, std::move(lhs), mk<ram::SubroutineArgument>(i));
            op = mk<ram::Filter>(std::move(constraint), std::move(op));
        } else if (const auto* func = dynamic_cast<const ast::Functor*>(arg)) {
            TypeAttribute returnType = context.getFunctorReturnType(func);
            auto opEq = returnType == TypeAttribute::Float ? BinaryConstraintOp::FEQ : BinaryConstraintOp::EQ;
            auto lhs = context.translateValue(symbolTable, *valueIndex, func);
            auto constraint = mk<ram::Constraint>(opEq, std::move(lhs), mk<ram::SubroutineArgument>(i));
            op = mk<ram::Filter>(std::move(constraint), std::move(op));
        } else if (const auto* rec = dynamic_cast<const ast::RecordInit*>(arg)) {
            auto lhs = context.translateValue(symbolTable, *valueIndex, rec);
            auto constraint = mk<ram::Constraint>(
                    BinaryConstraintOp::EQ, std::move(lhs), mk<ram::SubroutineArgument>(i));
            op = mk<ram::Filter>(std::move(constraint), std::move(op));
        } else if (const auto* adt = dynamic_cast<const ast::BranchInit*>(arg)) {
            // TODO (azreika): fill this out like record arguments
            assert(false && adt && "unhandled");
        }
    }

    // add constraint for each argument in head of atom

    // add level constraints, i.e., that each body literal has height less than that of the head atom
    for (const auto* lit : clause.getBodyLiterals()) {
        if (const auto* atom = dynamic_cast<const ast::Atom*>(lit)) {
            size_t levelNumber = 0;
            while (getAtomOrdering(clause).at(levelNumber) != atom) {
                levelNumber++;
                assert(levelNumber < getAtomOrdering(clause).size());
            }
            auto varRepr = mk<ast::Variable>("@level_num_" + std::to_string(levelNumber));
            auto valLHS = context.translateValue(symbolTable, *valueIndex, varRepr.get());

            // add the constraint
            auto constraint = mk<ram::Constraint>(
                    BinaryConstraintOp::LT, std::move(valLHS), mk<ram::SubroutineArgument>(levelIndex));
            op = mk<ram::Filter>(std::move(constraint), std::move(op));
        }
    }

    if (isRecursive()) {
        if (clause.getHead()->getArity() > 0) {
            // also negate the head
            op = addNegatedAtom(std::move(op), clause.getHead());
        }

        // also add in prev stuff
        for (size_t i = version + 1; i < sccAtoms.size(); i++) {
            op = addNegatedDeltaAtom(std::move(op), sccAtoms.at(i));
        }
    }

    return op;
}

Own<ram::Operation> SubproofGenerator::generateReturnInstantiatedValues(const ast::Clause& clause) const {
    VecOwn<ram::Expression> values;

    // get all values in the body
    for (const auto* lit : clause.getBodyLiterals()) {
        if (auto atom = dynamic_cast<const ast::Atom*>(lit)) {
            for (const auto* arg : atom->getArguments()) {
                values.push_back(context.translateValue(symbolTable, *valueIndex, arg));
            }
            // TODO: put helper methods for these variables
            size_t levelNumber = 0;
            while (getAtomOrdering(clause).at(levelNumber) != atom) {
                levelNumber++;
                assert(levelNumber < getAtomOrdering(clause).size());
            }
            auto levelVarRepr = mk<ast::Variable>("@level_num_" + std::to_string(levelNumber));
            auto ruleNumRepr = mk<ast::Variable>("@rule_num_" + std::to_string(levelNumber));
            auto level = context.translateValue(symbolTable, *valueIndex, levelVarRepr.get());
            auto ruleNum = context.translateValue(symbolTable, *valueIndex, ruleNumRepr.get());

            values.push_back(std::move(ruleNum));
            values.push_back(std::move(level));
        } else if (auto neg = dynamic_cast<const ast::Negation*>(lit)) {
            for (ast::Argument* arg : neg->getAtom()->getArguments()) {
                values.push_back(context.translateValue(symbolTable, *valueIndex, arg));
            }
            values.push_back(mk<ram::UndefValue>());
            values.push_back(mk<ram::UndefValue>());
        }
    }

    for (const auto* constraint : ast::getBodyLiterals<const ast::BinaryConstraint>(clause)) {
        values.push_back(context.translateValue(symbolTable, *valueIndex, constraint->getLHS()));
        values.push_back(context.translateValue(symbolTable, *valueIndex, constraint->getRHS()));
    }

    // final provenance negation
    if (isRecursive()) {
        const auto* head = clause.getHead();
        for (size_t i = 0; i < head->getArguments().size(); i++) {
            auto arg = head->getArguments().at(i);
            values.push_back(context.translateValue(symbolTable, *valueIndex, arg));
        }
        values.push_back(mk<ram::SignedConstant>(-1));
        values.push_back(mk<ram::SignedConstant>(-1));
    }

    const auto* head = clause.getHead();
    const auto& headArgs = head->getArguments();
    size_t levelIndex = clause.getHead()->getArguments().size();
    for (size_t i = 0; i < head->getArity(); i++) {
        auto arg = headArgs.at(i);
        if (const auto* var = dynamic_cast<const ast::Variable*>(arg)) {
            values.push_back(context.translateValue(symbolTable, *valueIndex, var));
            values.push_back(mk<ram::SubroutineArgument>(i));
        } else if (const auto* func = dynamic_cast<const ast::Functor*>(arg)) {
            values.push_back(context.translateValue(symbolTable, *valueIndex, func));
            values.push_back(mk<ram::SubroutineArgument>(i));
        } else if (const auto* rec = dynamic_cast<const ast::RecordInit*>(arg)) {
            values.push_back(context.translateValue(symbolTable, *valueIndex, rec));
            values.push_back(mk<ram::SubroutineArgument>(i));
        } else if (const auto* adt = dynamic_cast<const ast::BranchInit*>(arg)) {
            // TODO (azreika): fill this out like record arguments
            assert(false && adt && "unhandled");
        }
    }

    for (const auto* lit : clause.getBodyLiterals()) {
        if (const auto* atom = dynamic_cast<const ast::Atom*>(lit)) {
            auto arity = atom->getArity();
            auto atomArgs = atom->getArguments();
            values.push_back(context.translateValue(symbolTable, *valueIndex, atomArgs.at(arity - 1)));
            values.push_back(mk<ram::SubroutineArgument>(levelIndex));
        }
    }

    return mk<ram::SubroutineReturn>(std::move(values));
}

}  // namespace souffle::ast2ram::provenance
