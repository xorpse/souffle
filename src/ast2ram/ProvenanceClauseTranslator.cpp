/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2018 The Souffle Developers. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file ProvenanceClauseTranslator.cpp
 *
 * Clause translator when provenance is used
 *
 ***********************************************************************/

#include "ast2ram/ProvenanceClauseTranslator.h"
#include "Global.h"
#include "ast/Atom.h"
#include "ast/BinaryConstraint.h"
#include "ast/Clause.h"
#include "ast/utility/Utils.h"
#include "ast2ram/AstToRamTranslator.h"
#include "ast2ram/ConstraintTranslator.h"
#include "ast2ram/ValueTranslator.h"
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

namespace souffle::ast2ram {

Own<ram::Statement> ProvenanceClauseTranslator::generateClause(const TranslatorContext& context,
        SymbolTable& symbolTable, const ast::Clause& clause, int /* version */) {
    return ProvenanceClauseTranslator(context, symbolTable).translateClause(clause);
}

Own<ram::Operation> ProvenanceClauseTranslator::addNegatedAtom(
        Own<ram::Operation> op, const ast::Atom* atom) const {
    size_t auxiliaryArity = context.getEvaluationArity(atom);
    assert(auxiliaryArity <= atom->getArity() && "auxiliary arity out of bounds");
    size_t arity = atom->getArity() - auxiliaryArity;

    VecOwn<ram::Expression> values;

    auto args = atom->getArguments();
    for (size_t i = 0; i < arity; i++) {
        values.push_back(ValueTranslator::translate(context, symbolTable, *valueIndex, args[i]));
    }

    // undefined value for rule number
    values.push_back(mk<ram::UndefValue>());
    // add the height annotation for provenanceNotExists
    for (size_t height = 1; height < auxiliaryArity; height++) {
        values.push_back(ValueTranslator::translate(context, symbolTable, *valueIndex, args[arity + height]));
    }

    return mk<ram::Filter>(mk<ram::Negation>(mk<ram::ProvenanceExistenceCheck>(
                                   getConcreteRelationName(atom->getQualifiedName()), std::move(values))),
            std::move(op));
}

Own<ram::Statement> ProvenanceClauseTranslator::createRamFactQuery(const ast::Clause& clause) const {
    assert(isFact(clause) && "clause should be fact");
    assert(!isRecursive() && "recursive clauses cannot have facts");
    return mk<ram::Query>(generateReturnInstantiatedValues(clause));
}

Own<ram::Statement> ProvenanceClauseTranslator::createRamRuleQuery(const ast::Clause& clause) {
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

Own<ram::Operation> ProvenanceClauseTranslator::addBodyLiteralConstraints(
        const ast::Clause& clause, Own<ram::Operation> op) const {
    for (const auto* lit : clause.getBodyLiterals()) {
        // constraints become literals
        if (auto condition = ConstraintTranslator::translate(context, symbolTable, *valueIndex, lit)) {
            op = mk<ram::Filter>(std::move(condition), std::move(op));
        }
    }

    // index of level argument in argument list
    size_t auxiliaryArity = context.getAuxiliaryArity(clause.getHead());
    size_t levelIndex = clause.getHead()->getArguments().size() - auxiliaryArity;

    // add level constraints, i.e., that each body literal has height less than that of the head atom
    for (const auto* lit : clause.getBodyLiterals()) {
        if (const auto* atom = dynamic_cast<const ast::Atom*>(lit)) {
            // arity - 1 is the level number in body atoms
            auto arity = atom->getArity();
            auto atomArgs = atom->getArguments();
            auto valLHS =
                    ValueTranslator::translate(context, symbolTable, *valueIndex, atomArgs.at(arity - 1));

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

Own<ram::Operation> ProvenanceClauseTranslator::generateReturnInstantiatedValues(
        const ast::Clause& clause) const {
    VecOwn<ram::Expression> values;

    // get all values in the body
    for (ast::Literal* lit : clause.getBodyLiterals()) {
        if (auto atom = dynamic_cast<ast::Atom*>(lit)) {
            for (ast::Argument* arg : atom->getArguments()) {
                values.push_back(ValueTranslator::translate(context, symbolTable, *valueIndex, arg));
            }
        } else if (auto neg = dynamic_cast<ast::Negation*>(lit)) {
            for (ast::Argument* arg : neg->getAtom()->getArguments()) {
                values.push_back(ValueTranslator::translate(context, symbolTable, *valueIndex, arg));
            }
        } else if (auto con = dynamic_cast<ast::BinaryConstraint*>(lit)) {
            values.push_back(ValueTranslator::translate(context, symbolTable, *valueIndex, con->getLHS()));
            values.push_back(ValueTranslator::translate(context, symbolTable, *valueIndex, con->getRHS()));
        }
    }

    // final provenance negation
    if (isRecursive()) {
        const auto* head = clause.getHead();
        size_t auxiliaryArity = context.getEvaluationArity(head);
        for (size_t i = 0; i < head->getArguments().size() - auxiliaryArity; i++) {
            auto arg = head->getArguments().at(i);
            values.push_back(ValueTranslator::translate(context, symbolTable, *valueIndex, arg));
        }
        for (size_t i = 0; i < auxiliaryArity; ++i) {
            values.push_back(mk<ram::SignedConstant>(-1));
        }
    }

    return mk<ram::SubroutineReturn>(std::move(values));
}

}  // namespace souffle::ast2ram
