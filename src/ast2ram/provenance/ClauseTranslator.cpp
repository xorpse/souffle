/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2018 The Souffle Developers. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file ClauseTranslator.cpp
 *
 ***********************************************************************/

#include "ast2ram/provenance/ClauseTranslator.h"
#include "ast/Argument.h"
#include "ast/Atom.h"
#include "ast/Clause.h"
#include "ast/IntrinsicFunctor.h"
#include "ast/NumericConstant.h"
#include "ast/Variable.h"
#include "ast/utility/Utils.h"
#include "ast2ram/utility/TranslatorContext.h"
#include "ast2ram/utility/Utils.h"
#include "ram/EmptinessCheck.h"
#include "ram/ExistenceCheck.h"
#include "ram/Filter.h"
#include "ram/GuardedProject.h"
#include "ram/Negation.h"
#include "ram/Operation.h"
#include "ram/ProvenanceExistenceCheck.h"
#include "ram/SignedConstant.h"
#include "ram/UndefValue.h"

namespace souffle::ast2ram::provenance {

Own<ram::Operation> ClauseTranslator::addNegatedDeltaAtom(
        Own<ram::Operation> op, const ast::Atom* atom) const {
    size_t auxiliaryArity = context.getEvaluationArity(atom);
    assert(auxiliaryArity <= atom->getArity() && "auxiliary arity out of bounds");
    size_t arity = atom->getArity() - auxiliaryArity;
    std::string name = getDeltaRelationName(atom->getQualifiedName());

    if (arity == 0) {
        // for a nullary, negation is a simple emptiness check
        return mk<ram::Filter>(mk<ram::EmptinessCheck>(name), std::move(op));
    }

    // else, we construct the atom and create a negation
    VecOwn<ram::Expression> values;
    auto args = atom->getArguments();
    for (size_t i = 0; i < arity; i++) {
        values.push_back(context.translateValue(symbolTable, *valueIndex, args[i]));
    }
    for (size_t i = 0; i < auxiliaryArity; i++) {
        values.push_back(mk<ram::UndefValue>());
    }

    return mk<ram::Filter>(
            mk<ram::Negation>(mk<ram::ExistenceCheck>(name, std::move(values))), std::move(op));
}

Own<ram::Operation> ClauseTranslator::addNegatedAtom(Own<ram::Operation> op, const ast::Atom* atom) const {
    size_t auxiliaryArity = context.getEvaluationArity(atom);
    assert(auxiliaryArity <= atom->getArity() && "auxiliary arity out of bounds");
    size_t arity = atom->getArity() - auxiliaryArity;

    VecOwn<ram::Expression> values;

    auto args = atom->getArguments();
    for (size_t i = 0; i < arity; i++) {
        values.push_back(context.translateValue(symbolTable, *valueIndex, args[i]));
    }

    // undefined value for rule number
    values.push_back(mk<ram::UndefValue>());
    // add the height annotation for provenanceNotExists
    for (size_t height = 1; height < auxiliaryArity; height++) {
        values.push_back(context.translateValue(symbolTable, *valueIndex, args[arity + height]));
    }

    return mk<ram::Filter>(mk<ram::Negation>(mk<ram::ProvenanceExistenceCheck>(
                                   getConcreteRelationName(atom->getQualifiedName()), std::move(values))),
            std::move(op));
}

Own<ast::Argument> ClauseTranslator::getNextLevelNumber(const std::vector<ast::Argument*>& levels) {
    if (levels.empty()) return mk<ast::NumericConstant>(0);

    auto max = levels.size() == 1 ? Own<ast::Argument>(levels[0])
                                  : mk<ast::IntrinsicFunctor>("max",
                                            map(levels, [](auto&& x) { return Own<ast::Argument>(x); }));

    return mk<ast::IntrinsicFunctor>("+", std::move(max), mk<ast::NumericConstant>(1));
}

Own<ram::Operation> ClauseTranslator::createProjection(const ast::Clause& clause) const {
    const auto head = clause.getHead();
    auto headRelationName = getClauseAtomName(clause, head);

    VecOwn<ram::Expression> values;
    for (const auto* arg : head->getArguments()) {
        values.push_back(context.translateValue(symbolTable, *valueIndex, arg));
    }

    // add rule number + level number
    if (isFact(clause)) {
        values.push_back(mk<ram::SignedConstant>(0));
        values.push_back(mk<ram::SignedConstant>(0));
    } else {
        values.push_back(mk<ram::SignedConstant>(ast::getClauseNum(context.getProgram(), &clause)));
        std::vector<ast::Argument*> bodyLevels;
        const auto& bodyLiterals = clause.getBodyLiterals();
        for (size_t i = 0; i < bodyLiterals.size(); i++) {
            const auto* lit = bodyLiterals.at(i);
            if (isA<ast::Atom>(lit)) {
                bodyLevels.push_back(new ast::Variable("@level_num_" + std::to_string(i)));
            }
        }
        auto levelNumber = getNextLevelNumber(bodyLevels);
        values.push_back(context.translateValue(symbolTable, *valueIndex, levelNumber.get()));
    }

    // Relations with functional dependency constraints
    if (auto guardedConditions = getFunctionalDependencies(clause)) {
        return mk<ram::GuardedProject>(headRelationName, std::move(values), std::move(guardedConditions));
    }

    // Everything else
    return mk<ram::Project>(headRelationName, std::move(values));
}

}  // namespace souffle::ast2ram::provenance
