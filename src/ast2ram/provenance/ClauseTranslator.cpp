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
#include "ast2ram/utility/ValueIndex.h"
#include "ram/EmptinessCheck.h"
#include "ram/ExistenceCheck.h"
#include "ram/Filter.h"
#include "ram/GuardedProject.h"
#include "ram/IntrinsicOperator.h"
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

void ClauseTranslator::indexAtoms(const ast::Clause& clause) {
    size_t atomIdx = 0;
    for (const auto* atom : getAtomOrdering(clause)) {
        // give the atom the current level
        int scanLevel = addOperatorLevel(atom);
        indexNodeArguments(scanLevel, atom->getArguments());

        // add level num variable
        auto tmpVar = mk<ast::Variable>("@level_num_" + std::to_string(atomIdx++));
        valueIndex->addVarReference(*tmpVar, scanLevel, atom->getArity() + 1);
    }
}

Own<ram::Expression> ClauseTranslator::getLevelNumber(const ast::Clause& clause) const {
    auto getLevelVariable = [&](size_t atomIdx) { return "@level_num_" + std::to_string(atomIdx); };

    const auto& bodyAtoms = getAtomOrdering(clause);
    if (bodyAtoms.empty()) return mk<ram::SignedConstant>(0);
    if (bodyAtoms.size() == 1) {
        auto levelVar = mk<ast::Variable>(getLevelVariable(0));
        return context.translateValue(symbolTable, *valueIndex, levelVar.get());
    }
    VecOwn<ram::Expression> values;
    for (size_t i = 0; i < bodyAtoms.size(); i++) {
        auto levelVar = mk<ast::Variable>(getLevelVariable(i));
        values.push_back(context.translateValue(symbolTable, *valueIndex, levelVar.get()));
    }
    return mk<ram::IntrinsicOperator>(FunctorOp::MAX, std::move(values));
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
        values.push_back(getLevelNumber(clause));
    }

    // Relations with functional dependency constraints
    if (auto guardedConditions = getFunctionalDependencies(clause)) {
        return mk<ram::GuardedProject>(headRelationName, std::move(values), std::move(guardedConditions));
    }

    // Everything else
    return mk<ram::Project>(headRelationName, std::move(values));
}

}  // namespace souffle::ast2ram::provenance
