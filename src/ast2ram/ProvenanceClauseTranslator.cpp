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
#include "ast/Atom.h"
#include "ast/BinaryConstraint.h"
#include "ast/Clause.h"
#include "ast/ProvenanceNegation.h"
#include "ast/utility/Utils.h"
#include "ast2ram/AstToRamTranslator.h"
#include "ast2ram/ValueTranslator.h"
#include "ast2ram/utility/TranslatorContext.h"
#include "ast2ram/utility/Utils.h"
#include "ast2ram/utility/ValueIndex.h"
#include "ram/Condition.h"
#include "ram/Relation.h"
#include "ram/SignedConstant.h"
#include "ram/SubroutineReturn.h"

namespace souffle::ast2ram {

Own<ast::Clause> ProvenanceClauseTranslator::createDeltaClause(
        const ast::Clause* original, size_t recursiveAtomIdx) const {
    auto recursiveVersion = souffle::clone(original);

    // @new :- ...
    const auto* headAtom = original->getHead();
    recursiveVersion->getHead()->setQualifiedName(getNewRelationName(headAtom->getQualifiedName()));

    // ... :- ..., @delta, ...
    auto* recursiveAtom = ast::getBodyLiterals<ast::Atom>(*recursiveVersion).at(recursiveAtomIdx);
    recursiveAtom->setQualifiedName(getDeltaRelationName(recursiveAtom->getQualifiedName()));

    // ... :- ..., !head.
    recursiveVersion->addToBody(mk<ast::ProvenanceNegation>(souffle::clone(original->getHead())));

    return recursiveVersion;
}

// TODO (azreika): should change these to a ram query overload!!!

Own<ram::Condition> ProvenanceClauseTranslator::createCondition(
        const ast::Clause& /* originalClause */) const {
    return nullptr;
}

Own<ram::Operation> ProvenanceClauseTranslator::createProjection(const ast::Clause& clause) const {
    VecOwn<ram::Expression> values;

    // get all values in the body
    for (ast::Literal* lit : clause.getBodyLiterals()) {
        if (auto atom = dynamic_cast<ast::Atom*>(lit)) {
            for (ast::Argument* arg : atom->getArguments()) {
                values.push_back(ValueTranslator::translate(context, symbolTable, *valueIndex, arg));
            }
        } else if (auto neg = dynamic_cast<ast::ProvenanceNegation*>(lit)) {
            size_t auxiliaryArity = context.getEvaluationArity(neg->getAtom());
            for (size_t i = 0; i < neg->getAtom()->getArguments().size() - auxiliaryArity; ++i) {
                auto arg = neg->getAtom()->getArguments()[i];
                values.push_back(ValueTranslator::translate(context, symbolTable, *valueIndex, arg));
            }
            for (size_t i = 0; i < auxiliaryArity; ++i) {
                values.push_back(mk<ram::SignedConstant>(-1));
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

    return mk<ram::SubroutineReturn>(std::move(values));
}

}  // namespace souffle::ast2ram
