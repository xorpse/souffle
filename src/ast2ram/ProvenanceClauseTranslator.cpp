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
#include "ast/ProvenanceNegation.h"
#include "ast/utility/Utils.h"
#include "ast2ram/AstToRamTranslator.h"
#include "ast2ram/ValueTranslator.h"
#include "ast2ram/utility/TranslatorContext.h"
#include "ast2ram/utility/Utils.h"
#include "ast2ram/utility/ValueIndex.h"
#include "ram/Condition.h"
#include "ram/Filter.h"
#include "ram/Negation.h"
#include "ram/ProvenanceExistenceCheck.h"
#include "ram/Relation.h"
#include "ram/SignedConstant.h"
#include "ram/Statement.h"
#include "ram/SubroutineReturn.h"
#include "ram/UndefValue.h"

namespace souffle::ast2ram {

Own<ram::Statement> ProvenanceClauseTranslator::generateClause(const TranslatorContext& context,
        SymbolTable& symbolTable, const ast::Clause& clause, const ast::Clause& originalClause, int version) {
    return ProvenanceClauseTranslator(context, symbolTable).translateClause(clause, originalClause, version);
}

Own<ram::Operation> ProvenanceClauseTranslator::addNegate(
        const ast::Clause& clause, const ast::Atom* atom, Own<ram::Operation> op, bool isDelta) const {
    if (isDelta) {
        return ClauseTranslator::addNegate(clause, atom, std::move(op), isDelta);
    }

    size_t auxiliaryArity = context.getEvaluationArity(atom);
    assert(auxiliaryArity <= atom->getArity() && "auxiliary arity out of bounds");
    size_t arity = atom->getArity() - auxiliaryArity;

    VecOwn<ram::Expression> values;

    auto args = atom->getArguments();
    for (size_t i = 0; i < arity; i++) {
        values.push_back(ValueTranslator::translate(context, symbolTable, *valueIndex, args[i]));
    }

    // we don't care about the provenance columns when doing the existence check
    if (Global::config().has("provenance")) {
        // undefined value for rule number
        values.push_back(mk<ram::UndefValue>());
        // add the height annotation for provenanceNotExists
        for (size_t height = 1; height < auxiliaryArity; height++) {
            values.push_back(
                    ValueTranslator::translate(context, symbolTable, *valueIndex, args[arity + height]));
        }
    }
    return mk<ram::Filter>(mk<ram::Negation>(mk<ram::ProvenanceExistenceCheck>(
                                   getConcreteRelationName(atom->getQualifiedName()), std::move(values))),
            std::move(op));
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
