/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2020 The Souffle Developers. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file ConstraintTranslator.cpp
 *
 ***********************************************************************/

#include "ast2ram/ConstraintTranslator.h"
#include "ast/Atom.h"
#include "ast/BinaryConstraint.h"
#include "ast/TranslationUnit.h"
#include "ast/analysis/AuxArity.h"
#include "ast2ram/AstToRamTranslator.h"
#include "ast2ram/ValueIndex.h"
#include "ast2ram/utility/Utils.h"
#include "ram/Constraint.h"
#include "ram/EmptinessCheck.h"
#include "ram/ExistenceCheck.h"
#include "ram/Negation.h"
#include "ram/ProvenanceExistenceCheck.h"
#include "ram/UndefValue.h"

namespace souffle::ast2ram {

Own<ram::Condition> ConstraintTranslator::translate(
        AstToRamTranslator& translator, const ValueIndex& index, const ast::Literal& lit) {
    return ConstraintTranslator(translator, index)(lit);
}

Own<ram::Condition> ConstraintTranslator::visitAtom(const ast::Atom&) {
    return nullptr;  // covered already within the scan/lookup generation step
}

Own<ram::Condition> ConstraintTranslator::visitBinaryConstraint(const ast::BinaryConstraint& binRel) {
    assert(binRel.getFinalType().has_value() && "binary constraint has unset type");
    auto valLHS = translator.translateValue(binRel.getLHS(), index);
    auto valRHS = translator.translateValue(binRel.getRHS(), index);
    return mk<ram::Constraint>(binRel.getFinalType().value(), std::move(valLHS), std::move(valRHS));
}

Own<ram::Condition> ConstraintTranslator::visitProvenanceNegation(const ast::ProvenanceNegation& neg) {
    const auto* atom = neg.getAtom();
    size_t auxiliaryArity = translator.getEvaluationArity(atom);
    assert(auxiliaryArity <= atom->getArity() && "auxiliary arity out of bounds");
    size_t arity = atom->getArity() - auxiliaryArity;
    VecOwn<ram::Expression> values;

    auto args = atom->getArguments();
    for (size_t i = 0; i < arity; i++) {
        values.push_back(translator.translateValue(args[i], index));
    }
    // we don't care about the provenance columns when doing the existence check
    if (Global::config().has("provenance")) {
        // undefined value for rule number
        values.push_back(mk<ram::UndefValue>());
        // add the height annotation for provenanceNotExists
        for (size_t height = 1; height < auxiliaryArity; height++) {
            values.push_back(translator.translateValue(args[arity + height], index));
        }
    }
    return mk<ram::Negation>(
            mk<ram::ProvenanceExistenceCheck>(getConcreteRelationName(atom), std::move(values)));
}

Own<ram::Condition> ConstraintTranslator::visitNegation(const ast::Negation& neg) {
    const auto* atom = neg.getAtom();
    size_t auxiliaryArity = translator.getEvaluationArity(atom);
    assert(auxiliaryArity <= atom->getArity() && "auxiliary arity out of bounds");
    size_t arity = atom->getArity() - auxiliaryArity;

    if (arity == 0) {
        // for a nullary, negation is a simple emptiness check
        return mk<ram::EmptinessCheck>(getConcreteRelationName(atom));
    }

    // else, we construct the atom and create a negation
    VecOwn<ram::Expression> values;
    auto args = atom->getArguments();
    for (size_t i = 0; i < arity; i++) {
        values.push_back(translator.translateValue(args[i], index));
    }
    for (size_t i = 0; i < auxiliaryArity; i++) {
        values.push_back(mk<ram::UndefValue>());
    }
    return mk<ram::Negation>(mk<ram::ExistenceCheck>(getConcreteRelationName(atom), std::move(values)));
}
}  // namespace souffle::ast2ram
