/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2020 The Souffle Developers. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file ConstraintTranslator.h
 *
 ***********************************************************************/

#pragma once

#include "ast/BinaryConstraint.h"
#include "ast/utility/Visitor.h"
#include "ast2ram/AstToRamTranslator.h"
#include "ast2ram/ValueIndex.h"
#include "ram/Call.h"
#include "ram/Clear.h"
#include "ram/Condition.h"
#include "ram/Conjunction.h"
#include "ram/Constraint.h"
#include "ram/DebugInfo.h"
#include "ram/EmptinessCheck.h"
#include "ram/ExistenceCheck.h"
#include "ram/Exit.h"
#include "ram/Expression.h"
#include "ram/Extend.h"
#include "ram/Filter.h"
#include "ram/FloatConstant.h"
#include "ram/IO.h"
#include "ram/LogRelationTimer.h"
#include "ram/LogSize.h"
#include "ram/LogTimer.h"
#include "ram/Loop.h"
#include "ram/Negation.h"
#include "ram/Parallel.h"
#include "ram/Program.h"
#include "ram/Project.h"
#include "ram/ProvenanceExistenceCheck.h"
#include "ram/Query.h"
#include "ram/Relation.h"
#include "ram/RelationSize.h"
#include "ram/Scan.h"
#include "ram/Sequence.h"
#include "ram/SignedConstant.h"
#include "ram/Statement.h"
#include "ram/SubroutineArgument.h"
#include "ram/SubroutineReturn.h"
#include "ram/Swap.h"
#include "ram/TranslationUnit.h"
#include "ram/TupleElement.h"
#include "ram/UndefValue.h"
#include "ram/UnsignedConstant.h"
#include "ram/utility/Utils.h"
#include "souffle/utility/ContainerUtil.h"

namespace souffle::ast2ram {
class ConstraintTranslator : public ast::Visitor<Own<ram::Condition>> {
    AstToRamTranslator& translator;
    const ValueIndex& index;

public:
    ConstraintTranslator(AstToRamTranslator& translator, const ValueIndex& index)
            : translator(translator), index(index) {}

    /** for atoms */
    Own<ram::Condition> visitAtom(const ast::Atom&) override {
        return nullptr;  // covered already within the scan/lookup generation step
    }

    /** for binary relations */
    Own<ram::Condition> visitBinaryConstraint(const ast::BinaryConstraint& binRel) override {
        auto valLHS = translator.translateValue(binRel.getLHS(), index);
        auto valRHS = translator.translateValue(binRel.getRHS(), index);
        return mk<ram::Constraint>(binRel.getOperator(), std::move(valLHS), std::move(valRHS));
    }

    /** for provenance negation */
    Own<ram::Condition> visitProvenanceNegation(const ast::ProvenanceNegation& neg) override {
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
        return mk<ram::Negation>(mk<ram::ProvenanceExistenceCheck>(
                translator.getConcreteRelationName(atom), std::move(values)));
    }

    /** for negations */
    Own<ram::Condition> visitNegation(const ast::Negation& neg) override {
        const auto* atom = neg.getAtom();
        size_t auxiliaryArity = translator.getEvaluationArity(atom);
        assert(auxiliaryArity <= atom->getArity() && "auxiliary arity out of bounds");
        size_t arity = atom->getArity() - auxiliaryArity;

        if (arity == 0) {
            // for a nullary, negation is a simple emptiness check
            return mk<ram::EmptinessCheck>(translator.getConcreteRelationName(atom));
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
        return mk<ram::Negation>(
                mk<ram::ExistenceCheck>(translator.getConcreteRelationName(atom), std::move(values)));
    }
};
}  // namespace souffle::ast2ram
