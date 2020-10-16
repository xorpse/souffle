/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2020 The Souffle Developers. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file ValueTranslator.h
 *
 ***********************************************************************/

#pragma once

#include "ast/UnnamedVariable.h"
#include "ast/Variable.h"
#include "ast/analysis/Functor.h"
#include "ast/utility/Visitor.h"
#include "ast2ram/AstToRamTranslator.h"
#include "ast2ram/ValueIndex.h"
#include "ram/AutoIncrement.h"
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
#include "ram/IO.h"
#include "ram/IntrinsicOperator.h"
#include "ram/LogRelationTimer.h"
#include "ram/LogSize.h"
#include "ram/LogTimer.h"
#include "ram/Loop.h"
#include "ram/Negation.h"
#include "ram/PackRecord.h"
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
#include "ram/UserDefinedOperator.h"
#include "ram/utility/Utils.h"

namespace souffle::ast {
class Variable;
class UnnamedVariable;
class NumericConstant;
}  // namespace souffle::ast

namespace souffle::ast2ram {

class ValueTranslator : public ast::Visitor<Own<ram::Expression>> {
public:
    ValueTranslator(AstToRamTranslator& translator, const ValueIndex& index, SymbolTable& symTab)
            : translator(translator), index(index), symTab(symTab) {}

    Own<ram::Expression> visitVariable(const ast::Variable& var) override;

    Own<ram::Expression> visitUnnamedVariable(const ast::UnnamedVariable&) override;

    Own<ram::Expression> visitNumericConstant(const ast::NumericConstant& c);
    Own<ram::Expression> visitStringConstant(const ast::StringConstant& c) override {
        return mk<ram::SignedConstant>(symTab.lookup(c.getConstant()));
    }

    Own<ram::Expression> visitNilConstant(const ast::NilConstant&) override {
        return mk<ram::SignedConstant>(0);
    }

    Own<ram::Expression> visitIntrinsicFunctor(const ast::IntrinsicFunctor& inf) override {
        VecOwn<ram::Expression> values;
        for (const auto& cur : inf.getArguments()) {
            values.push_back(translator.translateValue(cur, index));
        }

        if (ast::analysis::FunctorAnalysis::isMultiResult(inf)) {
            return translator.makeRamTupleElement(index.getGeneratorLoc(inf));
        } else {
            return mk<ram::IntrinsicOperator>(inf.getFunctionOp().value(), std::move(values));
        }
    }

    Own<ram::Expression> visitUserDefinedFunctor(const ast::UserDefinedFunctor& udf) override {
        VecOwn<ram::Expression> values;
        for (const auto& cur : udf.getArguments()) {
            values.push_back(translator.translateValue(cur, index));
        }
        auto returnType = translator.getFunctorAnalysis()->getReturnType(&udf);
        auto argTypes = translator.getFunctorAnalysis()->getArgTypes(udf);
        return mk<ram::UserDefinedOperator>(udf.getName(), argTypes, returnType,
                translator.getFunctorAnalysis()->isStateful(&udf), std::move(values));
    }

    Own<ram::Expression> visitCounter(const ast::Counter&) override {
        return mk<ram::AutoIncrement>();
    }

    Own<ram::Expression> visitRecordInit(const ast::RecordInit& init) override {
        VecOwn<ram::Expression> values;
        for (const auto& cur : init.getArguments()) {
            values.push_back(translator.translateValue(cur, index));
        }
        return mk<ram::PackRecord>(std::move(values));
    }

    Own<ram::Expression> visitAggregator(const ast::Aggregator& agg) override {
        // here we look up the location the aggregation result gets bound
        return translator.makeRamTupleElement(index.getGeneratorLoc(agg));
    }

    Own<ram::Expression> visitSubroutineArgument(const ast::SubroutineArgument& subArg) override {
        return mk<ram::SubroutineArgument>(subArg.getNumber());
    }

private:
    AstToRamTranslator& translator;
    const ValueIndex& index;
    SymbolTable& symTab;
};

}  // namespace souffle::ast2ram
