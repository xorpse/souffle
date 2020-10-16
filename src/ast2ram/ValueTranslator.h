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

#include "ast/utility/Visitor.h"

namespace souffle {
class SymbolTable;
}

namespace souffle::ast {
class Aggregator;
class Counter;
class IntrinsicFunctor;
class NilConstant;
class NumericConstant;
class RecordInit;
class StringConstant;
class SubroutineArgument;
class UnnamedVariable;
class UserDefinedFunctor;
class Variable;
}  // namespace souffle::ast

namespace souffle::ram {
class Expression;
}

namespace souffle::ast2ram {

class AstToRamTranslator;
class ValueIndex;

class ValueTranslator : public ast::Visitor<Own<ram::Expression>> {
public:
    ValueTranslator(AstToRamTranslator& translator, const ValueIndex& index, SymbolTable& symTab)
            : translator(translator), index(index), symTab(symTab) {}

    Own<ram::Expression> visitVariable(const ast::Variable& var) override;
    Own<ram::Expression> visitUnnamedVariable(const ast::UnnamedVariable& var) override;
    Own<ram::Expression> visitNumericConstant(const ast::NumericConstant& c) override;
    Own<ram::Expression> visitStringConstant(const ast::StringConstant& c) override;
    Own<ram::Expression> visitNilConstant(const ast::NilConstant& c) override;
    Own<ram::Expression> visitIntrinsicFunctor(const ast::IntrinsicFunctor& inf) override;
    Own<ram::Expression> visitUserDefinedFunctor(const ast::UserDefinedFunctor& udf) override;
    Own<ram::Expression> visitCounter(const ast::Counter& ctr) override;
    Own<ram::Expression> visitRecordInit(const ast::RecordInit& init) override;
    Own<ram::Expression> visitAggregator(const ast::Aggregator& agg) override;
    Own<ram::Expression> visitSubroutineArgument(const ast::SubroutineArgument& subArg) override;

private:
    AstToRamTranslator& translator;
    const ValueIndex& index;
    SymbolTable& symTab;
};

}  // namespace souffle::ast2ram
