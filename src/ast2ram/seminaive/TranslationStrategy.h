/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2020 The Souffle Developers. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file TranslationStrategy.h
 *
 * Implementation of the Datalog semi-naive evaluation strategy.
 *
 ***********************************************************************/

#pragma once

#include "souffle/utility/ContainerUtil.h"

namespace souffle {
class SymbolTable;
}

namespace souffle::ast2ram {
class ClauseTranslator;
class ConstraintTranslator;
class UnitTranslator;
class TranslatorContext;
class ValueIndex;
class ValueTranslator;
}  // namespace souffle::ast2ram

namespace souffle::ast2ram::seminaive {

class TranslationStrategy : public ast2ram::TranslationStrategy {
public:
    Own<UnitTranslator> createUnitTranslator() const override;
    Own<ClauseTranslator> createClauseTranslator(
            const TranslatorContext& context, SymbolTable& symbolTable) const override;
    Own<ConstraintTranslator> createConstraintTranslator(const TranslatorContext& context,
            SymbolTable& symbolTable, const ValueIndex& index) const override;
    Own<ValueTranslator> createValueTranslator(const TranslatorContext& context, SymbolTable& symbolTable,
            const ValueIndex& index) const override;
};

}  // namespace souffle::ast2ram::seminaive
