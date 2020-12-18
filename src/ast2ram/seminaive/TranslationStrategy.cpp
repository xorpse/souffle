/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2020 The Souffle Developers. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file TranslationStrategy.cpp
 *
 ***********************************************************************/

#include "ast2ram/seminaive/TranslationStrategy.h"
#include "ast2ram/seminaive/ClauseTranslator.h"
#include "ast2ram/seminaive/ConstraintTranslator.h"
#include "ast2ram/seminaive/UnitTranslator.h"
#include "ast2ram/seminaive/ValueTranslator.h"
#include "ast2ram/utility/TranslatorContext.h"
#include "ram/Condition.h"
#include "ram/Expression.h"
#include "souffle/SymbolTable.h"

namespace souffle::ast2ram::seminaive {

// TODO: check this overload

Own<ast2ram::UnitTranslator> TranslationStrategy::createUnitTranslator() const {
    return mk<UnitTranslator>();
}

Own<ast2ram::ClauseTranslator> TranslationStrategy::createClauseTranslator(
        const TranslatorContext& context, SymbolTable& symbolTable) const {
    return mk<ClauseTranslator>(context, symbolTable);
}

Own<ast2ram::ConstraintTranslator> TranslationStrategy::createConstraintTranslator(
        const TranslatorContext& context, SymbolTable& symbolTable, const ValueIndex& index) const {
    return mk<ConstraintTranslator>(context, symbolTable, index);
}

Own<ast2ram::ValueTranslator> TranslationStrategy::createValueTranslator(
        const TranslatorContext& context, SymbolTable& symbolTable, const ValueIndex& index) const {
    return mk<ValueTranslator>(context, symbolTable, index);
}

}  // namespace souffle::ast2ram::seminaive
