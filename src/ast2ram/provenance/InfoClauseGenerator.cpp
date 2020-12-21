/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2020 The Souffle Developers. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file InfoClauseGenerator.cpp
 *
 ***********************************************************************/

#include "ast2ram/provenance/InfoClauseGenerator.h"
#include "ast2ram/provenance/ClauseTranslator.h"

namespace souffle::ast2ram::provenance {

InfoClauseGenerator::InfoClauseGenerator(const TranslatorContext& context, SymbolTable& symbolTable)
        : ClauseTranslator(context, symbolTable) {}

InfoClauseGenerator::~InfoClauseGenerator() = default;

Own<ram::Statement> InfoClauseGenerator::createRamFactQuery(const ast::Clause& clause) const {
    return ClauseTranslator::createRamFactQuery(clause);
}

Own<ram::Statement> InfoClauseGenerator::createRamRuleQuery(const ast::Clause& clause) {
    return ClauseTranslator::createRamRuleQuery(clause);
}

}  // namespace souffle::ast2ram::provenance
