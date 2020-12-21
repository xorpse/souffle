/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2020 The Souffle Developers. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file InfoClauseGenerator.h
 *
 ***********************************************************************/

#pragma once

#include "ast2ram/provenance/ClauseTranslator.h"

namespace souffle::ast {
class Clause;
}

namespace souffle::ast2ram {
class TranslatorContext;
}

namespace souffle::ast2ram::provenance {

class InfoClauseGenerator : public ast2ram::provenance::ClauseTranslator {
public:
    InfoClauseGenerator(const TranslatorContext& context, SymbolTable& symbolTable);
    ~InfoClauseGenerator();
};

}  // namespace souffle::ast2ram::provenance
