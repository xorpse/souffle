/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2020 The Souffle Developers. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file TranslatorContext.cpp
 *
 ***********************************************************************/

#include "ast/TranslationUnit.h"
#include "ast/analysis/RecursiveClauses.h"
#include "ast2ram/utility/TranslatorContext.h"

namespace souffle::ast2ram {

TranslatorContext::TranslatorContext(ast::TranslationUnit& tu) : tu(tu) {
    recursiveClauses = tu.getAnalysis<ast::analysis::RecursiveClausesAnalysis>();
}

bool TranslatorContext::isRecursiveClause(const ast::Clause* clause) const {
    return recursiveClauses->recursive(clause);
}

}  // namespace souffle::ast2ram
