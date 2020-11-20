/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2020 The Souffle Developers. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file TranslatorContext.h
 *
 ***********************************************************************/

#pragma once

namespace souffle::ast {
class Clause;
class TranslationUnit;
}  // namespace souffle::ast

namespace souffle::ast::analysis {
class RecursiveClausesAnalysis;
}

namespace souffle::ast2ram {

class TranslatorContext {
public:
    TranslatorContext(ast::TranslationUnit& tu);

    bool isRecursiveClause(const ast::Clause* clause) const;

private:
    const ast::TranslationUnit& tu;
    const ast::analysis::RecursiveClausesAnalysis* recursiveClauses;
};

}  // namespace souffle::ast2ram
