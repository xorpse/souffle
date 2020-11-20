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

#include "ast2ram/utility/TranslatorContext.h"
#include "ast/TranslationUnit.h"
#include "ast/analysis/RecursiveClauses.h"
#include "ast/analysis/RelationSchedule.h"
#include "ast/analysis/SCCGraph.h"

namespace souffle::ast2ram {

TranslatorContext::TranslatorContext(ast::TranslationUnit& tu) : tu(tu) {
    recursiveClauses = tu.getAnalysis<ast::analysis::RecursiveClausesAnalysis>();
    sccGraph = tu.getAnalysis<ast::analysis::SCCGraphAnalysis>();
    relationSchedule = tu.getAnalysis<ast::analysis::RelationScheduleAnalysis>();
}

bool TranslatorContext::isRecursiveClause(const ast::Clause* clause) const {
    return recursiveClauses->recursive(clause);
}

size_t TranslatorContext::getNumberOfSCCs() const {
    return sccGraph->getNumberOfSCCs();
}

bool TranslatorContext::isRecursiveSCC(size_t scc) const {
    return sccGraph->isRecursive(scc);
}

std::set<const ast::Relation*> TranslatorContext::getRelationsInSCC(size_t scc) const {
    return sccGraph->getInternalRelations(scc);
}

std::set<const ast::Relation*> TranslatorContext::getInputRelationsInSCC(size_t scc) const {
    return sccGraph->getInternalInputRelations(scc);
}

std::set<const ast::Relation*> TranslatorContext::getOutputRelationsInSCC(size_t scc) const {
    return sccGraph->getInternalOutputRelations(scc);
}

std::set<const ast::Relation*> TranslatorContext::getExpiredRelations(size_t scc) const {
    return relationSchedule->schedule().at(scc).expired();
}

}  // namespace souffle::ast2ram
