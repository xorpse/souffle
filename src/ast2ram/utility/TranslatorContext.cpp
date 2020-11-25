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
#include "Global.h"
#include "ast/QualifiedName.h"
#include "ast/TranslationUnit.h"
#include "ast/analysis/Functor.h"
#include "ast/analysis/RecursiveClauses.h"
#include "ast/analysis/RelationDetailCache.h"
#include "ast/analysis/RelationSchedule.h"
#include "ast/analysis/SCCGraph.h"
#include "ast/utility/SipsMetric.h"
#include <set>

namespace souffle::ast2ram {

TranslatorContext::TranslatorContext(SymbolTable& symbolTable, const ast::TranslationUnit& tu)
        : symbolTable(symbolTable) {
    // Set up analyses
    recursiveClauses = tu.getAnalysis<ast::analysis::RecursiveClausesAnalysis>();
    sccGraph = tu.getAnalysis<ast::analysis::SCCGraphAnalysis>();
    relationSchedule = tu.getAnalysis<ast::analysis::RelationScheduleAnalysis>();
    relationDetail = tu.getAnalysis<ast::analysis::RelationDetailCacheAnalysis>();

    // Set up SIPS metric
    std::string sipsChosen = "all-bound";
    if (Global::config().has("RamSIPS")) {
        sipsChosen = Global::config().get("RamSIPS");
    }
    sipsMetric = ast::SipsMetric::create(sipsChosen, tu);
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

std::set<ast::Clause*> TranslatorContext::getClauses(const ast::QualifiedName& name) const {
    return relationDetail->getClauses(name);
}

ast::Relation* TranslatorContext::getRelation(const ast::QualifiedName& name) const {
    return relationDetail->getRelation(name);
}

TypeAttribute TranslatorContext::getFunctorReturnType(const ast::Functor* functor) const {
    return functorAnalysis->getReturnType(functor);
}

TypeAttribute TranslatorContext::getFunctorArgType(const ast::Functor* functor, size_t idx) const {
    return functorAnalysis->getArgType(functor, idx);
}

const std::vector<TypeAttribute>& TranslatorContext::getFunctorArgTypes(
        const ast::UserDefinedFunctor& udf) const {
    return functorAnalysis->getArgTypes(udf);
}

bool TranslatorContext::isStatefulFunctor(const ast::UserDefinedFunctor* udf) const {
    return functorAnalysis->isStateful(udf);
}

}  // namespace souffle::ast2ram
