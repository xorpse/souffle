/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2018 The Souffle Developers. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file ProvenanceTranslator.h
 *
 ***********************************************************************/

#pragma once

#include "ast2ram/AstToRamTranslator.h"

namespace souffle::ast2ram {

class ProvenanceTranslator : public AstToRamTranslator {
public:
    ProvenanceTranslator() = default;
    ~ProvenanceTranslator() = default;

protected:
    Own<ram::Sequence> translateProgram(const ast::TranslationUnit& translationUnit) override;
    Own<ast::Clause> createDeltaClause(const ast::Clause* original, size_t recursiveAtomIdx) const override;
    VecOwn<ram::Statement> clearExpiredRelations(
            const std::set<const ast::Relation*>& expiredRelations) const override;

private:
    /** translate RAM code for subroutine to get subproofs */
    Own<ram::Statement> makeSubproofSubroutine(const ast::Clause& clause);

    /** translate RAM code for subroutine to get subproofs for non-existence of a tuple */
    Own<ram::Statement> makeNegationSubproofSubroutine(const ast::Clause& clause);

    void addProvenanceClauseSubroutines(const ast::Program* program);
};
}  // namespace souffle::ast2ram
