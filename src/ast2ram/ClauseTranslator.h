/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2013, 2015, Oracle and/or its affiliates. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file ClauseTranslator.h
 *
 * Translator for clauses from AST to RAM
 *
 ***********************************************************************/

#pragma once

#include "ast2ram/AstToRamTranslator.h"
#include "ast2ram/ValueIndex.h"
#include <map>
#include <vector>

namespace souffle::ast {
class Argument;
class Clause;
class Node;
}  // namespace souffle::ast

namespace souffle::ast::analysis {
class AuxiliaryArityAnalsyis;
}

namespace souffle::ram {
class Operation;
class Condition;
class RelationReference;
}  // namespace souffle::ram

namespace souffle::ast2ram {

class ClauseTranslator {
public:
    ClauseTranslator(AstToRamTranslator& translator)
            : translator(translator), auxArityAnalysis(translator.getAuxArityAnalysis()) {}

    Own<ram::Statement> translateClause(
            const ast::Clause& clause, const ast::Clause& originalClause, const int version = 0);

protected:
    AstToRamTranslator& translator;

    // create value index
    ValueIndex valueIndex;

    // current nesting level
    int level = 0;

    virtual Own<ram::Operation> createOperation(const ast::Clause& clause);
    virtual Own<ram::Condition> createCondition(const ast::Clause& originalClause);

    /** translate RAM code for a constant value */
    Own<ram::Operation> filterByConstraints(size_t level, const std::vector<ast::Argument*>& args,
            Own<ram::Operation> op, bool constrainByFunctors = true);

    const ast::analysis::AuxiliaryArityAnalysis* auxArityAnalysis;

private:
    // index nested variables and records
    using arg_list = std::vector<ast::Argument*>;

    std::vector<const ast::Argument*> generators;

    // the order of processed operations
    std::vector<const ast::Node*> op_nesting;

    Own<ast::Clause> getReorderedClause(const ast::Clause& clause, const int version) const;

    arg_list* getArgList(const ast::Node* curNode, std::map<const ast::Node*, Own<arg_list>>& nodeArgs) const;

    void indexValues(const ast::Node* curNode, std::map<const ast::Node*, Own<arg_list>>& nodeArgs,
            std::map<const arg_list*, int>& arg_level, const std::string &relation);

    void createValueIndex(const ast::Clause& clause);
};

}  // namespace souffle::ast2ram
