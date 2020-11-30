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

#include "souffle/RamTypes.h"
#include "souffle/utility/ContainerUtil.h"
#include <map>
#include <vector>

namespace souffle {
class SymbolTable;
}

namespace souffle::ast {
class Argument;
class Clause;
class Constant;
class Node;
}  // namespace souffle::ast

namespace souffle::ram {
class Condition;
class Expression;
class Operation;
class Relation;
class Statement;
}  // namespace souffle::ram

namespace souffle::ast2ram {

class TranslatorContext;
class ValueIndex;

class ClauseTranslator {
public:
    ClauseTranslator(const TranslatorContext& context, SymbolTable& symbolTable)
            : context(context), symbolTable(symbolTable) {}

    /** Generate RAM code for a clause */
    Own<ram::Statement> translateClause(
            const ast::Clause& clause, const ast::Clause& originalClause, int version = 0);

protected:
    const TranslatorContext& context;
    SymbolTable& symbolTable;

    // value index to keep track of references in the loop nest
    Own<ValueIndex> valueIndex = mk<ValueIndex>();

    // current nesting level
    int level = 0;

    virtual Own<ram::Operation> createProjection(const ast::Clause& clause);
    virtual Own<ram::Condition> createCondition(const ast::Clause& originalClause);

    /** apply constraint filters to a given operation */
    Own<ram::Operation> filterByConstraints(size_t level, const std::vector<ast::Argument*>& arguments,
            Own<ram::Operation> op, bool constrainByFunctors = true);

private:
    std::vector<const ast::Argument*> generators;

    // the order of processed operations
    std::vector<const ast::Node*> op_nesting;

    Own<ast::Clause> getReorderedClause(const ast::Clause& clause, const int version) const;

    void indexClause(const ast::Clause& clause);
    void indexAtoms(const ast::Clause& clause);
    void indexAggregators(const ast::Clause& clause);
    void indexMultiResultFunctors(const ast::Clause& clause);
    void indexNodeArguments(int nodeLevel, const std::vector<ast::Argument*>& nodeArgs);

    // Add equivalence constraints imposed by variable bindings
    Own<ram::Operation> addVariableBindingConstraints(Own<ram::Operation> op);

    // Add constraints imposed by the body literals
    Own<ram::Operation> addBodyLiteralConstraints(const ast::Clause& clause, Own<ram::Operation> op);

    // Add aggregator conditions
    Own<ram::Operation> addAggregatorConstraints(Own<ram::Operation> op);

    // Add generator levels
    Own<ram::Operation> addGeneratorLevels(Own<ram::Operation> op);

    // Build operation bottom-up
    Own<ram::Operation> buildFinalOperation(const ast::Clause& clause, const ast::Clause& originalClause,
            int version, Own<ram::Operation> op);

    // Return the write-location for the generator, or {} if an equivalent arg was already seen
    std::optional<int> addGenerator(const ast::Argument& arg);

    static RamDomain getConstantRamRepresentation(SymbolTable& symbolTable, const ast::Constant& constant);
    static Own<ram::Expression> translateConstant(SymbolTable& symbolTable, const ast::Constant& constant);
};

}  // namespace souffle::ast2ram
