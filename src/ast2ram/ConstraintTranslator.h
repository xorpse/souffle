/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2020 The Souffle Developers. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file ConstraintTranslator.h
 *
 ***********************************************************************/

#pragma once

#include "ast/utility/Visitor.h"
#include "souffle/utility/ContainerUtil.h"

namespace souffle {
class SymbolTable;
}

namespace souffle::ast {
class Atom;
class BinaryConstraint;
class Negation;
class ProvenanceNegation;
}  // namespace souffle::ast

namespace souffle::ram {
class Condition;
}

namespace souffle::ast2ram {

class TranslatorContext;
class ValueIndex;

class ConstraintTranslator : public ast::Visitor<Own<ram::Condition>> {
public:
    ConstraintTranslator(const TranslatorContext& context, SymbolTable& symbolTable, const ValueIndex& index)
            : context(context), symbolTable(symbolTable), index(index) {}

    static Own<ram::Condition> translate(const TranslatorContext& context, SymbolTable& symbolTable,
            const ValueIndex& index, const ast::Literal* lit);

    /** -- Visitors -- */
    Own<ram::Condition> visitAtom(const ast::Atom&) override;
    Own<ram::Condition> visitBinaryConstraint(const ast::BinaryConstraint& binRel) override;
    Own<ram::Condition> visitProvenanceNegation(const ast::ProvenanceNegation& neg) override;
    Own<ram::Condition> visitNegation(const ast::Negation& neg) override;

private:
    const TranslatorContext& context;
    SymbolTable& symbolTable;
    const ValueIndex& index;
};

}  // namespace souffle::ast2ram
