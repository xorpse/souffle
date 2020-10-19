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

class ValueIndex;
class AstToRamTranslator;

class ConstraintTranslator : public ast::Visitor<Own<ram::Condition>> {
public:
    ConstraintTranslator(AstToRamTranslator& translator, const ValueIndex& index)
            : translator(translator), index(index) {}

    /** -- Visitors -- */
    Own<ram::Condition> visitAtom(const ast::Atom&) override;
    Own<ram::Condition> visitBinaryConstraint(const ast::BinaryConstraint& binRel) override;
    Own<ram::Condition> visitProvenanceNegation(const ast::ProvenanceNegation& neg) override;
    Own<ram::Condition> visitNegation(const ast::Negation& neg) override;

private:
    AstToRamTranslator& translator;
    const ValueIndex& index;
};

}  // namespace souffle::ast2ram
