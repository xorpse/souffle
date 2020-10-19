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
#include "ram/Call.h"
#include "ram/Clear.h"
#include "ram/Condition.h"
#include "ram/Conjunction.h"
#include "ram/Constraint.h"
#include "ram/DebugInfo.h"
#include "ram/EmptinessCheck.h"
#include "ram/ExistenceCheck.h"
#include "ram/Exit.h"
#include "ram/Expression.h"
#include "ram/Extend.h"
#include "ram/Filter.h"
#include "ram/FloatConstant.h"
#include "ram/IO.h"
#include "ram/LogRelationTimer.h"
#include "ram/LogSize.h"
#include "ram/LogTimer.h"
#include "ram/Loop.h"
#include "ram/Negation.h"
#include "ram/Parallel.h"
#include "ram/Program.h"
#include "ram/Project.h"
#include "ram/ProvenanceExistenceCheck.h"
#include "ram/Query.h"
#include "ram/Relation.h"
#include "ram/RelationSize.h"
#include "ram/Scan.h"
#include "ram/Sequence.h"
#include "ram/SignedConstant.h"
#include "ram/Statement.h"
#include "ram/SubroutineArgument.h"
#include "ram/SubroutineReturn.h"
#include "ram/Swap.h"
#include "ram/TranslationUnit.h"
#include "ram/TupleElement.h"
#include "ram/UndefValue.h"
#include "ram/UnsignedConstant.h"
#include "ram/utility/Utils.h"
#include "souffle/utility/ContainerUtil.h"

namespace souffle::ast {
class Atom;
class BinaryConstraint;
class Negation;
class ProvenanceNegation;
}  // namespace souffle::ast

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
