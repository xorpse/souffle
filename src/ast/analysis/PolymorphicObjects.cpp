/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2020, The Souffle Developers. All rights reserved.
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file PolymorphicObjects.cpp
 *
 ***********************************************************************/

#include "ast/analysis/PolymorphicObjects.h"
#include "ast/Aggregator.h"
#include "ast/BinaryConstraint.h"
#include "ast/IntrinsicFunctor.h"
#include "ast/NumericConstant.h"

namespace souffle::ast::analysis {

void PolymorphicObjectsAnalysis::run(const TranslationUnit& /* translationUnit */) {}

void PolymorphicObjectsAnalysis::print(std::ostream& /* os */) const {}

FunctorOp PolymorphicObjectsAnalysis::getFunctionOp(const IntrinsicFunctor* inf) const {
    const auto& op = inf->getFunctionOp();
    if (op) return op.value();
    return FunctorOp::ORD;
}

NumericConstant::Type PolymorphicObjectsAnalysis::getType(const NumericConstant* nc) const {
    const auto& op = nc->getType();
    if (op) return op.value();
    return NumericConstant::Type::Int;
}

BinaryConstraintOp PolymorphicObjectsAnalysis::getOperator(const BinaryConstraint* bc) const {
    return bc->getOperator();
}

AggregateOp PolymorphicObjectsAnalysis::getOperator(const Aggregator* aggr) const {
    return aggr->getOperator();
}

}  // namespace souffle::ast::analysis
