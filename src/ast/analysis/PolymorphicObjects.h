/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2020, The Souffle Developers. All rights reserved.
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file PolymorphicObjects.h
 *
 ***********************************************************************/

#pragma once

#include "ast/NumericConstant.h"
#include "ast/analysis/Analysis.h"
#include <map>

namespace souffle {
enum class AggregateOp;
enum class BinaryConstraintOp;
enum class FunctorOp;
}  // namespace souffle

namespace souffle::ast {
class Aggregator;
class BinaryConstraint;
class IntrinsicFunctor;
class TranslationUnit;
}  // namespace souffle::ast

namespace souffle::ast::analysis {

class PolymorphicObjectsAnalysis : public Analysis {
public:
    static constexpr const char* name = "polymorphic-objects";

    PolymorphicObjectsAnalysis() : Analysis(name) {}

    void run(const TranslationUnit& translationUnit) override;

    void print(std::ostream& os) const override;

    FunctorOp getFunctionOp(const IntrinsicFunctor* inf) const;
    NumericConstant::Type getType(const NumericConstant* nc) const;
    BinaryConstraintOp getOperator(const BinaryConstraint* bc) const;
    AggregateOp getOperator(const Aggregator* aggr) const;

private:
    std::map<const IntrinsicFunctor*, FunctorOp> functorType{};
    std::map<const NumericConstant*, NumericConstant::Type> constantType{};
    std::map<const BinaryConstraint*, BinaryConstraintOp> constraintType{};
    std::map<const Aggregator*, AggregateOp> aggregatorType{};
};

}  // namespace souffle::ast::analysis
