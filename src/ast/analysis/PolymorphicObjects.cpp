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
#include "ast/TranslationUnit.h"
#include "ast/utility/Visitor.h"
#include "souffle/utility/ContainerUtil.h"

namespace souffle::ast::analysis {

void PolymorphicObjectsAnalysis::run(const TranslationUnit& translationUnit) {
    typeAnalysis = translationUnit.getAnalysis<analysis::TypeAnalysis>();
    const auto& program = translationUnit.getProgram();

    auto isFloat = [&](const Argument* argument) {
        return isOfKind(typeAnalysis->getTypes(argument), TypeAttribute::Float);
    };
    auto isUnsigned = [&](const Argument* argument) {
        return isOfKind(typeAnalysis->getTypes(argument), TypeAttribute::Unsigned);
    };
    auto isSymbol = [&](const Argument* argument) {
        return isOfKind(typeAnalysis->getTypes(argument), TypeAttribute::Symbol);
    };

    // Handle binary constraints
    visitDepthFirst(program, [&](const BinaryConstraint& binaryConstraint) {
        if (isOverloaded(binaryConstraint.getBaseOperator())) {
            // Get arguments
            auto* leftArg = binaryConstraint.getLHS();
            auto* rightArg = binaryConstraint.getRHS();

            // Both args must be of the same type
            if (isFloat(leftArg) && isFloat(rightArg)) {
                constraintType[&binaryConstraint] =
                        convertOverloadedConstraint(binaryConstraint.getBaseOperator(), TypeAttribute::Float);
            } else if (isUnsigned(leftArg) && isUnsigned(rightArg)) {
                constraintType[&binaryConstraint] = convertOverloadedConstraint(
                        binaryConstraint.getBaseOperator(), TypeAttribute::Unsigned);
            } else if (isSymbol(leftArg) && isSymbol(rightArg)) {
                constraintType[&binaryConstraint] = convertOverloadedConstraint(
                        binaryConstraint.getBaseOperator(), TypeAttribute::Symbol);
            }
        }
    });
}

void PolymorphicObjectsAnalysis::print(std::ostream& /* os */) const {}

FunctorOp PolymorphicObjectsAnalysis::getOverloadedFunctionOp(const IntrinsicFunctor* inf) const {
    const auto& op = inf->getFunctionOp();
    if (op) return op.value();
    return FunctorOp::ORD;
}

NumericConstant::Type PolymorphicObjectsAnalysis::getInferredType(const NumericConstant* nc) const {
    return typeAnalysis->getPolymorphicNumericConstantType(nc);
}

bool PolymorphicObjectsAnalysis::hasInvalidType(const NumericConstant* nc) const {
    return typeAnalysis->hasInvalidPolymorphicNumericConstantType(nc);
}

BinaryConstraintOp PolymorphicObjectsAnalysis::getOverloadedOperator(const BinaryConstraint* bc) const {
    return constraintType.at(bc);
}

AggregateOp PolymorphicObjectsAnalysis::getOverloadedOperator(const Aggregator* aggr) const {
    return typeAnalysis->getPolymorphicOperator(aggr);
}

}  // namespace souffle::ast::analysis
