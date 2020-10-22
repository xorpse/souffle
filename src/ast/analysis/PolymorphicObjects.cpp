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

namespace souffle::ast::analysis {

void PolymorphicObjectsAnalysis::run(const TranslationUnit& translationUnit) {
    const auto& program = translationUnit.getProgram();
    const auto& typeAnalysis = *translationUnit.getAnalysis<analysis::TypeAnalysis>();

    visitDepthFirst(program, [&](const NumericConstant& numericConstant) {
        // Constant has a fixed type
        if (numericConstant.hasFixedType()) {
            constantType[&numericConstant] = numericConstant.getType().value();
            return;
        }

        // Otherwise, type should be inferred
        TypeSet types = typeAnalysis.getTypes(&numericConstant);
        auto hasOfKind = [&](TypeAttribute kind) -> bool {
            return any_of(types, [&](const analysis::Type& type) { return isOfKind(type, kind); });
        };
        if (hasOfKind(TypeAttribute::Signed)) {
            constantType[&numericConstant] = NumericConstant::Type::Int;
        } else if (hasOfKind(TypeAttribute::Unsigned)) {
            constantType[&numericConstant] = NumericConstant::Type::Uint;
        } else if (hasOfKind(TypeAttribute::Float)) {
            constantType[&numericConstant] = NumericConstant::Type::Float;
        } else {
            assert(false && "could not deduce type of numeric constant");
        }
    });
}

void PolymorphicObjectsAnalysis::print(std::ostream& /* os */) const {}

FunctorOp PolymorphicObjectsAnalysis::getOverloadedFunctionOp(const IntrinsicFunctor* inf) const {
    const auto& op = inf->getFunctionOp();
    if (op) return op.value();
    return FunctorOp::ORD;
}

NumericConstant::Type PolymorphicObjectsAnalysis::getOverloadedType(const NumericConstant* nc) const {
    return constantType.at(nc);
}

BinaryConstraintOp PolymorphicObjectsAnalysis::getOverloadedOperator(const BinaryConstraint* bc) const {
    return bc->getOperator();
}

AggregateOp PolymorphicObjectsAnalysis::getOverloadedOperator(const Aggregator* aggr) const {
    return aggr->getOperator();
}

}  // namespace souffle::ast::analysis
