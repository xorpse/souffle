/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2015, Oracle and/or its affiliates. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file PolymorphicObjects.cpp
 *
 ***********************************************************************/

#include "ast/transform/PolymorphicObjects.h"
#include "AggregateOp.h"
#include "ast/Aggregator.h"
#include "ast/Argument.h"
#include "ast/BinaryConstraint.h"
#include "ast/IntrinsicFunctor.h"
#include "ast/Node.h"
#include "ast/Program.h"
#include "ast/TranslationUnit.h"
#include "ast/analysis/Type.h"
#include "ast/analysis/TypeSystem.h"
#include "ast/utility/NodeMapper.h"
#include "ast/utility/Utils.h"
#include "souffle/BinaryConstraintOps.h"
#include "souffle/TypeAttribute.h"
#include "souffle/utility/ContainerUtil.h"
#include "souffle/utility/FunctionalUtil.h"
#include <memory>
#include <optional>
#include <stdexcept>
#include <vector>

namespace souffle::ast::transform {

using namespace analysis;

bool PolymorphicObjectsTransformer::transform(TranslationUnit& translationUnit) {
    struct TypeRewriter : public NodeMapper {
        mutable bool changed{false};
        const TypeAnalysis& typeAnalysis;
        ErrorReport& report;

        TypeRewriter(const TypeAnalysis& typeAnalysis, ErrorReport& report)
                : typeAnalysis(typeAnalysis), report(report) {}

        Own<Node> operator()(Own<Node> node) const override {
            // Utility lambdas to determine if all args are of the same type.
            auto isFloat = [&](const Argument* argument) {
                return isOfKind(typeAnalysis.getTypes(argument), TypeAttribute::Float);
            };
            auto isUnsigned = [&](const Argument* argument) {
                return isOfKind(typeAnalysis.getTypes(argument), TypeAttribute::Unsigned);
            };
            auto isSymbol = [&](const Argument* argument) {
                return isOfKind(typeAnalysis.getTypes(argument), TypeAttribute::Symbol);
            };

            // rewrite sub-expressions first
            node->apply(*this);

            // It's possible that at this stage we get an undeclared clause.
            // In this case types can't be assigned to it, and the procedure getTypes can fail
            try {
                // Handle functor
                if (auto* functor = dynamic_cast<IntrinsicFunctor*>(node.get())) {
                    if (functor->getFunctionOp().has_value()) {
                        if (typeAnalysis.isInvalidFunctor(functor)) {
                            functor->clearFunctionOp();
                            changed = true;
                        } else {
                            auto candidates = typeAnalysis.validOverloads(*functor);
                            if (!candidates.empty()) {
                                const auto& repr = candidates.front().get();
                                if (repr.op != functor->getFunctionOp().value()) {
                                    functor->setFunctionOp(repr.op);
                                    changed = true;
                                }
                            } else {
                                functor->clearFunctionOp();
                                changed = true;
                            }
                        }
                    } else {
                        auto candidates = typeAnalysis.validOverloads(*functor);
                        if (!candidates.empty()) {
                            const auto& repr = candidates.front().get();
                            functor->setFunctionOp(repr.op);
                            changed = true;
                        }
                    }
                }

                // Handle binary constraint
                if (auto* binaryConstraint = dynamic_cast<BinaryConstraint*>(node.get())) {
                    if (isOverloaded(binaryConstraint->getOperator())) {
                        // Get arguments
                        auto* leftArg = binaryConstraint->getLHS();
                        auto* rightArg = binaryConstraint->getRHS();
                        auto oldOp = binaryConstraint->getOperator();

                        // Both args must be of the same type
                        if (isFloat(leftArg) && isFloat(rightArg)) {
                            binaryConstraint->setOperator(convertOverloadedConstraint(
                                    binaryConstraint->getOperator(), TypeAttribute::Float));
                        } else if (isUnsigned(leftArg) && isUnsigned(rightArg)) {
                            binaryConstraint->setOperator(convertOverloadedConstraint(
                                    binaryConstraint->getOperator(), TypeAttribute::Unsigned));
                        } else if (isSymbol(leftArg) && isSymbol(rightArg)) {
                            binaryConstraint->setOperator(convertOverloadedConstraint(
                                    binaryConstraint->getOperator(), TypeAttribute::Symbol));
                        }

                        changed |= binaryConstraint->getOperator() != oldOp;
                    }
                }

                if (auto* aggregator = dynamic_cast<Aggregator*>(node.get())) {
                    if (isOverloadedAggregator(aggregator->getBaseOperator())) {
                        auto* targetExpression = aggregator->getTargetExpression();
                        auto oldOp = aggregator->getBaseOperator();

                        if (isFloat(targetExpression)) {
                            aggregator->setOverloadedOperator(convertOverloadedAggregator(
                                    aggregator->getBaseOperator(), TypeAttribute::Float));
                        } else if (isUnsigned(targetExpression)) {
                            aggregator->setOverloadedOperator(convertOverloadedAggregator(
                                    aggregator->getBaseOperator(), TypeAttribute::Unsigned));
                        }

                        changed |= aggregator->getOverloadedOperator() != oldOp;
                    }
                }
            } catch (std::out_of_range&) {
                // No types to convert in undeclared clauses
            }

            return node;
        }
    };
    const TypeAnalysis& typeAnalysis = *translationUnit.getAnalysis<analysis::TypeAnalysis>();
    TypeRewriter update(typeAnalysis, translationUnit.getErrorReport());
    translationUnit.getProgram().apply(update);
    return update.changed;
}

}  // namespace souffle::ast::transform
