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
#include "ast/Argument.h"
#include "ast/IntrinsicFunctor.h"
#include "ast/Node.h"
#include "ast/Program.h"
#include "ast/TranslationUnit.h"
#include "ast/analysis/Type.h"
#include "ast/analysis/TypeSystem.h"
#include "ast/utility/NodeMapper.h"
#include "ast/utility/Utils.h"
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
            // rewrite sub-expressions first
            node->apply(*this);

            // It's possible that at this stage we get an undeclared clause.
            // In this case types can't be assigned to it, and the procedure getTypes can fail
            try {
                // Handle functor
                if (auto* functor = dynamic_cast<IntrinsicFunctor*>(node.get())) {
                    if (typeAnalysis.hasInvalidPolymorphicOperator(functor)) {
                        if (functor->getFunctionOp().has_value()) {
                            changed = true;
                            functor->clearFunctionOp();
                        }
                    } else {
                        auto overloadedOp = typeAnalysis.getPolymorphicOperator(functor);
                        if (!functor->getFunctionOp().has_value() ||
                                functor->getFunctionOp().value() != overloadedOp) {
                            functor->setFunctionOp(overloadedOp);
                            changed = true;
                        }
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
