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
            node->apply(*this);
            return node;
        }
    };
    const TypeAnalysis& typeAnalysis = *translationUnit.getAnalysis<analysis::TypeAnalysis>();
    TypeRewriter update(typeAnalysis, translationUnit.getErrorReport());
    translationUnit.getProgram().apply(update);
    return update.changed;
}

}  // namespace souffle::ast::transform
