/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2020 The Souffle Developers. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file Functor.cpp
 *
 * Analysis that provides type information for functors
 *
 ***********************************************************************/

#include "ast/analysis/Functor.h"
#include "FunctorOps.h"
#include "ast/FunctorDeclaration.h"
#include "ast/IntrinsicFunctor.h"
#include "ast/TranslationUnit.h"
#include "ast/utility/Utils.h"

namespace souffle::ast::analysis {

void FunctorAnalysis::run(const TranslationUnit& translationUnit) {
    Program& program = translationUnit.getProgram();
    typeAnalysis = translationUnit.getAnalysis<TypeAnalysis>();

    visitDepthFirst(program, [&](const IntrinsicFunctor& functor) {
        // any valid candidate will do. pick the first.
        try {
            auto candidates = validOverloads(*typeAnalysis, functor);
            if (!candidates.empty()) {
                functorInfo[functor.getFunction()] = &candidates.front().get();
            }
        } catch (...) {
            // type analysis in validOverloads failed.
        }
    });
}

bool FunctorAnalysis::isStateful(const UserDefinedFunctor* udf) const {
    return typeAnalysis->isStatefulFunctor(udf);
}

TypeAttribute FunctorAnalysis::getReturnType(const Functor* functor) const {
    if (auto* intrinsic = as<IntrinsicFunctor>(functor)) {
        return functorInfo.at(intrinsic->getFunction())->result;
    }
    return typeAnalysis->getFunctorReturnType(functor);
}

const std::vector<TypeAttribute>& FunctorAnalysis::getArgTypes(const UserDefinedFunctor& udf) const {
    return typeAnalysis->getFunctorArgTypes(udf);
}

/** Return argument type of functor */
TypeAttribute FunctorAnalysis::getArgType(const Functor* functor, const size_t idx) const {
    if (auto* intrinsic = as<IntrinsicFunctor>(functor)) {
        auto* info = functorInfo.at(intrinsic->getFunction());
        return info->params.at(info->variadic ? 0 : idx);
    }
    return typeAnalysis->getFunctorArgType(functor, idx);
}

bool FunctorAnalysis::isMultiResult(const Functor& functor) {
    if (auto* intrinsic = as<IntrinsicFunctor>(functor)) {
        auto op = intrinsic->getFunctionOp();
        return op && functorBuiltIn(*op).front().get().multipleResults;
    }
    return TypeAnalysis::isMultiResultFunctor(functor);
}

}  // namespace souffle::ast::analysis
