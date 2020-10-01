/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2020, The Souffle Developers. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file NormaliseRanges.cpp
 *
 * Transform pass to normalise all appearances of ranges.
 *
 ***********************************************************************/

#include "ast/transform/NormaliseRanges.h"
#include "ast/BinaryConstraint.h"
#include "ast/IntrinsicFunctor.h"
#include "ast/Program.h"
#include "ast/TranslationUnit.h"
#include "ast/Variable.h"
#include "souffle/utility/ContainerUtil.h"

namespace souffle::ast::transform {

bool NormaliseRangesTransformer::transform(TranslationUnit& translationUnit) {
    auto& program = translationUnit.getProgram();

    // Assign a unique name to each range
    struct name_ranges : public NodeMapper {
        mutable int count{0};
        mutable std::vector<std::pair<std::string, Own<IntrinsicFunctor>>> rangeNames{};
        name_ranges() = default;

        std::vector<std::pair<std::string, Own<IntrinsicFunctor>>> getRangeNames() {
            return std::move(rangeNames);
        }

        Own<Node> operator()(Own<Node> node) const override {
            node->apply(*this);
            if (auto* inf = dynamic_cast<IntrinsicFunctor*>(node.get())) {
                const auto& op = inf->getFunctionOp();
                if (op && (op.value() == FunctorOp::URANGE || op.value() == FunctorOp::FRANGE ||
                                  op.value() == FunctorOp::RANGE)) {
                    std::stringstream newName;
                    newName << "@range_" << count++;
                    rangeNames.push_back({newName.str(), Own<IntrinsicFunctor>(inf->clone())});
                    return mk<Variable>(newName.str());
                }
            }
            return node;
        }
    };

    // Apply the mapper to each clause
    for (auto* clause : program.getClauses()) {
        name_ranges update;
        clause->apply(update);
        for (auto& [name, func] : update.getRangeNames()) {
            clause->addToBody(
                    mk<BinaryConstraint>(BinaryConstraintOp::EQ, mk<Variable>(name), std::move(func)));
        }
    }
    return true;
}

}  // namespace souffle::ast::transform
