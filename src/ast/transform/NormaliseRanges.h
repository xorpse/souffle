/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2020, The Souffle Developers. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file NormaliseRanges.h
 *
 * Transform pass to normalise all appearances of ranges.
 *
 ***********************************************************************/

#pragma once

#include "ast/transform/Transformer.h"

namespace souffle::ast::transform {

/** Uniquely names all appearances of ranges */
class NormaliseRangesTransformer : public Transformer {
public:
    std::string getName() const override {
        return "NormaliseRangesTransformer";
    }

private:
    bool transform(TranslationUnit& translationUnit) override;

    NormaliseRangesTransformer* clone() const override {
        return new NormaliseRangesTransformer();
    }
};

}  // namespace souffle::ast::transform
