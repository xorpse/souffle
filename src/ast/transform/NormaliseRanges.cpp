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

namespace souffle::ast::transform {
bool NormaliseRangesTransformer::transform(TranslationUnit& translationUnit) {
    return false;
}
}  // namespace souffle::ast::transform
