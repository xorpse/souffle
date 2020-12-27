/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2013, Oracle and/or its affiliates. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file Types.h
 *
 * @brief Shared type definitions
 *
 ***********************************************************************/

#pragma once

#include <memory>
#include <vector>

namespace souffle {
template <typename A>
using Own = std::unique_ptr<A>;

template <typename A, typename B = A, typename... Args>
Own<A> mk(Args&&... xs) {
    return std::make_unique<B>(std::forward<Args>(xs)...);
}

template <typename A>
using VecOwn = std::vector<Own<A>>;

}  // namespace souffle
