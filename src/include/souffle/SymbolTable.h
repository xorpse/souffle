/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2013, 2014, 2015 Oracle and/or its affiliates. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file SymbolTable.h
 *
 * Encodes/decodes symbols to numbers (and vice versa).
 *
 ***********************************************************************/

#pragma once

#include "souffle/RamTypes.h"
#include "souffle/datastructure/ConcurrentFlyweight.hpp"
#include "souffle/utility/MiscUtil.h"
#include "souffle/utility/ParallelUtil.h"
#include "souffle/utility/StreamUtil.h"
#include <algorithm>
#include <cstdlib>
#include <deque>
#include <initializer_list>
#include <iostream>
#include <string>
#include <unordered_map>
#include <utility>
#include <vector>

namespace souffle {

class SymbolTable : protected FlyweightImpl<std::string> {
private:
    using Base = FlyweightImpl<std::string>;

public:
    using iterator = typename Base::iterator;

    SymbolTable() = default;

    SymbolTable(std::initializer_list<std::string> symbols) : Base(symbols.size()) {
        for (const auto& symbol : symbols) {
            findOrInsert(symbol);
        }
    }

    iterator begin() const {
        return Base::begin();
    }

    iterator end() const {
        return Base::end();
    }

    bool weakContains(const std::string& symbol) const {
        return Base::weakContains(symbol);
    }

    RamDomain encode(const std::string& symbol) {
        return Base::findOrInsert(symbol).first;
    }

    const std::string& decode(const RamDomain index) const {
        return Base::fetch(index);
    }

    /// Kept for API compatibility.
    RamDomain unsafeEncode(const std::string& symbol) {
        return encode(symbol);
    }

    /// Kept for API compatibility.
    const std::string& unsafeDecode(const RamDomain index) const {
        return decode(index);
    }

    std::pair<RamDomain, bool> findOrInsert(const std::string& symbol) {
        auto Res = Base::findOrInsert(symbol);
        return std::make_pair(Res.first, Res.second);
    }
};

}  // namespace souffle
