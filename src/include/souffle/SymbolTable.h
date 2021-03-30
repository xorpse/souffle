/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2013, Oracle and/or its affiliates. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file SymbolTable.h
 *
 * Data container to store symbols of the Datalog program.
 *
 ***********************************************************************/

#pragma once

#include "souffle/RamTypes.h"
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

/**
 * @class SymbolTable
 *
 * Global pool of re-usable strings
 *
 * SymbolTable stores Datalog symbols and converts them to numbers and vice versa.
 */
class SymbolTable {
private:
    /** A lock to synchronize parallel accesses */
    mutable Lock access;

    /** Map indices to strings. */
    std::deque<std::string> numToStr;

    /** Map strings to indices. */
    std::unordered_map<std::string, std::size_t> strToNum;

    /** Convenience method to place a new symbol in the table, if it does not exist, and return the index of
     * it. */
    inline std::size_t newSymbolOfIndex(const std::string& symbol) {
        std::size_t index;
        auto it = strToNum.find(symbol);
        if (it == strToNum.end()) {
            index = numToStr.size();
            strToNum[symbol] = index;
            numToStr.push_back(symbol);
        } else {
            index = it->second;
        }
        return index;
    }

    /** Convenience method to place a new symbol in the table, if it does not exist. */
    inline void newSymbol(const std::string& symbol) {
        if (strToNum.find(symbol) == strToNum.end()) {
            strToNum[symbol] = numToStr.size();
            numToStr.push_back(symbol);
        }
    }

public:
    SymbolTable() = default;
    SymbolTable(std::initializer_list<std::string> symbols) {
        strToNum.reserve(symbols.size());
        for (const auto& symbol : symbols) {
            newSymbol(symbol);
        }
    }
    virtual ~SymbolTable() = default;

    /** Find the index of a symbol in the table, inserting a new symbol if it does not exist there
     * already. */
    RamDomain lookup(const std::string& symbol) {
        {
            auto lease = access.acquire();
            (void)lease;  // avoid warning;
            return static_cast<RamDomain>(newSymbolOfIndex(symbol));
        }
    }

    /** Find the index of a symbol in the table, inserting a new symbol if it does not exist there
     * already. */
    RamDomain unsafeLookup(const std::string& symbol) {
        return static_cast<RamDomain>(newSymbolOfIndex(symbol));
    }

    /** Find a symbol in the table by its index, note that this gives an error if the index is out of
     * bounds.
     */
    const std::string& resolve(const RamDomain index) const {
        {
            auto lease = access.acquire();
            (void)lease;  // avoid warning;
            auto pos = static_cast<std::size_t>(index);
            if (pos >= size()) {
                // TODO: use different error reporting here!!
                fatal("Error index out of bounds in call to `SymbolTable::resolve`. index = `%d`", index);
            }
            return numToStr[pos];
        }
    }

    const std::string& unsafeResolve(const RamDomain index) const {
        return numToStr[static_cast<std::size_t>(index)];
    }

    /* Return the size of the symbol table, being the number of symbols it currently holds. */
    std::size_t size() const {
        return numToStr.size();
    }

    Lock::Lease acquireLock() const {
        return access.acquire();
    }
};

}  // namespace souffle
