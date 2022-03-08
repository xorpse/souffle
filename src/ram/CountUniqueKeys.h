/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2022, The Souffle Developers. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file CountUniqueKeys.h
 *
 ***********************************************************************/

#pragma once

#include "ram/Node.h"
#include "ram/Relation.h"
#include "ram/RelationStatement.h"
#include "souffle/utility/MiscUtil.h"
#include "souffle/utility/StreamUtil.h"
#include "souffle/utility/StringUtil.h"
#include <memory>
#include <ostream>
#include <string>
#include <unordered_map>
#include <utility>
#include <vector>

namespace souffle::ram {

/**
 * @class CountUniqueKeys
 * @brief Count the number of unique keys on a relation given the key columns etc.
 *
 * TODO: Example
 */
class CountUniqueKeys : public RelationStatement {
public:
    CountUniqueKeys(std::string rel, std::size_t providedArity, const std::vector<std::size_t>& columns,
            const std::map<std::size_t, std::string>& keyToConstants, bool isRecursive)
            : RelationStatement(rel), arity(providedArity), keyColumns(columns), constantsMap(keyToConstants),
              recursiveRelation(isRecursive) {}

    std::size_t getArity() const {
        return arity;
    }

    const std::vector<std::size_t>& getKeyColumns() const {
        return keyColumns;
    }

    const std::map<std::size_t, std::string>& getConstantsMap() const {
        return constantsMap;
    }

    bool isRecursiveRelation() const {
        return recursiveRelation;
    }

    CountUniqueKeys* cloning() const override {
        return new CountUniqueKeys(relation, arity, keyColumns, constantsMap, recursiveRelation);
    }

protected:
    void print(std::ostream& os, int tabpos) const override {
        os << times(" ", tabpos) << "COUNT UNIQUE KEYS " << relation
           << " RECURSIVE: " << (recursiveRelation ? "true" : "false") << " COLUMNS: " << keyColumns
           << " CONSTANTS: " << constantsMap;
        os << std::endl;
    }

    bool equal(const Node& node) const override {
        const auto& other = asAssert<CountUniqueKeys>(node);
        return RelationStatement::equal(other) && arity == other.getArity() &&
               keyColumns == other.getKeyColumns() && constantsMap == other.getConstantsMap() &&
               recursiveRelation == other.isRecursiveRelation();
    }

    std::size_t arity;
    std::vector<std::size_t> keyColumns;
    std::map<std::size_t, std::string> constantsMap;
    bool recursiveRelation;
};

}  // namespace souffle::ram
