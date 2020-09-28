/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2013, 2015, Oracle and/or its affiliates. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file Location.h
 *
 ***********************************************************************/

#pragma once

#include "souffle/utility/ContainerUtil.h"
#include "souffle/utility/MiscUtil.h"

namespace souffle::ram {
class RelationReference;
}

namespace souffle::ast2ram {

struct Location {
    int identifier{};
    int element{};
    Own<ram::RelationReference> relation{nullptr};

    Location() = default;

    Location(int ident, int elem, Own<ram::RelationReference> rel = nullptr)
            : identifier(ident), element(elem), relation(std::move(rel)) {}

    Location(const Location& l) : identifier(l.identifier), element(l.element) {
        if (l.relation != nullptr) {
            relation = souffle::clone(l.relation);
        }
    }

    Location& operator=(Location other) {
        identifier = other.identifier;
        element = other.element;
        relation = std::move(other.relation);
        return *this;
    }

    bool operator==(const Location& loc) const {
        return identifier == loc.identifier && element == loc.element;
    }

    bool operator!=(const Location& loc) const {
        return !(*this == loc);
    }

    bool operator<(const Location& loc) const {
        return identifier < loc.identifier || (identifier == loc.identifier && element < loc.element);
    }

    void print(std::ostream& out) const {
        out << "(" << identifier << "," << element << ")";
    }

    friend std::ostream& operator<<(std::ostream& out, const Location& loc) {
        loc.print(out);
        return out;
    }
};

}  // namespace souffle::ast2ram
