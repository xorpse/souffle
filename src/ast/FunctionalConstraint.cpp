/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2020 The Souffle Developers. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

#include "ast/FunctionalConstraint.h"
#include "ast/utility/NodeMapper.h"
#include "souffle/utility/ContainerUtil.h"
#include "souffle/utility/MiscUtil.h"
#include "souffle/utility/StreamUtil.h"
#include <cassert>
#include <iostream>
#include <string>
#include <utility>

namespace souffle::ast {

FunctionalConstraint::FunctionalConstraint(VecOwn<Variable> keys, SrcLocation loc)
        : Constraint(std::move(loc)), keys(std::move(keys)) {
    assert(allValidPtrs(this->keys));
}

FunctionalConstraint::FunctionalConstraint(Own<Variable> key, SrcLocation loc) : Constraint(std::move(loc)) {
    assert(key != nullptr);
    keys.push_back(std::move(key));
}

std::vector<Variable*> FunctionalConstraint::getKeys() const {
    return toPtrVector(keys);
}

Node::NodeVec FunctionalConstraint::getChildNodesImpl() const {
    auto rn = makePtrRange(keys);
    return {rn.begin(), rn.end()};
}

void FunctionalConstraint::print(std::ostream& os) const {
    os << "keys ";
    if (keys.size() > 1) {
        os << "(";
    }
    os << join(keys, ",", print_deref<Own<ast::Variable>>());
    if (keys.size() > 1) {
        os << ")";
    }
}

bool FunctionalConstraint::equal(const Node& node) const {
    const auto& other = asAssert<FunctionalConstraint>(node);
    if (keys.size() != other.keys.size()) {
        return false;
    }
    for (std::size_t i = 0; i < keys.size(); i++) {
        if (!equal_ptr(keys.at(i), other.keys.at(i))) {
            return false;
        }
    }
    return true;
}

bool FunctionalConstraint::equivalentConstraint(const FunctionalConstraint& other) const {
    if (this->getArity() != other.getArity()) {
        return false;
    }
    std::set<std::string> keyNames;
    for (const auto& key : keys) {
        keyNames.insert(key->getName());
    }
    for (const auto& key : other.keys) {
        if (keyNames.find(key->getName()) == keyNames.end()) {
            return false;
        }
    }
    return true;
}

FunctionalConstraint* FunctionalConstraint::cloneImpl() const {
    return new FunctionalConstraint(souffle::clone(keys), getSrcLoc());
}

}  // namespace souffle::ast
