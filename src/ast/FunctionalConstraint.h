/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2013, Oracle and/or its affiliates. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file FunctionalConstraint.h
 *
 * Defines the functional constraint class
 *
 ***********************************************************************/

#pragma once

#include "ast/Argument.h"
#include "ast/Constraint.h"
#include "ast/Node.h"
#include "ast/Variable.h"
#include "ast/utility/NodeMapper.h"
#include "parser/SrcLocation.h"
#include "souffle/utility/ContainerUtil.h"
#include "souffle/utility/MiscUtil.h"
#include <cassert>
#include <iostream>
#include <memory>
#include <string>
#include <utility>
#include <vector>

namespace souffle::ast {

/**
 * @class FunctionalConstraint
 * @brief Functional constraint (choice construct) class
 * 
 * Representing a functional dependency (choice construct)
 * eg. x -> y
 * x uniquely identifies some y value
 * LHS of dependency stored as a vector of pointers (each pointer = 1 LHS argument) 
 * eg. (x,y) -> z
 */
class FunctionalConstraint : public Constraint {
public:
    FunctionalConstraint(
            VecOwn<Variable> ls, Own<Variable> rs, SrcLocation loc = {})
            : Constraint(std::move(loc)), lhs(std::move(ls)), positions(lhs.size(), 0), rhs(std::move(rs)) {}

    FunctionalConstraint(
            Own<Variable> ls, Own<Variable> rs, SrcLocation loc = {})
            : Constraint(std::move(loc)), positions(1, 0), rhs(std::move(rs)) 
            {
                lhs.push_back(std::move(ls));
            }

    /** get left-hand side of functional constraint */ 
    const Variable *getLHS(size_t lhsNum) const {
        return lhs.at(lhsNum).get();
    }

    /** get arity of the functional constraint (i.e. number of source nodes: (x,y)->z has an arity of 2) */
    size_t getArity() const {
        return lhs.size();
    }

    /** get left-hand side of functional constraint */ 
    const Variable *getRHS() const {
        return rhs.get(); 
    }

    std::vector<const Node*> getChildNodes() const override {
        std::vector<const Node*> res;
        for (auto& cur : lhs) {
            res.push_back(cur.get());
        }
        res.push_back(rhs.get());
        return res;
    }

    FunctionalConstraint* clone() const override {
        VecOwn<Variable> newLHS;
        for (size_t i = 0; i < lhs.size(); i++) {
            newLHS.push_back(Own<Variable>(lhs.at(i)->clone()));
        }
        auto* res = new FunctionalConstraint(
            std::move(newLHS),
            Own<Variable>(rhs->clone()));
        for (size_t i = 0; i < lhs.size(); i++) {
            res->setPosition(i, (this->getPosition(i)));
        }
        res->setSrcLoc(getSrcLoc());
        return res;
    }

    /* get index position of LHS (source) in relation arguments (0-indexed) */ 
    size_t getPosition(size_t lhsNum) const {
        return positions.at(lhsNum);
    }

    /** set index position of LHS (source) in relation arguments (0-indexed) 
     * lhsNum = the LHS source currently wanting to be set
     * i.e. (x,y)->z: to set y, lhsNum = 1
     * pos: the attribute's index position in relation (0-indexed)
    */
    void setPosition (size_t lhsNum, size_t pos) {
        positions.at(lhsNum) = pos;
    }

public:
    void print(std::ostream& os) const override {
        if (lhs.size() > 1) {
            os << "(";
        }
        os << join(lhs, ",", print_deref<Own<ast::Variable>>());
        if (lhs.size() > 1) {
            os << ")";
        }
        os << "->" << *rhs;
    }

    bool equal(const Node& node) const override {
        assert(nullptr != dynamic_cast<const FunctionalConstraint*>(&node));
        const auto& other = static_cast<const FunctionalConstraint&>(node);
        if (lhs.size() != other.lhs.size()) {
            return false;
        }
        bool lhsMatch = true;
        // Check that each pointer and position match in LHS
        for (size_t i = 0; i < lhs.size(); i++) {
            if (!equal_ptr(lhs.at(i), other.lhs.at(i))) {
                lhsMatch = false;
                break;
            }

            if (positions.at(i) != other.positions.at(i)) {
                lhsMatch = false;
                break;
            }
        }
        return lhsMatch && equal_ptr(rhs,other.rhs);
    }

    /* lhs of functional constraint */ 
    VecOwn<Variable> lhs; 

    /** index positions of LHS (source) nodes in relation arguments (0-indexed)
     * eg. A(x,y,z) constrains y->z 
     * Dependency y->z has position 1
     **/
    std::vector<size_t> positions;

    /* rhs of functional constraint */ 
    Own<Variable> rhs;
}; 

} // namespace souffle::ast