/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2013, 2014, Oracle and/or its affiliates. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file Node.h
 *
 * Defines the AST abstract node class
 *
 ***********************************************************************/

#pragma once

#include "parser/SrcLocation.h"
#include "souffle/utility/ContainerUtil.h"
#include "souffle/utility/MiscUtil.h"
#include <iosfwd>
#include <string>
#include <typeinfo>
#include <utility>
#include <vector>

namespace souffle::ast {

class NodeMapper;

/**
 *  @class Node
 *  @brief Abstract class for syntactic elements in an input program.
 */
class Node {
public:
    Node(SrcLocation loc = {}) : location(std::move(loc)){};
    virtual ~Node() = default;

    /** Return source location of the Node */
    const SrcLocation& getSrcLoc() const {
        return location;
    }

    /** Set source location for the Node */
    void setSrcLoc(SrcLocation l) {
        location = std::move(l);
    }

    /** Return source location of the syntactic element */
    std::string extloc() const {
        return location.extloc();
    }

    /** Equivalence check for two AST nodes */
    bool operator==(const Node& other) const {
        if (this == &other) {
            return true;
        } else if (typeid(*this) == typeid(*&other)) {
            return equal(other);
        }
        return false;
    }

    /** Inequality check for two AST nodes */
    bool operator!=(const Node& other) const {
        return !(*this == other);
    }

    /** Create a clone (i.e. deep copy) of this node */
    virtual Node* clone() const = 0;

    /** Apply the mapper to all child nodes */
    virtual void apply(const NodeMapper& /* mapper */) {}

    using ConstChildNodes = std::vector<Node const*>;
    /**
     * This wraps the ChildNodes container, and does the const-casting
     * in place.  This allows us to move all the uses to be const-correct,
     * while it's confined to a single location.
     * It also saves the user from having to write getChildNodes()
     * and getChildNodes() const
     */
    class ChildNodes {
    private:
        auto caster() const {
            return [](Node const* node) { return const_cast<Node*>(node); };
        }

    public:
        ChildNodes(ConstChildNodes&& cn) : childNodes(std::move(cn)) {}

        auto begin() const {
            return makeTransformIter(childNodes.begin(), caster());
        }

        auto cbegin() const {
            return makeTransformIter(childNodes.cbegin(), caster());
        }

        auto end() const {
            return makeTransformIter(childNodes.end(), caster());
        }

        auto cend() const {
            return makeTransformIter(childNodes.cend(), caster());
        }

        auto size() const {
            return childNodes.size();
        }
        auto operator[](std::size_t ii) const {
            return childNodes[ii];
        }

    private:
        ConstChildNodes childNodes;
    };

    /** Obtain a list of all embedded AST child nodes */
    ConstChildNodes getChildNodes() const {
        return getChildNodesImpl();
    }

    ChildNodes getChildNodes() {
        return ChildNodes(getChildNodesImpl());
    }

    /** Print node onto an output stream */
    friend std::ostream& operator<<(std::ostream& out, const Node& node) {
        node.print(out);
        return out;
    }

protected:
    /** Output to a given output stream */
    virtual void print(std::ostream& os) const = 0;

    /** Abstract equality check for two AST nodes */
    virtual bool equal(const Node& /* other */) const {
        return true;
    }

    virtual ConstChildNodes getChildNodesImpl() const {
        return {};
    }

private:
    /** Source location of a syntactic element */
    SrcLocation location;
};

}  // namespace souffle::ast
