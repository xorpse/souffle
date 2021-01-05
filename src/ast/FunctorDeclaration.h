/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2018 The Souffle Developers. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file FunctorDeclaration.h
 *
 * Defines the external functor class
 *
 ***********************************************************************/

#pragma once

#include "ast/Node.h"
#include "parser/SrcLocation.h"
#include "souffle/TypeAttribute.h"
#include <iosfwd>
#include <string>
#include <vector>

namespace souffle::ast {

/**
 * @class FunctorDeclaration
 * @brief User-defined functor declaration
 *
 * Example:
 *    .declfun foo(x:number, y:number):number
 */

class FunctorDeclaration : public Node {
public:
    FunctorDeclaration(std::string name, std::vector<TypeAttribute> argsTypes, TypeAttribute returnType,
            bool stateful, SrcLocation loc = {});

    /** Return name */
    const std::string& getName() const {
        return name;
    }

    /** Return type */
    const std::vector<TypeAttribute>& getArgsTypes() const {
        return argsTypes;
    }

    /** Get return type */
    TypeAttribute getReturnType() const {
        return returnType;
    }

    /** Return number of arguments */
    size_t getArity() const {
        return argsTypes.size();
    }

    /** Check whether functor is stateful */
    bool isStateful() const {
        return stateful;
    }

protected:
    void print(std::ostream& out) const override;

private:
    bool equal(const Node& node) const override;

    FunctorDeclaration* cloneImpl() const override;

private:
    /** Name of functor */
    const std::string name;

    /** Types of arguments */
    const std::vector<TypeAttribute> argsTypes;

    /** Type of the return value */
    const TypeAttribute returnType;

    /** Stateful flag */
    const bool stateful;
};

}  // namespace souffle::ast
