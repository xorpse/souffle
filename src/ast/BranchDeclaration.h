/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2020 The Souffle Developers. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file BranchDeclaration.h
 *
 * Defines the wrapper for a single branch in ADT declaration.
 *
 ***********************************************************************/

#pragma once

#include "ast/Attribute.h"
#include "ast/Node.h"
#include "parser/SrcLocation.h"
#include "souffle/utility/Types.h"
#include <iosfwd>
#include <string>
#include <vector>

namespace souffle::ast {

/**
 * @class BranchDeclaration
 * @brief Wrapper for the single branch declaration (product type) inside ADT declaration.
 *
 * @param constructor An entity used to create a variant type. Can be though of as a name of the branch.
 * @param fields Branch arguments and their types.
 *
 * A branch declaration corresponds to a product type and forms a part of ADT declaration.
 * Currently it's required for all the branches to have unique names.
 *
 * TODO (b-scholz): Rename to BranchDeclaration to BranchType (to be consistent to other type declarations)
 * TODO (b-scholz): Rename Constructor to BranchName
 * TODO (b-scholz): Make Constructor/BranchName a QualifiedName
 */
class BranchDeclaration : public Node {
public:
    BranchDeclaration(std::string constructor, VecOwn<Attribute> fields, SrcLocation loc = {});

    /** Get name of branch identifier */
    const std::string& getConstructor() const {
        return constructor;
    }

    /** Set branch identifier */
    void setConstructor(const std::string name) {
        constructor = name;
    }

    /** Get fields of branch */
    std::vector<Attribute*> getFields();

protected:
    void print(std::ostream& os) const override;

private:
    BranchDeclaration* cloning() const override;

private:
    /** Name of branch */
    std::string constructor;

    /** Fields of branch */
    VecOwn<Attribute> fields;
};

}  // namespace souffle::ast
