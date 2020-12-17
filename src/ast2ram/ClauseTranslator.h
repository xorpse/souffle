/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2020 The Souffle Developers. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file ClauseTranslator.h
 *
 * Abstract class providing an interface for translating an
 * ast::Clause into an equivalent ram::Statement.
 *
 ***********************************************************************/

#pragma once

namespace souffle::ast {
class Clause;
}

namespace souffle::ram {
class Statement;
}

namespace souffle::ast2ram {

class ClauseTranslator {
public:
    virtual Own<ram::Statement> translateClause(const ast::Clause& clause) = 0;
};

}  // namespace souffle::ast2ram
