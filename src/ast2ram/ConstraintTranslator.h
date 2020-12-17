/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2020 The Souffle Developers. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file ConstraintTranslator.h
 *
 * Abstract class providing an interface for translating an
 * ast::Literal into an equivalent ram::Condition.
 *
 ***********************************************************************/

#pragma once

#include "souffle/utility/ContainerUtil.h"

namespace souffle::ast {
class Literal;
}

namespace souffle::ram {
class Condition;
}

namespace souffle::ast2ram {

class ConstraintTranslator {
public:
    virtual Own<ram::Condition> translateConstraint(const ast::Literal* lit) = 0;
};

}  // namespace souffle::ast2ram
