/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2020 The Souffle Developers. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file ValueTranslator.cpp
 *
 ***********************************************************************/

#include "ast2ram/ValueTranslator.h"
#include "ast/NumericConstant.h"
#include "ast2ram/AstToRamTranslator.h"
#include "ast2ram/ValueIndex.h"
#include "ram/FloatConstant.h"
#include "ram/SignedConstant.h"
#include "ram/UndefValue.h"
#include "ram/UnsignedConstant.h"

namespace souffle::ast2ram {

Own<ram::Expression> ValueTranslator::visitVariable(const ast::Variable& var) {
    assert(index.isDefined(var) && "variable not grounded");
    return translator.makeRamTupleElement(index.getDefinitionPoint(var));
}

Own<ram::Expression> ValueTranslator::visitUnnamedVariable(const ast::UnnamedVariable&) {
    return mk<ram::UndefValue>();
}
Own<ram::Expression> ValueTranslator::visitNumericConstant(const ast::NumericConstant& c) {
    assert(c.getType().has_value() && "At this points all constants should have type.");

    switch (*c.getType()) {
        case ast::NumericConstant::Type::Int:
            return mk<ram::SignedConstant>(RamSignedFromString(c.getConstant(), nullptr, 0));
        case ast::NumericConstant::Type::Uint:
            return mk<ram::UnsignedConstant>(RamUnsignedFromString(c.getConstant(), nullptr, 0));
        case ast::NumericConstant::Type::Float:
            return mk<ram::FloatConstant>(RamFloatFromString(c.getConstant()));
    }

    fatal("unexpected numeric constant type");
}

}  // namespace souffle::ast2ram
