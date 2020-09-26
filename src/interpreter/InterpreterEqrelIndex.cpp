/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2019, The Souffle Developers. All rights reserved.
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file InterpreterEqrelIndex.cpp
 *
 * Interpreter index with generic interface.
 *
 ***********************************************************************/

#include "interpreter/InterpreterRelation.h"
#include "ram/Relation.h"
#include "ram/analysis/Index.h"

namespace souffle {

Own<InterpreterRelationWrapper> createEqrelRelation(
        const ram::Relation& id, const ram::analysis::MinIndexSelection& orderSet) {
    assert(id.getArity() == 2 && "Eqivalence relation must have arity size 2.");
    return mk<InterpreterEqrelRelation>(id.getAuxiliaryArity(), id.getName(), orderSet);
}

}  // namespace souffle
