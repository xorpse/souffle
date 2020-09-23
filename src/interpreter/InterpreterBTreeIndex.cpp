/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2019, The Souffle Developers. All rights reserved.
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file InterpreterBTreeIndex.cpp
 *
 * Interpreter index with generic interface.
 *
 ***********************************************************************/

#include "interpreter/InterpreterRelation.h"

namespace souffle {

#define CREATE_BTREE_REL(Structure, Arity, ...)                  \
    case (Arity): {                                              \
        return mk<InterpreterRelation<Arity, InterpreterBtree>>( \
                id.getAuxiliaryArity(), id.getName(), orderSet); \
    }

Own<InterpreterRelationWrapper> createBTreeRelation(
        const ram::Relation& id, const ram::analysis::MinIndexSelection& orderSet) {
    switch (id.getArity()) {
        FOR_EACH_BTREE(CREATE_BTREE_REL);

        default: fatal("Requested arity not yet supported. Feel free to add it.");
    }
}
}  // namespace souffle
