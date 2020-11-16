/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2020 The Souffle Developers. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file Utils.h
 *
 * A collection of utilities used in translation
 *
 ***********************************************************************/

#pragma once

#include "souffle/utility/ContainerUtil.h"
#include <string>

namespace souffle::ast {
class Atom;
class QualifiedName;
class Relation;
}  // namespace souffle::ast

namespace souffle::ram {
class TupleElement;
}

namespace souffle::ast2ram {

struct Location;

/** Get the corresponding concretised RAM relation name for the atom */
std::string getConcreteRelationName(const ast::Atom* atom);

/** Get the corresponding concretised RAM relation name for the relation */
std::string getConcreteRelationName(const ast::Relation* rel, const std::string relationNamePrefix = "");

/** converts the given relation identifier into a relation name */
std::string getRelationName(const ast::QualifiedName& id);

/** Get the corresponding RAM delta relation name for the relation */
std::string getDeltaRelationName(const ast::Relation* rel);

/** Get the corresponding RAM 'new' relation name for the relation */
std::string getNewRelationName(const ast::Relation* rel);

/** create a RAM element access node */
Own<ram::TupleElement> makeRamTupleElement(const Location& loc);

}  // namespace souffle::ast2ram
