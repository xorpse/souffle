/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2020 The Souffle Developers. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file Utils.cpp
 *
 * A collection of utilities used in translation
 *
 ***********************************************************************/

#include "ast2ram/utility/Utils.h"
#include "ast/Atom.h"
#include "ast/QualifiedName.h"
#include "ast/Relation.h"
#include "ast2ram/Location.h"
#include "ram/Clear.h"
#include "ram/TupleElement.h"
#include "souffle/utility/ContainerUtil.h"
#include "souffle/utility/StringUtil.h"
#include <string>

namespace souffle::ast2ram {

std::string getConcreteRelationName(const ast::Atom* atom) {
    return getRelationName(atom->getQualifiedName());
}

std::string getConcreteRelationName(const ast::Relation* rel, const std::string relationNamePrefix) {
    return relationNamePrefix + getRelationName(rel->getQualifiedName());
}

std::string getDeltaRelationName(const ast::Relation* rel) {
    return getConcreteRelationName(rel, "@delta_");
}

std::string getNewRelationName(const ast::Relation* rel) {
    return getConcreteRelationName(rel, "@new_");
}

std::string getRelationName(const ast::QualifiedName& id) {
    return toString(join(id.getQualifiers(), "."));
}

Own<ram::TupleElement> makeRamTupleElement(const Location& loc) {
    return mk<ram::TupleElement>(loc.identifier, loc.element);
}

Own<ram::Clear> makeRamClear(const ast::Relation* relation) {
    return mk<ram::Clear>(getConcreteRelationName(relation));
}

}  // namespace souffle::ast2ram
