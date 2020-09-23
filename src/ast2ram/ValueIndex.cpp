/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2013, 2015, Oracle and/or its affiliates. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file ValueIndex.h
 *
 * Defines the ValueIndex class, which indexes the location of variables
 * and record references within a loop nest during rule conversion.
 *
 ***********************************************************************/

#include "ast2ram/ValueIndex.h"
#include "ast/Variable.h"
#include "ast2ram/AstToRamTranslator.h"
#include "ast2ram/Location.h"
#include "ram/Relation.h"
#include <cassert>
#include <set>

namespace souffle::ast2ram {

void AstToRamTranslator::ValueIndex::addVarReference(
        const ast::Variable& var, const AstToRamTranslator::Location& l) {
    std::set<AstToRamTranslator::Location>& locs = var_references[var.getName()];
    locs.insert(l);
}

void AstToRamTranslator::ValueIndex::addVarReference(
        const ast::Variable& var, int ident, int pos, Own<ram::RelationReference> rel) {
    addVarReference(var, AstToRamTranslator::Location({ident, pos, std::move(rel)}));
}

bool AstToRamTranslator::ValueIndex::isDefined(const ast::Variable& var) const {
    return var_references.find(var.getName()) != var_references.end();
}

const AstToRamTranslator::Location& AstToRamTranslator::ValueIndex::getDefinitionPoint(
        const ast::Variable& var) const {
    auto pos = var_references.find(var.getName());
    assert(pos != var_references.end() && "Undefined variable referenced!");
    return *pos->second.begin();
}

void AstToRamTranslator::ValueIndex::setGeneratorLoc(
        const ast::Argument& agg, const AstToRamTranslator::Location& loc) {
    arg_generator_locations.push_back(std::make_pair(&agg, loc));
}

const AstToRamTranslator::Location& AstToRamTranslator::ValueIndex::getGeneratorLoc(
        const ast::Argument& arg) const {
    // search list
    for (const auto& cur : arg_generator_locations) {
        if (*cur.first == arg) {
            return cur.second;
        }
    }

    fatal("arg `%s` has no generator location", arg);
}

void AstToRamTranslator::ValueIndex::setRecordDefinition(
        const ast::RecordInit& init, const AstToRamTranslator::Location& l) {
    record_definitions[&init] = l;
}

void AstToRamTranslator::ValueIndex::setRecordDefinition(
        const ast::RecordInit& init, int ident, int pos, Own<ram::RelationReference> rel) {
    setRecordDefinition(init, AstToRamTranslator::Location({ident, pos, std::move(rel)}));
}

const AstToRamTranslator::Location& AstToRamTranslator::ValueIndex::getDefinitionPoint(
        const ast::RecordInit& init) const {
    auto pos = record_definitions.find(&init);
    if (pos != record_definitions.end()) {
        return pos->second;
    }

    fatal("requested location for undefined record!");
}

bool AstToRamTranslator::ValueIndex::isGenerator(const int level) const {
    // check for aggregator definitions
    return any_of(arg_generator_locations,
            [&level](const auto& location) { return location.second.identifier == level; });
}

bool AstToRamTranslator::ValueIndex::isSomethingDefinedOn(int level) const {
    // check for variable definitions
    for (const auto& cur : var_references) {
        if (cur.second.begin()->identifier == level) {
            return true;
        }
    }
    // check for record definitions
    for (const auto& cur : record_definitions) {
        if (cur.second.identifier == level) {
            return true;
        }
    }
    // nothing defined on this level
    return false;
}

}  // namespace souffle::ast2ram
