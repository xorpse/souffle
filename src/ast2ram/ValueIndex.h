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

#pragma once

#include "ast/Variable.h"
#include "ast2ram/AstToRamTranslator.h"
#include "ram/Relation.h"

namespace souffle::ast {
class Argument;
class RecordInit;
}  // namespace souffle::ast

namespace souffle::ast2ram {

class AstToRamTranslator::ValueIndex {
public:
    /**
     * The type mapping variables (referenced by their names) to the
     * locations where they are used.
     */
    using variable_reference_map = std::map<std::string, std::set<Location>>;

    /**
     * The type mapping record init expressions to their definition points,
     * hence the point where they get grounded/bound.
     */
    using record_definition_map = std::map<const ast::RecordInit*, Location>;

    /**
     * A map from generative `ast::Argument`s to storage locations. Note,
     * since in this case ast::Argument are indexed by their values (not their
     * address) no standard map can be utilized.
     * (By-value indexing induces an ad-hoc form of CSE.)
     */
    using generator_location_map = std::vector<std::pair<const ast::Argument*, Location>>;

    // -- variables --

    void addVarReference(const ast::Variable& var, const Location& l) {
        std::set<Location>& locs = var_references[var.getName()];
        locs.insert(l);
    }

    void addVarReference(
            const ast::Variable& var, int ident, int pos, Own<ram::RelationReference> rel = nullptr) {
        addVarReference(var, Location({ident, pos, std::move(rel)}));
    }

    bool isDefined(const ast::Variable& var) const {
        return var_references.find(var.getName()) != var_references.end();
    }

    const Location& getDefinitionPoint(const ast::Variable& var) const {
        auto pos = var_references.find(var.getName());
        assert(pos != var_references.end() && "Undefined variable referenced!");
        return *pos->second.begin();
    }

    const variable_reference_map& getVariableReferences() const {
        return var_references;
    }

    // -- records --

    // - definition -

    void setRecordDefinition(const ast::RecordInit& init, const Location& l) {
        record_definitions[&init] = l;
    }

    void setRecordDefinition(
            const ast::RecordInit& init, int ident, int pos, Own<ram::RelationReference> rel = nullptr) {
        setRecordDefinition(init, Location({ident, pos, std::move(rel)}));
    }

    const Location& getDefinitionPoint(const ast::RecordInit& init) const {
        auto pos = record_definitions.find(&init);
        if (pos != record_definitions.end()) {
            return pos->second;
        }

        fatal("requested location for undefined record!");
    }

    // -- generators (aggregates & some functors) --

    void setGeneratorLoc(const ast::Argument& agg, const Location& loc) {
        arg_generator_locations.push_back(std::make_pair(&agg, loc));
    }

    const Location& getGeneratorLoc(const ast::Argument& arg) const {
        // search list
        for (const auto& cur : arg_generator_locations) {
            if (*cur.first == arg) {
                return cur.second;
            }
        }

        fatal("arg `%s` has no generator location", arg);
    }

    // -- others --

    bool isGenerator(const int level) const {
        // check for aggregator definitions
        return any_of(arg_generator_locations,
                [&level](const auto& location) { return location.second.identifier == level; });
    }

    bool isSomethingDefinedOn(int level) const {
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

    void print(std::ostream& out) const {
        out << "Variables:\n\t";
        out << join(var_references, "\n\t");
    }

    friend std::ostream& operator<<(std::ostream& out, const ValueIndex& index) __attribute__((unused)) {
        index.print(out);
        return out;
    }

private:
    /** The index of variable accesses */
    variable_reference_map var_references;

    /** The index of record definition points */
    record_definition_map record_definitions;

    /** The level of a nested ram operation that is handling a generator operation */
    generator_location_map arg_generator_locations;
};

}  // namespace souffle::ast2ram
