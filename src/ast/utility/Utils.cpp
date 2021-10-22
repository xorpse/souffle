/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2013, 2015, Oracle and/or its affiliates. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file Utils.cpp
 *
 * A collection of utilities operating on AST constructs.
 *
 ***********************************************************************/

#include "ast/utility/Utils.h"
#include "ast/Aggregator.h"
#include "ast/Argument.h"
#include "ast/Atom.h"
#include "ast/BinaryConstraint.h"
#include "ast/BooleanConstraint.h"
#include "ast/Clause.h"
#include "ast/Constraint.h"
#include "ast/Directive.h"
#include "ast/ExecutionPlan.h"
#include "ast/IntrinsicFunctor.h"
#include "ast/Literal.h"
#include "ast/Negation.h"
#include "ast/Node.h"
#include "ast/Program.h"
#include "ast/QualifiedName.h"
#include "ast/Relation.h"
#include "ast/TranslationUnit.h"
#include "ast/analysis/Functor.h"
#include "ast/analysis/RelationDetailCache.h"
#include "ast/analysis/typesystem/Type.h"
#include "ast/analysis/typesystem/TypeSystem.h"

#include "ast/utility/Visitor.h"
#include "souffle/BinaryConstraintOps.h"
#include "souffle/TypeAttribute.h"
#include "souffle/utility/ContainerUtil.h"
#include "souffle/utility/FunctionalUtil.h"
#include "souffle/utility/MiscUtil.h"
#include "souffle/utility/StringUtil.h"
#include <algorithm>
#include <cassert>
#include <memory>

namespace souffle::ast {

std::string pprint(const Node& node) {
    return toString(node);
}

std::vector<Clause*> getClauses(const Program& program, const QualifiedName& relationName) {
    // TODO: REMOVE ENTIRELY
    return program.getClauses(relationName);
}

std::vector<Clause*> getClauses(const Program& program, const Relation& rel) {
    // TODO: REMOVE ENTIRELY
    return getClauses(program, rel.getQualifiedName());
}

std::vector<Directive*> getDirectives(const Program& program, const QualifiedName& name) {
    // TODO: REMOVE ENTIRELY
    return program.getDirectives(name);
}

Relation* getRelation(const Program& program, const QualifiedName& name) {
    // TODO: REMOVE ENTIRELY
    return program.getRelation(name);
}

FunctorDeclaration* getFunctorDeclaration(const Program& program, const std::string& name) {
    // FIXME: O(n). This is awful.
    return getIf(program.getFunctorDeclarations(),
            [&](const FunctorDeclaration* r) { return r->getName() == name; });
}

void removeRelation(TranslationUnit& tu, const QualifiedName& name) {
    // TODO: REMOVE ENTIRELY
    tu.getProgram().removeRelation(name);
}

const Relation* getAtomRelation(const Atom* atom, const Program* program) {
    return getRelation(*program, atom->getQualifiedName());
}

const Relation* getHeadRelation(const Clause* clause, const Program* program) {
    return getAtomRelation(clause->getHead(), program);
}

std::set<const Relation*> getBodyRelations(const Clause* clause, const Program* program) {
    std::set<const Relation*> bodyRelations;
    for (const auto& lit : clause->getBodyLiterals()) {
        visit(*lit, [&](const Atom& atom) { bodyRelations.insert(getAtomRelation(&atom, program)); });
    }
    for (const auto& arg : clause->getHead()->getArguments()) {
        visit(*arg, [&](const Atom& atom) { bodyRelations.insert(getAtomRelation(&atom, program)); });
    }
    return bodyRelations;
}

bool hasClauseWithNegatedRelation(const Relation* relation, const Relation* negRelation,
        const Program* program, const Literal*& foundLiteral) {
    for (const Clause* cl : getClauses(*program, *relation)) {
        for (const auto* neg : getBodyLiterals<Negation>(*cl)) {
            if (negRelation == getAtomRelation(neg->getAtom(), program)) {
                foundLiteral = neg;
                return true;
            }
        }
    }
    return false;
}

bool hasClauseWithAggregatedRelation(const Relation* relation, const Relation* aggRelation,
        const Program* program, const Literal*& foundLiteral) {
    for (const Clause* cl : getClauses(*program, *relation)) {
        bool hasAgg = false;
        visit(*cl, [&](const Aggregator& cur) {
            visit(cur, [&](const Atom& atom) {
                if (aggRelation == getAtomRelation(&atom, program)) {
                    foundLiteral = &atom;
                    hasAgg = true;
                }
            });
        });
        if (hasAgg) {
            return true;
        }
    }
    return false;
}

bool isRecursiveClause(const Clause& clause) {
    QualifiedName relationName = clause.getHead()->getQualifiedName();
    bool recursive = false;
    visit(clause.getBodyLiterals(), [&](const Atom& atom) {
        if (atom.getQualifiedName() == relationName) {
            recursive = true;
        }
    });
    return recursive;
}

bool isFact(const Clause& clause) {
    // there must be a head
    if (clause.getHead() == nullptr) {
        return false;
    }
    // there must not be any body clauses
    if (!clause.getBodyLiterals().empty()) {
        return false;
    }

    // and there are no aggregates
    bool hasAggregatesOrMultiResultFunctor = false;
    visit(*clause.getHead(), [&](const Argument& arg) {
        if (isA<Aggregator>(arg)) {
            hasAggregatesOrMultiResultFunctor = true;
        }

        auto* func = as<IntrinsicFunctor>(arg);
        hasAggregatesOrMultiResultFunctor |=
                (func != nullptr) && analysis::FunctorAnalysis::isMultiResult(*func);
    });
    return !hasAggregatesOrMultiResultFunctor;
}

bool isRule(const Clause& clause) {
    return (clause.getHead() != nullptr) && !isFact(clause);
}

bool isProposition(const Atom* atom) {
    return atom->getArguments().empty();
}

bool isDeltaRelation(const QualifiedName& name) {
    const auto& qualifiers = name.getQualifiers();
    if (qualifiers.empty()) {
        return false;
    }
    return isPrefix("@delta_", qualifiers[0]);
}

Own<Clause> cloneHead(const Clause& clause) {
    auto myClone = mk<Clause>(clone(clause.getHead()), clause.getSrcLoc());
    if (clause.getExecutionPlan() != nullptr) {
        myClone->setExecutionPlan(clone(clause.getExecutionPlan()));
    }
    return myClone;
}

std::vector<Atom*> reorderAtoms(const std::vector<Atom*>& atoms, const std::vector<unsigned int>& newOrder) {
    // Validate given order
    assert(newOrder.size() == atoms.size());
    std::vector<unsigned int> nopOrder;
    for (unsigned int i = 0; i < atoms.size(); i++) {
        nopOrder.push_back(i);
    }
    assert(std::is_permutation(nopOrder.begin(), nopOrder.end(), newOrder.begin()));

    // Create the result
    std::vector<Atom*> result(atoms.size());
    for (std::size_t i = 0; i < atoms.size(); i++) {
        result[i] = atoms[newOrder[i]];
    }
    return result;
}

Clause* reorderAtoms(const Clause* clause, const std::vector<unsigned int>& newOrder) {
    // Find all atom positions
    std::vector<unsigned int> atomPositions;
    std::vector<Literal*> bodyLiterals = clause->getBodyLiterals();
    for (unsigned int i = 0; i < bodyLiterals.size(); i++) {
        if (isA<Atom>(bodyLiterals[i])) {
            atomPositions.push_back(i);
        }
    }

    // Validate given order
    assert(newOrder.size() == atomPositions.size());
    std::vector<unsigned int> nopOrder;
    for (unsigned int i = 0; i < atomPositions.size(); i++) {
        nopOrder.push_back(i);
    }
    assert(std::is_permutation(nopOrder.begin(), nopOrder.end(), newOrder.begin()));

    // Create a new clause with the given atom order, leaving the rest unchanged
    auto newClause = cloneHead(*clause);
    unsigned int currentAtom = 0;
    for (unsigned int currentLiteral = 0; currentLiteral < bodyLiterals.size(); currentLiteral++) {
        Literal* literalToAdd = bodyLiterals[currentLiteral];
        if (isA<Atom>(literalToAdd)) {
            // Atoms should be reordered
            literalToAdd = bodyLiterals[atomPositions[newOrder[currentAtom++]]];
        }
        newClause->addToBody(clone(literalToAdd));
    }

    // FIXME: tomp - fix ownership
    return newClause.release();
}

void negateConstraintInPlace(Constraint& constraint) {
    if (auto* bcstr = as<BooleanConstraint>(constraint)) {
        bcstr->set(!bcstr->isTrue());
    } else if (auto* cstr = as<BinaryConstraint>(constraint)) {
        cstr->setBaseOperator(souffle::negatedConstraintOp(cstr->getBaseOperator()));
    } else {
        fatal("Unknown ast-constraint type");
    }
}

bool renameAtoms(Own<Clause>& clause, const std::map<QualifiedName, QualifiedName>& oldToNew) {
    bool changed = false;
    visit(clause, [&](Atom& atom) {
        auto it = oldToNew.find(atom.getQualifiedName());
        if (it != oldToNew.end()) {
            atom.setQualifiedName(it->second);
            changed = true;
        }
    });
    return changed;
}

bool renameAtoms(Program& program, const std::map<QualifiedName, QualifiedName>& oldToNew) {
    bool changed = false;

    // Can't rename clause head atoms while the clause is attached w/o invalidating by-name lookup tables.
    // -> Detach clauses w/ renamed head atoms. Reinsert after atom rename.
    VecOwn<Clause> renamed_clauses;
    for ([[maybe_unused]] auto&& [name, info] : program.getRelationInfo()) {
        bool clauses_changed = false;
        for (auto&& cl : info.clauses) {
            assert(cl->getQualifiedName() == name && "sanity check - name lookup tables corrupted");
            clauses_changed |= renameAtoms(cl, oldToNew);
        }

        if (contains(oldToNew, name)) {
            assert((clauses_changed || info.clauses.empty()) && "clause heads should have been renamed");
            renamed_clauses = concat(std::move(renamed_clauses), std::move(info.clauses));
            info.clauses = VecOwn<Clause>{};
        }

        changed |= clauses_changed;
    }

    // Reattach clauses after doing renames to prevent transitive renames.
    // This function only does a single renaming 'step'.
    for (auto& cl : renamed_clauses)
        program.addClause(std::move(cl));

    return changed;
}

bool renameAtoms(Program& program, QualifiedName const& relation,
        const std::map<QualifiedName, QualifiedName>& oldToNew) {
    auto info = program.getRelationInfo(relation);
    if (!info) return false;

    bool changed = false;
    for (auto& cl : info->clauses)
        changed |= renameAtoms(cl, oldToNew);

    // if clause heads were renamed -> detach/re-insert clauses to maintain by-name lookup tables
    if (contains(oldToNew, relation)) {
        assert(changed && "clause head atoms should have been renamed");

        for (auto& cl : info->clauses)
            program.addClause(std::move(cl));  // steal & reinsert under new name

        info->clauses.clear();
    }

    return changed;
}
}  // namespace souffle::ast
