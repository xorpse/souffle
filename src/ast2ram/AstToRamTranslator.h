/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2013, 2015, Oracle and/or its affiliates. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file AstToRamTranslator.h
 *
 * Translator from AST into RAM
 *
 ***********************************************************************/

#pragma once

#include "souffle/RamTypes.h"
#include "souffle/utility/ContainerUtil.h"
#include <cassert>
#include <map>
#include <set>
#include <string>
#include <vector>

namespace souffle {
class SymbolTable;
}

namespace souffle::ast {
class Argument;
class Atom;
class Clause;
class Constant;
class Literal;
class Program;
class QualifiedName;
class Relation;
class SipsMetric;
class TranslationUnit;
}  // namespace souffle::ast

namespace souffle::ast::analysis {
class IOTypeAnalysis;
class AuxiliaryArityAnalysis;
class FunctorAnalysis;
class PolymorphicObjectsAnalysis;
class RecursiveClausesAnalysis;
class RelationDetailCacheAnalysis;
class RelationScheduleAnalysis;
class SCCGraphAnalysis;
class TypeEnvironment;
}  // namespace souffle::ast::analysis

namespace souffle::ram {
class Condition;
class Expression;
class Relation;
class Sequence;
class Statement;
class TranslationUnit;
class TupleElement;
}  // namespace souffle::ram

namespace souffle::ast2ram {

struct Location;
class ValueIndex;

class AstToRamTranslator {
public:
    AstToRamTranslator();
    ~AstToRamTranslator();

    const ast::analysis::AuxiliaryArityAnalysis* getAuxArityAnalysis() const {
        return auxArityAnalysis;
    }

    const ast::analysis::FunctorAnalysis* getFunctorAnalysis() const {
        return functorAnalysis;
    }

    const ast::analysis::PolymorphicObjectsAnalysis* getPolymorphicObjectsAnalysis() const {
        return polyAnalysis;
    }

    const ast::SipsMetric* getSipsMetric() const {
        return sips.get();
    }

    /** translates AST to translation unit */
    Own<ram::TranslationUnit> translateUnit(ast::TranslationUnit& tu);

    /** translate an AST argument to a RAM value */
    Own<ram::Expression> translateValue(const ast::Argument* arg, const ValueIndex& index);

    /** determine the auxiliary for relations */
    size_t getEvaluationArity(const ast::Atom* atom) const;

    /** create a RAM element access node */
    static Own<ram::TupleElement> makeRamTupleElement(const Location& loc);

    /** translate an AST constraint to a RAM condition */
    Own<ram::Condition> translateConstraint(const ast::Literal* arg, const ValueIndex& index);

    /** translate RAM code for a constant value */
    Own<ram::Expression> translateConstant(ast::Constant const& c);

    const ram::Relation* lookupRelation(const std::string& name) const {
        auto it = ramRels.find(name);
        assert(it != ramRels.end() && "relation not found");
        return (*it).second.get();
    }

protected:
    /** AST program */
    const ast::Program* program = nullptr;

    /** RAM program */
    Own<ram::Statement> ramMain;

    /** Subroutines */
    std::map<std::string, Own<ram::Statement>> ramSubs;

    /** RAM relations */
    std::map<std::string, Own<ram::Relation>> ramRels;

    const ast::analysis::AuxiliaryArityAnalysis* auxArityAnalysis = nullptr;

    /**
     * assigns names to unnamed variables such that enclosing
     * constructs may be cloned without losing the variable-identity
     */
    virtual void addNegation(ast::Clause& clause, const ast::Atom* atom);

    void nameUnnamedVariables(ast::Clause* clause);

    void appendStmt(VecOwn<ram::Statement>& stmtList, Own<ram::Statement> stmt);

    /** translate AST to RAM Program */
    virtual void translateProgram(const ast::TranslationUnit& translationUnit);

    virtual void clearExpiredRelations(
            VecOwn<ram::Statement>& stmts, const std::set<const ast::Relation*>& expiredRelations);

private:
    /** Type environment */
    const ast::analysis::TypeEnvironment* typeEnv = nullptr;

    /** Analyses needed */
    const ast::analysis::IOTypeAnalysis* ioType = nullptr;
    const ast::analysis::FunctorAnalysis* functorAnalysis = nullptr;
    const ast::analysis::RelationScheduleAnalysis* relationSchedule = nullptr;
    const ast::analysis::SCCGraphAnalysis* sccGraph = nullptr;
    const ast::analysis::RecursiveClausesAnalysis* recursiveClauses = nullptr;
    const ast::analysis::RelationDetailCacheAnalysis* relDetail = nullptr;
    const ast::analysis::PolymorphicObjectsAnalysis* polyAnalysis = nullptr;

    /** SIPS metric for reordering */
    Own<ast::SipsMetric> sips;

    /** replace ADTs with special records */
    static bool removeADTs(const ast::TranslationUnit& translationUnit);

    // TODO (b-scholz): revisit / refactor so that only one directive is translated
    std::vector<std::map<std::string, std::string>> getInputDirectives(const ast::Relation* rel);

    // TODO (b-scholz): revisit / refactor so that only one directive is translated
    std::vector<std::map<std::string, std::string>> getOutputDirectives(const ast::Relation* rel);

    /** Return a symbol table **/
    SymbolTable& getSymbolTable();

    /** Get ram representation of constant */
    RamDomain getConstantRamRepresentation(const ast::Constant& constant);

    /** translate RAM code for a given SCC */
    VecOwn<ram::Statement> translateSCC(size_t scc, size_t idx);

    /** create RAM relations for a given SCC */
    void createRamRelation(size_t scc);

    /** translate RAM code for the non-recursive clauses of the given relation */
    Own<ram::Statement> translateNonRecursiveRelation(const ast::Relation& rel);

    /** translate RAM code for recursive relations in a strongly-connected component */
    Own<ram::Statement> translateRecursiveRelation(const std::set<const ast::Relation*>& scc);

    /** add a statement to store a relation */
    void makeRamClear(VecOwn<ram::Statement>& curStmts, const ast::Relation* relation);

    /** add a statement to drop a relation */
    void makeRamStore(VecOwn<ram::Statement>& curStmts, const ast::Relation* relation);

    /** add a statement to load a relation */
    void makeRamLoad(VecOwn<ram::Statement>& curStmts, const ast::Relation* relation);
};

}  // namespace souffle::ast2ram
