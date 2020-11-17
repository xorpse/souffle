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
        return sipsMetric.get();
    }

    size_t getEvaluationArity(const ast::Atom* atom) const;
    const ram::Relation* lookupRelation(const std::string& name) const;

    /** AST->RAM translation methods */
    Own<ram::TranslationUnit> translateUnit(ast::TranslationUnit& tu);
    Own<ram::Expression> translateValue(const ast::Argument* arg, const ValueIndex& index) const;
    Own<ram::Condition> translateConstraint(const ast::Literal* arg, const ValueIndex& index);
    Own<ram::Expression> translateConstant(const ast::Constant& c);
    virtual Own<ram::Sequence> translateProgram(const ast::TranslationUnit& translationUnit);

protected:
    const ast::Program* program = nullptr;
    Own<ast::SipsMetric> sipsMetric;

    /**
     * Analyses needed
     */
    const ast::analysis::TypeEnvironment* typeEnv = nullptr;
    const ast::analysis::IOTypeAnalysis* ioType = nullptr;
    const ast::analysis::FunctorAnalysis* functorAnalysis = nullptr;
    const ast::analysis::AuxiliaryArityAnalysis* auxArityAnalysis = nullptr;
    const ast::analysis::RelationScheduleAnalysis* relationSchedule = nullptr;
    const ast::analysis::SCCGraphAnalysis* sccGraph = nullptr;
    const ast::analysis::RecursiveClausesAnalysis* recursiveClauses = nullptr;
    const ast::analysis::RelationDetailCacheAnalysis* relDetail = nullptr;
    const ast::analysis::PolymorphicObjectsAnalysis* polyAnalysis = nullptr;

    /**
     * Translation methods
     */
    Own<ram::Sequence> translateSCC(size_t scc, size_t idx);
    virtual void addNegation(ast::Clause& clause, const ast::Atom* atom) const;
    virtual VecOwn<ram::Statement> clearExpiredRelations(
            const std::set<const ast::Relation*>& expiredRelations) const;
    RamDomain getConstantRamRepresentation(const ast::Constant& constant);

    /* translate RAM code for the non-recursive clauses of the given relation */
    Own<ram::Statement> translateNonRecursiveRelation(const ast::Relation& rel);

    /** translate RAM code for recursive relations in a strongly-connected component */
    Own<ram::Statement> translateRecursiveRelation(const std::set<const ast::Relation*>& scc);

    /** add a statement to drop a relation */
    void makeRamStore(VecOwn<ram::Statement>& curStmts, const ast::Relation* relation);

    /** add a statement to load a relation */
    void makeRamLoad(VecOwn<ram::Statement>& curStmts, const ast::Relation* relation);

    void addRamSubroutine(std::string subroutineID, Own<ram::Statement> subroutine);
    void addRamRelation(std::string relationName, Own<ram::Relation> ramRelation);

private:
    std::map<std::string, Own<ram::Statement>> ramSubroutines;
    std::map<std::string, Own<ram::Relation>> ramRelations;
    Own<SymbolTable> symbolTable;

    // TODO (b-scholz): revisit / refactor so that only one directive is translated
    std::vector<std::map<std::string, std::string>> getInputDirectives(const ast::Relation* rel);
    std::vector<std::map<std::string, std::string>> getOutputDirectives(const ast::Relation* rel);

    /** create RAM relations for a given SCC */
    void createRamRelation(size_t scc);

    /** replace ADTs with special records */
    static bool removeADTs(const ast::TranslationUnit& translationUnit);

    /** finalise the types of polymorphic objects */
    // TODO (azreika): should be removed once the translator is refactored to avoid cloning
    void finaliseAstTypes();

    Own<ram::Statement> generateRelationMerge(
            const ast::Relation* rel, const std::string& destRelation, const std::string& srcRelation) const;

    VecOwn<ram::Statement> createRecursiveClauseVersions(
            const std::set<const ast::Relation*>& scc, const ast::Relation* rel);

    Own<ram::Statement> generateStratumPreamble(const std::set<const ast::Relation*>& scc);
    Own<ram::Statement> generateStratumPostamble(const std::set<const ast::Relation*>& scc) const;
    Own<ram::Statement> generateStratumTableUpdates(const std::set<const ast::Relation*>& scc) const;
    Own<ram::Statement> generateStratumMainLoop(const std::set<const ast::Relation*>& scc);
    Own<ram::Statement> generateStratumExitSequence(const std::set<const ast::Relation*>& scc) const;
};

}  // namespace souffle::ast2ram
