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
class TranslationUnit;
}  // namespace souffle::ast

namespace souffle::ast::analysis {
class IOTypeAnalysis;
class AuxiliaryArityAnalysis;
class FunctorAnalysis;
class PolymorphicObjectsAnalysis;
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
class TranslatorContext;
class ValueIndex;

class AstToRamTranslator {
public:
    AstToRamTranslator();
    ~AstToRamTranslator();

    /** Translates an AST program into a corresponding RAM program */
    Own<ram::TranslationUnit> translateUnit(ast::TranslationUnit& tu);

    // TODO (azreika): these probably belong more to the clause translator
    size_t getEvaluationArity(const ast::Atom* atom) const;
    static Own<ram::Condition> translateConstraint(
            const TranslatorContext& context, const ast::Literal* arg, const ValueIndex& index);
    static Own<ram::Expression> translateConstant(const TranslatorContext& context, const ast::Constant& c);

protected:
    const ast::Program* program = nullptr;
    Own<TranslatorContext> context;

    /** Analyses needed */
    const ast::analysis::PolymorphicObjectsAnalysis* polyAnalysis = nullptr;
    const ast::analysis::TypeEnvironment* typeEnv = nullptr;
    const ast::analysis::IOTypeAnalysis* ioType = nullptr;

    void addRamSubroutine(std::string subroutineID, Own<ram::Statement> subroutine);

    Own<ram::Relation> createRamRelation(
            const ast::Relation* baseRelation, std::string ramRelationName) const;
    VecOwn<ram::Relation> createRamRelations(const std::vector<size_t>& sccOrdering) const;
    virtual Own<ast::Clause> createDeltaClause(const ast::Clause* original, size_t recursiveAtomIdx) const;
    static RamDomain getConstantRamRepresentation(
            const TranslatorContext& context, const ast::Constant& constant);
    Own<ram::Statement> generateClauseVersion(const std::set<const ast::Relation*>& scc,
            const ast::Clause* cl, size_t deltaAtomIdx, size_t version) const;
    Own<ram::Statement> translateRecursiveClauses(
            const std::set<const ast::Relation*>& scc, const ast::Relation* rel) const;

    /** -- Generation methods -- */

    /** High-level relation translation */
    virtual Own<ram::Sequence> generateProgram(const ast::TranslationUnit& translationUnit);
    Own<ram::Statement> generateNonRecursiveRelation(const ast::Relation& rel) const;
    Own<ram::Statement> generateRecursiveStratum(const std::set<const ast::Relation*>& scc) const;

    /** IO translation */
    Own<ram::Statement> generateStoreRelation(const ast::Relation* relation) const;
    Own<ram::Statement> generateLoadRelation(const ast::Relation* relation) const;

    /** Low-level stratum translation */
    Own<ram::Statement> generateStratum(size_t scc) const;
    Own<ram::Statement> generateStratumPreamble(const std::set<const ast::Relation*>& scc) const;
    Own<ram::Statement> generateStratumPostamble(const std::set<const ast::Relation*>& scc) const;
    Own<ram::Statement> generateStratumLoopBody(const std::set<const ast::Relation*>& scc) const;
    Own<ram::Statement> generateStratumTableUpdates(const std::set<const ast::Relation*>& scc) const;
    Own<ram::Statement> generateStratumExitSequence(const std::set<const ast::Relation*>& scc) const;

    /** Other helper generations */
    virtual Own<ram::Statement> generateClearExpiredRelations(
            const std::set<const ast::Relation*>& expiredRelations) const;
    Own<ram::Statement> generateClearRelation(const ast::Relation* relation) const;
    Own<ram::Statement> generateMergeRelations(
            const ast::Relation* rel, const std::string& destRelation, const std::string& srcRelation) const;

    /** -- AST preprocessing -- */

    /** Main general preprocessor */
    virtual void preprocessAstProgram(ast::TranslationUnit& tu);

    /** Replace ADTs with special records */
    bool removeADTs(ast::TranslationUnit& translationUnit);

    /** Finalise the types of polymorphic objects */
    // TODO (azreika): should be removed once the translator is refactored to avoid cloning
    void finaliseAstTypes(ast::Program& program);

private:
    std::map<std::string, Own<ram::Statement>> ramSubroutines;
    Own<SymbolTable> symbolTable;
};

}  // namespace souffle::ast2ram
