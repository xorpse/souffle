/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2019, The Souffle Developers. All rights reserved.
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file InterpreterEngine.h
 *
 * Declares the Interpreter Engine class. The engine takes in an InterpreterNode
 * representation and execute them.
 ***********************************************************************/

#pragma once

#include "Global.h"
#include "interpreter/InterpreterContext.h"
#include "interpreter/InterpreterGenerator.h"
#include "interpreter/InterpreterIndex.h"
#include "interpreter/InterpreterNode.h"
#include "interpreter/InterpreterRelation.h"
#include "ram/TranslationUnit.h"
#include "ram/analysis/Index.h"
#include "souffle/RamTypes.h"
#include "souffle/RecordTable.h"
#include "souffle/SymbolTable.h"
#include "souffle/utility/ContainerUtil.h"
#include <atomic>
#include <cstddef>
#include <deque>
#include <map>
#include <memory>
#include <string>
#include <vector>
#ifdef _OPENMP
#include <omp.h>
#endif

namespace souffle {

class InterpreterProgInterface;

/**
 * @class InterpreterEngine
 * @brief This class translate the RAM Program into executable format and interpreter it.
 */
class InterpreterEngine {
    using RelationHandle = Own<InterpreterRelationWrapper>;
    friend InterpreterProgInterface;

public:
    InterpreterEngine(ram::TranslationUnit& tUnit)
            : profileEnabled(Global::config().has("profile")),
              numOfThreads(std::stoi(Global::config().get("jobs"))), tUnit(tUnit),
              isa(tUnit.getAnalysis<ram::analysis::IndexAnalysis>()), generator(isa) {
#ifdef _OPENMP
        if (numOfThreads > 0) {
            omp_set_num_threads(numOfThreads);
        }
#endif
    }
    /** @brief Execute the main program */
    void executeMain();
    /** @brief Execute the subroutine program */
    void executeSubroutine(
            const std::string& name, const std::vector<RamDomain>& args, std::vector<RamDomain>& ret);

private:
    /** @brief Generate intermediate representation from RAM */
    void generateIR();
    /** @brief Remove a relation from the environment */
    void dropRelation(const size_t relId);
    /** @brief Swap the content of two relations */
    void swapRelation(const size_t ramRel1, const size_t ramRel2);
    /** @brief Return a reference to the relation on the given index */
    RelationHandle& getRelationHandle(const size_t idx);
    /** @brief Return the string symbol table */
    SymbolTable& getSymbolTable();
    /** @brief Return the record table */
    RecordTable& getRecordTable();
    /** @brief Return the ram::TranslationUnit */
    ram::TranslationUnit& getTranslationUnit();
    /** @brief Execute the program */
    RamDomain execute(const InterpreterNode*, InterpreterContext&);
    /** @brief Return method handler */
    void* getMethodHandle(const std::string& method);
    /** @brief Load DLL */
    const std::vector<void*>& loadDLL();
    /** @brief Return current iteration number for loop operation */
    size_t getIterationNumber() const;
    /** @brief Increase iteration number by one */
    void incIterationNumber();
    /** @brief Reset iteration number */
    void resetIterationNumber();
    /** @brief Increment the counter */
    int incCounter();
    /** @brief Return the relation map. */
    VecOwn<RelationHandle>& getRelationMap();

    /** If profile is enable in this program */
    const bool profileEnabled;
    /** subroutines */
    VecOwn<InterpreterNode> subroutine;
    /** main program */
    Own<InterpreterNode> main;
    /** Number of threads enabled for this program */
    size_t numOfThreads;
    /** Profile counter */
    std::atomic<RamDomain> counter{0};
    /** Loop iteration counter */
    size_t iteration = 0;
    /** Profile for rule frequencies */
    std::map<std::string, std::deque<std::atomic<size_t>>> frequencies;
    /** Profile for relation reads */
    std::map<std::string, std::atomic<size_t>> reads;
    /** DLL */
    std::vector<void*> dll;
    /** Program */
    ram::TranslationUnit& tUnit;
    /** IndexAnalysis */
    ram::analysis::IndexAnalysis* isa;
    /** Interpreter program generator */
    NodeGenerator generator;
    /** Record Table*/
    RecordTable recordTable;

    // -- Defines template for specialized interpreter operation -- */
private:
    template <typename Rel>
    RamDomain evalExistenceCheck(const ram::ExistenceCheck& cur, const InterpreterExistenceCheck& shadow,
            InterpreterContext& ctxt);

    template <typename Rel>
    RamDomain evalProvenanceExistenceCheck(
            const InterpreterProvenanceExistenceCheck& shadow, InterpreterContext& ctxt);

    template <typename Rel>
    RamDomain evalScan(
            const Rel& rel, const ram::Scan& cur, const InterpreterScan& shadow, InterpreterContext& ctxt);

    template <typename Rel>
    RamDomain evalParallelScan(const Rel& rel, const ram::ParallelScan& cur,
            const InterpreterParallelScan& shadow, InterpreterContext& ctxt);

    template <typename Rel>
    RamDomain evalIndexScan(
            const ram::IndexScan& cur, const InterpreterIndexScan& shadow, InterpreterContext& ctxt);

    template <typename Rel>
    RamDomain evalParallelIndexScan(const Rel& rel, const ram::ParallelIndexScan& cur,
            const InterpreterParallelIndexScan& shadow, InterpreterContext& ctxt);

    template <typename Rel>
    RamDomain evalChoice(const Rel& rel, const ram::Choice& cur, const InterpreterChoice& shadow,
            InterpreterContext& ctxt);

    template <typename Rel>
    RamDomain evalParallelChoice(const Rel& rel, const ram::ParallelChoice& cur,
            const InterpreterParallelChoice& shadow, InterpreterContext& ctxt);

    template <typename Rel>
    RamDomain evalIndexChoice(
            const ram::IndexChoice& cur, const InterpreterIndexChoice& shadow, InterpreterContext& ctxt);

    template <typename Rel>
    RamDomain evalParallelIndexChoice(const Rel& rel, const ram::ParallelIndexChoice& cur,
            const InterpreterParallelIndexChoice& shadow, InterpreterContext& ctxt);

    template <typename Aggregate, typename Iter>
    RamDomain evalAggregate(const Aggregate& aggregate, const InterpreterNode& filter,
            const InterpreterNode* expression, const InterpreterNode& nestedOperation, const Iter& ranges,
            InterpreterContext& ctxt);

    template <typename Rel>
    RamDomain evalParallelAggregate(const Rel& rel, const ram::ParallelAggregate& cur,
            const InterpreterParallelAggregate& shadow, InterpreterContext& ctxt);

    template <typename Rel>
    RamDomain evalParallelIndexAggregate(const ram::ParallelIndexAggregate& cur,
            const InterpreterParallelIndexAggregate& shadow, InterpreterContext& ctxt);

    template <typename Rel>
    RamDomain evalIndexAggregate(const ram::IndexAggregate& cur, const InterpreterIndexAggregate& shadow,
            InterpreterContext& ctxt);

    template <typename Rel>
    RamDomain evalProject(Rel& rel, const InterpreterProject& shadow, InterpreterContext& ctxt);
};

}  // namespace souffle
