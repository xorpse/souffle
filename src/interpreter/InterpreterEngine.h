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
#include "souffle/io/IOSystem.h"  // Remove later
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
    /** Execute helper. Common part of Aggregate & AggregateIndex. */
    template <typename Aggregate, typename Iter>
    RamDomain executeAggregate(const Aggregate& aggregate, const InterpreterNode& filter,
            const InterpreterNode* expression, const InterpreterNode& nestedOperation, const Iter& ranges,
            InterpreterContext& ctxt);
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

private:
    // TODO
#define CAL_SEARCH_BOUND(superInfo, low, high)                          \
    /** Unbounded and Constant */                                       \
    memcpy(low, superInfo.first.data(), sizeof(low));                   \
    memcpy(high, superInfo.second.data(), sizeof(high));                \
    /* TupleElement */                                                  \
    for (const auto& tupleElement : superInfo.tupleFirst) {             \
        low[tupleElement[0]] = ctxt[tupleElement[1]][tupleElement[2]];  \
    }                                                                   \
    for (const auto& tupleElement : superInfo.tupleSecond) {            \
        high[tupleElement[0]] = ctxt[tupleElement[1]][tupleElement[2]]; \
    }                                                                   \
    /* Generic */                                                       \
    for (const auto& expr : superInfo.exprFirst) {                      \
        low[expr.first] = execute(expr.second.get(), ctxt);             \
    }                                                                   \
    for (const auto& expr : superInfo.exprSecond) {                     \
        high[expr.first] = execute(expr.second.get(), ctxt);            \
    }
    // TODO
    /* template <typename Rel> */
    /* RamDomain evalEmptinessCheck(const Rel& rel); */

    // TODO
    /* template <std::size_t arity, typename Index> */
    /* RamDomain evalRelationSize(const InterpreterRelation<arity, Index>& rel, InterpreterContext& ctxt); */

    //
    template <typename Rel>
    RamDomain evalExistenceCheck(const ram::ExistenceCheck& cur, const InterpreterExistenceCheck& shadow,
            InterpreterContext& ctxt) {
        constexpr size_t arity = Rel::arity;
        size_t viewPos = shadow.getViewId();

        if (profileEnabled && !cur.getRelation().isTemp()) {
            reads[cur.getRelation().getName()]++;
        }

        const auto& superInfo = shadow.getSuperInst();
        // for total we use the exists test
        if (shadow.isTotalSearch()) {
            souffle::Tuple<RamDomain, arity> tuple;
            memcpy(tuple.data, superInfo.first.data(), sizeof(tuple));
            /* TupleElement */
            for (const auto& tupleElement : superInfo.tupleFirst) {
                tuple[tupleElement[0]] = ctxt[tupleElement[1]][tupleElement[2]];
            }
            /* Generic */
            for (const auto& expr : superInfo.exprFirst) {
                tuple[expr.first] = execute(expr.second.get(), ctxt);
            }
            return Rel::castView(ctxt.getView(viewPos))->contains(tuple);
        }

        // for partial we search for lower and upper boundaries
        souffle::Tuple<RamDomain, arity> low;
        souffle::Tuple<RamDomain, arity> high;
        memcpy(low.data, superInfo.first.data(), sizeof(low));
        memcpy(high.data, superInfo.second.data(), sizeof(high));

        /* TupleElement */
        for (const auto& tupleElement : superInfo.tupleFirst) {
            low[tupleElement[0]] = ctxt[tupleElement[1]][tupleElement[2]];
            high[tupleElement[0]] = low[tupleElement[0]];
        }
        /* Generic */
        for (const auto& expr : superInfo.exprFirst) {
            low[expr.first] = execute(expr.second.get(), ctxt);
            high[expr.first] = low[expr.first];
        }

        return Rel::castView(ctxt.getView(viewPos))->contains(low, high);
    }

    template <typename Rel>
    RamDomain evalProvenanceExistenceCheck(
            const InterpreterProvenanceExistenceCheck& shadow, InterpreterContext& ctxt) {
        // construct the pattern tuple
        constexpr size_t arity = Rel::arity;
        const auto& superInfo = shadow.getSuperInst();

        // for partial we search for lower and upper boundaries
        souffle::Tuple<RamDomain, arity> low;
        souffle::Tuple<RamDomain, arity> high;
        memcpy(low.data, superInfo.first.data(), sizeof(low));
        memcpy(high.data, superInfo.second.data(), sizeof(high));

        /* TupleElement */
        for (const auto& tupleElement : superInfo.tupleFirst) {
            low[tupleElement[0]] = ctxt[tupleElement[1]][tupleElement[2]];
            high[tupleElement[0]] = low[tupleElement[0]];
        }
        /* Generic */
        for (const auto& expr : superInfo.exprFirst) {
            assert(expr.second.get() != nullptr &&
                    "ProvenanceExistenceCheck should always be specified for payload");
            low[expr.first] = execute(expr.second.get(), ctxt);
            high[expr.first] = low[expr.first];
        }

        low[arity - 2] = MIN_RAM_SIGNED;
        low[arity - 1] = MIN_RAM_SIGNED;
        high[arity - 2] = MAX_RAM_SIGNED;
        high[arity - 1] = MAX_RAM_SIGNED;

        // obtain view
        size_t viewPos = shadow.getViewId();

        // get an equalRange
        auto equalRange = Rel::castView(ctxt.getView(viewPos))->range(low, high);

        // if range is empty
        if (equalRange.begin() == equalRange.end()) {
            return false;
        }

        // check whether the height is less than the current height
        return (*equalRange.begin())[arity - 1] <= execute(shadow.getChild(), ctxt);
    }

    template <typename Rel>
    RamDomain evalScan(
            const Rel& rel, const ram::Scan& cur, const InterpreterScan& shadow, InterpreterContext& ctxt) {
        for (const auto& tuple : rel.scan()) {
            ctxt[cur.getTupleId()] = tuple.data;
            if (!execute(shadow.getNestedOperation(), ctxt)) {
                break;
            }
        }
        return true;
    }

    // TODO
    /* template <std::size_t arity, typename Index> */
    /* RamDomain evalParallelScan(InterpreterContext& ctxt); */

    template <typename Rel>
    RamDomain evalIndexScan(
            const ram::IndexScan& cur, const InterpreterIndexScan& shadow, InterpreterContext& ctxt) {
        constexpr size_t arity = Rel::arity;
        // create pattern tuple for range query
        const auto& superInfo = shadow.getSuperInst();
        souffle::Tuple<RamDomain, arity> low;
        souffle::Tuple<RamDomain, arity> high;
        CAL_SEARCH_BOUND(superInfo, low.data, high.data);

        size_t viewId = shadow.getViewId();
        auto view = Rel::castView(ctxt.getView(viewId));
        // conduct range query
        for (const auto& tuple : view->range(low, high)) {
            ctxt[cur.getTupleId()] = tuple.data;
            if (!execute(shadow.getNestedOperation(), ctxt)) {
                break;
            }
        }
        return true;
    }

    // TODO
    /* template <std::size_t arity, typename Index> */
    /* RamDomain evalParallelIndexScan(InterpreterContext& ctxt); */

    template <typename Rel>
    RamDomain evalChoice(const Rel& rel, const ram::Choice& cur, const InterpreterChoice& shadow,
            InterpreterContext& ctxt) {
        // use simple iterator
        for (const auto& tuple : rel.scan()) {
            ctxt[cur.getTupleId()] = tuple.data;
            if (execute(shadow.getCondition(), ctxt)) {
                execute(shadow.getNestedOperation(), ctxt);
                break;
            }
        }
        return true;
    }

    // TODO
    /* template <std::size_t arity, typename Index> */
    /* RamDomain evalParallelChoice(InterpreterContext& ctxt); */

    template <typename Rel>
    RamDomain evalIndexChoice(
            const ram::IndexChoice& cur, const InterpreterIndexChoice& shadow, InterpreterContext& ctxt) {
        constexpr size_t arity = Rel::arity;
        const auto& superInfo = shadow.getSuperInst();
        souffle::Tuple<RamDomain, arity> low;
        souffle::Tuple<RamDomain, arity> high;
        CAL_SEARCH_BOUND(superInfo, low.data, high.data);

        size_t viewId = shadow.getViewId();
        auto view = Rel::castView(ctxt.getView(viewId));

        for (const auto& tuple : view->range(low, high)) {
            ctxt[cur.getTupleId()] = tuple.data;
            if (execute(shadow.getCondition(), ctxt)) {
                execute(shadow.getNestedOperation(), ctxt);
                break;
            }
        }
        return true;
    }
    // TODO
    /* template <std::size_t arity, typename Index> */
    /* RamDomain evalParallelIndexChoice(InterpreterContext& ctxt); */

    // TODO
    /* template <std::size_t arity, typename Index> */
    /* RamDomain evalParallelAggregate(InterpreterContext& ctxt); */

    // TODO merge with executeAggregate
    /* template <std::size_t arity, typename Index> */
    /* RamDomain evalAggregate(InterpreterContext& ctxt); */

    // TODO
    template <std::size_t arity, typename Index>
    RamDomain evalParallelIndexAggregate(InterpreterContext& ctxt);

    template <typename Rel>
    RamDomain evalIndexAggregate(const ram::IndexAggregate& cur, const InterpreterIndexAggregate& shadow,
            InterpreterContext& ctxt) {
        // init temporary tuple for this level
        const size_t arity = Rel::arity;
        const auto& superInfo = shadow.getSuperInst();
        souffle::Tuple<RamDomain, arity> low;
        souffle::Tuple<RamDomain, arity> high;
        CAL_SEARCH_BOUND(superInfo, low.data, high.data)

        size_t viewId = shadow.getViewId();
        auto view = Rel::castView(ctxt.getView(viewId));

        return executeAggregate(cur, *shadow.getCondition(), shadow.getExpr(), *shadow.getNestedOperation(),
                view->range(low, high), ctxt);
    }

    template <typename Rel>
    RamDomain evalProject(Rel& rel, const InterpreterProject& shadow, InterpreterContext& ctxt) {
        constexpr size_t arity = Rel::arity;
        const auto& superInfo = shadow.getSuperInst();
        souffle::Tuple<RamDomain, arity> tuple;
        memcpy(tuple.data, superInfo.first.data(), (arity * sizeof(RamDomain)));
        /* TupleElement */
        for (const auto& tupleElement : superInfo.tupleFirst) {
            tuple[tupleElement[0]] = ctxt[tupleElement[1]][tupleElement[2]];
        }
        /* Generic */
        for (const auto& expr : superInfo.exprFirst) {
            tuple[expr.first] = execute(expr.second.get(), ctxt);
        }

        // insert in target relation
        rel.insert(tuple);
        return true;
    }

    template <typename Rel>
    RamDomain evalClear(Rel& rel);

    // TODO: Try not specialized IO with virtual?
    template <typename Rel>
    RamDomain evalIO(const ram::IO& cur, Rel& rel) {
        const auto& directive = cur.getDirectives();
        const std::string& op = cur.get("operation");

        if (op == "input") {
            try {
                IOSystem::getInstance()
                        .getReader(directive, getSymbolTable(), getRecordTable())
                        ->readAll(rel);
            } catch (std::exception& e) {
                std::cerr << "Error loading data: " << e.what() << "\n";
            }
            return true;
        } else if (op == "output" || op == "printsize") {
            try {
                IOSystem::getInstance()
                        .getWriter(directive, getSymbolTable(), getRecordTable())
                        ->writeAll(rel);
            } catch (std::exception& e) {
                std::cerr << e.what();
                exit(EXIT_FAILURE);
            }
            return true;
        } else {
            assert("wrong i/o operation");
            return true;
        }
    }
};

}  // namespace souffle
