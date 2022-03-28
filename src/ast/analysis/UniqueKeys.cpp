/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2022, The Souffle Developers. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file UniqueKeys.cpp
 *
 * CountUniqueKeys are used for accumulating selectivity statistics for the auto scheduler
 * This analysis determines which CountUniqueKeys statements to emit in the RAM
 *
 ***********************************************************************/

#include "ast/analysis/UniqueKeys.h"
#include "Global.h"
#include "GraphUtils.h"
#include "ast/BinaryConstraint.h"
#include "ast/Constant.h"
#include "ast/NilConstant.h"
#include "ast/QualifiedName.h"
#include "ast/Relation.h"
#include "ast/StringConstant.h"
#include "ast/SubsumptiveClause.h"
#include "ast/UnnamedVariable.h"
#include "ast2ram/utility/Utils.h"
#include "ram/FloatConstant.h"
#include "ram/SignedConstant.h"
#include "ram/StringConstant.h"
#include "ram/UnsignedConstant.h"
#include "souffle/BinaryConstraintOps.h"
#include "souffle/utility/ContainerUtil.h"
#include <algorithm>
#include <cstddef>
#include <iterator>
#include <memory>
#include <numeric>
#include <optional>
#include <set>
#include <unordered_map>
#include <unordered_set>

namespace souffle::ast::analysis {

const std::vector<std::vector<std::size_t>>& UniqueKeysAnalysis::getSubsets(
        std::size_t N, std::size_t K) const {
    if (cache.count({N, K})) {
        return cache.at({N, K});
    }
    // result of all combinations
    std::vector<std::vector<std::size_t>> res;

    // specific combination
    std::vector<std::size_t> cur;
    cur.reserve(K);

    // use bitmask for subset generation
    std::string bitmask(K, 1);  // K leading 1's
    bitmask.resize(N, 0);       // N-K trailing 0's

    // generate the combination while there are combinations to go
    do {
        cur.clear();
        for (std::size_t i = 0; i < N; ++i)  // [0..N-1] integers
        {
            if (bitmask[i]) {
                cur.push_back(i);
            }
        }
        res.push_back(cur);
    } while (std::prev_permutation(bitmask.begin(), bitmask.end()));

    cache[std::make_pair(N, K)] = res;
    return cache.at({N, K});
}

std::vector<Own<souffle::ram::CountUniqueKeys>> UniqueKeysAnalysis::computeRuleVersionStatements(
        const std::set<const ast::Relation*>& scc, const ast::Clause& clause,
        std::optional<std::size_t> version) {
    auto* prog = program;
    auto* poly = polyAnalysis;
    auto sccAtoms = filter(ast::getBodyLiterals<ast::Atom>(clause),
            [&](auto* atom) { return contains(scc, prog->getRelation(*atom)); });

    auto translateConstant = [poly](const ast::Constant& constant) -> Own<souffle::ram::Expression> {
        if (auto strConstant = as<ast::StringConstant>(constant)) {
            return mk<ram::StringConstant>(strConstant->getConstant());
        } else if (isA<ast::NilConstant>(&constant)) {
            return mk<ram::SignedConstant>(0);
        } else if (auto* numConstant = as<ast::NumericConstant>(constant)) {
            switch (poly->getInferredType(*numConstant)) {
                case ast::NumericConstant::Type::Int:
                    return mk<ram::SignedConstant>(
                            RamSignedFromString(numConstant->getConstant(), nullptr, 0));
                case ast::NumericConstant::Type::Uint:
                    return mk<ram::UnsignedConstant>(
                            RamUnsignedFromString(numConstant->getConstant(), nullptr, 0));
                case ast::NumericConstant::Type::Float:
                    return mk<ram::FloatConstant>(RamFloatFromString(numConstant->getConstant()));
            }
        }
        fatal("unaccounted-for constant");
    };

    std::vector<Own<souffle::ram::CountUniqueKeys>> statements;

    auto getClauseAtomName = [&sccAtoms, &version](
                                     const ast::Clause& clause, const ast::Atom* atom, bool isRecursive) {
        using namespace souffle::ast2ram;
        if (!isRecursive) {
            return getConcreteRelationName(atom->getQualifiedName());
        }

        assert(version.has_value() && "Recursive rule must have a rule version");

        if (clause.getHead() == atom) {
            return getNewRelationName(atom->getQualifiedName());
        }
        if (sccAtoms.at(*version) == atom) {
            return getDeltaRelationName(atom->getQualifiedName());
        }
        return getConcreteRelationName(atom->getQualifiedName());
    };

    std::unordered_set<std::size_t> recursiveInCurrentStratum;
    auto atoms = ast::getBodyLiterals<ast::Atom>(clause);
    auto constraints = ast::getBodyLiterals<ast::BinaryConstraint>(clause);

    for (auto* a : sccAtoms) {
        for (std::size_t i = 0; i < atoms.size(); ++i) {
            if (*atoms[i] == *a) {
                recursiveInCurrentStratum.insert(i);
            }
        }
    }

    // map variable name to constants if possible
    std::unordered_map<std::string, ast::Constant*> varToConstant;

    // map variables to necessary variables on other side of the equality
    // i.e. x = y + z we should map x -> { y, z }
    std::unordered_map<std::string, std::set<std::string>> varToOtherVars;

    std::unordered_map<std::string, std::pair<std::set<std::string>, std::set<std::string>>> ineqToUpperLower;

    for (auto* constraint : constraints) {
        auto* lhs = constraint->getLHS();
        auto* rhs = constraint->getRHS();

        if (isIneqConstraint(constraint->getBaseOperator())) {
            if (auto* var = as<ast::Variable>(lhs)) {
                std::set<std::string> otherVars;
                visit(rhs, [&](const ast::Variable& v) { otherVars.insert(v.getName()); });
                if (isLessThan(constraint->getBaseOperator()) || isLessEqual(constraint->getBaseOperator())) {
                    ineqToUpperLower[var->getName()].second = otherVars;
                }
                if (isGreaterThan(constraint->getBaseOperator()) ||
                        isGreaterEqual(constraint->getBaseOperator())) {
                    ineqToUpperLower[var->getName()].first = otherVars;
                }
            }

            if (auto* var = as<ast::Variable>(rhs)) {
                std::set<std::string> otherVars;
                visit(lhs, [&](const ast::Variable& v) { otherVars.insert(v.getName()); });
                if (isLessThan(constraint->getBaseOperator()) || isLessEqual(constraint->getBaseOperator())) {
                    ineqToUpperLower[var->getName()].first = otherVars;
                }
                if (isGreaterThan(constraint->getBaseOperator()) ||
                        isGreaterEqual(constraint->getBaseOperator())) {
                    ineqToUpperLower[var->getName()].second = otherVars;
                }
            }
        }

        // only consider = constraint
        if (!isEqConstraint(constraint->getBaseOperator())) {
            continue;
        }

        if (isA<ast::Variable>(lhs) && isA<ast::Constant>(rhs)) {
            varToConstant[as<ast::Variable>(lhs)->getName()] = as<ast::Constant>(rhs);
            continue;
        }

        if (isA<ast::Constant>(lhs) && isA<ast::Variable>(rhs)) {
            varToConstant[as<ast::Variable>(rhs)->getName()] = as<ast::Constant>(lhs);
            continue;
        }

        if (auto* var = as<ast::Variable>(lhs)) {
            std::set<std::string> otherVars;
            visit(rhs, [&](const ast::Variable& v) { otherVars.insert(v.getName()); });
            varToOtherVars[var->getName()] = otherVars;
            continue;
        }

        if (auto* var = as<ast::Variable>(rhs)) {
            std::set<std::string> otherVars;
            visit(lhs, [&](const ast::Variable& v) { otherVars.insert(v.getName()); });
            varToOtherVars[var->getName()] = otherVars;
            continue;
        }
    }

    // check for bounded inequality i.e. EA < EA2 < EA + Size
    for (auto& p : ineqToUpperLower) {
        // consider this like an equality
        auto& [lower, upper] = p.second;
        if (!lower.empty() && !upper.empty() &&
                std::includes(upper.begin(), upper.end(), lower.begin(), lower.end())) {
            varToOtherVars[p.first] = upper;
        }
    }

    std::unordered_map<std::size_t, std::set<std::string>> atomIdxToGroundedVars;
    for (std::size_t i = 0; i < atoms.size(); ++i) {
        std::set<std::string> groundedVars;
        visit(*atoms[i], [&](const ast::Variable& v) { groundedVars.insert(v.getName()); });
        atomIdxToGroundedVars[i] = groundedVars;
    }

    // #atoms -> variables to join
    std::map<std::size_t, std::set<std::set<std::size_t>>> cache;

    std::unordered_map<std::size_t, std::map<std::size_t, const ram::Expression*>> atomToIdxConstants;

    VecOwn<const ram::Expression> constants;

    std::size_t atomIdx = 0;
    for (auto* atom : atoms) {
        bool isRecursive = recursiveInCurrentStratum.count(atomIdx) > 0;
        std::string name = getClauseAtomName(clause, atom, isRecursive);
        std::map<std::size_t, const ram::Expression*> idxConstant;

        std::size_t i = 0;
        for (auto* argument : atom->getArguments()) {
            // if we have a variable and a constraint of the form x = 2 then treat x as 2
            if (auto* var = as<ast::Variable>(argument)) {
                if (varToConstant.count(var->getName()) > 0) {
                    argument = varToConstant.at(var->getName());
                }
            }

            if (auto* constant = as<ast::Constant>(argument)) {
                auto ramConstant = translateConstant(*constant);
                idxConstant[i] = ramConstant.get();
                constants.push_back(std::move(ramConstant));
            }
            ++i;
        }

        atomToIdxConstants[atomIdx] = std::move(idxConstant);

        // start by storing the access cost for each individual relation
        cache[1].insert({atomIdx});
        ++atomIdx;
    }

    // for each element in the atom
    for (std::size_t i = 0; i < atoms.size(); ++i) {
        // construct the set S \ S[i] and S[i]
        std::unordered_set<std::size_t> otherAtoms;
        for (std::size_t j = 0; j < atoms.size(); ++j) {
            if (i != j) {
                otherAtoms.insert(j);
            }
        }

        // construct the set of variables that can be used for an indexed scan on this atom
        std::unordered_set<std::string> varDependencies;
        for (const auto& arg : atoms[i]->getArguments()) {
            if (const auto* var = as<const ast::Variable>(arg)) {
                varDependencies.insert(var->getName());
                auto& dependentVars = varToOtherVars[var->getName()];
                varDependencies.insert(dependentVars.begin(), dependentVars.end());
            }
        }

        // remove atoms which don't ground any variables in the current atom
        std::unordered_set<std::size_t> toRemove;
        for (std::size_t atomIdx : otherAtoms) {
            auto& varsGroundedByAtom = atomIdxToGroundedVars[atomIdx];
            bool requiredAtom = std::any_of(varsGroundedByAtom.begin(), varsGroundedByAtom.end(),
                    [&varDependencies](const std::string& var) { return varDependencies.count(var) > 0; });
            if (!requiredAtom) {
                toRemove.insert(atomIdx);
            }
        }

        for (auto idx : toRemove) {
            otherAtoms.erase(idx);
        }

        // Next step is to remove atoms which ground the same set of variables in the current atom
        toRemove.clear();
        std::set<std::set<std::string>> relevantGroundedVars;
        std::unordered_map<std::size_t, std::set<std::string>> atomIdxToRelevantGroundedVars;
        for (std::size_t atomIdx : otherAtoms) {
            std::set<std::string> groundedVars;
            auto& varsGroundedByAtom = atomIdxToGroundedVars[atomIdx];
            for (const std::string& var : varsGroundedByAtom) {
                if (varDependencies.count(var) > 0) {
                    groundedVars.insert(var);
                }
            }
            if (relevantGroundedVars.count(groundedVars) > 0) {
                toRemove.insert(atomIdx);
            } else {
                relevantGroundedVars.insert(groundedVars);
                atomIdxToRelevantGroundedVars[atomIdx] = groundedVars;
            }
        }

        for (auto idx : toRemove) {
            otherAtoms.erase(idx);
        }
        std::size_t N = otherAtoms.size();
        for (std::size_t K = 0; K <= N; ++K) {
            for (auto& subset : getSubsets(N, K)) {
                auto* atom = atoms[i];
                // do set union of the atoms

                std::set<std::string> providedVars;
                for (std::size_t x : subset) {
                    auto it = otherAtoms.begin();
                    std::advance(it, x);
                    auto atomIdx = *it;
                    auto& newVars = atomIdxToRelevantGroundedVars[atomIdx];
                    providedVars.insert(newVars.begin(), newVars.end());
                }

                // construct the node
                std::vector<std::size_t> joinColumns;
                const auto& args = atom->getArguments();
                std::size_t numBound = 0;
                for (std::size_t argIdx = 0; argIdx < args.size(); ++argIdx) {
                    auto* arg = args[argIdx];
                    // if we have a constant or var = constant then we ignore
                    if (atomToIdxConstants.at(i).count(argIdx) > 0) {
                        ++numBound;
                        joinColumns.push_back(argIdx);
                        continue;
                    }

                    // unnamed variable i.e. _
                    if (isA<ast::UnnamedVariable>(arg)) {
                        ++numBound;
                        continue;
                    }

                    if (auto* var = as<ast::Variable>(arg)) {
                        // free variable so we can't join on it
                        if (varToOtherVars.count(var->getName()) > 0) {
                            auto& dependentVars = varToOtherVars.at(var->getName());
                            if (!dependentVars.empty() &&
                                    std::includes(providedVars.begin(), providedVars.end(),
                                            dependentVars.begin(), dependentVars.end())) {
                                joinColumns.push_back(argIdx);
                                ++numBound;
                                continue;
                            }
                        }

                        // direct match on variable
                        if (providedVars.count(var->getName()) > 0) {
                            joinColumns.push_back(argIdx);
                            ++numBound;
                            continue;
                        }
                    }
                }

                // construct a CountUniqueKeys ram node
                bool isRecursive = recursiveInCurrentStratum.count(i) > 0;
                auto relation = getClauseAtomName(clause, atom, isRecursive);
                auto& constantMap = atomToIdxConstants.at(i);

                std::stringstream ss;
                ss << relation << " " << joinColumns << " ";
                for (auto& p : constantMap) {
                    ss << "(" << p.first << ", " << *p.second << ") ";
                }
                ss << isRecursive;

                if (seenNodes.count(ss.str()) == 0) {
                    auto node = mk<souffle::ram::CountUniqueKeys>(
                            relation, joinColumns, constantMap, isRecursive);
                    seenNodes.insert(ss.str());

                    if (!joinColumns.empty() || isRecursive) {
                        statements.push_back(std::move(node));
                    }
                }
            }
        }
    }
    return statements;
}

std::vector<std::vector<Own<souffle::ram::CountUniqueKeys>>>
UniqueKeysAnalysis::computeUniqueKeyStatements() {
    auto* prog = program;
    auto getSccAtoms = [prog](const ast::Clause* clause, const std::set<const ast::Relation*>& scc) {
        const auto& sccAtoms = filter(ast::getBodyLiterals<ast::Atom>(*clause),
                [&](const ast::Atom* atom) { return contains(scc, prog->getRelation(*atom)); });
        return sccAtoms;
    };

    const auto& sccOrdering = topsortSCCGraphAnalysis->order();

    std::vector<std::vector<Own<souffle::ram::CountUniqueKeys>>> uniqueKeyStatements;
    uniqueKeyStatements.resize(sccOrdering.size());

    auto& config = Global::config();
    if (!config.has("profile") || !config.has("index-stats")) {
        return uniqueKeyStatements;
    }

    // for each stratum (formed from scc ordering)
    for (std::size_t i = 0; i < sccOrdering.size(); i++) {
        std::vector<Own<souffle::ram::CountUniqueKeys>> stratumNodes;

        auto scc = sccOrdering[i];
        const std::set<const ast::Relation*> sccRelations = sccGraph->getInternalRelations(scc);
        for (auto* rel : sccRelations) {
            // Translate each recursive clasue
            for (auto&& clause : program->getClauses(*rel)) {
                // Assumption: no subsumption
                assert(!isA<SubsumptiveClause>(clause) &&
                        "Error: assumed no subsumptive clauses while auto-scheduling!");
                auto sccAtoms = getSccAtoms(clause, sccRelations);
                if (recursiveClauses->recursive(clause)) {
                    // for each rule version
                    for (std::size_t version = 0; version < sccAtoms.size(); version++) {
                        auto res = computeRuleVersionStatements(sccRelations, *clause, {version});
                        for (auto& s : res) {
                            stratumNodes.push_back(std::move(s));
                        }
                    }
                } else {
                    auto res = computeRuleVersionStatements(sccRelations, *clause, {});
                    for (auto& s : res) {
                        stratumNodes.push_back(std::move(s));
                    }
                }
            }
        }
        uniqueKeyStatements[scc] = std::move(stratumNodes);
    }

    std::map<std::string, std::size_t> relationToCompletedStratum;

    // first step is to compute the earliest stratum that a non-recursive relation completes
    for (std::size_t i = 0; i < sccOrdering.size(); ++i) {
        auto scc = sccOrdering[i];
        for (const auto& statement : uniqueKeyStatements[scc]) {
            const auto& rel = statement->getRelation();

            if (statement->isRecursiveRelation()) {
                continue;
            }

            if (relationToCompletedStratum.count(rel) == 0) {
                assert(i > 0 && "Can't access non-recursive relation on stratum 0");
                relationToCompletedStratum[rel] = sccOrdering[i - 1];
            }
        }
    }

    for (std::size_t i = 0; i < sccOrdering.size(); ++i) {
        auto scc = sccOrdering[i];
        for (auto& statement : uniqueKeyStatements[scc]) {
            const auto& rel = statement->getRelation();
            if (statement->isRecursiveRelation()) {
                continue;
            }
            // sanity check that we have an earliest stratum
            assert(relationToCompletedStratum.count(rel) > 0 &&
                    "Must have earliest stratum where relation is fully computed!");
            std::size_t newStratum = relationToCompletedStratum.at(rel);

            // move the node into the new stratum
            uniqueKeyStatements[newStratum].push_back(std::move(statement));
        }

        // erase remove all nullptr from the vector since moved from unique_ptr are guaranteed to be nullptr
        auto& v = uniqueKeyStatements[scc];
        v.erase(std::remove(v.begin(), v.end(), nullptr), v.end());
    }
    return uniqueKeyStatements;
}

void UniqueKeysAnalysis::run(const TranslationUnit& translationUnit) {
    program = &translationUnit.getProgram();
    sccGraph = &translationUnit.getAnalysis<SCCGraphAnalysis>();
    topsortSCCGraphAnalysis = &translationUnit.getAnalysis<TopologicallySortedSCCGraphAnalysis>();
    recursiveClauses = &translationUnit.getAnalysis<RecursiveClausesAnalysis>();
    polyAnalysis = &translationUnit.getAnalysis<ast::analysis::PolymorphicObjectsAnalysis>();
    uniqueKeyStatements = computeUniqueKeyStatements();
}

void UniqueKeysAnalysis::print(std::ostream& os) const {
    os << "Begin UniqueKeyStatements\n";
    for (std::size_t i = 0; i < uniqueKeyStatements.size(); ++i) {
        os << "Stratum: " << i << "\n";
        for (auto& s : uniqueKeyStatements[i]) {
            os << *s << "\n";
        }
    }
    os << "End UniqueKeyStatements\n";
}

}  // namespace souffle::ast::analysis
