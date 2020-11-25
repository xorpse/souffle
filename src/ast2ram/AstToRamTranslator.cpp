/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2013, 2015, Oracle and/or its affiliates. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file AstToRamTranslator.cpp
 *
 * Translator from AST to RAM structures.
 *
 ***********************************************************************/

#include "ast2ram/AstToRamTranslator.h"
#include "Global.h"
#include "LogStatement.h"
#include "ast/Aggregator.h"
#include "ast/Argument.h"
#include "ast/Atom.h"
#include "ast/BinaryConstraint.h"
#include "ast/BranchInit.h"
#include "ast/Clause.h"
#include "ast/Constant.h"
#include "ast/Directive.h"
#include "ast/Negation.h"
#include "ast/NilConstant.h"
#include "ast/Node.h"
#include "ast/NumericConstant.h"
#include "ast/Program.h"
#include "ast/QualifiedName.h"
#include "ast/RecordInit.h"
#include "ast/Relation.h"
#include "ast/StringConstant.h"
#include "ast/TranslationUnit.h"
#include "ast/analysis/AuxArity.h"
#include "ast/analysis/Functor.h"
#include "ast/analysis/IOType.h"
#include "ast/analysis/PolymorphicObjects.h"
#include "ast/analysis/RecursiveClauses.h"
#include "ast/analysis/RelationDetailCache.h"
#include "ast/analysis/RelationSchedule.h"
#include "ast/analysis/SCCGraph.h"
#include "ast/analysis/SumTypeBranches.h"
#include "ast/analysis/TopologicallySortedSCCGraph.h"
#include "ast/analysis/TypeEnvironment.h"
#include "ast/utility/NodeMapper.h"
#include "ast/utility/SipsMetric.h"
#include "ast/utility/Utils.h"
#include "ast/utility/Visitor.h"
#include "ast2ram/ClauseTranslator.h"
#include "ast2ram/ConstraintTranslator.h"
#include "ast2ram/ValueIndex.h"
#include "ast2ram/ValueTranslator.h"
#include "ast2ram/utility/Utils.h"
#include "ram/Call.h"
#include "ram/Clear.h"
#include "ram/Condition.h"
#include "ram/Conjunction.h"
#include "ram/Constraint.h"
#include "ram/DebugInfo.h"
#include "ram/EmptinessCheck.h"
#include "ram/Exit.h"
#include "ram/Expression.h"
#include "ram/Extend.h"
#include "ram/Filter.h"
#include "ram/FloatConstant.h"
#include "ram/IO.h"
#include "ram/LogRelationTimer.h"
#include "ram/LogSize.h"
#include "ram/LogTimer.h"
#include "ram/Loop.h"
#include "ram/Negation.h"
#include "ram/Parallel.h"
#include "ram/Program.h"
#include "ram/Project.h"
#include "ram/Query.h"
#include "ram/Relation.h"
#include "ram/RelationSize.h"
#include "ram/Scan.h"
#include "ram/Sequence.h"
#include "ram/SignedConstant.h"
#include "ram/Statement.h"
#include "ram/Swap.h"
#include "ram/TranslationUnit.h"
#include "ram/TupleElement.h"
#include "ram/UnsignedConstant.h"
#include "ram/utility/Utils.h"
#include "reports/DebugReport.h"
#include "reports/ErrorReport.h"
#include "souffle/BinaryConstraintOps.h"
#include "souffle/SymbolTable.h"
#include "souffle/TypeAttribute.h"
#include "souffle/utility/ContainerUtil.h"
#include "souffle/utility/FunctionalUtil.h"
#include "souffle/utility/MiscUtil.h"
#include "souffle/utility/StringUtil.h"
#include <algorithm>
#include <cassert>
#include <chrono>
#include <cstddef>
#include <iterator>
#include <map>
#include <memory>
#include <optional>
#include <set>
#include <sstream>
#include <string>
#include <utility>
#include <vector>

namespace souffle::ast2ram {

AstToRamTranslator::AstToRamTranslator() = default;
AstToRamTranslator::~AstToRamTranslator() = default;

void AstToRamTranslator::addRamSubroutine(std::string subroutineID, Own<ram::Statement> subroutine) {
    assert(!contains(ramSubroutines, subroutineID) && "subroutine ID should not already exist");
    ramSubroutines[subroutineID] = std::move(subroutine);
}

void AstToRamTranslator::addRamRelation(std::string relationName, Own<ram::Relation> ramRelation) {
    assert(!contains(ramRelations, relationName) && "ram relation should not already exist");
    ramRelations[relationName] = std::move(ramRelation);
}

size_t AstToRamTranslator::getEvaluationArity(const ast::Atom* atom) const {
    std::string relName = atom->getQualifiedName().toString();
    if (isPrefix("@info_", relName)) return 0;

    // Get the original relation name
    if (isPrefix("@delta_", relName)) {
        relName = stripPrefix("@delta_", relName);
    } else if (isPrefix("@new_", relName)) {
        relName = stripPrefix("@new_", relName);
    }

    const auto* originalRelation = relDetail->getRelation(ast::QualifiedName(relName));
    return auxArityAnalysis->getArity(originalRelation);
}

Own<ram::Expression> AstToRamTranslator::translateValue(
        const ast::Argument* arg, const ValueIndex& index) const {
    if (arg == nullptr) return nullptr;
    return ValueTranslator::translate(*this, index, *symbolTable, *arg);
}

Own<ram::Condition> AstToRamTranslator::translateConstraint(
        const ast::Literal* lit, const ValueIndex& index) const {
    assert(lit != nullptr && "literal should be defined");
    return ConstraintTranslator::translate(*this, index, *lit);
}

RamDomain AstToRamTranslator::getConstantRamRepresentation(const ast::Constant& constant) const {
    if (auto strConstant = dynamic_cast<const ast::StringConstant*>(&constant)) {
        return symbolTable->lookup(strConstant->getConstant());
    } else if (isA<ast::NilConstant>(&constant)) {
        return 0;
    } else if (auto* numConstant = dynamic_cast<const ast::NumericConstant*>(&constant)) {
        assert(numConstant->getFinalType().has_value() && "constant should have valid type");
        switch (numConstant->getFinalType().value()) {
            case ast::NumericConstant::Type::Int:
                return RamSignedFromString(numConstant->getConstant(), nullptr, 0);
            case ast::NumericConstant::Type::Uint:
                return RamUnsignedFromString(numConstant->getConstant(), nullptr, 0);
            case ast::NumericConstant::Type::Float: return RamFloatFromString(numConstant->getConstant());
        }
    }

    fatal("unaccounted-for constant");
}

Own<ram::Expression> AstToRamTranslator::translateConstant(ast::Constant const& c) const {
    auto const rawConstant = getConstantRamRepresentation(c);
    if (auto* const c_num = dynamic_cast<const ast::NumericConstant*>(&c)) {
        switch (c_num->getFinalType().value()) {
            case ast::NumericConstant::Type::Int: return mk<ram::SignedConstant>(rawConstant);
            case ast::NumericConstant::Type::Uint: return mk<ram::UnsignedConstant>(rawConstant);
            case ast::NumericConstant::Type::Float: return mk<ram::FloatConstant>(rawConstant);
        }
        fatal("unaccounted-for constant");
    }
    return mk<ram::SignedConstant>(rawConstant);
}

Own<ram::Statement> AstToRamTranslator::generateClearRelation(const ast::Relation* relation) const {
    return mk<ram::Clear>(getConcreteRelationName(relation->getQualifiedName()));
}

Own<ram::Statement> AstToRamTranslator::generateNonRecursiveRelation(const ast::Relation& rel) const {
    VecOwn<ram::Statement> result;
    std::string relName = getConcreteRelationName(rel.getQualifiedName());

    // Iterate over all non-recursive clauses that belong to the relation
    for (ast::Clause* clause : relDetail->getClauses(rel.getQualifiedName())) {
        // Skip recursive rules
        if (recursiveClauses->recursive(clause)) {
            continue;
        }

        // Translate clause
        Own<ram::Statement> rule = ClauseTranslator(*this).translateClause(*clause, *clause);

        // Add logging
        if (Global::config().has("profile")) {
            const std::string& relationName = toString(rel.getQualifiedName());
            const auto& srcLocation = clause->getSrcLoc();
            const std::string clauseText = stringify(toString(*clause));
            const std::string logTimerStatement =
                    LogStatement::tNonrecursiveRule(relationName, srcLocation, clauseText);
            const std::string logSizeStatement =
                    LogStatement::nNonrecursiveRule(relationName, srcLocation, clauseText);
            rule = mk<ram::LogRelationTimer>(std::move(rule), logTimerStatement, relName);
        }

        // Add debug info
        std::ostringstream ds;
        ds << toString(*clause) << "\nin file ";
        ds << clause->getSrcLoc();
        rule = mk<ram::DebugInfo>(std::move(rule), ds.str());

        // Add rule to result
        appendStmt(result, std::move(rule));
    }

    // Add logging for entire relation
    if (Global::config().has("profile")) {
        const std::string& relationName = toString(rel.getQualifiedName());
        const auto& srcLocation = rel.getSrcLoc();
        const std::string logSizeStatement = LogStatement::nNonrecursiveRelation(relationName, srcLocation);

        // Add timer if we did any work
        if (!result.empty()) {
            const std::string logTimerStatement =
                    LogStatement::tNonrecursiveRelation(relationName, srcLocation);
            auto newStmt = mk<ram::LogRelationTimer>(
                    mk<ram::Sequence>(std::move(result)), logTimerStatement, relName);
            result.clear();
            appendStmt(result, std::move(newStmt));
        } else {
            // Add table size printer
            appendStmt(result, mk<ram::LogSize>(relName, logSizeStatement));
        }
    }

    return mk<ram::Sequence>(std::move(result));
}

Own<ram::Statement> AstToRamTranslator::generateStratum(size_t scc) const {
    // Make a new ram statement for the current SCC
    VecOwn<ram::Statement> current;

    // load all internal input relations from the facts dir with a .facts extension
    const auto& sccInputRelations = sccGraph->getInternalInputRelations(scc);
    for (const auto& relation : sccInputRelations) {
        appendStmt(current, generateLoadRelation(relation));
    }

    // Compute the current stratum
    const auto& isRecursive = sccGraph->isRecursive(scc);
    const auto& sccRelations = sccGraph->getInternalRelations(scc);
    if (isRecursive) {
        appendStmt(current, generateRecursiveStratum(sccRelations));
    } else {
        assert(sccRelations.size() == 1 && "only one relation should exist in non-recursive stratum");
        const auto* relation = *sccRelations.begin();
        appendStmt(current, generateNonRecursiveRelation(*relation));
    }

    // Store all internal output relations to the output dir with a .csv extension
    const auto& sccOutputRelations = sccGraph->getInternalOutputRelations(scc);
    for (const auto& relation : sccOutputRelations) {
        appendStmt(current, generateStoreRelation(relation));
    }

    return mk<ram::Sequence>(std::move(current));
}

Own<ram::Statement> AstToRamTranslator::generateClearExpiredRelations(
        const std::set<const ast::Relation*>& expiredRelations) const {
    VecOwn<ram::Statement> stmts;
    for (const auto& relation : expiredRelations) {
        appendStmt(stmts, generateClearRelation(relation));
    }
    return mk<ram::Sequence>(std::move(stmts));
}

Own<ram::Statement> AstToRamTranslator::generateMergeRelations(
        const ast::Relation* rel, const std::string& destRelation, const std::string& srcRelation) const {
    VecOwn<ram::Expression> values;

    // Proposition - project if not empty
    if (rel->getArity() == 0) {
        auto projection = mk<ram::Project>(destRelation, std::move(values));
        return mk<ram::Query>(mk<ram::Filter>(
                mk<ram::Negation>(mk<ram::EmptinessCheck>(srcRelation)), std::move(projection)));
    }

    // Predicate - project all values
    for (size_t i = 0; i < rel->getArity(); i++) {
        values.push_back(mk<ram::TupleElement>(0, i));
    }
    auto projection = mk<ram::Project>(destRelation, std::move(values));
    auto stmt = mk<ram::Query>(mk<ram::Scan>(srcRelation, 0, std::move(projection)));
    if (rel->getRepresentation() == RelationRepresentation::EQREL) {
        return mk<ram::Sequence>(mk<ram::Extend>(destRelation, srcRelation), std::move(stmt));
    }
    return stmt;
}

Own<ast::Clause> AstToRamTranslator::createDeltaClause(
        const ast::Clause* original, size_t recursiveAtomIdx) const {
    auto recursiveVersion = souffle::clone(original);

    // @new :- ...
    const auto* headAtom = original->getHead();
    recursiveVersion->getHead()->setQualifiedName(getNewRelationName(headAtom->getQualifiedName()));

    // ... :- ..., @delta, ...
    auto* recursiveAtom = ast::getBodyLiterals<ast::Atom>(*recursiveVersion).at(recursiveAtomIdx);
    recursiveAtom->setQualifiedName(getDeltaRelationName(recursiveAtom->getQualifiedName()));

    // ... :- ..., !head.
    if (headAtom->getArity() > 0) {
        recursiveVersion->addToBody(mk<ast::Negation>(souffle::clone(headAtom)));
    }

    return recursiveVersion;
}

Own<ram::Statement> AstToRamTranslator::generateClauseVersion(const std::set<const ast::Relation*>& scc,
        const ast::Clause* cl, size_t deltaAtomIdx, size_t version) const {
    const auto& atoms = ast::getBodyLiterals<ast::Atom>(*cl);

    // Modify the processed rule to use delta relation and write to new relation
    auto fixedClause = createDeltaClause(cl, deltaAtomIdx);

    // Replace wildcards with variables to reduce indices
    nameUnnamedVariables(fixedClause.get());

    // Add in negated deltas for later recursive relations to simulate prev construct
    for (size_t j = deltaAtomIdx + 1; j < atoms.size(); j++) {
        const auto* atomRelation = getAtomRelation(atoms[j], program);
        if (contains(scc, atomRelation)) {
            auto deltaAtom = souffle::clone(ast::getBodyLiterals<ast::Atom>(*fixedClause)[j]);
            deltaAtom->setQualifiedName(getDeltaRelationName(atomRelation->getQualifiedName()));
            fixedClause->addToBody(mk<ast::Negation>(std::move(deltaAtom)));
        }
    }

    // Translate the resultant clause as would be done normally
    Own<ram::Statement> rule = ClauseTranslator(*this).translateClause(*fixedClause, *cl, version);

    // Add loging
    if (Global::config().has("profile")) {
        const std::string& relationName = toString(cl->getHead()->getQualifiedName());
        const auto& srcLocation = cl->getSrcLoc();
        const std::string clauseText = stringify(toString(*cl));
        const std::string logTimerStatement =
                LogStatement::tRecursiveRule(relationName, version, srcLocation, clauseText);
        const std::string logSizeStatement =
                LogStatement::nRecursiveRule(relationName, version, srcLocation, clauseText);
        rule = mk<ram::LogRelationTimer>(
                std::move(rule), logTimerStatement, getNewRelationName(cl->getHead()->getQualifiedName()));
    }

    // Add debug info
    std::ostringstream ds;
    ds << toString(*cl) << "\nin file ";
    ds << cl->getSrcLoc();
    rule = mk<ram::DebugInfo>(std::move(rule), ds.str());

    // Add to loop body
    return mk<ram::Sequence>(std::move(rule));
}

Own<ram::Statement> AstToRamTranslator::translateRecursiveClauses(
        const std::set<const ast::Relation*>& scc, const ast::Relation* rel) const {
    assert(contains(scc, rel) && "relation should belong to scc");
    VecOwn<ram::Statement> result;

    // Translate each recursive clasue
    for (const auto& cl : relDetail->getClauses(rel->getQualifiedName())) {
        // Skip non-recursive clauses
        if (!recursiveClauses->recursive(cl)) {
            continue;
        }

        // Create each version
        int version = 0;
        const auto& atoms = ast::getBodyLiterals<ast::Atom>(*cl);
        for (size_t i = 0; i < atoms.size(); i++) {
            const auto* atom = atoms[i];

            // Only interested in atoms within the same SCC
            if (!contains(scc, getAtomRelation(atom, program))) {
                continue;
            }

            appendStmt(result, generateClauseVersion(scc, cl, i, version));

            // increment version counter
            version++;
        }

        // Check that the correct number of versions have been created
        if (cl->getExecutionPlan() != nullptr) {
            int maxVersion = -1;
            for (auto const& cur : cl->getExecutionPlan()->getOrders()) {
                maxVersion = std::max(cur.first, maxVersion);
            }
            assert(version > maxVersion && "missing clause versions");
        }
    }

    return mk<ram::Sequence>(std::move(result));
}

Own<ram::Statement> AstToRamTranslator::generateStratumPreamble(
        const std::set<const ast::Relation*>& scc) const {
    VecOwn<ram::Statement> preamble;
    for (const ast::Relation* rel : scc) {
        // Generate code for the non-recursive part of the relation */
        appendStmt(preamble, generateNonRecursiveRelation(*rel));

        // Copy the result into the delta relation
        std::string deltaRelation = getDeltaRelationName(rel->getQualifiedName());
        std::string mainRelation = getConcreteRelationName(rel->getQualifiedName());
        appendStmt(preamble, generateMergeRelations(rel, deltaRelation, mainRelation));
    }
    return mk<ram::Sequence>(std::move(preamble));
}

Own<ram::Statement> AstToRamTranslator::generateStratumPostamble(
        const std::set<const ast::Relation*>& scc) const {
    VecOwn<ram::Statement> postamble;
    for (const ast::Relation* rel : scc) {
        // Drop temporary tables after recursion
        appendStmt(postamble, mk<ram::Clear>(getDeltaRelationName(rel->getQualifiedName())));
        appendStmt(postamble, mk<ram::Clear>(getNewRelationName(rel->getQualifiedName())));
    }
    return mk<ram::Sequence>(std::move(postamble));
}

Own<ram::Statement> AstToRamTranslator::generateStratumTableUpdates(
        const std::set<const ast::Relation*>& scc) const {
    VecOwn<ram::Statement> updateTable;
    for (const ast::Relation* rel : scc) {
        // Copy @new into main relation, @delta := @new, and empty out @new
        std::string mainRelation = getConcreteRelationName(rel->getQualifiedName());
        std::string newRelation = getNewRelationName(rel->getQualifiedName());
        std::string deltaRelation = getDeltaRelationName(rel->getQualifiedName());
        Own<ram::Statement> updateRelTable =
                mk<ram::Sequence>(generateMergeRelations(rel, mainRelation, newRelation),
                        mk<ram::Swap>(deltaRelation, newRelation), mk<ram::Clear>(newRelation));

        // Measure update time
        if (Global::config().has("profile")) {
            updateRelTable = mk<ram::LogRelationTimer>(std::move(updateRelTable),
                    LogStatement::cRecursiveRelation(toString(rel->getQualifiedName()), rel->getSrcLoc()),
                    newRelation);
        }

        appendStmt(updateTable, std::move(updateRelTable));
    }
    return mk<ram::Sequence>(std::move(updateTable));
}

Own<ram::Statement> AstToRamTranslator::generateStratumLoopBody(
        const std::set<const ast::Relation*>& scc) const {
    VecOwn<ram::Statement> loopBody;
    for (const ast::Relation* rel : scc) {
        auto relClauses = translateRecursiveClauses(scc, rel);

        // add profiling information
        if (Global::config().has("profile")) {
            const std::string& relationName = toString(rel->getQualifiedName());
            const auto& srcLocation = rel->getSrcLoc();
            const std::string logTimerStatement = LogStatement::tRecursiveRelation(relationName, srcLocation);
            const std::string logSizeStatement = LogStatement::nRecursiveRelation(relationName, srcLocation);
            relClauses = mk<ram::LogRelationTimer>(mk<ram::Sequence>(std::move(relClauses)),
                    logTimerStatement, getNewRelationName(rel->getQualifiedName()));
        }

        appendStmt(loopBody, mk<ram::Sequence>(std::move(relClauses)));
    }
    return mk<ram::Sequence>(std::move(loopBody));
}

Own<ram::Statement> AstToRamTranslator::generateStratumExitSequence(
        const std::set<const ast::Relation*>& scc) const {
    // Helper function to add a new term to a conjunctive condition
    auto addCondition = [&](Own<ram::Condition>& cond, Own<ram::Condition> term) {
        cond = (cond == nullptr) ? std::move(term) : mk<ram::Conjunction>(std::move(cond), std::move(term));
    };

    VecOwn<ram::Statement> exitConditions;

    // (1) if all relations in the scc are empty
    Own<ram::Condition> emptinessCheck;
    for (const ast::Relation* rel : scc) {
        addCondition(emptinessCheck, mk<ram::EmptinessCheck>(getNewRelationName(rel->getQualifiedName())));
    }
    appendStmt(exitConditions, mk<ram::Exit>(std::move(emptinessCheck)));

    // (2) if the size limit has been reached for any limitsize relations
    for (const ast::Relation* rel : scc) {
        if (ioType->isLimitSize(rel)) {
            Own<ram::Condition> limit = mk<ram::Constraint>(BinaryConstraintOp::GE,
                    mk<ram::RelationSize>(getConcreteRelationName(rel->getQualifiedName())),
                    mk<ram::SignedConstant>(ioType->getLimitSize(rel)));
            appendStmt(exitConditions, mk<ram::Exit>(std::move(limit)));
        }
    }

    return mk<ram::Sequence>(std::move(exitConditions));
}

/** generate RAM code for recursive relations in a strongly-connected component */
Own<ram::Statement> AstToRamTranslator::generateRecursiveStratum(
        const std::set<const ast::Relation*>& scc) const {
    assert(!scc.empty() && "scc set should not be empty");
    VecOwn<ram::Statement> result;

    // Add in the preamble
    appendStmt(result, generateStratumPreamble(scc));

    // Add in the main fixpoint loop
    auto loopBody = mk<ram::Parallel>(generateStratumLoopBody(scc));
    auto exitSequence = generateStratumExitSequence(scc);
    auto updateSequence = generateStratumTableUpdates(scc);
    auto fixpointLoop = mk<ram::Loop>(
            mk<ram::Sequence>(std::move(loopBody), std::move(exitSequence), std::move(updateSequence)));
    appendStmt(result, std::move(fixpointLoop));

    // Add in the postamble
    appendStmt(result, generateStratumPostamble(scc));

    return mk<ram::Sequence>(std::move(result));
}

bool AstToRamTranslator::removeADTs(const ast::TranslationUnit& translationUnit) {
    struct ADTsFuneral : public ast::NodeMapper {
        mutable bool changed{false};
        const ast::analysis::SumTypeBranchesAnalysis& sumTypesBranches;

        ADTsFuneral(const ast::TranslationUnit& tu)
                : sumTypesBranches(*tu.getAnalysis<ast::analysis::SumTypeBranchesAnalysis>()) {}

        Own<ast::Node> operator()(Own<ast::Node> node) const override {
            // Rewrite sub-expressions first
            node->apply(*this);

            if (!isA<ast::BranchInit>(node)) {
                return node;
            }

            changed = true;
            auto& adt = *as<ast::BranchInit>(node);
            auto& type = sumTypesBranches.unsafeGetType(adt.getConstructor());
            auto& branches = type.getBranches();

            // Find branch ID.
            ast::analysis::AlgebraicDataType::Branch searchDummy{adt.getConstructor(), {}};
            auto iterToBranch = std::lower_bound(branches.begin(), branches.end(), searchDummy,
                    [](const ast::analysis::AlgebraicDataType::Branch& left,
                            const ast::analysis::AlgebraicDataType::Branch& right) {
                        return left.name < right.name;
                    });

            // Branch id corresponds to the position in lexicographical ordering.
            auto branchID = std::distance(std::begin(branches), iterToBranch);

            if (isADTEnum(type)) {
                auto branchTag = mk<ast::NumericConstant>(branchID);
                branchTag->setFinalType(ast::NumericConstant::Type::Int);
                return branchTag;
            } else {
                // Collect branch arguments
                VecOwn<ast::Argument> branchArguments;
                for (auto* arg : adt.getArguments()) {
                    branchArguments.emplace_back(arg->clone());
                }

                // Branch is stored either as [branch_id, [arguments]]
                // or [branch_id, argument] in case of a single argument.
                auto branchArgs = [&]() -> Own<ast::Argument> {
                    if (branchArguments.size() != 1) {
                        return mk<ast::Argument, ast::RecordInit>(std::move(branchArguments));
                    } else {
                        return std::move(branchArguments.at(0));
                    }
                }();

                // Arguments for the resulting record [branch_id, branch_args].
                VecOwn<ast::Argument> finalRecordArgs;

                auto branchTag = mk<ast::NumericConstant>(branchID);
                branchTag->setFinalType(ast::NumericConstant::Type::Int);
                finalRecordArgs.push_back(std::move(branchTag));
                finalRecordArgs.push_back(std::move(branchArgs));

                return mk<ast::RecordInit>(std::move(finalRecordArgs), adt.getSrcLoc());
            }
        }
    };

    ADTsFuneral mapper(translationUnit);
    translationUnit.getProgram().apply(mapper);
    return mapper.changed;
}

Own<ram::Statement> AstToRamTranslator::generateLoadRelation(const ast::Relation* relation) const {
    VecOwn<ram::Statement> loadStmts;
    for (const auto* load : getDirectives(*program, relation->getQualifiedName())) {
        // Must be a load
        if (load->getType() != ast::DirectiveType::input) {
            continue;
        }

        // Set up the corresponding directive map
        std::map<std::string, std::string> directives;
        for (const auto& [key, value] : load->getParameters()) {
            directives.insert(std::make_pair(key, unescape(value)));
        }

        // Create the resultant load statement, with profile information
        std::string ramRelationName = getConcreteRelationName(relation->getQualifiedName());
        Own<ram::Statement> loadStmt = mk<ram::IO>(ramRelationName, directives);
        if (Global::config().has("profile")) {
            const std::string logTimerStatement =
                    LogStatement::tRelationLoadTime(ramRelationName, relation->getSrcLoc());
            loadStmt = mk<ram::LogRelationTimer>(std::move(loadStmt), logTimerStatement, ramRelationName);
        }

        appendStmt(loadStmts, std::move(loadStmt));
    }
    return mk<ram::Sequence>(std::move(loadStmts));
}

Own<ram::Statement> AstToRamTranslator::generateStoreRelation(const ast::Relation* relation) const {
    VecOwn<ram::Statement> storeStmts;
    for (const auto* store : getDirectives(*program, relation->getQualifiedName())) {
        // Must be a storage relation
        if (store->getType() != ast::DirectiveType::printsize &&
                store->getType() != ast::DirectiveType::output) {
            continue;
        }

        // Set up the corresponding directive map
        std::map<std::string, std::string> directives;
        for (const auto& [key, value] : store->getParameters()) {
            directives.insert(std::make_pair(key, unescape(value)));
        }

        // Create the resultant store statement, with profile information
        std::string ramRelationName = getConcreteRelationName(relation->getQualifiedName());
        Own<ram::Statement> storeStmt = mk<ram::IO>(ramRelationName, directives);
        if (Global::config().has("profile")) {
            const std::string logTimerStatement =
                    LogStatement::tRelationSaveTime(ramRelationName, relation->getSrcLoc());
            storeStmt = mk<ram::LogRelationTimer>(std::move(storeStmt), logTimerStatement, ramRelationName);
        }

        appendStmt(storeStmts, std::move(storeStmt));
    }
    return mk<ram::Sequence>(std::move(storeStmts));
}

void AstToRamTranslator::createRamRelations(size_t scc) {
    const auto& isRecursive = sccGraph->isRecursive(scc);
    const auto& sccRelations = sccGraph->getInternalRelations(scc);
    for (const auto& rel : sccRelations) {
        std::string name = getRelationName(rel->getQualifiedName());
        auto arity = rel->getArity();
        auto auxiliaryArity = auxArityAnalysis->getArity(rel);
        auto representation = rel->getRepresentation();
        const auto& attributes = rel->getAttributes();

        std::vector<std::string> attributeNames;
        std::vector<std::string> attributeTypeQualifiers;
        for (size_t i = 0; i < rel->getArity(); ++i) {
            attributeNames.push_back(attributes[i]->getName());
            if (typeEnv != nullptr) {
                attributeTypeQualifiers.push_back(
                        getTypeQualifier(typeEnv->getType(attributes[i]->getTypeName())));
            }
        }

        // Add main relation
        auto ramRelation = mk<ram::Relation>(
                name, arity, auxiliaryArity, attributeNames, attributeTypeQualifiers, representation);
        addRamRelation(name, std::move(ramRelation));

        // Recursive relations also require @delta and @new variants, with the same signature
        if (isRecursive) {
            // Add delta relation
            std::string deltaName = getDeltaRelationName(rel->getQualifiedName());
            auto deltaRelation = mk<ram::Relation>(deltaName, arity, auxiliaryArity, attributeNames,
                    attributeTypeQualifiers, representation);
            addRamRelation(deltaName, std::move(deltaRelation));

            // Add new relation
            std::string newName = getNewRelationName(rel->getQualifiedName());
            auto newRelation = mk<ram::Relation>(
                    newName, arity, auxiliaryArity, attributeNames, attributeTypeQualifiers, representation);
            addRamRelation(newName, std::move(newRelation));
        }
    }
}

const ram::Relation* AstToRamTranslator::lookupRelation(const std::string& name) const {
    assert(contains(ramRelations, name) && "relation not found");
    return ramRelations.at(name).get();
}

void AstToRamTranslator::finaliseAstTypes(ast::Program& program) const {
    visitDepthFirst(program, [&](const ast::NumericConstant& nc) {
        const_cast<ast::NumericConstant&>(nc).setFinalType(polyAnalysis->getInferredType(&nc));
    });
    visitDepthFirst(program, [&](const ast::Aggregator& aggr) {
        const_cast<ast::Aggregator&>(aggr).setFinalType(polyAnalysis->getOverloadedOperator(&aggr));
    });
    visitDepthFirst(program, [&](const ast::BinaryConstraint& bc) {
        const_cast<ast::BinaryConstraint&>(bc).setFinalType(polyAnalysis->getOverloadedOperator(&bc));
    });
    visitDepthFirst(program, [&](const ast::IntrinsicFunctor& inf) {
        const_cast<ast::IntrinsicFunctor&>(inf).setFinalOpType(polyAnalysis->getOverloadedFunctionOp(&inf));
        const_cast<ast::IntrinsicFunctor&>(inf).setFinalReturnType(functorAnalysis->getReturnType(&inf));
    });
    visitDepthFirst(program, [&](const ast::UserDefinedFunctor& udf) {
        const_cast<ast::UserDefinedFunctor&>(udf).setFinalReturnType(functorAnalysis->getReturnType(&udf));
    });
}

Own<ram::Sequence> AstToRamTranslator::generateProgram(const ast::TranslationUnit& translationUnit) {
    // Check if trivial program
    if (sccGraph->getNumberOfSCCs() == 0) {
        return mk<ram::Sequence>();
    }

    // Create all relevant RAM relations
    const auto& sccOrdering =
            translationUnit.getAnalysis<ast::analysis::TopologicallySortedSCCGraphAnalysis>()->order();
    for (const auto& scc : sccOrdering) {
        createRamRelations(scc);
    }

    // Create subroutines for each SCC according to topological order
    for (size_t i = 0; i < sccOrdering.size(); i++) {
        // Generate the main stratum code
        auto stratum = generateStratum(sccOrdering.at(i));

        // Clear expired relations
        const auto& expiredRelations = relationSchedule->schedule().at(i).expired();
        stratum = mk<ram::Sequence>(std::move(stratum), generateClearExpiredRelations(expiredRelations));

        // Add the subroutine
        std::string stratumID = "stratum_" + toString(i);
        addRamSubroutine(stratumID, std::move(stratum));
    }

    // Invoke all strata
    VecOwn<ram::Statement> res;
    for (size_t i = 0; i < sccOrdering.size(); i++) {
        appendStmt(res, mk<ram::Call>("stratum_" + toString(i)));
    }

    // Add main timer if profiling
    if (!res.empty() && Global::config().has("profile")) {
        auto newStmt = mk<ram::LogTimer>(mk<ram::Sequence>(std::move(res)), LogStatement::runtime());
        res.clear();
        appendStmt(res, std::move(newStmt));
    }

    // Program translated!
    return mk<ram::Sequence>(std::move(res));
}

Own<ram::TranslationUnit> AstToRamTranslator::translateUnit(ast::TranslationUnit& tu) {
    // Start timer
    auto ram_start = std::chrono::high_resolution_clock::now();

    /* -- Set-up -- */
    // Set up the translator
    program = &tu.getProgram();
    symbolTable = mk<SymbolTable>();
    std::string sipsChosen = "all-bound";
    if (Global::config().has("RamSIPS")) {
        sipsChosen = Global::config().get("RamSIPS");
    }
    sipsMetric = ast::SipsMetric::create(sipsChosen, tu);

    // Grab all relevant analyses
    ioType = tu.getAnalysis<ast::analysis::IOTypeAnalysis>();
    typeEnv = &tu.getAnalysis<ast::analysis::TypeEnvironmentAnalysis>()->getTypeEnvironment();
    relationSchedule = tu.getAnalysis<ast::analysis::RelationScheduleAnalysis>();
    sccGraph = tu.getAnalysis<ast::analysis::SCCGraphAnalysis>();
    recursiveClauses = tu.getAnalysis<ast::analysis::RecursiveClausesAnalysis>();
    auxArityAnalysis = tu.getAnalysis<ast::analysis::AuxiliaryArityAnalysis>();
    functorAnalysis = tu.getAnalysis<ast::analysis::FunctorAnalysis>();
    relDetail = tu.getAnalysis<ast::analysis::RelationDetailCacheAnalysis>();
    polyAnalysis = tu.getAnalysis<ast::analysis::PolymorphicObjectsAnalysis>();

    // Finalise polymorphic types in the AST
    finaliseAstTypes(tu.getProgram());

    // Replace ADTs with record representatives
    removeADTs(tu);

    /* -- Translation -- */
    // Create the final RAM program
    auto ramMain = generateProgram(tu);
    ErrorReport& errReport = tu.getErrorReport();
    DebugReport& debugReport = tu.getDebugReport();
    VecOwn<ram::Relation> rels;
    for (auto& cur : ramRelations) {
        rels.push_back(std::move(cur.second));
    }
    auto ramProgram = mk<ram::Program>(std::move(rels), std::move(ramMain), std::move(ramSubroutines));

    // Add the translated program to the debug report
    if (Global::config().has("debug-report")) {
        auto ram_end = std::chrono::high_resolution_clock::now();
        std::string runtimeStr =
                "(" + std::to_string(std::chrono::duration<double>(ram_end - ram_start).count()) + "s)";
        std::stringstream ramProgramStr;
        ramProgramStr << *ramProgram;
        debugReport.addSection("ram-program", "RAM Program " + runtimeStr, ramProgramStr.str());
    }

    // Wrap the program into a translation unit
    return mk<ram::TranslationUnit>(std::move(ramProgram), *symbolTable, errReport, debugReport);
}

}  // namespace souffle::ast2ram
