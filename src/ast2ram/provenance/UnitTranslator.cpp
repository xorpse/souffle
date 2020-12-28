/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2018 The Souffle Developers. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file UnitTranslator.h
 *
 ***********************************************************************/

#include "ast2ram/provenance/UnitTranslator.h"
#include "Global.h"
#include "LogStatement.h"
#include "ast/BinaryConstraint.h"
#include "ast/Clause.h"
#include "ast/Constraint.h"
#include "ast/utility/Utils.h"
#include "ast/utility/Visitor.h"
#include "ast2ram/provenance/SubproofGenerator.h"
#include "ast2ram/utility/TranslatorContext.h"
#include "ast2ram/utility/Utils.h"
#include "ast2ram/utility/ValueIndex.h"
#include "ram/Call.h"
#include "ram/DebugInfo.h"
#include "ram/ExistenceCheck.h"
#include "ram/Expression.h"
#include "ram/Extend.h"
#include "ram/Filter.h"
#include "ram/LogRelationTimer.h"
#include "ram/Negation.h"
#include "ram/Project.h"
#include "ram/Query.h"
#include "ram/Scan.h"
#include "ram/Sequence.h"
#include "ram/SignedConstant.h"
#include "ram/Statement.h"
#include "ram/SubroutineArgument.h"
#include "ram/SubroutineReturn.h"
#include "ram/TupleElement.h"
#include "ram/UndefValue.h"
#include "souffle/SymbolTable.h"
#include "souffle/utility/StringUtil.h"
#include <sstream>

namespace souffle::ast2ram::provenance {

Own<ram::Sequence> UnitTranslator::generateProgram(const ast::TranslationUnit& translationUnit) {
    // do the regular translation
    auto ramProgram = seminaive::UnitTranslator::generateProgram(translationUnit);

    // create info clauses
    auto infoClauses = generateInfoClauses(context->getProgram());

    // append into a single ram program
    ramProgram = mk<ram::Sequence>(std::move(ramProgram), std::move(infoClauses));

    // add subroutines for each clause
    addProvenanceClauseSubroutines(context->getProgram());

    return ramProgram;
}

Own<ram::Relation> UnitTranslator::createRamRelation(
        const ast::Relation* baseRelation, std::string ramRelationName) const {
    auto arity = baseRelation->getArity();
    auto representation = baseRelation->getRepresentation();

    // add in base relation information
    std::vector<std::string> attributeNames;
    std::vector<std::string> attributeTypeQualifiers;
    for (const auto& attribute : baseRelation->getAttributes()) {
        attributeNames.push_back(attribute->getName());
        attributeTypeQualifiers.push_back(context->getAttributeTypeQualifier(attribute->getTypeName()));
    }

    // add in provenance information
    attributeNames.push_back("@rule_number");
    attributeTypeQualifiers.push_back("i:number");

    attributeNames.push_back("@level_number");
    attributeTypeQualifiers.push_back("i:number");

    return mk<ram::Relation>(
            ramRelationName, arity + 2, 2, attributeNames, attributeTypeQualifiers, representation);
}

VecOwn<ram::Relation> UnitTranslator::createRamRelations(const std::vector<size_t>& sccOrdering) const {
    // Regular relations
    auto ramRelations = seminaive::UnitTranslator::createRamRelations(sccOrdering);

    // Info relations
    for (const auto* relation : context->getProgram()->getRelations()) {
        size_t clauseID = 1;
        for (const auto* clause : context->getClauses(relation->getQualifiedName())) {
            if (isFact(*clause)) {
                continue;
            }

            auto infoRelQualifiedName = clause->getHead()->getQualifiedName();
            infoRelQualifiedName.append("@info");
            infoRelQualifiedName.append(toString(clauseID++));
            std::string infoRelName = getConcreteRelationName(infoRelQualifiedName);

            std::vector<std::string> attributeNames;
            std::vector<std::string> attributeTypeQualifiers;

            // (1) Clause ID
            attributeNames.push_back("clause_num");
            attributeTypeQualifiers.push_back("i:number");

            // (2) Head variable string
            attributeNames.push_back("head_vars");
            attributeTypeQualifiers.push_back("s:symbol");

            // (3) For all atoms + negs + bcs: rel_<i>:symbol
            for (size_t i = 0; i < clause->getBodyLiterals().size(); i++) {
                const auto* literal = clause->getBodyLiterals().at(i);
                if (isA<ast::Atom>(literal) || isA<ast::Negation>(literal) ||
                        isA<ast::BinaryConstraint>(literal)) {
                    attributeNames.push_back("rel_" + std::to_string(i));
                    attributeTypeQualifiers.push_back("s:symbol");
                }
            }

            // (4) Clause representation
            attributeNames.push_back("clause_repr");
            attributeTypeQualifiers.push_back("s:symbol");

            ramRelations.push_back(mk<ram::Relation>(infoRelName, attributeNames.size(), 0, attributeNames,
                    attributeTypeQualifiers, RelationRepresentation::INFO));
        }
    }

    return ramRelations;
}

void UnitTranslator::addAuxiliaryArity(
        const ast::Relation* /* relation */, std::map<std::string, std::string>& directives) const {
    directives.insert(std::make_pair("auxArity", "2"));
}

Own<ram::Statement> UnitTranslator::generateClearExpiredRelations(
        const std::set<const ast::Relation*>& /* expiredRelations */) const {
    // relations should be preserved if provenance is enabled
    return mk<ram::Sequence>();
}

void UnitTranslator::addProvenanceClauseSubroutines(const ast::Program* program) {
    visitDepthFirst(*program, [&](const ast::Clause& clause) {
        std::string relName = toString(clause.getHead()->getQualifiedName());

        // do not add subroutines for info relations or facts
        if (isPrefix("info", relName) || isFact(clause)) {
            return;
        }

        std::string subroutineLabel =
                relName + "_" + std::to_string(getClauseNum(program, &clause)) + "_subproof";
        addRamSubroutine(subroutineLabel, makeSubproofSubroutine(clause));

        std::string negationSubroutineLabel =
                relName + "_" + std::to_string(getClauseNum(program, &clause)) + "_negation_subproof";
        addRamSubroutine(negationSubroutineLabel, makeNegationSubproofSubroutine(clause));
    });
}

Own<ram::Statement> UnitTranslator::generateMergeRelations(
        const ast::Relation* rel, const std::string& destRelation, const std::string& srcRelation) const {
    VecOwn<ram::Expression> values;

    // Predicate - project all values
    for (size_t i = 0; i < rel->getArity() + 2; i++) {
        values.push_back(mk<ram::TupleElement>(0, i));
    }

    auto projection = mk<ram::Project>(destRelation, std::move(values));
    auto stmt = mk<ram::Query>(mk<ram::Scan>(srcRelation, 0, std::move(projection)));
    if (rel->getRepresentation() == RelationRepresentation::EQREL) {
        return mk<ram::Sequence>(mk<ram::Extend>(destRelation, srcRelation), std::move(stmt));
    }
    return stmt;
}

Own<ram::Sequence> UnitTranslator::generateInfoClauses(const ast::Program* program) {
    VecOwn<ram::Statement> infoClauseCalls;

    size_t stratumCount = context->getNumberOfSCCs();
    for (const auto* relation : program->getRelations()) {
        size_t clauseID = 1;
        for (const auto* clause : context->getClauses(relation->getQualifiedName())) {
            if (isFact(*clause)) {
                continue;
            }

            // Argument info generator
            int functorNumber = 0;
            int aggregateNumber = 0;
            auto getArgInfo = [&](const ast::Argument* arg) -> std::string {
                if (auto* var = dynamic_cast<const ast::Variable*>(arg)) {
                    return toString(*var);
                } else if (auto* constant = dynamic_cast<const ast::Constant*>(arg)) {
                    return toString(*constant);
                }
                if (isA<ast::UnnamedVariable>(arg)) {
                    return "_";
                }
                if (isA<ast::Functor>(arg)) {
                    return tfm::format("functor_%d", functorNumber++);
                }
                if (isA<ast::Aggregator>(arg)) {
                    return tfm::format("agg_%d", aggregateNumber++);
                }

                fatal("Unhandled argument type");
            };

            // Construct info relation name for the clause
            auto infoRelQualifiedName = clause->getHead()->getQualifiedName();
            infoRelQualifiedName.append("@info");
            infoRelQualifiedName.append(toString(clauseID));
            std::string infoRelName = getConcreteRelationName(infoRelQualifiedName);

            // Generate clause head arguments
            VecOwn<ram::Expression> factArguments;

            // (1) Clause ID
            factArguments.push_back(mk<ram::SignedConstant>(clauseID++));

            // (2) Head variables
            std::vector<std::string> headVariables;
            for (const auto* arg : clause->getHead()->getArguments()) {
                headVariables.push_back(getArgInfo(arg));
            }
            std::stringstream headVariableInfo;
            headVariableInfo << join(headVariables, ",");
            factArguments.push_back(mk<ram::SignedConstant>(symbolTable->lookup(headVariableInfo.str())));

            // (3) For all atoms || negs:
            //      - atoms: relName,{atom arg info}
            //      - negs: !relName
            for (const auto* literal : clause->getBodyLiterals()) {
                if (const auto* atom = dynamic_cast<const ast::Atom*>(literal)) {
                    std::string atomDescription = toString(atom->getQualifiedName());
                    for (const auto* arg : atom->getArguments()) {
                        atomDescription.append("," + getArgInfo(arg));
                    }
                    factArguments.push_back(mk<ram::SignedConstant>(symbolTable->lookup(atomDescription)));
                } else if (const auto* neg = dynamic_cast<const ast::Negation*>(literal)) {
                    const auto* atom = neg->getAtom();
                    std::string relName = toString(atom->getQualifiedName());
                    factArguments.push_back(mk<ram::SignedConstant>(symbolTable->lookup("!" + relName)));
                }
            }

            // (4) For all bcs:
            //      - symbol,lhs arg info,rhs arg info
            for (const auto* binaryConstraint : ast::getBodyLiterals<ast::BinaryConstraint>(*clause)) {
                std::stringstream constraintDescription;
                constraintDescription << toBinaryConstraintSymbol(binaryConstraint->getBaseOperator());
                constraintDescription << "," << getArgInfo(binaryConstraint->getLHS());
                constraintDescription << "," << getArgInfo(binaryConstraint->getRHS());
                factArguments.push_back(
                        mk<ram::SignedConstant>(symbolTable->lookup(constraintDescription.str())));
            }

            // (5) The actual clause
            factArguments.push_back(mk<ram::SignedConstant>(symbolTable->lookup(toString(*clause))));

            /* -- Finalising -- */
            // Push in the final clause
            auto factProjection = mk<ram::Project>(infoRelName, std::move(factArguments));
            auto infoClause = mk<ram::Statement, ram::Query>(std::move(factProjection));

            // Add logging
            if (Global::config().has("profile")) {
                std::stringstream clauseText;
                clauseText << "@info.clause[" << stringify(toString(*clause)) << "]";
                const std::string logTimerStatement =
                        LogStatement::tNonrecursiveRule(infoRelName, clause->getSrcLoc(), clauseText.str());
                infoClause = mk<ram::LogRelationTimer>(std::move(infoClause), logTimerStatement, infoRelName);
            }

            // Add debug info
            std::ostringstream ds;
            ds << "@info.clause[";
            ds << toString(*clause) << "]"
               << "\nin file ";
            ds << clause->getSrcLoc();
            infoClause = mk<ram::DebugInfo>(std::move(infoClause), ds.str());

            std::string stratumID = "stratum_" + toString(stratumCount++);
            addRamSubroutine(stratumID, std::move(infoClause));

            infoClauseCalls.push_back(mk<ram::Call>(stratumID));
        }
    }

    return mk<ram::Sequence>(std::move(infoClauseCalls));
}

Own<ram::Statement> UnitTranslator::makeSubproofSubroutine(const ast::Clause& clause) {
    return SubproofGenerator(*context, *symbolTable).translateNonRecursiveClause(clause);
}

Own<ram::ExistenceCheck> UnitTranslator::makeRamAtomExistenceCheck(
        const ast::Atom* atom, const std::map<int, std::string>& idToVarName, ValueIndex& valueIndex) const {
    auto relName = getConcreteRelationName(atom->getQualifiedName());

    // construct a query
    VecOwn<ram::Expression> query;

    // add each value (subroutine argument) to the search query
    for (const auto* arg : atom->getArguments()) {
        auto translatedValue = context->translateValue(*symbolTable, valueIndex, arg);
        transformVariablesToSubroutineArgs(translatedValue.get(), idToVarName);
        query.push_back(std::move(translatedValue));
    }

    // fill up query with nullptrs for the provenance columns
    query.push_back(mk<ram::UndefValue>());
    query.push_back(mk<ram::UndefValue>());

    // create existence checks to check if the tuple exists or not
    return mk<ram::ExistenceCheck>(relName, std::move(query));
}

Own<ram::SubroutineReturn> UnitTranslator::makeRamReturnTrue() const {
    VecOwn<ram::Expression> returnTrue;
    returnTrue.push_back(mk<ram::SignedConstant>(1));
    return mk<ram::SubroutineReturn>(std::move(returnTrue));
}

Own<ram::SubroutineReturn> UnitTranslator::makeRamReturnFalse() const {
    VecOwn<ram::Expression> returnFalse;
    returnFalse.push_back(mk<ram::SignedConstant>(0));
    return mk<ram::SubroutineReturn>(std::move(returnFalse));
}

void UnitTranslator::transformVariablesToSubroutineArgs(
        ram::Node* node, const std::map<int, std::string>& idToVarName) const {
    // a mapper to replace variables with subroutine arguments
    struct VariablesToArguments : public ram::NodeMapper {
        const std::map<int, std::string>& idToVarName;

        VariablesToArguments(const std::map<int, std::string>& idToVarName) : idToVarName(idToVarName) {}

        Own<ram::Node> operator()(Own<ram::Node> node) const override {
            if (const auto* tuple = dynamic_cast<const ram::TupleElement*>(node.get())) {
                const auto& varName = idToVarName.at(tuple->getTupleId());
                // TODO (azreika): shouldn't rely on name for singleton variables; do an index check
                if (isPrefix("@level_num", varName) || isPrefix("+underscore", varName)) {
                    return mk<ram::UndefValue>();
                }
                return mk<ram::SubroutineArgument>(tuple->getTupleId());
            }

            // Apply recursive
            node->apply(*this);
            return node;
        }
    };

    VariablesToArguments varsToArgs(idToVarName);
    node->apply(varsToArgs);
}

Own<ram::Sequence> UnitTranslator::makeIfStatement(
        Own<ram::Condition> condition, Own<ram::Operation> trueOp, Own<ram::Operation> falseOp) const {
    auto negatedCondition = mk<ram::Negation>(souffle::clone(condition));

    auto trueBranch = mk<ram::Query>(mk<ram::Filter>(std::move(condition), std::move(trueOp)));
    auto falseBranch = mk<ram::Query>(mk<ram::Filter>(std::move(negatedCondition), std::move(falseOp)));

    return mk<ram::Sequence>(std::move(trueBranch), std::move(falseBranch));
}

Own<ram::Statement> UnitTranslator::makeNegationSubproofSubroutine(const ast::Clause& clause) {
    // TODO (taipan-snake): Currently we only deal with atoms (no constraints or negations or aggregates
    // or anything else...)

    // The resulting subroutine looks something like this:
    // IF (arg(0), arg(1), _, _) IN rel_1:
    //   return 1
    // IF (arg(0), arg(1), _ ,_) NOT IN rel_1:
    //   return 0
    // ...

    std::vector<const ast::Literal*> lits;
    for (const auto* bodyLit : clause.getBodyLiterals()) {
        // first add all the things that are not constraints
        if (!isA<ast::Constraint>(bodyLit)) {
            lits.push_back(bodyLit);
        }
    }

    // now add all constraints
    for (const auto* bodyLit : ast::getBodyLiterals<ast::Constraint>(clause)) {
        lits.push_back(bodyLit);
    }

    size_t count = 0;
    std::map<int, std::string> idToVarName;
    auto dummyValueIndex = mk<ValueIndex>();
    visitDepthFirst(clause, [&](const ast::Variable& var) {
        if (dummyValueIndex->isDefined(var) || isPrefix("@level_num", var.getName()) ||
                isPrefix("+underscore", var.getName())) {
            return;
        }
        idToVarName[count] = var.getName();
        dummyValueIndex->addVarReference(var, count++, 0);
    });

    // TODO (azreika): index aggregators too, then override the value translator to treat them as variables

    visitDepthFirst(clause, [&](const ast::Variable& var) {
        if (isPrefix("+underscore", var.getName())) {
            idToVarName[count] = var.getName();
            dummyValueIndex->addVarReference(var, count++, 0);
        }
    });

    // the structure of this subroutine is a sequence where each nested statement is a search in each
    // relation
    VecOwn<ram::Statement> searchSequence;

    // go through each body atom and create a return
    size_t litNumber = 0;
    for (const auto* lit : lits) {
        if (const auto* atom = dynamic_cast<const ast::Atom*>(lit)) {
            auto existenceCheck = makeRamAtomExistenceCheck(atom, idToVarName, *dummyValueIndex);
            transformVariablesToSubroutineArgs(existenceCheck.get(), idToVarName);
            auto ifStatement =
                    makeIfStatement(std::move(existenceCheck), makeRamReturnTrue(), makeRamReturnFalse());
            appendStmt(searchSequence, std::move(ifStatement));
        } else if (const auto* neg = dynamic_cast<const ast::Negation*>(lit)) {
            auto existenceCheck = makeRamAtomExistenceCheck(neg->getAtom(), idToVarName, *dummyValueIndex);
            transformVariablesToSubroutineArgs(existenceCheck.get(), idToVarName);
            auto ifStatement =
                    makeIfStatement(std::move(existenceCheck), makeRamReturnFalse(), makeRamReturnTrue());
            appendStmt(searchSequence, std::move(ifStatement));
        } else if (const auto* con = dynamic_cast<const ast::Constraint*>(lit)) {
            auto condition = context->translateConstraint(*symbolTable, *dummyValueIndex, con);
            transformVariablesToSubroutineArgs(condition.get(), idToVarName);
            auto ifStatement =
                    makeIfStatement(std::move(condition), makeRamReturnTrue(), makeRamReturnFalse());
            appendStmt(searchSequence, std::move(ifStatement));
        }

        litNumber++;
    }

    return mk<ram::Sequence>(std::move(searchSequence));
}

}  // namespace souffle::ast2ram::provenance
