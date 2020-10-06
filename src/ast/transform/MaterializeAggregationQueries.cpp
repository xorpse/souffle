/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2015, Oracle and/or its affiliates. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file MaterializeAggregationQueries.cpp
 *
 ***********************************************************************/

#include "ast/transform/MaterializeAggregationQueries.h"
#include "AggregateOp.h"
#include "ast/analysis/Aggregate.h"    
#include "ast/Aggregator.h"
#include "ast/Argument.h"
#include "ast/Atom.h"
#include "ast/Attribute.h"
#include "ast/Clause.h"
#include "ast/Literal.h"
#include "ast/Node.h"
#include "ast/Program.h"
#include "ast/QualifiedName.h"
#include "ast/Relation.h"
#include "ast/TranslationUnit.h"
#include "ast/UnnamedVariable.h"
#include "ast/Variable.h"
#include "ast/analysis/Ground.h"
#include "ast/analysis/Type.h"
#include "ast/analysis/TypeSystem.h"
#include "ast/utility/LambdaNodeMapper.h"
#include "ast/utility/Utils.h"
#include "ast/utility/Visitor.h"
#include "souffle/TypeAttribute.h"
#include "souffle/utility/MiscUtil.h"
#include "souffle/utility/StringUtil.h"
#include <algorithm>
#include <map>
#include <memory>
#include <set>
#include <utility>
#include <vector>

namespace souffle::ast::transform {

void MaterializeAggregationQueriesTransformer::instantiateUnnamedVariables(Clause& aggClause) {
// I should not be fiddling with aggregates that are in the aggregate clause.
// We can short circuit if we find an aggregate node.
    struct InstantiateUnnamedVariables : public NodeMapper {
        static int count = 0;
        Own<Node> operator()(Own<Node> node) const override {
           if (auto* variable = dynamic_cast<UnnamedVariable*>(node.get())) {
               return mk<Variable>("_" + toString(count++));
           }
           if (isA<Aggregator>(node)) {
                // then DON'T recurse
                return node;
           }
           node->apply(*this);
           return node; 
        }
    };

    InstantiateUnnamedVariables update;
    for (const auto& lit : aggClause.getBodyLiterals()) {
        lit->apply(update);
    }
}

std::set<std::string> MaterializeAggregationQueriesTransformer::distinguishHeadArguments(
        const TranslationUnit& tu, const Clause& clause, const Aggregator& aggregate) {
    /**
     * The head atom should contain immediate local and injected variables.
     * No witnesses! They have already been transformed away.
     * This means that we exclude any inner aggregate local variables. But 
     * we do NOT exclude inner aggregate injected variables!! It's important
     * that the injected variable ends up in this head so that we do not obfuscate
     * the injected variable's relationship to the outer scope.
     * It does not affect the aggregate value because adding an extra column
     * for the injected variable, where that column will only have one value at a time,
     * will essentially replicate the aggregate body relation for as many possible
     * values of the injected variable that there are. The fact that the injected variable
     * will take one value at a time is key. 
     **/
     std::set<std::string> headArguments;
     // find local variables of this aggregate and add them
     for (const auto& localVarName : analysis::getLocalVariables(tu, clause, aggregate)) {
        headArguments.insert(localVarName);
     }
     // find local variables of inner aggregate and remove them
     visitDepthFirst(aggregate, [&](const Aggregator& innerAggregate) {
        if (aggregate == innerAggregate) {
            return;
        }
        for (const auto& innerLocalVariableName : analysis::getLocalVariables(tu, clause, innerAggregate)) {
           headArguments.erase(innerLocalVariableName); 
        }
     });
     // find injected variables of this aggregate and add them
     for (const auto& injectedVarName : analysis::getInjectedVariables(tu, clause, aggregate)) {
        headArguments.insert(injectedVarName);
     }
     return headArguments;
}

// TODO: Deal with recursive parameters with an assert statement.
void MaterializeAggregationQueriesTransformer::groundInjectedParameters(
        TranslationUnit& translationUnit, Clause* aggClause, const Clause* originalClause) {
    std::set<std::string> alreadyGrounded;
    for (const auto& argPair : analysis::getGroundedTerms(translationUint, *aggClause)) {
        const auto* variable = dynamic_cast<const ast::Variable*>(argPair.first);
        bool variableIsGrounded = argPair.second;
        if (variable == nullptr || variableIsGrounded) {
            continue;
        }
        std::string ungroundedVariableName = variable->getName();
        // Try to find any atom in the rule where this ungrounded variable is mentioned
        for (const auto& lit : originalClause->getBodyLiterals()) {
            // 1. Variable must occur in this literal
            bool variableOccursInLit = false;
            visitDepthFirst(*lit, [&](const Variable& var) {
                if (var.getName() == ungroundedVariableName) {
                   variableOccursInLit = true; 
                }
            }); 
            if (!variableOccursInLit) {
                return;
            }
            // 2. Variable must be grounded by this literal.
            auto singleLiteralClause = mk<Clause>();
            singleLiteralClause->addToBody(souffle::clone(lit));
            bool variableGroundedByLiteral = false;
            for (const auto& ap : analysis::getGroundedTerms(translationUnit, *singleLiteralClause)) {
                const auto* var = dynamic_cast<const ast::Variable*>(ap.first);
                bool isGrounded = ap.second;
                if (var->getName() == ungroundedVariableName && isGrounded) {
                    variableGroundedByLiteral = true;
                }
            }
            if (!variableGroundedByLiteral) {
                return;
            }
            // 3. if it's an atom:
            //  TODO: the relation must be of a lower stratum for us to be able to add it.
            //  sanitise the atom by removing any unnecessary arguments that aren't constants
            //  or basically just any other variables
            if (const auto* atom = dynamic_cast<const Atom*>(lit.get())) {
                //  add it to the aggClause since it's successfully passed all the tests
                aggClause->addToBody(souffle::clone(lit));
            }


        }
        // after this loop, we should have added at least one thing to provide a grounding.
        // If not, we should error out. The program will not be able to run.
        // We have an ungrounded variable that we cannot ground once the aggregate body is
        // outlined.
    }
 //            for (const auto& argPair : analysis::getGroundedTerms(translationUnit, *aggClause)) {
//                const auto* variable = dynamic_cast<const ast::Variable*>(argPair.first);
//                bool variableIsGrounded = argPair.second;
//                // if it's not even a variable type or the term is grounded
//                // then skip it
//                if (variable == nullptr || variableIsGrounded) {
//                    continue;
//                }
//
//                for (const auto& lit : clause.getBodyLiterals()) {
//                    const auto* atom = dynamic_cast<const Atom*>(lit);
//                    if (atom == nullptr) {
//                        continue;  // it's not an atom so it can't help ground anything
//                    }
//                    // Pull in a grounding atom
//                    visitDepthFirst(*atom, [&](const ast::Variable& var) {
//                        if (groundedVariables.find(var.getName()) != groundedVariables.end()) {
//                            return;
//                        }
//                        if (var.getName() == variable->getName()) {
//                            // auto groundingAtom = souffle::clone(atom);
//                            // remove other unnecessary bloating arguments and replace with an underscore
//                            VecOwn<Argument> arguments;
//                            for (auto arg : atom->getArguments()) {
//                                if (auto* var = dynamic_cast<ast::Variable*>(arg)) {
//                                    if (var->getName() == variable->getName()) {
//                                        arguments.emplace_back(arg->clone());
//                                        continue;
//                                    }
//                                }
//                                arguments.emplace_back(new UnnamedVariable());
//                            }
//
//                            auto groundingAtom = mk<Atom>(atom->getQualifiedName(), std::move(arguments), atom->getSrcLoc());
//                            aggClause->addToBody(souffle::clone(groundingAtom));
//                            groundedVariables.insert(var.getName());
//                        }
//                    });
//                }
//            }
   
}

bool MaterializeAggregationQueriesTransformer::materializeAggregationQueries(
        TranslationUnit& translationUnit) {
    bool changed = false;
    Program& program = *translationUnit.getProgram();
    /**
     * GENERAL PROCEDURE FOR MATERIALISING THE BODY OF AN AGGREGATE:
     * NB:
     * * Only bodies with more than one atom or an inner aggregate need to be materialised.
     * * Ignore inner aggregates (they will be unwound in subsequent applications of this transformer)
     * 
     * * Copy aggregate body literals into a new clause
     * * Pull in grounding atoms
     *
     * * Set up the head: This will include any local and injected variables in the body.
     * * Instantiate unnamed variables in count operation (idk why but it's fine)
     *
     **/
    std::set<const Aggregator*> innerAggregates;
    visitDepthFirst(program, [&](const Aggregator& agg) {
        visitDepthFirst(agg, [&](const Aggregator& innerAgg) {
            if (agg != innerAgg) {
                innerAggregates.insert(&innerAgg);
            }        
        });        
    });

    visitDepthFirst(program [&](const Clause& clause) {
        visitDepthFirst(clause, [&](const Aggregator& agg) {
            if (!needsMaterializedRelation(agg)) {
                return;
            }
            // only materialise bottom level aggregates
            if (innerAggregates.find(&agg) != innerAggregates.end()) {
                return;
            }
            // begin materialisation process
            auto aggregateBodyRelationName = analysis::findUniqueRelationName(program, "__agg_body_rel");
            auto aggClause = mk<Clause>();
            // quickly copy in all the literals from the aggregate body
            for (const auto& lit : agg.getBodyLiterals()) {
               aggClause->addToBody(souffle::clone(lit)); 
            }
            if (agg.getOperator() == AggregateOp::COUNT) {
                instantiateUnnamedVariables(*aggClause);
            }
            // pull in any necessary grounding atoms
            groundInjectedParameters(translationUnit, aggClause, clause);
            // the head must contain all injected/local variables, but not variables
            // local to any inner aggregates. So we'll just take a set minus here.
            auto aggClauseHead = mk<Atom>(aggregateBodyRelationName);
            std::vector<std::string> headArguments = distinguishHeadArguments(translationUnit, clause, agg);
            // insert the head arguments into the fricken head atom
            for (const auto& variableName : headArguments) {
                aggClauseHead->addArgument(mk<Variable>(variableName));
            }
            auto aggBodyAtom = souffle::clone(aggClauseHead);
            // add them to the relation as well (need to do a bit of type analysis to make this work)
            auto aggRel = mk<Relation>(aggregateBodyRelationName);
            std::map<const Argument*, analysis::TypeSet> argTypes =
                    analysis::TypeAnalysis::analyseTypes(translationUnit, *aggClause);
            for (const auto& cur : aggClauseHead->getArguments()) {
                aggRel->addAttribute(mk<Attribute>(toString(*cur),
                        (analysis::isOfKind(argTypes[cur], TypeAttribute::Signed)) ? "number" : "symbol"));
            }
            aggClause->setHead(std::move(aggClauseHead));
            // Now we can just add these new things (relation and its single clause) to the program
            program.addClause(std::move(aggClause));
            program.addRelation(std::move(aggRel));
            // Now it's time to update the aggregate body atom. We can now
            // replace the complex body (with literals) with a body with just the single atom referring
            // to the new relation we just created.
            VecOwn<Argument> args;
            for (auto arg : head->getArguments()) {
                if (auto* var = dynamic_cast<ast::Variable*>(arg)) {
                    // replace local variable by underscore if local
                    if (varCtr[var->getName()] == 0) {
                        args.emplace_back(new UnnamedVariable());
                        continue;
                    }
                }
                args.emplace_back(arg->clone());
            }
            auto aggAtom = mk<Atom>(head->getQualifiedName(), std::move(args), head->getSrcLoc());

            VecOwn<Literal> newBody;
            newBody.push_back(std::move(aggAtom));
            const_cast<Aggregator&>(agg).setBody(std::move(newBody));
        });
    });


//    // if an aggregator has a body consisting of more than an atom => create new relation
//    visitDepthFirst(program, [&](const Clause& clause) {
//        visitDepthFirst(clause, [&](const Aggregator& agg) {
//            // check whether a materialization is required
//            if (!needsMaterializedRelation(agg)) {
//                return;
//            }
//            changed = true;
//
//            // -- create a new clause --
//            auto relName = analysis::findUniqueRelationName(program, "__agg_body_rel");
//            // create the new clause for the materialised rule
//            auto* aggClause = new Clause();
//            // create the body of the new materialised rule
//            for (const auto& cur : agg.getBodyLiterals()) {
//                aggClause->addToBody(souffle::clone(cur));
//            }
//            std::set<std::string> groundedVariables;
//            // find stuff for which we need a grounding
//            for (const auto& argPair : analysis::getGroundedTerms(translationUnit, *aggClause)) {
//                const auto* variable = dynamic_cast<const ast::Variable*>(argPair.first);
//                bool variableIsGrounded = argPair.second;
//                // if it's not even a variable type or the term is grounded
//                // then skip it
//                if (variable == nullptr || variableIsGrounded) {
//                    continue;
//                }
//
//                for (const auto& lit : clause.getBodyLiterals()) {
//                    const auto* atom = dynamic_cast<const Atom*>(lit);
//                    if (atom == nullptr) {
//                        continue;  // it's not an atom so it can't help ground anything
//                    }
//                    // Pull in a grounding atom
//                    visitDepthFirst(*atom, [&](const ast::Variable& var) {
//                        if (groundedVariables.find(var.getName()) != groundedVariables.end()) {
//                            return;
//                        }
//                        if (var.getName() == variable->getName()) {
//                            // auto groundingAtom = souffle::clone(atom);
//                            // remove other unnecessary bloating arguments and replace with an underscore
//                            VecOwn<Argument> arguments;
//                            for (auto arg : atom->getArguments()) {
//                                if (auto* var = dynamic_cast<ast::Variable*>(arg)) {
//                                    if (var->getName() == variable->getName()) {
//                                        arguments.emplace_back(arg->clone());
//                                        continue;
//                                    }
//                                }
//                                arguments.emplace_back(new UnnamedVariable());
//                            }
//
//                            auto groundingAtom = mk<Atom>(atom->getQualifiedName(), std::move(arguments), atom->getSrcLoc());
//                            aggClause->addToBody(souffle::clone(groundingAtom));
//                            groundedVariables.insert(var.getName());
//                        }
//                    });
//                }
//            }
//            // -- update aggregate --
//
//            // Keep track of variables that occur in the outer scope (i.e. NOT inside any aggregate)
//            std::map<std::string, int> varCtr;
//
//            // Start by counting occurrences of all variables in the clause
//            visitDepthFirst(clause, [&](const ast::Variable& var) { varCtr[var.getName()]++; });
//
//            // Then count variables occurring in each aggregate
//            // so that we can deduce which variable occur only on the outer scope
//            std::map<const Aggregator*, std::map<std::string, int>> aggVarMap;
//            visitDepthFirst(clause, [&](const Aggregator& agg) {
//                visitDepthFirst(agg, [&](const ast::Variable& var) { aggVarMap[&agg][var.getName()]++; });
//            });
//
//            std::map<const Aggregator*, const Aggregator*> parent;
//            // Figure out parent/child relationships between the aggregates
//            // so that we know which variables are occurring on each level
//            visitDepthFirstPostOrder(clause, [&](const Aggregator& agg) {
//                visitDepthFirst(agg, [&](const Aggregator& descendantAgg) {
//                    if (agg == descendantAgg) {
//                        return;
//                    }
//                    if (parent[&descendantAgg] == nullptr) {
//                        parent[&descendantAgg] = &agg;
//                    }
//                });
//            });
//
//            // Figure out which variables occur on the outer scope by looking at
//            // the aggregates without agggregate parents, and minusing those from
//            // the outer scope varCtr map
//            visitDepthFirst(clause, [&](const Aggregator& agg) {
//                if (parent[&agg] == nullptr) {
//                    for (auto const& pair : aggVarMap[&agg]) {
//                        std::string varName = pair.first;
//                        int numOccurrences = pair.second;
//                        varCtr[varName] -= numOccurrences;
//                    }
//                }
//            });
//            // But the current aggregate we're dealing with's target expression
//            // "counts" as the outer scope, so restore this
//            if (agg.getTargetExpression() != nullptr) {
//                visitDepthFirst(*agg.getTargetExpression(),
//                        [&](const ast::Variable& var) { varCtr[var.getName()]++; });
//            }
//
//            // correct aggVarMap so that it counts which variables occurr in the aggregate,
//            // and not the variables that occur in an inner aggregate
//            // This way, we know which arguments are necessary for the head of the aggregate body relation
//            visitDepthFirst(clause, [&](const Aggregator& agg) {
//                if (parent[&agg] != nullptr) {
//                    // iterate through child map and minus it from the parent map
//                    for (auto const& pair : aggVarMap[&agg]) {
//                        std::string varName = pair.first;
//                        int numOccurrences = pair.second;
//                        aggVarMap[parent[&agg]][varName] -= numOccurrences;
//                    }
//                }
//            });
//
//            // build new relation and atom
//            auto* head = new Atom();
//            head->setQualifiedName(relName);
//            std::vector<bool> symbolArguments;
//
//            // Insert all variables occurring in the body of the aggregate into the head
//            for (const auto& pair : aggVarMap[&agg]) {
//                std::string var = pair.first;
//                int n = pair.second;
//                // if it doesn't occur in this level, don't add it
//                if (n > 0) {
//                    head->addArgument(mk<ast::Variable>(var));
//                }
//            }
//
//            aggClause->setHead(Own<Atom>(head));
//
//            // instantiate unnamed variables in count operations
//            if (agg.getOperator() == AggregateOp::COUNT) {
//                int count = 0;
//                for (const auto& cur : aggClause->getBodyLiterals()) {
//                    cur->apply(makeLambdaAstMapper([&](Own<Node> node) -> Own<Node> {
//                        // check whether it is a unnamed variable
//                        auto* var = dynamic_cast<UnnamedVariable*>(node.get());
//                        if (var == nullptr) {
//                            return node;
//                        }
//
//                        // replace by variable
//                        auto name = " _" + toString(count++);
//                        auto res = new ast::Variable(name);
//
//                        // extend head
//                        head->addArgument(souffle::clone(res));
//
//                        // return replacement
//                        return Own<Node>(res);
//                    }));
//                }
//            }
//
//            // -- build relation --
//
//            auto* rel = new Relation();
//            rel->setQualifiedName(relName);
//            // add attributes
//            std::map<const Argument*, analysis::TypeSet> argTypes =
//                    analysis::TypeAnalysis::analyseTypes(translationUnit, *aggClause);
//            for (const auto& cur : head->getArguments()) {
//                rel->addAttribute(mk<Attribute>(toString(*cur),
//                        (analysis::isOfKind(argTypes[cur], TypeAttribute::Signed)) ? "number" : "symbol"));
//            }
//
//            program.addClause(Own<Clause>(aggClause));
//            program.addRelation(Own<Relation>(rel));
//
//            // add arguments to head of aggregate body atom (__agg_body_rel_n)
//            VecOwn<Argument> args;
//            for (auto arg : head->getArguments()) {
//                if (auto* var = dynamic_cast<ast::Variable*>(arg)) {
//                    // replace local variable by underscore if local
//                    if (varCtr[var->getName()] == 0) {
//                        args.emplace_back(new UnnamedVariable());
//                        continue;
//                    }
//                }
//                args.emplace_back(arg->clone());
//            }
//            auto aggAtom = mk<Atom>(head->getQualifiedName(), std::move(args), head->getSrcLoc());
//
//            VecOwn<Literal> newBody;
//            newBody.push_back(std::move(aggAtom));
//            const_cast<Aggregator&>(agg).setBody(std::move(newBody));
//        });
//    });
    return changed;
}

bool MaterializeAggregationQueriesTransformer::needsMaterializedRelation(const Aggregator& agg) {
    // everything with more than 1 atom  => materialize
    int countAtoms = 0;
    const Atom* atom = nullptr;
    for (const auto& literal : agg.getBodyLiterals()) {
        const Atom* currentAtom = dynamic_cast<const Atom*>(literal);
        if (currentAtom != nullptr) {
            ++countAtoms;
            atom = currentAtom;
        }
    }

    if (countAtoms > 1) {
        return true;
    }

    bool seenInnerAggregate = false;
    // If we have an aggregate within this aggregate => materialize
    visitDepthFirst(agg, [&](const Aggregator& innerAgg) {
        if (agg != innerAgg) {
            seenInnerAggregate = true;
        }
    });

    if (seenInnerAggregate) {
        return true;
    }

    // If the same variable occurs several times => materialize
    bool duplicates = false;
    std::set<std::string> vars;
    if (atom != nullptr) {
        visitDepthFirst(*atom, [&](const ast::Variable& var) {
            duplicates = duplicates || !vars.insert(var.getName()).second;
        });
    }

    // If there are duplicates a materialization is required
    // for all others the materialization can be skipped
    return duplicates;
}

}  // namespace souffle::ast::transform
