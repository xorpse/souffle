/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2020, The Souffle Developers. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file SimplifyAggregateTargetExpression.cpp
 *
 ***********************************************************************/

#include "ast/transform/SimplifyAggregateTargetExpression.h"
#include "ast/Argument.h"
#include "ast/TranslationUnit.h"
#include "ast/analysis/Aggregate.h"
#include "ast/transform/GroundWitnesses.h"
#include "ast/utility/NodeMapper.h"
#include "ast/utility/Visitor.h"

namespace souffle::ast::transform {

Aggregator* SimplifyAggregateTargetExpressionTransformer::simplifyTargetExpression(
        const TranslationUnit& tu, const Clause* clause, const Aggregator* aggregator) {
    const auto* origTargetExpression = aggregator->getTargetExpression();
    assert(origTargetExpression != nullptr && !isA<Variable>(origTargetExpression) &&
            "aggregator should have complex target expression");

    // Complex target expressions should be replaced with unique variables.
    // What we might have though now is that a variable in the TE was shadowing
    // a variable from the outer scope. Now we have "forgotten" this shadowing
    // and need to restore it by scoping that variable properly.
    // We know that a variable from the TE is shadowing another variable
    // if a variable with the same name appears range-restricted in the outer scope.

    // Create the new simplified target expression
    auto newTargetExpression = mk<Variable>(analysis::findUniqueVariableName(*clause, "x"));

    // Create the new body, with the necessary equality between old and new target expressions
    auto equalityLiteral = std::make_unique<BinaryConstraint>(BinaryConstraintOp::EQ,
            souffle::clone(newTargetExpression), souffle::clone(origTargetExpression));

    std::vector<Own<Literal>> newBody;
    for (const auto* literal : aggregator->getBodyLiterals()) {
        newBody.push_back(souffle::clone(literal));
    }
    newBody.push_back(std::move(equalityLiteral));

    // If there are occurrences of the same variable in the outer scope
    // Then we need to be careful. There are two ensuing situations:
    // 1) The variable in the outer scope is ungrounded (or occurs in the head)
    //      => We have a witness, and we shouldn't rename this variable, because it is not
    //      local.
    // 2) The variable in the outer scope is grounded
    //      => We need to rename this because it is a local variable (the grounding of the
    //      outer scope
    //         variable is shadowed by occurrence of the variable in the target expression)

    // We already have a way to find witnesses and also to find variables occurring outside
    // this aggregate. We will take the set minus of variablesOccurringOutside - witnesses.
    // Whichever variables are in this set need to be renamed within the aggregate subclause.
    auto witnesses = analysis::getWitnessVariables(tu, *clause, *aggregator);
    std::set<std::string> varsOutside = analysis::getVariablesOutsideAggregate(*clause, *aggregator);

    std::set<std::string> varsGroundedOutside;
    for (auto& varName : varsOutside) {
        if (!contains(witnesses, varName)) {
            varsGroundedOutside.insert(varName);
        }
    }

    // rename all variables that were grounded outside
    // (we had an occurrence of the same variable name in the TE,
    // the implication being that the occurrence of that variable
    // in the scope of the aggregate subclause should be local,
    // not grounded from the outer scope (injected)
    visitDepthFirst(*origTargetExpression, [&](const Variable& v) {
        if (contains(varsGroundedOutside, v.getName())) {
            // rename it everywhere in the body so that we've scoped this properly.
            std::string newVarName = analysis::findUniqueVariableName(*clause, v.getName());
            visitDepthFirst(newBody, [&](const Variable& literalVar) {
                if (literalVar.getName() == v.getName()) {
                    const_cast<Variable&>(literalVar).setName(newVarName);
                }
            });
        }
    });

    // set up a new aggregate to replace this one
    return new Aggregator(aggregator->getBaseOperator(), std::move(newTargetExpression), std::move(newBody));
}

bool SimplifyAggregateTargetExpressionTransformer::transform(TranslationUnit& translationUnit) {
    Program& program = translationUnit.getProgram();
    // Map all aggregates with complex target expressions to aggregates
    // with simple target expressions and an extra equality literal in the body
    // Note: we need to be careful that we don't inadvertently turn
    // a local variable (recognised as local by the fact that it occurs in the target expression)
    // to an injected variable.
    // I.e it is possible that this happens:
    // .. :- A(y), x = sum y + z : { B(y, z) }
    // -> :- A(y), x = sum z0: { B(y, z), z0 = y + z.

    struct replace_aggregators : public NodeMapper {
        const std::map<const Aggregator*, Aggregator*>& oldToNew;

        replace_aggregators(const std::map<const Aggregator*, Aggregator*>& oldToNew) : oldToNew(oldToNew) {}

        std::unique_ptr<Node> operator()(std::unique_ptr<Node> node) const override {
            if (auto* aggregator = dynamic_cast<Aggregator*>(node.get())) {
                if (contains(oldToNew, aggregator)) {
                    return Own<Aggregator>(oldToNew.at(aggregator));
                }
            }
            node->apply(*this);
            return node;
        }
    };

    // Generate the necessary simplified forms for each complex aggregator
    std::map<const Aggregator*, Aggregator*> complexToSimple;
    for (const auto* clause : program.getClauses()) {
        visitDepthFirst(*clause, [&](const Aggregator& aggregator) {
            const auto* targetExpression = aggregator.getTargetExpression();
            if (targetExpression != nullptr && !isA<Variable>(targetExpression)) {
                complexToSimple.insert(
                        {&aggregator, simplifyTargetExpression(translationUnit, clause, &aggregator)});
            }
        });
    }

    // Replace the old aggregators with the new
    replace_aggregators update(complexToSimple);
    program.apply(update);
    return !complexToSimple.empty();
}

}  // namespace souffle::ast::transform
