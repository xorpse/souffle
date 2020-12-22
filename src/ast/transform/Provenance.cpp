/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2017, The Souffle Developers. All rights reserved.
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file Provenance.cpp
 *
 * Implements Transformer for adding provenance information via extra columns
 *
 ***********************************************************************/

#include "ast/transform/Provenance.h"
#include "RelationTag.h"
#include "ast/Aggregator.h"
#include "ast/Argument.h"
#include "ast/Atom.h"
#include "ast/Attribute.h"
#include "ast/BinaryConstraint.h"
#include "ast/Clause.h"
#include "ast/Constant.h"
#include "ast/Functor.h"
#include "ast/IntrinsicFunctor.h"
#include "ast/Literal.h"
#include "ast/Negation.h"
#include "ast/Node.h"
#include "ast/NumericConstant.h"
#include "ast/Program.h"
#include "ast/QualifiedName.h"
#include "ast/Relation.h"
#include "ast/StringConstant.h"
#include "ast/TranslationUnit.h"
#include "ast/UnnamedVariable.h"
#include "ast/Variable.h"
#include "ast/utility/NodeMapper.h"
#include "ast/utility/Utils.h"
#include "souffle/BinaryConstraintOps.h"
#include "souffle/utility/ContainerUtil.h"
#include "souffle/utility/MiscUtil.h"
#include "souffle/utility/StreamUtil.h"
#include "souffle/utility/StringUtil.h"
#include "souffle/utility/tinyformat.h"
#include <algorithm>
#include <cassert>
#include <cstddef>
#include <iosfwd>
#include <memory>
#include <string>
#include <utility>
#include <vector>

namespace souffle::ast::transform {

void ProvenanceTransformer::makeInfoRelation(
        const Clause& originalClause, size_t originalClauseNum, TranslationUnit& translationUnit) {
    QualifiedName name = originalClause.getHead()->getQualifiedName();
    name.append("@info");
    name.append(toString(originalClauseNum));

    // initialise info relation
    auto infoRelation = mk<Relation>(name);
    infoRelation->setRepresentation(RelationRepresentation::INFO);

    // create new clause containing a single fact
    auto infoClause = mk<Clause>();
    auto infoClauseHead = mk<Atom>(name);

    // add clause num attribute
    infoRelation->addAttribute(mk<Attribute>("clause_num", QualifiedName("number")));
    infoClauseHead->addArgument(mk<NumericConstant>(originalClauseNum));

    // add head relation as meta info
    std::vector<std::string> headVariables;

    // a method to stringify an Argument, translating functors and aggregates
    // keep a global counter of functor and aggregate numbers, which increment for each unique
    // functor/aggregate
    int functorNumber = 0;
    int aggregateNumber = 0;
    auto getArgInfo = [&](Argument* arg) -> std::string {
        if (auto* var = dynamic_cast<ast::Variable*>(arg)) {
            return toString(*var);
        } else if (auto* constant = dynamic_cast<Constant*>(arg)) {
            return toString(*constant);
        }
        if (isA<UnnamedVariable>(arg)) {
            return "_";
        }
        if (isA<Functor>(arg)) {
            return tfm::format("functor_%d", functorNumber++);
        }
        if (isA<Aggregator>(arg)) {
            return tfm::format("agg_%d", aggregateNumber++);
        }

        fatal("Unhandled argument type");
    };

    // add head arguments
    for (auto& arg : originalClause.getHead()->getArguments()) {
        headVariables.push_back(getArgInfo(arg));
    }

    // join variables in the head with commas
    std::stringstream headVariableString;
    headVariableString << join(headVariables, ",");

    // add an attribute to infoRelation for the head of clause
    infoRelation->addAttribute(mk<Attribute>(std::string("head_vars"), QualifiedName("symbol")));
    infoClauseHead->addArgument(mk<StringConstant>(toString(join(headVariables, ","))));

    // visit all body literals and add to info clause head
    for (size_t i = 0; i < originalClause.getBodyLiterals().size(); i++) {
        auto lit = originalClause.getBodyLiterals()[i];

        const Atom* atom = nullptr;
        if (isA<Atom>(lit)) {
            atom = static_cast<Atom*>(lit);
        } else if (isA<Negation>(lit)) {
            atom = static_cast<Negation*>(lit)->getAtom();
        }

        // add an attribute for atoms and binary constraints
        if (atom != nullptr || isA<BinaryConstraint>(lit)) {
            infoRelation->addAttribute(
                    mk<Attribute>(std::string("rel_") + std::to_string(i), QualifiedName("symbol")));
        }

        if (atom != nullptr) {
            std::string relName = toString(atom->getQualifiedName());

            // for an atom, add its name and variables (converting aggregates to variables)
            if (isA<Atom>(lit)) {
                std::string atomDescription = relName;

                for (auto& arg : atom->getArguments()) {
                    atomDescription.append("," + getArgInfo(arg));
                }

                infoClauseHead->addArgument(mk<StringConstant>(atomDescription));
                // for a negation, add a marker with the relation name
            } else if (isA<Negation>(lit)) {
                infoClauseHead->addArgument(mk<StringConstant>("!" + relName));
            }
        }
    }

    // visit all body constraints and add to info clause head
    for (const auto* con : getBodyLiterals<BinaryConstraint>(originalClause)) {
        std::stringstream constraintDescription;
        constraintDescription << toBinaryConstraintSymbol(con->getBaseOperator());
        constraintDescription << "," << getArgInfo(con->getLHS());
        constraintDescription << "," << getArgInfo(con->getRHS());
        infoClauseHead->addArgument(mk<StringConstant>(constraintDescription.str()));
    }

    infoRelation->addAttribute(mk<Attribute>("clause_repr", QualifiedName("symbol")));
    infoClauseHead->addArgument(mk<StringConstant>(toString(originalClause)));

    // set clause head and add clause to info relation
    infoClause->setHead(std::move(infoClauseHead));

    Program& program = translationUnit.getProgram();
    program.addClause(std::move(infoClause));
    program.addRelation(std::move(infoRelation));
}

bool ProvenanceTransformer::transform(TranslationUnit& translationUnit) {
    Program& program = translationUnit.getProgram();

    for (auto relation : program.getRelations()) {
        // generate info relations for each clause
        // do this before all other transformations so that we record
        // the original rule without any instrumentation
        for (auto clause : getClauses(program, *relation)) {
            if (!isFact(*clause)) {
                // add info relation
                makeInfoRelation(*clause, getClauseNum(&program, clause), translationUnit);
            }
        }

        for (auto clause : getClauses(program, *relation)) {
            // mapper to add two provenance columns to atoms
            struct M : public NodeMapper {
                using NodeMapper::operator();

                Own<Node> operator()(Own<Node> node) const override {
                    // add provenance columns
                    if (auto atom = dynamic_cast<Atom*>(node.get())) {
                        atom->addArgument(mk<UnnamedVariable>());
                        atom->addArgument(mk<UnnamedVariable>());
                    }

                    // otherwise - apply mapper recursively
                    node->apply(*this);
                    return node;
                }
            };

            // add unnamed vars to each atom nested in arguments of head
            clause->getHead()->apply(M());

            if (!isFact(*clause)) {
                for (size_t i = 0; i < clause->getBodyLiterals().size(); i++) {
                    auto lit = clause->getBodyLiterals()[i];

                    // add unnamed vars to each atom nested in arguments of lit
                    lit->apply(M());

                    // add two provenance columns to lit; first is rule num, second is level num
                    if (auto atom = dynamic_cast<Atom*>(lit)) {
                        atom->addArgument(mk<UnnamedVariable>());
                        atom->addArgument(mk<ast::Variable>("@level_num_" + std::to_string(i)));
                    }
                }
            }
        }
    }
    return true;
}

}  // namespace souffle::ast::transform
