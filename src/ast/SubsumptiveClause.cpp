/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2020 The Souffle Developers. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

#include "ast/SubsumptiveClause.h"
#include "souffle/utility/ContainerUtil.h"
#include "souffle/utility/MiscUtil.h"
#include "souffle/utility/NodeMapperFwd.h"
#include "souffle/utility/StreamUtil.h"
#include <cassert>
#include <ostream>
#include <utility>

namespace souffle::ast {

SubsumptiveClause::SubsumptiveClause(
        Own<Atom> head, VecOwn<Literal> bodyLiterals, Own<ExecutionPlan> plan, SrcLocation loc)
        : Clause(std::move(head), std::move(bodyLiterals),
          std::move(plan), std::move(loc)) {
}

SubsumptiveClause::SubsumptiveClause(Own<Atom> head,  SrcLocation loc)
        : SubsumptiveClause(std::move(head), {}, {}, std::move(loc)) {}

SubsumptiveClause::SubsumptiveClause(QualifiedName name, SrcLocation loc)
        : SubsumptiveClause(mk<Atom>(name), std::move(loc)) {}

void SubsumptiveClause::addToBodyFront(Own<Literal> literal) {
    assert(literal != nullptr);
    bodyLiterals.insert(bodyLiterals.begin(), std::move(literal));
}

void SubsumptiveClause::print(std::ostream& os) const {
    if (head != nullptr) {
        os << *head;
    }
    if (!bodyLiterals.empty()) {
        os << " <= "; 
        os << *bodyLiterals[0]; 
        // TODO: print from second onwards 
        os << " :- \n   " << join(bodyLiterals, ",\n   ");
    }
    os << ".";
    if (plan != nullptr) {
        os << *plan;
    }
}


SubsumptiveClause* SubsumptiveClause::cloning() const {
    return new SubsumptiveClause(clone(head), clone(bodyLiterals), clone(plan), getSrcLoc());
}

}  // namespace souffle::ast
