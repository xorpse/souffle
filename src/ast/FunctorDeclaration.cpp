/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2020 The Souffle Developers. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

#include "ast/FunctorDeclaration.h"
#include "souffle/utility/ContainerUtil.h"
#include "souffle/utility/MiscUtil.h"
#include "souffle/utility/StreamUtil.h"
#include "souffle/utility/tinyformat.h"
#include <cassert>
#include <ostream>
#include <utility>

namespace souffle::ast {

FunctorDeclaration::FunctorDeclaration(std::string name, std::vector<TypeAttribute> argsTypes,
        TypeAttribute returnType, bool stateful, SrcLocation loc)
        : Node(std::move(loc)), name(std::move(name)), argsTypes(std::move(argsTypes)),
          returnType(returnType), stateful(stateful) {
    assert(this->name.length() > 0 && "functor name is empty");
}

void FunctorDeclaration::print(std::ostream& out) const {
    auto convert = [&](TypeAttribute type) {
        switch (type) {
            case TypeAttribute::Signed: return "number";
            case TypeAttribute::Symbol: return "symbol";
            case TypeAttribute::Float: return "float";
            case TypeAttribute::Unsigned: return "unsigned";
            case TypeAttribute::Record: break;
            case TypeAttribute::ADT: break;
        }
        fatal("unhandled `TypeAttribute`");
    };

    tfm::format(out, ".declfun %s(%s): %s", name, join(map(argsTypes, convert), ","), convert(returnType));
    if (stateful) {
        out << " stateful";
    }
    out << std::endl;
}

bool FunctorDeclaration::equal(const Node& node) const {
    const auto& other = asAssert<FunctorDeclaration>(node);
    return name == other.name && argsTypes == other.argsTypes && returnType == other.returnType &&
           stateful == other.stateful;
}

FunctorDeclaration* FunctorDeclaration::cloneImpl() const {
    return new FunctorDeclaration(name, argsTypes, returnType, stateful, getSrcLoc());
}

}  // namespace souffle::ast
