/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2018 The Souffle Developers. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file ProvenanceClauseTranslator.h
 *
 ***********************************************************************/

#include "ast2ram/ClauseTranslator.h"

namespace souffle {
class SymbolTable;
}

namespace souffle::ast {
class Atom;
}

namespace souffle::ram {
class Operation;
}

namespace souffle::ast2ram {

class TranslatorContext;

class ProvenanceClauseTranslator : public ClauseTranslator {
public:
    ProvenanceClauseTranslator(const TranslatorContext& context, SymbolTable& symbolTable)
            : ClauseTranslator(context, symbolTable) {}

protected:
    Own<ram::Operation> addNegatedDeltaAtom(Own<ram::Operation> op, const ast::Atom* atom) const override;
    Own<ram::Operation> addNegatedAtom(Own<ram::Operation> op, const ast::Atom* atom) const override;
};

}  // namespace souffle::ast2ram
