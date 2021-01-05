#include "ast/Clause.h"

namespace souffle::ast {

void Clause::addToBody(VecOwn<Literal>&& literals) {
    assert(allValidPtrs(literals));
    bodyLiterals.insert(bodyLiterals.end(), std::make_move_iterator(literals.begin()),
            std::make_move_iterator(literals.end()));
}
}  // namespace souffle::ast
