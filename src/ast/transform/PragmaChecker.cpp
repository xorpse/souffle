/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2017, The Souffle Developers. All rights reserved.
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file PragmaChecker.cpp
 *
 * Defines a transformer that applies pragmas found in parsed input.
 *
 ***********************************************************************/

#include "ast/transform/PragmaChecker.h"
#include "Global.h"
#include "ast/Pragma.h"
#include "ast/Program.h"
#include "ast/TranslationUnit.h"
#include "ast/utility/Visitor.h"
#include "reports/ErrorReport.h"
#include <utility>
#include <vector>

namespace souffle::ast::transform {
bool PragmaChecker::transform(TranslationUnit& translationUnit) {
    // Command line options take precedence
    auto& config = Global::config();
    std::set<std::string> cmdln_keys;
    for (auto&& [k, v] : config.data()) {
        if (config.state(k) == MainConfig::State::set) {
            cmdln_keys.insert(k);
        }
    }

    auto& program = translationUnit.getProgram();
    auto& error = translationUnit.getErrorReport();
    bool changed = false;
    std::map<std::string, Pragma const*> previous_pragma;

    // Take in pragma options from the datalog file
    for (auto&& pragma : program.getPragmaDirectives()) {
        auto&& [k, v] = pragma->getkvp();

        // warn if subsequent pragmas override one another
        if (!config.allowsMultiple(k)) {
            auto it = previous_pragma.find(k);
            if (it != previous_pragma.end()) {
                error.addDiagnostic({Diagnostic::Type::WARNING,
                        {tfm::format("overriding previous pragma for key `%s`", k), pragma->getSrcLoc()},
                        {{tfm::format("previous pragma for key `%s`", k), it->second->getSrcLoc()}}});
            }

            previous_pragma[k] = pragma.get();
        }

        // Command line options take precedence, even if the param allows multiple
        if (!contains(cmdln_keys, k)) {
            changed = true;

            if (config.allowsMultiple(k))
                config.append(k, v);
            else
                config.set(k, v);
        }
    }

    return changed;
}
}  // namespace souffle::ast::transform
