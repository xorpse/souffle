/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2013, 2015, Oracle and/or its affiliates. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file symbol_table_test.cpp
 *
 * Tests souffle's symbol table.
 *
 ***********************************************************************/

#include "tests/test.h"

#include "souffle/SymbolTable.h"
#include "souffle/utility/MiscUtil.h"
#include <algorithm>
#include <cstddef>
#include <iostream>
#include <string>
#include <vector>

namespace souffle::test {

TEST(SymbolTable, Basics) {
    SymbolTable table;

    EXPECT_STREQ("Hello", table.resolve(table.lookup(table.resolve(table.lookup("Hello")))));

    EXPECT_EQ(table.lookup("Hello"), table.lookup(table.resolve(table.lookup("Hello"))));

    EXPECT_STREQ("Hello", table.resolve(table.lookup(table.resolve(table.lookup("Hello")))));

    EXPECT_EQ(table.lookup("Hello"),
            table.lookup(table.resolve(table.lookup(table.resolve(table.lookup("Hello"))))));
}

TEST(SymbolTable, Inserts) {
    SymbolTable X({{"A", "B", "C", "D"}});
    EXPECT_EQ(X.size(), 4);
}

}  // namespace souffle::test
