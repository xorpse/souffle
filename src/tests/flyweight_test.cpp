/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2013, 2015, Oracle and/or its affiliates. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file flyweight_test.cpp
 *
 * Test (concurrent) flyweights.
 *
 ***********************************************************************/

#include "tests/test.h"

#include "souffle/datastructure/ConcurrentFlyweight.h"
#include "souffle/utility/MiscUtil.h"
#include <algorithm>
#include <cstddef>
#include <iostream>
#include <random>
#include <string>
#include <vector>

// How many times to repeat each randomized test
#define RANDOM_TESTS 32
#define RANDOM_TEST_SIZE 32

// For some reason, just using `assert` doesn't work, even if cassert is
// included.
void local_assert(bool cond, std::string msg) {
    if (!cond) {
        std::cerr << "Tests failed.\n";
        std::cerr << msg << "\n";
        exit(99);
    }
}

// The ASSERT_* macros make use of scoping in such a way that they can't be used
// outside a TEST macro block.
#define LOCAL_ASSERT_LE(x, y) local_assert(x <= y, #x " <= " #y)
#define LOCAL_ASSERT_LT(x, y) local_assert(x < y, #x " < " #y)
#define LOCAL_ASSERT_EQ(x, y) local_assert(x == y, #x " == " #y)
#define LOCAL_ASSERT_TRUE(x) local_assert(x, #x)

namespace souffle::test {

static char random_char() {
    static constexpr char charset[] =
            "0123456789"
            "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
            "abcdefghijklmnopqrstuvwxyz";
    static constexpr size_t max_index = sizeof(charset) - 1;
    std::random_device rd;
    std::mt19937 mt(rd());
    std::uniform_int_distribution<std::size_t> dist(0, max_index);
    return charset[dist(mt)];
}

static std::string random_string_with_length(std::size_t length) {
    std::string str(length, 0);
    std::generate_n(str.begin(), length, random_char);
    return str;
}

static std::string random_string() {
    // The number 16 was chosen so that the strings are relatively short - both
    // to be light on CPU/memory, and to make collisions possible.
    static constexpr std::size_t max_size = 16;
    std::random_device rd;
    std::mt19937 mt(rd());
    std::uniform_int_distribution<std::size_t> dist(0, max_size);
    return random_string_with_length(dist(mt));
}

static void check_iter_contains(FlyweightImpl<std::string>& flyweight) {
    for (const auto& it : flyweight) {
        LOCAL_ASSERT_TRUE(flyweight.weakContains(it.first));
    }
}

static void check_iter_find(FlyweightImpl<std::string>& flyweight) {
    for (const auto& it : flyweight) {
        bool was_new;
        std::tie(std::ignore, was_new) = flyweight.findOrInsert(it.first);
        LOCAL_ASSERT_TRUE(!was_new);
    }
}

// Check invariants
static void check(FlyweightImpl<std::string>& flyweight) {
    check_iter_contains(flyweight);
    check_iter_find(flyweight);
}

// Construct a random flyweight by performing random API operations
static std::vector<std::pair<FlyweightImpl<std::string>::index_type, std::string>> random_flyweight(
        FlyweightImpl<std::string>& flyweight, std::size_t min_size, std::size_t max_size) {
    assert(min_size < max_size);
    std::vector<std::pair<FlyweightImpl<std::string>::index_type, std::string>> values;
    std::mutex mu;
    const auto add_new = [&] {
        FlyweightImpl<std::string>::index_type key;
        std::string s = random_string();
        std::tie(key, std::ignore) = flyweight.findOrInsert(s);
        LOCAL_ASSERT_TRUE(flyweight.weakContains(s));
        const std::lock_guard<std::mutex> lock(mu);
        values.emplace_back(key, s);
    };
#ifdef _OPENMP
#pragma omp parallel for
#endif
    for (std::size_t i = 0; i < max_size; ++i) {
        if (random() % 2) {
            add_new();
        } else {
            if (random() % 2 && values.size() > 0) {
                FlyweightImpl<std::string>::index_type key;
                std::string val;
                {
                    const std::lock_guard<std::mutex> lock(mu);
                    std::tie(key, val) = values[random() % values.size()];
                }
                LOCAL_ASSERT_EQ(val, flyweight.fetch(key));
            } else if (values.size() > 0) {
                std::string val;
                {
                    const std::lock_guard<std::mutex> lock(mu);
                    val = values[random() % values.size()].second;
                }
                LOCAL_ASSERT_TRUE(flyweight.weakContains(val));
            }
        }
    }
    while (values.size() < min_size) {
        add_new();
    }
    check(flyweight);
    return values;
}

TEST(Flyweight, FindOrInsertCopy) {
    for (int i = 0; i < RANDOM_TESTS; ++i) {
        FlyweightImpl<std::string> flyweight{1};
        random_flyweight(flyweight, 0, RANDOM_TEST_SIZE);
#ifdef _OPENMP
#pragma omp parallel for
#endif
        for (int j = 0; j < RANDOM_TEST_SIZE; ++j) {
            // Guarantee uniqueness by appending something not in the character set
            // and then the index.
            std::string s = random_string() + "~" + std::to_string(j);
            FlyweightImpl<std::string>::index_type data1, data2;
            bool was_new1, was_new2;
            std::tie(data1, was_new1) = flyweight.findOrInsert(s);
            std::tie(data2, was_new2) = flyweight.findOrInsert(std::string(s));
            EXPECT_EQ(data1, data2);
            EXPECT_TRUE(was_new1);
            EXPECT_FALSE(was_new2);
        }
    }
}

TEST(Flyweight, IteratorEmptyBeginEnd) {
    FlyweightImpl<std::string> flyweight{1};
    EXPECT_EQ(flyweight.begin(), flyweight.begin());
    EXPECT_EQ(flyweight.end(), flyweight.end());
    EXPECT_EQ(flyweight.begin()++, flyweight.end());
    for (const auto val : flyweight) {
        testutil::ignore(val);
        EXPECT_TRUE(false);
    }
}

}  // namespace souffle::test
