# Souffle - A Datalog Compiler
# Copyright (c) 2021 The Souffle Developers. All rights reserved
# Licensed under the Universal Permissive License v 1.0 as shown at:
# - https://opensource.org/licenses/UPL
# - <souffle root>/licenses/SOUFFLE-UPL.txt

include(SouffleTests)

function(POSITIVE_TEST NAME)
    souffle_positive_test(${NAME} example)
endfunction()

function(NEGATIVE_TEST NAME)
    souffle_negative_test(${NAME} example)
endfunction()

# Make sure that functors are built for the graph_coloring example
add_subdirectory(graph_coloring)

# swig test which will run python, java or both
function(SOUFFLE_POSITIVE_FUNCTOR_TEST TEST_NAME)
        souffle_run_test_helper(TEST_NAME ${TEST_NAME} FUNCTORS ${ARGN})
        souffle_run_test_helper(TEST_NAME ${TEST_NAME} COMPILED FUNCTORS ${ARGN})
endfunction()


function(SOUFFLE_RUN_CPP_TEST)
    cmake_parse_arguments(
        PARAM
        ""
        "PARAM_TEST_NAME;QUALIFIED_TEST_NAME;INPUT_DIR;OUTPUT_DIR;FIXTURE_NAME;TEST_LABELS;FACTS_DIR"
        ""
        ${ARGV}
    )


    add_test(NAME ${PARAM_QUALIFIED_TEST_NAME}_compile_cpp
             COMMAND sh -c "set -e; ${CMAKE_CXX_COMPILER} --std=c++17 -D__EMBEDDED_SOUFFLE__  -I${CMAKE_SOURCE_DIR}/src/include ${TEST_NAME}.cpp ${PARAM_INPUT_DIR}/driver.cpp -o ${TEST_NAME}")
    set_tests_properties(${PARAM_QUALIFIED_TEST_NAME}_compile_cpp PROPERTIES
                         WORKING_DIRECTORY "${PARAM_OUTPUT_DIR}"
                         LABELS "${PARAM_TEST_LABELS}"
                         FIXTURES_SETUP ${PARAM_FIXTURE_NAME}_compile_cpp
                         FIXTURES_REQUIRED ${PARAM_FIXTURE_NAME}_run_souffle
                        )

    add_test(NAME ${PARAM_QUALIFIED_TEST_NAME}_run_cpp
            COMMAND sh -c "set -e; ./${TEST_NAME} ${FACTS_DIR} >${TEST_NAME}.out 2>${TEST_NAME}.err")
    set_tests_properties(${PARAM_QUALIFIED_TEST_NAME}_run_cpp PROPERTIES
                         WORKING_DIRECTORY "${PARAM_OUTPUT_DIR}"
                         LABELS "${PARAM_TEST_LABELS}"
                         FIXTURES_SETUP ${PARAM_FIXTURE_NAME}_run_cpp
                         FIXTURES_REQUIRED ${PARAM_FIXTURE_NAME}_compile_cpp
                        )
endfunction()

function(SOUFFLE_RUN_CPP_TEST_HELPER)
    # PARAM_TEST_NAME - the name of the test, the short directory name under tests/<category>/<test_name>
    cmake_parse_arguments(
        PARAM
        "COMPARE_STDOUT"
        "TEST_NAME" #Single valued options
        ""
        ${ARGV}
    )

    set(INPUT_DIR "${CMAKE_CURRENT_SOURCE_DIR}/${PARAM_TEST_NAME}")
    set(FACTS_DIR "${INPUT_DIR}/facts")
    set(OUTPUT_DIR "${CMAKE_CURRENT_BINARY_DIR}/${PARAM_TEST_NAME}")
    # Give the test a name which has good info about it when running
    # People can then search for the test by the name, or the labels we create
    set(QUALIFIED_TEST_NAME interface/${PARAM_TEST_NAME})
    set(FIXTURE_NAME ${QUALIFIED_TEST_NAME}_fixture)
    set(TEST_LABELS "positive;integration")

    souffle_setup_integration_test_dir(TEST_NAME ${PARAM_TEST_NAME}
                                       QUALIFIED_TEST_NAME ${QUALIFIED_TEST_NAME}
                                       DATA_CHECK_DIR ${INPUT_DIR}
                                       OUTPUT_DIR ${OUTPUT_DIR}
                                       EXTRA_DATA ${EXTRA}
                                       FIXTURE_NAME ${FIXTURE_NAME}
                                       TEST_LABELS ${TEST_LABELS})

    souffle_run_integration_test(TEST_NAME ${PARAM_TEST_NAME}
                                 QUALIFIED_TEST_NAME ${QUALIFIED_TEST_NAME}
                                 INPUT_DIR ${INPUT_DIR}
                                 OUTPUT_DIR ${OUTPUT_DIR}
                                 FIXTURE_NAME ${FIXTURE_NAME}
                                 TEST_LABELS "${TEST_LABELS}"
                                 SOUFFLE_PARAMS "-g ${OUTPUT_DIR}/${TEST_NAME}.cpp")
        
    souffle_run_cpp_test(TEST_NAME ${PARAM_TEST_NAME}
                         QUALIFIED_TEST_NAME ${QUALIFIED_TEST_NAME}
                         INPUT_DIR ${INPUT_DIR}
                         OUTPUT_DIR ${OUTPUT_DIR}
                         FIXTURE_NAME ${FIXTURE_NAME}
                         FACTS_DIR "${FACTS_DIR}"
                         TEST_LABELS ${TEST_LABELS})

    souffle_compare_std_outputs(TEST_NAME ${PARAM_TEST_NAME}
                                 QUALIFIED_TEST_NAME ${QUALIFIED_TEST_NAME}
                                 OUTPUT_DIR ${OUTPUT_DIR}
                                 EXTRA_DATA ${EXTRA}
                                 RUN_AFTER_FIXTURE ${FIXTURE_NAME}_run_cpp
                                 TEST_LABELS ${TEST_LABELS})

    souffle_compare_csv(QUALIFIED_TEST_NAME ${QUALIFIED_TEST_NAME}
                        INPUT_DIR ${INPUT_DIR}
                        OUTPUT_DIR ${OUTPUT_DIR}
                        RUN_AFTER_FIXTURE ${FIXTURE_NAME}_run_cpp
                        NEGATIVE ${PARAM_NEGATIVE}
                        TEST_LABELS ${TEST_LABELS})

endfunction()

# cpp test which will compile Souffle programs externally
function(SOUFFLE_POSITIVE_CPP_TEST TEST_NAME)
    souffle_run_cpp_test_helper(TEST_NAME ${TEST_NAME} ${ARGN})
endfunction()


positive_test(2sat)
positive_test(access-policy)
positive_test(ackermann)
positive_test(amicable)
positive_test(andersen)
positive_test(array)
positive_test(bigrams)
positive_test(catalan)
positive_test(cba_expr_value)
positive_test(cellular_automata)
positive_test(centroids)
positive_test(circuit_eval)
positive_test(circuit_records)
positive_test(circuit_sat)
positive_test(clique)
positive_test(cliquer)
positive_test(comp-parametrized-inherit)
positive_test(comp-parametrized-multilvl)
positive_test(comp-parametrized)
positive_test(convex_hull)
positive_test(counter)
positive_test(degree)
positive_test(dfa)
positive_test(dfa_live_vars)
positive_test(dfa_min)
positive_test(dfa_parse)
positive_test(dfa_summary_function)
positive_test(disconnected)
positive_test(dnf)
positive_test(dominance)
positive_test(double_tree)
positive_test(earley)
positive_test(edit_distance)
positive_test(equal)
positive_test(euclid)
positive_test(factorial)
positive_test(factoring)
positive_test(family)
positive_test(fib)
positive_test(fib1)
positive_test(flights)
positive_test(flights2)
positive_test(floydwarshall)
positive_test(fractional_knapsack)
positive_test(func)
positive_test(game2)
positive_test(grad)
positive_test(graph)
souffle_positive_functor_test(graph_coloring CATEGORY example)
positive_test(grid)
positive_test(highest_degree)
positive_test(hmmer)
positive_test(independent_body3)
positive_test(inline_nats)
positive_test(input_output)
positive_test(java-pointsto)
positive_test(josephus)
positive_test(k-permutations)
positive_test(linksum)
positive_test(longest_path)
positive_test(lubm)
souffle_positive_multi_test(TEST_NAME lucas CATEGORY example FACTS_DIR_NAMES fibonacci lucas mersenne pell)
positive_test(magic_access-policy)
positive_test(magic_prime2)
positive_test(magic_pointsto)
positive_test(metro)
positive_test(minesweeper)
positive_test(minimum_spanning_tree)
positive_test(minpath1)
positive_test(minpath2)
positive_test(minpath3)
positive_test(mmult)
positive_test(movies)
souffle_positive_multi_test(TEST_NAME nfsa2fsa CATEGORY example FACTS_DIR_NAMES ex1 ex2)
positive_test(not_match)
positive_test(nqueens)
positive_test(orbits1)
positive_test(orbits)
positive_test(palindrome)
positive_test(paths)
positive_test(perfect_numbers)
positive_test(planar)
positive_test(po1)
positive_test(po2)
positive_test(po3)
positive_test(po4)
positive_test(po5)
positive_test(pointsto)
positive_test(prime2)
positive_test(prime)
positive_test(profile_default)
positive_test(profile_json)
positive_test(puzzle)
positive_test(ranpo)
positive_test(recipes)
positive_test(rsg)
positive_test(rewrite)
positive_test(sequences)
positive_test(ship)
positive_test(shortest_edges)
positive_test(shortest_path)
positive_test(small)
positive_test(sort)
positive_test(speed)
positive_test(strategies)
positive_test(tak)
positive_test(tc)
positive_test(tic-tac-toe)
positive_test(topological_ordering)
positive_test(traffic)
positive_test(trains)
positive_test(trans)
positive_test(tree)
positive_test(triangle)
positive_test(turing1)
positive_test(weighted_distances)
positive_test(lucas)
positive_test(nfsa2fsa)
