function(ADD_SOUFFLE_BINARY_TEST TEST_NAME CATEGORY)
    add_executable(${TEST_NAME} ${TEST_NAME}.cpp)
    target_link_libraries(${TEST_NAME} libsouffle)
    add_test(NAME ${TEST_NAME} COMMAND ${TEST_NAME})
    set_tests_properties(${TEST_NAME} PROPERTIES LABELS "binary;${CATEGORY}")
endfunction()

function(RUN_SOUFFLE_TEST_HELPER TEST_NAME CATEGORY EXTRA_FLAGS NEGATIVE)
    set(TARGET_NAME "test_${CATEGORY}_${TEST_NAME}")
    set(INPUT_DIR "${CMAKE_CURRENT_SOURCE_DIR}/${TEST_NAME}")
    set(OUTPUT_DIR "${CMAKE_CURRENT_BINARY_DIR}/${TEST_NAME}")

    file(GLOB_RECURSE INPUT_FILES LIST_DIRECTORIES false
         "${INPUT_DIR}/*.csv" "${INPUT_DIR}/${TEST_NAME}.out" "${INPUT_DIR}/${TEST_NAME}.err")

    add_custom_target(${TARGET_NAME}
                      DEPENDS ${INPUT_FILES}
                      COMMAND "${CMAKE_SOURCE_DIR}/cmake/setup_test_dir.sh" "${INPUT_DIR}" "${OUTPUT_DIR}" "${TEST_NAME}"
                      COMMAND $<TARGET_FILE:souffle> -D"${OUTPUT_DIR}" -F"${INPUT_DIR}/facts" "${INPUT_DIR}/${TEST_NAME}.dl"
                              1>"${OUTPUT_DIR}/${TEST_NAME}.out" 2>"${OUTPUT_DIR}/${TEST_NAME}.err"
                      COMMAND "${CMAKE_SOURCE_DIR}/cmake/check_std_outputs.sh" "${OUTPUT_DIR}" "${TEST_NAME}"
                      COMMAND "${CMAKE_SOURCE_DIR}/cmake/check_test_results.sh" "${OUTPUT_DIR}"
    )

    # FIXME - add list of "outputs" to target above

    add_test(NAME ${CATEGORY}/${TEST_NAME}>
             COMMAND ${CMAKE_COMMAND} --build "${CMAKE_BINARY_DIR}" --target ${TARGET_NAME})

endfunction()
