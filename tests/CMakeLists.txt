add_subdirectory(evaluation)
# Turns out these were not part of the usual souffle testing
if (TEST_EXAMPLES)
    add_subdirectory(example)
endif()
add_subdirectory(syntactic)
