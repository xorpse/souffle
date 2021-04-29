add_subdirectory(evaluation)
# Turns out these were not part of the usual souffle testing
if (SOUFFLE_TEST_EXAMPLES)
    add_subdirectory(example)
endif()
add_subdirectory(semantic)
add_subdirectory(swig)
add_subdirectory(syntactic)
add_subdirectory(interface)
