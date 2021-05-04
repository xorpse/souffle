# Make sure that the functor library is built 
add_subdirectory(functors)

souffle_positive_functor_test(functors CATEGORY interface)
