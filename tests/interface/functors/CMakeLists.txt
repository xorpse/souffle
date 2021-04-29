add_library(functors SHARED functors.cpp)
set_target_properties(functors PROPERTIES
    VERSION ${PROJECT_VERSION}
    SOVERSION 1)
target_include_directories(functors PRIVATE "${CMAKE_SOURCE_DIR}/src/include")

target_compile_features(functors
                        PUBLIC cxx_std_17)
set_target_properties(functors PROPERTIES CXX_EXTENSIONS OFF)

target_compile_options(functors
                       PUBLIC "-Wall;-Wextra;-fwrapv")

