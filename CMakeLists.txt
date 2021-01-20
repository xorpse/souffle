cmake_minimum_required(VERSION 3.15)
# PACKAGE_VERSION is the full tag with git hash
execute_process(COMMAND git describe --tags --always
                OUTPUT_VARIABLE GIT_PACKAGE_VERSION)

string(REGEX REPLACE "\n$" "" PACKAGE_VERSION ${GIT_PACKAGE_VERSION})
message(STATUS "Building souffle version ${PACKAGE_VERSION}")

# SOUFFLE_VERSION only includes the major/minor triplet
string(REGEX REPLACE "-.*$" "" SOUFFLE_VERSION ${PACKAGE_VERSION})

project(souffle VERSION ${SOUFFLE_VERSION}
                DESCRIPTION "A datalog compiler"
                LANGUAGES CXX)

if (NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE "Release" CACHE STRING
        "Choose the type of build" FORCE)
endif()
message(STATUS "Build Type: ${CMAKE_BUILD_TYPE}")

# --------------------------------------------------
# User options available from the command line/cache
# --------------------------------------------------
option(DOMAIN_64BIT "Enable/Disable 64-bit number values in Datalog tuples" ON)
option(USE_CURSES "Enable/Disable ncurses-based provenance display" ON)
option(SWIG "Enable/Disable all SWIG builds" OFF)
option(SWIG_PYTHON "Enable/Disable Python SWIG" OFF)
option(SWIG_JAVA "Enable/Disable Java SWIG" OFF)
option(USE_ZLIB "Enable/Disable use of libz file compression" ON)
option(USE_SQLITE "Enable/Disable use sqlite IO" ON)
# This is how souffle is built by default in Debug
option(FAST_DEBUG "Enable/Disable ON 'fast debug' mode" ON)
# NDEBUG = ON means -DNDEBUG on the compiler command line = no cassert
# Therefor NDEBUG = OFF means keep asserts
option(NDEBUG "Enable/Disable runtime checks even in release mode" OFF)
# Souffle, by default, uses -O3 for release builds
option(FAST_RELEASE "Enable/Disable 'fast release' mode" ON)

# Add aditional modules to CMake
set(CMAKE_MODULE_PATH "${PROJECT_SOURCE_DIR}/cmake" ${CMAKE_MODULE_PATH})

# --------------------------------------------------
# 32/64-bit domain
# --------------------------------------------------
if (DOMAIN_64BIT)
    add_compile_definitions(RAM_DOMAIN_SIZE=64)
endif()

# --------------------------------------------------
# curses libraries for Provenance information
# --------------------------------------------------
if (USE_CURSES)
    find_package(Curses REQUIRED)
    add_compile_definitions(USE_NCURSES)
endif()

# --------------------------------------------------
# swig support
# --------------------------------------------------
if (SWIG)
    # Enable both
    set(SWIG_PYTHON "ON" CACHE STRING "" FORCE)
    set(SWIG_JAVA "ON" CACHE STRING "" FORCE)
endif()

if (SWIG_PYTHON OR SWIG_JAVA)
    find_package(SWIG REQUIRED)

    if (SWIG_PYTHON)
        find_package(Python "3.7" REQUIRED)
    endif()

    if (SWIG_JAVA)
        find_package(Java REQUIRED
                     COMPONENTS Development)
    endif()
endif()

# --------------------------------------------------
# Set C++ standard globally to C++17
# --------------------------------------------------
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

# --------------------------------------------------
# flex and bison
# --------------------------------------------------
find_package(FLEX REQUIRED)
find_package(BISON "3.0.4" REQUIRED)

# --------------------------------------------------
# mcpp
# --------------------------------------------------
find_program(MCPP mcpp)

# --------------------------------------------------
# libz
# --------------------------------------------------
if (USE_ZLIB)
    find_package(ZLIB REQUIRED)
    add_compile_definitions(USE_LIBZ)
endif()

# --------------------------------------------------
# sqlite
# --------------------------------------------------
if (USE_SQLITE)
    find_package(SQLite3 REQUIRED)
    add_compile_definitions(USE_SQLITE)
endif()

# --------------------------------------------------
# libffi
# --------------------------------------------------
find_package(LibFFI REQUIRED)

# --------------------------------------------------
# pthreads
# --------------------------------------------------
set(THREADS_PREFER_PTHREAD_FLAG ON)
find_package(Threads REQUIRED)

# --------------------------------------------------
# OpenMP
# --------------------------------------------------
find_package(OpenMP REQUIRED
             COMPONENTS CXX)

# --------------------------------------------------
# Generate the config file
# --------------------------------------------------
configure_file("${PROJECT_SOURCE_DIR}/cmake/config.h.in"
               "${PROJECT_BINARY_DIR}/src/config.h")

# --------------------------------------------------
# Change compile flags
# --------------------------------------------------
if (CMAKE_BUILD_TYPE MATCHES "Debug")
    if (FAST_DEBUG)
        string(REGEX REPLACE "-g" "-O0 -g3" CMAKE_CXX_FLAGS_DEBUG ${CMAKE_CXX_FLAGS_DEBUG})
    endif()
else()
    foreach(FLAG_VAR
            CMAKE_CXX_FLAGS_RELEASE
            CMAKE_CXX_FLAGS_MINSIZEREL
            CMAKE_CXX_FLAGS_RELWITHDEBINFO)

            # Remove/keep NDEBUG in Release builds
            if (NOT NDEBUG)
                string(REGEX REPLACE "-DNDEBUG" "" ${FLAG_VAR} "${${FLAG_VAR}}")
            endif()

            # Change -O2 to -O3
            if (FAST_RELEASE)
                string(REGEX REPLACE "-O2" "-O3" ${FLAG_VAR} "${${FLAG_VAR}}")
            endif()

    endforeach()
endif()

set(CMAKE_CXX_FLAGS "-Wall -Wextra -fwrapv")

# TODO
# Tests:
#  - semantic.at
#  - swig.at
#  - interface.at
#  - provenance.at
# Update README
# Tests
# install
# add list of "outputs" to test targets
# bash completion
# Doxygen
# Sanitizers
# Packaging
# clang-format, clang-tidy etc
# substitute changelog

enable_testing()
add_subdirectory(src)
add_subdirectory(tests)
