cmake_minimum_required(VERSION 3.15)

# PACKAGE_VERSION is the full tag with git hash
execute_process(COMMAND git describe --tags --always
                OUTPUT_VARIABLE PACKAGE_VERSION)
message(STATUS "Building souffle version ${PACKAGE_VERSION}")

# SOUFFLE_VERSION only includes the major/minor triplet
string(REGEX REPLACE "-.*$" "" SOUFFLE_VERSION ${PACKAGE_VERSION})

project(souffle VERSION ${SOUFFLE_VERSION}
                DESCRIPTION "A datalog compiler"
                LANGUAGES CXX)

if (NOT DEFINED ${CMAKE_BUILD_TYPE})
    set(CMAKE_BUILD_TYPE "Release" CACHE STRING "" FORCE)
endif()

# --------------------------------------------------
# User options available from the command line/cache
# --------------------------------------------------
option(DOMAIN_64BIT "Enable/Disable 64-bit number values in Datalog tuples" ON)
option(CURSES "Enable/Disable ncurses-based provenance display" ON)
option(SWIG "Enable/Disable all SWIG builds" OFF)
option(SWIG_PYTHON "Enable/Disable Python SWIG" OFF)
option(SWIG_JAVA "Enable/Disable Java SWIG" OFF)
option(LIBZ "Enable/Disable use of libz file compression" ON)
option(SQLITE "Enable/Disable use sqlite IO" ON)

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
if (CURSES)
    find_package(Curses REQUIRED)
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
if (LIBZ)
    find_package(ZLIB REQUIRED)
endif()

# --------------------------------------------------
# sqlite
# --------------------------------------------------
if (SQLITE)
    find_package(SQLite3 REQUIRED)
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
find_package(OpenMP REQUIRED)

# --------------------------------------------------
# Generate the config file
# --------------------------------------------------
configure_file("${PROJECT_SOURCE_DIR}/cmake/config.h.in"
               "${PROJECT_BINARY_DIR}/config.h")

# TODO

# Tests
# bash completion
# Check headers
# CXXFLAGS="-Wall -Wextra $CXXFLAGS $ENV_CXXFLAGS"
# CXXFLAGS=" -fwrapv "?
# Add 'fast debug" mode: CXXFLAGS="$CXXFLAGS -O0 -g3"
# Souffle version
# Doxygen
# Sanitizers
# Packaging
# clang-format, clang-tidy etc
# dlopen?
#AC_OUTPUT(
#  [ src/souffle-compile src/souffle-config debian/changelog ],
#  [ chmod +x src/souffle-compile src/souffle-config ]
#)
