set(SOUFFLE_SOURCES
    FunctorOps.cpp
    Global.cpp
    ast/Aggregator.cpp
    ast/AlgebraicDataType.cpp
    ast/Atom.cpp
    ast/Attribute.cpp
    ast/BinaryConstraint.cpp
    ast/BooleanConstraint.cpp
    ast/BranchDeclaration.cpp
    ast/BranchInit.cpp
    ast/Clause.cpp
    ast/Component.cpp
    ast/ComponentInit.cpp
    ast/ComponentType.cpp
    ast/Constant.cpp
    ast/Counter.cpp
    ast/Directive.cpp
    ast/ExecutionOrder.cpp
    ast/ExecutionPlan.cpp
    ast/FunctorDeclaration.cpp
    ast/FunctionalConstraint.cpp
    ast/IntrinsicFunctor.cpp
    ast/Negation.cpp
    ast/NilConstant.cpp
    ast/Node.cpp
    ast/NumericConstant.cpp
    ast/Pragma.cpp
    ast/Program.cpp
    ast/QualifiedName.cpp
    ast/RecordInit.cpp
    ast/RecordType.cpp
    ast/Relation.cpp
    ast/StringConstant.cpp
    ast/SubsetType.cpp
    ast/Term.cpp
    ast/TranslationUnit.cpp
    ast/Type.cpp
    ast/TypeCast.cpp
    ast/UnionType.cpp
    ast/UnnamedVariable.cpp
    ast/UserDefinedFunctor.cpp
    ast/Variable.cpp
    ast/analysis/Aggregate.cpp
    ast/analysis/AuxArity.cpp
    ast/analysis/ClauseNormalisation.cpp
    ast/analysis/ComponentLookup.cpp
    ast/analysis/Functor.cpp
    ast/analysis/Ground.cpp
    ast/analysis/IOType.cpp
    ast/analysis/PolymorphicObjects.cpp
    ast/analysis/PrecedenceGraph.cpp
    ast/analysis/ProfileUse.cpp
    ast/analysis/RecursiveClauses.cpp
    ast/analysis/RedundantRelations.cpp
    ast/analysis/RelationDetailCache.cpp
    ast/analysis/RelationSchedule.cpp
    ast/analysis/SCCGraph.cpp
    ast/analysis/SumTypeBranches.cpp
    ast/analysis/TopologicallySortedSCCGraph.cpp
    ast/analysis/Type.cpp
    ast/analysis/TypeConstraints.cpp
    ast/analysis/TypeSystem.cpp
    ast/analysis/TypeEnvironment.cpp
    ast/transform/AddNullariesToAtomlessAggregates.cpp
    ast/transform/ComponentChecker.cpp
    ast/transform/ComponentInstantiation.cpp
    ast/transform/DebugReporter.cpp
    ast/transform/ExecutionPlanChecker.cpp
    ast/transform/FoldAnonymousRecords.cpp
    ast/transform/GroundedTermsChecker.cpp
    ast/transform/GroundWitnesses.cpp
    ast/transform/InlineRelations.cpp
    ast/transform/MagicSet.cpp
    ast/transform/MaterializeAggregationQueries.cpp
    ast/transform/MaterializeSingletonAggregation.cpp
    ast/transform/Meta.cpp
    ast/transform/MinimiseProgram.cpp
    ast/transform/NameUnnamedVariables.cpp
    ast/transform/NormaliseGenerators.cpp
    ast/transform/PartitionBodyLiterals.cpp
    ast/transform/PragmaChecker.cpp
    ast/transform/Provenance.cpp
    ast/transform/ReduceExistentials.cpp
    ast/transform/RemoveBooleanConstraints.cpp
    ast/transform/RemoveEmptyRelations.cpp
    ast/transform/RemoveRedundantRelations.cpp
    ast/transform/RemoveRedundantSums.cpp
    ast/transform/RemoveRelationCopies.cpp
    ast/transform/ReorderLiterals.cpp
    ast/transform/ReplaceSingletonVariables.cpp
    ast/transform/ResolveAliases.cpp
    ast/transform/ResolveAnonymousRecordAliases.cpp
    ast/transform/SemanticChecker.cpp
    ast/transform/SimplifyAggregateTargetExpression.cpp
    ast/transform/Transformer.cpp
    ast/transform/TypeChecker.cpp
    ast/transform/UniqueAggregationVariables.cpp
    ast/utility/BindingStore.cpp
    ast/utility/SipsMetric.cpp
    ast/utility/Utils.cpp
    ast2ram/provenance/ClauseTranslator.cpp
    ast2ram/provenance/SubproofGenerator.cpp
    ast2ram/provenance/TranslationStrategy.cpp
    ast2ram/provenance/UnitTranslator.cpp
    ast2ram/seminaive/ClauseTranslator.cpp
    ast2ram/seminaive/ConstraintTranslator.cpp
    ast2ram/seminaive/TranslationStrategy.cpp
    ast2ram/seminaive/UnitTranslator.cpp
    ast2ram/seminaive/ValueTranslator.cpp
    ast2ram/utility/Utils.cpp
    ast2ram/utility/TranslatorContext.cpp
    ast2ram/utility/ValueIndex.cpp
    interpreter/Engine.cpp
    interpreter/Generator.cpp
    interpreter/BrieIndex.cpp
    interpreter/BTreeIndex.cpp
    interpreter/EqrelIndex.cpp
    interpreter/ProvenanceIndex.cpp
    parser/ParserDriver.cpp
    parser/ParserUtils.cpp
    parser/SrcLocation.cpp
    ram/analysis/Complexity.cpp
    ram/analysis/Index.cpp
    ram/analysis/Level.cpp
    ram/analysis/Relation.cpp
    ram/transform/ChoiceConversion.cpp
    ram/transform/CollapseFilters.cpp
    ram/transform/EliminateDuplicates.cpp
    ram/transform/ExpandFilter.cpp
    ram/transform/HoistAggregate.cpp
    ram/transform/HoistConditions.cpp
    ram/transform/IfConversion.cpp
    ram/transform/MakeIndex.cpp
    ram/transform/Parallel.cpp
    ram/transform/ReorderConditions.cpp
    ram/transform/ReorderFilterBreak.cpp
    ram/transform/Transformer.cpp
    ram/transform/TupleId.cpp
    reports/DebugReport.cpp
    synthesiser/Synthesiser.cpp
    synthesiser/Relation.cpp
)


# --------------------------------------------------
# Flex/Bison
# --------------------------------------------------
file(MAKE_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/parser")
flex_target(scanner parser/scanner.ll ${CMAKE_CURRENT_BINARY_DIR}/parser/scanner.cc)
bison_target(parser parser/parser.yy ${CMAKE_CURRENT_BINARY_DIR}/parser/parser.cc
             COMPILE_FLAGS "-Wall -Werror -Wno-error=deprecated -Wno-error=other -v -d")
add_flex_bison_dependency(scanner parser)

# --------------------------------------------------
# Souffle library
# --------------------------------------------------
add_library(libsouffle STATIC
            ${SOUFFLE_SOURCES}
            ${FLEX_scanner_OUTPUTS}
            ${BISON_parser_OUTPUTS}
            )

target_include_directories(libsouffle
                           PUBLIC "${CMAKE_CURRENT_SOURCE_DIR}/include"
                                  "${CMAKE_CURRENT_SOURCE_DIR}"
                                  "${CMAKE_CURRENT_BINARY_DIR}"
                           )

target_link_libraries(libsouffle
                      PUBLIC OpenMP::OpenMP_CXX
                             $<$<BOOL:${USE_ZLIB}>:ZLIB::ZLIB>
                             $<$<BOOL:${USE_SQLITE}>:SQLite::SQLite3>
                             $<$<BOOL:${USE_CURSES}>:Curses::NCurses>
                             LibFFI::LibFFI
                             "${CMAKE_DL_LIBS}"
                      )

# The target names "souffle" for the library and "souffle" for the binary
# clash in cmake.  I could just rename the library (since it's private)
# but to keep things "familiar", I renamed the target to "libsouffle"
# but have to adjust the output name, otherwise the library would be
# called "liblibsouffle"
set_target_properties(libsouffle PROPERTIES OUTPUT_NAME "souffle")

# --------------------------------------------------
# Souffle binary
# --------------------------------------------------
add_executable(souffle
               main.cpp)
target_link_libraries(souffle libsouffle)

# --------------------------------------------------
# Substitutions for souffle-compile and souffle-config
# --------------------------------------------------
# This is a bit fragile but there is actually no better way
# to do this, AFAIK
# Maybe we could make a cmake script instead of using shell?
function(AUTOMAKE_COMPATIBLE_SUBST F_IN F_OUT)
    # enclose in scope so we can create the
    # values to substitute
    set(CXX ${CMAKE_CXX_COMPILER})

    set(CMAKE_HEADER_DIRS "-I${CMAKE_CURRENT_SOURCE_DIR}/include")

    set(CPPFLAGS "")

    # Compile definitions
    get_property(COMP_DEFS DIRECTORY PROPERTY COMPILE_DEFINITIONS)
    set(SUBST_DEFS "")
    foreach(DEF ${COMP_DEFS})
        string(APPEND SUBST_DEFS "-D${DEF} ")
    endforeach()

    # Compile flags
    string(TOUPPER ${CMAKE_BUILD_TYPE} BT_UPPER)
    # Remove -Wall/-Wextra since the code is not warning-free
    string(REGEX REPLACE "-Wall|-Wextra" ""  SOUFFLE_CXXFLAGS ${CMAKE_CXX_FLAGS})
    set(SOUFFLE_CXXFLAGS "${SOUFFLE_CXXFLAGS} ${CMAKE_CXX_FLAGS_${BT_UPPER}} ${SUBST_DEFS} -fopenmp")

    # Libraries
    set(LIBS "-l${CMAKE_DL_LIBS} -lpthread")
    if (USE_SQLITE)
        string(APPEND LIBS " ${SQLite3_LIBRARY}")
    endif()

    if (USE_ZLIB)
        string(APPEND LIBS " ${ZLIB_LIBRARY_RELEASE}")
    endif()

    if (USE_CURSES)
        string(APPEND LIBS " ${CURSES_NCURSES_LIBRARY}")
    endif()

    configure_file("${F_IN}" "${F_OUT}" @ONLY)
endfunction()

automake_compatible_subst("${CMAKE_SOURCE_DIR}/src/souffle-compile.in"
                          "${CMAKE_CURRENT_BINARY_DIR}/souffle-compile")

automake_compatible_subst("${CMAKE_SOURCE_DIR}/src/souffle-config.in"
                          "${CMAKE_CURRENT_BINARY_DIR}/souffle-config")

# FIXME: Ideally, eventually we will move these out to the "tests" subdirectory
# now that we have a sane(er?) build system
add_subdirectory(ast/tests)
add_subdirectory(ram/tests)
add_subdirectory(interpreter/tests)
