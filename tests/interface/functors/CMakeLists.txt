add_library(functorslib SHARED functors.cpp)
set_target_properties(functorslib PROPERTIES
    VERSION ${PROJECT_VERSION}
    SOVERSION 1)
