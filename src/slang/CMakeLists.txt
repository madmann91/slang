set(HEADER_FILES
    ast.h
    cast.h
    environment.h
    keyword.h
    keywordlist.h
    lexer.h
    location.h
    logger.h
    parser.h
    preprocessor.h
    print.h
    sema.h
    token.h
    tokenlist.h
    types.h)

set(SOURCE_FILES
    environment.cpp
    lexer.cpp
    parser.cpp
    preprocessor.cpp
    print.cpp
    sema.cpp
    types.cpp)

if(CMAKE_COMPILER_IS_GNUCXX)
    set(CMAKE_CXX_FLAGS "-Wall -pedantic -std=c++11 ${CMAKE_CXX_FLAGS}")
    if(CMAKE_BUILD_TYPE STREQUAL "Debug")
        set(CMAKE_CXX_FLAGS "-fprofile-arcs -ftest-coverage ${CMAKE_CXX_FLAGS}")
    endif()
endif()

add_library(slang ${SOURCE_FILES} ${HEADER_FILES})
add_executable(slangc main.cpp)
target_link_libraries(slangc slang)
