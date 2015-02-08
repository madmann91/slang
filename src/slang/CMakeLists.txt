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
    print.h
    token.h
    tokenlist.h)

set(SOURCE_FILES
    lexer.cpp
    parser.cpp
    print.cpp)

if(CMAKE_COMPILER_IS_GNUCXX)
	list(APPEND CMAKE_CXX_FLAGS "-Wall -std=c++11 ${CMAKE_CXX_FLAGS}")
endif()

add_library(slang ${SOURCE_FILES} ${HEADER_FILES})
add_executable(slangc main.cpp)
target_link_libraries(slangc slang)
