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
    main.cpp
    lexer.cpp
    parser.cpp
    print.cpp)

if(CMAKE_COMPILER_IS_GNUCXX)
	list(APPEND CMAKE_CXX_FLAGS "-Wall -std=c++11 ${CMAKE_CXX_FLAGS}")
endif()

add_executable(slang ${SOURCE_FILES} ${HEADER_FILES})
