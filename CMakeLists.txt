cmake_minimum_required(VERSION 3.1.3 FATAL_ERROR)

project(SLANG)

set(PACKAGE_VERSION "0.0.1")

set(PROJ_ROOT_DIR ${CMAKE_CURRENT_SOURCE_DIR})
set(PROJ_SOURCE_DIR ${PROJ_ROOT_DIR}/src)

set(PROJ_BINARY_DIR  "bin")
set(PROJ_LIBRARY_DIR "lib")
set(PROJ_INCLUDE_DIR "include")

set(PROJ_NAME ${PROJECT_NAME})
string(TOUPPER ${PROJ_NAME} PROJ_PREFIX)

set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/${PROJ_BINARY_DIR})
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/${PROJ_LIBRARY_DIR})
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/${PROJ_LIBRARY_DIR})

set(CMAKE_MODULE_PATH ${PROJ_ROOT_DIR} ${CMAKE_MODULE_PATH})

set(CMAKE_CXX_STANDARD 11)

include_directories(${PROJ_SOURCE_DIR})

set(COLOR_TTY_AVAILABLE TRUE)
if(WIN32)
    # By default, Windows console does not support ANSI escape codes
    set(COLOR_TTY_AVAILABLE FALSE)
endif()
set(COLORIZE_OUTPUT ${COLOR_TTY_AVAILABLE} CACHE BOOL "Set to true to enable colored error messages")

if(COLORIZE_OUTPUT)
    add_definitions(-DCOLORIZE_OUTPUT)
endif()

add_subdirectory(${PROJ_SOURCE_DIR})

include(CTest)
if(BUILD_TESTING)
    add_subdirectory(examples)

    find_package(Python 3.5 REQUIRED)
    add_test(
        NAME test_suite
        COMMAND ${Python_EXECUTABLE} run.py -s $<TARGET_FILE:slangc>
        WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/test)
endif()
