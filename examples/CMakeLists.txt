link_libraries(slang)

add_executable(user_macros       user_macros.cpp)
add_executable(pattern_matching  pattern_matching.cpp)
add_executable(obfuscate         obfuscate.cpp)

add_test(NAME test_user_macros        COMMAND user_macros)
add_test(NAME test_pattern_matching   COMMAND pattern_matching)
add_test(NAME test_obfuscate          COMMAND obfuscate)
