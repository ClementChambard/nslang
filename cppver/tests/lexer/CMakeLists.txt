cmake_minimum_required(VERSION 4.1.1)

add_executable(test_lexer test_lexer.cpp)

target_link_libraries(test_lexer PRIVATE Catch2::Catch2WithMain ns)

target_compile_options(test_lexer PRIVATE -fsanitize=address)
target_link_options(test_lexer PRIVATE -fsanitize=address)

catch_discover_tests(test_lexer)
