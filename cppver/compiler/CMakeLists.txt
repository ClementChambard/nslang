cmake_minimum_required(VERSION 4.1.1)

file(GLOB_RECURSE LIB_SRCS lib/*.cpp lib/*.c)
file(GLOB_RECURSE LIB_HEADERS lib/*.hpp lib/*.h)

add_library(ns ${LIB_SRCS} ${LIB_HEADERS})

target_compile_options(ns PRIVATE -fsanitize=address)
target_link_options(ns PRIVATE -fsanitize=address)
target_link_libraries(ns PUBLIC LLVM)

add_executable(nslang main.cpp)

target_link_libraries(nslang PRIVATE ns)

target_compile_options(nslang PRIVATE -fsanitize=address)
target_link_options(nslang PRIVATE -fsanitize=address)
