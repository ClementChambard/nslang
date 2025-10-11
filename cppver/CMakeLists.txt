cmake_minimum_required(VERSION 4.1.1)

set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_STANDARD_REQUIRED True)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
set(CMAKE_BUILD_TYPE "Debug")

add_compile_options(-Wall -Wextra -Werror)
include_directories(compiler/lib)

project(nslang VERSION 1.0)

add_subdirectory(compiler)

include(FetchContent)
include(CTest)

FetchContent_Declare(Catch2 GIT_REPOSITORY https://github.com/catchorg/Catch2.git GIT_TAG devel)
FetchContent_MakeAvailable(Catch2)

include(Catch)

add_subdirectory(tests)
