cmake_minimum_required(VERSION 3.15)

project(BigNum VERSION 1.0 LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_EXTENSIONS OFF)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# Fail on warnings
if(MSVC)
  add_compile_options(/W4 /WX)
else()
  add_compile_options(-Wall -Wextra -Wpedantic -Werror)
endif()

# Locate libfmt-dev and catch2 (e.g. installed by apt)
find_package(fmt CONFIG REQUIRED)
find_package(Catch2 REQUIRED)

# BigInt library
add_library(BigNumLib STATIC src/big_int.cpp)
target_include_directories(BigNumLib PRIVATE ${PROJECT_SOURCE_DIR}/include)

# BigInt main
add_executable(BigInt src/main.cpp)
target_include_directories(BigInt PRIVATE ${PROJECT_SOURCE_DIR}/include)
target_link_libraries(BigInt PRIVATE BigNumLib fmt::fmt)

# BigInt tests
add_executable(BigNumTest tests/test.cpp)
target_include_directories(BigNumTest PRIVATE ${PROJECT_SOURCE_DIR}/include)
target_link_libraries(BigNumTest PRIVATE BigNumLib fmt::fmt Catch2::Catch2WithMain)

# Unit testing
include(CTest)
include(Catch)
catch_discover_tests(BigNumTest)
