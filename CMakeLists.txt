cmake_minimum_required(VERSION 3.15)

project(BigUInt VERSION 1.0 LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_EXTENSIONS OFF)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# Locate libfmt-dev and catch2 (e.g. installed by apt)
find_package(fmt CONFIG REQUIRED)
find_package(Catch2 REQUIRED)

# BigUInt library
add_library(BigUIntLib STATIC src/big_uint.cpp)
target_include_directories(BigUIntLib PRIVATE ${PROJECT_SOURCE_DIR}/include)

# BigUInt main
add_executable(BigUInt src/main.cpp)
target_include_directories(BigUInt PRIVATE ${PROJECT_SOURCE_DIR}/include)
target_link_libraries(BigUInt PRIVATE BigUIntLib fmt::fmt)

# BigUInt tests
add_executable(BigUIntTest tests/test.cpp)
target_include_directories(BigUIntTest PRIVATE ${PROJECT_SOURCE_DIR}/include)
target_link_libraries(BigUIntTest PRIVATE BigUIntLib fmt::fmt Catch2::Catch2WithMain)

# Unit testing
include(CTest)
include(Catch)
catch_discover_tests(BigUIntTest)
