cmake_minimum_required(VERSION 3.10)
project(SnakeGame LANGUAGES C CXX)

add_executable(csnake src/snake.c src/tio.c)
target_include_directories(csnake PRIVATE include)

add_executable(cxxsnake src/snake.cpp src/tio.c)
target_include_directories(cxxsnake PRIVATE include)

if(CMAKE_SYSTEM_NAME STREQUAL "Windows")
    target_compile_definitions(cxxsnake PRIVATE _WIN32)
    add_subdirectory(winsnake)
endif()

add_subdirectory(pas)
