cmake_minimum_required(VERSION 3.10)
project(SnakeGame LANGUAGES C)

add_executable(csnake src/snake.c)

if(CMAKE_SYSTEM_NAME STREQUAL "Windows")
    add_subdirectory(winsnake)
endif()

add_subdirectory(pas)
