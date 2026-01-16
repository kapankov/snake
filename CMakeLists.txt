cmake_minimum_required(VERSION 3.10)
project(SnakeGame LANGUAGES C CXX)

add_executable(csnake src/snake.c)
add_executable(cxxsnake src/snake.cpp)

if(CMAKE_SYSTEM_NAME STREQUAL "Windows")
    target_compile_definitions(cxxsnake PRIVATE _WIN32)
    target_include_directories(cxxsnake PRIVATE 
        $ENV{WindowsSdkDir}/Include/$ENV{WindowsSdkVersion}/um
        $ENV{WindowsSdkDir}/Include/$ENV{WindowsSdkVersion}/shared
    )
    add_subdirectory(winsnake)
endif()

add_subdirectory(pas)
