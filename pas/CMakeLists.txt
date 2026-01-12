cmake_minimum_required(VERSION 3.10)

find_program(FPC fpc REQUIRED)

# Просто собираем программу
add_custom_target(psnake ALL
    COMMAND ${FPC} -o${CMAKE_BINARY_DIR}/psnake${CMAKE_EXECUTABLE_SUFFIX} snake.pas
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
    COMMENT "Building Snake in Pascal"
)
