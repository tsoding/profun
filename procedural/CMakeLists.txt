cmake_minimum_required(VERSION 3.7)

project(procedural C)

find_package(SDL2 REQUIRED)
include_directories(${SDL2_INCLUDE_DIRS})

find_package(PkgConfig)
pkg_check_modules(SDL2_GFX SDL2_gfx)
include_directories(${SDL2_GFX_INCLUDE_DIRS})

SET(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -O0 -Wall -Werror -fdump-rtl-expand")

set(SRCS
  main.c
  logic.c
  rendering.c
)

set(HEADERS
  logic.h
  rendering.h
  game.h
)

add_executable(procedural ${SRCS} ${HEADERS})
target_link_libraries(procedural ${SDL2_LIBRARIES} ${SDL2_GFX_LIBRARIES})
