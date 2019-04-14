#
# Copyright 2019, Mokshasoft AB (mokshasoft.com)
#
# This software may be distributed and modified according to the terms of
# the GNU General Public License version 3. Note that NO WARRANTY is provided.
# See "LICENSE.txt" for details.
#

cmake_minimum_required(VERSION 3.7.2)

# Add the astyle command
include(astyle.cmake)
add_astyle_target()

# Enable debug info
set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -g")

add_subdirectory(test)

set(NAME data-structures)
project(${NAME} C)

file(GLOB static
    src/*.c
)

add_library(${NAME} EXCLUDE_FROM_ALL ${static})
target_include_directories(${NAME} PUBLIC interface)
target_link_libraries(
    ${NAME}
    c
)
