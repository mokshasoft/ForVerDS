#
# 2019, Mokshasoft AB (mokshasoft.com)
#
# This software may be distributed and modified according to the terms of
# the Unlicense license. Note that NO WARRANTY is provided.
# See "LICENSE.txt" for details.
#

cmake_minimum_required(VERSION 3.7.2)

# Function that creates a valgrind target
function(create_valgrind_target target)
    # Run the test in valgrind
    add_custom_target(valgrind.${target} DEPENDS ${target})
    add_custom_command(
        TARGET valgrind.${target}
        POST_BUILD
        COMMAND "valgrind"
                     "--leak-check=full"
                     "--show-leak-kinds=all"
                     "--trace-children=yes"
                     "${CMAKE_CURRENT_BINARY_DIR}/${target}"
        USES_TERMINAL
    )
endfunction()

#
# Tests for the mempool
#

set(TEST test-mempool)
project(${TEST} C)

file(GLOB static
    test-mempool.c
)

add_executable(${TEST} EXCLUDE_FROM_ALL ${static})
target_link_libraries(
    ${TEST}
    data-structures
)
create_valgrind_target(${TEST})
