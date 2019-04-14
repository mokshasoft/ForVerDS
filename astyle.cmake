#
# Copyright 2019, Mokshasoft AB (mokshasoft.com)
#
# This software may be distributed and modified according to the terms of
# the GNU General Public License version 3. Note that NO WARRANTY is provided.
# See "LICENSE.txt" for details.
#

# Function that creates an astyle target to format the C code
function(add_astyle_target)
    add_custom_target(astyle)
    add_custom_command(
        TARGET astyle
        POST_BUILD
        COMMAND "astyle"
                     "--recursive"
                     "--max-instatement-indent=100"
                     "--style=otbs"
                     "--pad-header"
                     "--indent=spaces=4"
                     "--pad-oper"
                     "${CMAKE_SOURCE_DIR}/*.c"
                     "${CMAKE_SOURCE_DIR}/*.h"
        USES_TERMINAL
    )
endfunction()
