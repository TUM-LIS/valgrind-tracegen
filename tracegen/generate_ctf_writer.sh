#!/bin/bash
#
# Regenerate the barectf-generated trace writer
#
# Requires barectf to be installed on the system.
# See https://github.com/efficios/barectf for installation instructions.
#

set -e # exit on eror

barectf ctf_config.yaml

# Valgrind requires the use of tl_assert() in tools.
# Quick-n-Dirty replace the standard asserts() with the valgrind tool version
sed -i 's/#include <assert.h>/#include "pub_tool_libcassert.h"'/ tg_ctf.c
sed -i 's/assert(/tl_assert(/g' tg_ctf.c

# Convert metadata into C header file to be included in the tool
xxd -i metadata tg_ctf_metadata.h

