#!/usr/bin/env bash

# Consider this script a template for reducing failing test cases. In
# particular, not each bug will manifest in the same way, giving the
# same error message, so modify the grep condition appropriately for
# each bug.

clang -I${CSMITH_PATH}/runtime -O -g -w -c -emit-llvm fuzz-temp-test.c -o fuzz-temp-test.bc;
if [ $? -ne 0 ]; then
    exit 1
fi
llvm-disasm fuzz-temp-test.bc 2>&1 | grep 'is missing from the symbol table'
