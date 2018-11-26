# llvm-pretty-bc-parser

Parser for the llvm bitcode format.

## Compatibility

`llvm-pretty-bc-parser` has been tested extensively against LLVM/Clang 3.8.

It is known to not be perfectly compatible with LLVM/Clang 3.9 in the presence
of debug information (see [issue
#79](https://github.com/GaloisInc/llvm-pretty-bc-parser/issues/79)), but when
compiling with out debug symbols, your mileage may vary.

## Documentation

Developers' documentation: [doc/developing.md](./doc/developing.md)
