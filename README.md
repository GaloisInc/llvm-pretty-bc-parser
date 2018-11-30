# llvm-pretty-bc-parser

A parser for the LLVM bitcode file format, yielding a `Module` from
[the llvm-pretty package](http://hackage.haskell.org/package/llvm-pretty).

## Compatibility

`llvm-pretty-bc-parser` has been tested extensively against LLVM/Clang 3.8.

It is known to not be perfectly compatible with LLVM/Clang 3.9 in the presence
of debug information (see [issue
#79](https://github.com/GaloisInc/llvm-pretty-bc-parser/issues/79)), but when
compiling with out debug symbols, your mileage may vary.

## Documentation

Developers' documentation: [doc/developing.md](./doc/developing.md)
