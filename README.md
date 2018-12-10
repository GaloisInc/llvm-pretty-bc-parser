# llvm-pretty-bc-parser

A parser for the LLVM bitcode file format, yielding a `Module` from
[the llvm-pretty package](http://hackage.haskell.org/package/llvm-pretty).

## Compatibility

The following table shows which versions of `clang` and `clang++` the
parser has been tested with.

| LLVM version  | `clang` (C)   | `clang++` (C++) | Notes |
| ------------- | ------------- | --------------- | ----- |
| v3.4          | :+1:          | :question:      |       |
| v3.5          | :+1:          | :question:      |       |
| v3.6          | :+1:          | :question:      |       |
| v3.7          | :+1:          | :question:      |       |
| v3.8          | :+1:          | :question:      |       |
| v3.8          | :+1:          | :question:      |       |
| v6.0          | :+1:          | :+1:            |       |
| v7.0          | :question:    | :question:      |       |
| v8.0          | :question:    | :question:      |       |

If you encounter problems with /any/ version of LLVM, please file
[an issue](https://github.com/GaloisInc/llvm-pretty-bc-parser/issues).

## Documentation

Developers' documentation: [doc/developing.md](./doc/developing.md)
