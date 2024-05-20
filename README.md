# llvm-pretty-bc-parser

A parser for the LLVM bitcode file format, yielding a `Module` from
[the llvm-pretty package](http://hackage.haskell.org/package/llvm-pretty).

## Compatibility

The following table shows what kinds of tests have been/are being run with which
compilers.

 - A check in the the randomized tests column indicates that such tests are
   regularly run [in Github Actions][fuzz-workflow].
 - A check in the manual tests column indicates that the parser seems to work on
   some code that was generated by this compiler.

| Compiler  | Version | [Randomized tests](./fuzzing) | Manual tests | Notes                |
|-----------|---------|-------------------------------|--------------|----------------------|
| `clang`   | v3.4    | ✓                             |              |                      |
|           | v3.5    | ✓                             |              |                      |
|           | v3.6    | ✓                             |              |                      |
|           | v3.7    | ✓                             |              |                      |
|           | v3.8    | ✓                             | ✓            |                      |
|           | v3.9    | ✓                             |              |                      |
|           | v4.0    | ✓                             |              |                      |
|           | v5.0    | ✓                             |              |                      |
|           | v6.0    | ✓                             | ✓            |                      |
|           | v7.0    | ✓                             |              |                      |
|           | v8.0    | ✓                             |              |                      |
|           | v9.0    | ✓                             |              |                      |
|           | v10.0   | ✓                             |              |                      |
|           | v11.0   | ✓                             |              |                      |
|           | v12.0   | ✓                             |              |                      |
|           | v13.0   | ✓                             |              | See [issues][llvm13] |
|           | v14.0   | ✓                             |              | See [issues][llvm14] |
|           | v15.0   | ✓                             |              | See [issues][llvm15] |
|           | v16.0   | ✓                             |              | See [issues][llvm16] |
|           | v17.0   | ✓                             |              | See [issues][llvm17] |
| `clang++` | v3.4    |                               |              |                      |
|           | v3.5    |                               |              |                      |
|           | v3.6    |                               |              |                      |
|           | v3.7    |                               |              |                      |
|           | v3.8    |                               |              |                      |
|           | v3.9    |                               |              |                      |
|           | v4.0    |                               |              |                      |
|           | v5.0    |                               |              |                      |
|           | v6.0    |                               |              |                      |
|           | v7.0    |                               | ✓            |                      |
|           | v8.0    |                               |              |                      |

If you encounter problems with the output of *any* compiler, please file [an
issue](https://github.com/GaloisInc/llvm-pretty-bc-parser/issues).

## Documentation

Developers' documentation: [doc/developing.md](./doc/developing.md)

## GHC Support

llvm-pretty-bc-parser endeavors to support three versions of GHC at a time. See
the developers' documentation for more details and a rationale:
[doc/developing.md](./doc/developing.md). Currently supported:

- GHC 9.4.5
- GHC 9.6.2
- GHC 9.8.1

[fuzz-workflow]: https://github.com/GaloisInc/llvm-pretty-bc-parser/blob/master/.github/workflows/llvm-quick-fuzz.yml
[llvm13]: https://github.com/GaloisInc/llvm-pretty-bc-parser/issues?q=is%3Aopen+is%3Aissue+label%3Allvm%2F13.0
[llvm14]: https://github.com/GaloisInc/llvm-pretty-bc-parser/issues?q=is%3Aopen+is%3Aissue+label%3Allvm%2F14.0
[llvm15]: https://github.com/GaloisInc/llvm-pretty-bc-parser/issues?q=is%3Aopen+is%3Aissue+label%3Allvm%2F15.0
[llvm16]: https://github.com/GaloisInc/llvm-pretty-bc-parser/issues?q=is%3Aopen+is%3Aissue+label%3Allvm%2F16.0
[llvm17]: https://github.com/GaloisInc/llvm-pretty-bc-parser/issues?q=is%3Aopen+is%3Aissue+label%3Allvm%2F17.0
