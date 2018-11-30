# Developers' documentation

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [Developers' documentation](#developers-documentation)
    - [Upstream documentation](#upstream-documentation)
    - [Running the tests](#running-the-tests)
        - [`llvm-disasm-test`](#llvm-disasm-test)
        - [`regression-test`](#regression-test)
<!-- markdown-toc end -->

## Upstream documentation

Official (yet incomplete) reference: https://llvm.org/docs/BitCodeFormat.html

C++ implementation:
 + Parser:
   * [Release 4.0](https://github.com/llvm-mirror/llvm/blob/release_40/lib/Bitcode/Reader/BitcodeReader.cpp)
   * [Release 5.0](https://github.com/llvm-mirror/llvm/blob/release_50/lib/Bitcode/Reader/BitcodeReader.cpp)
   * [Release 6.0](https://github.com/llvm-mirror/llvm/blob/release_60/lib/Bitcode/Reader/BitcodeReader.cpp)
 + Record codes:
   * Bitstream format:
     - [Release 4.0](https://github.com/llvm-mirror/llvm/blob/release_40/include/llvm/Bitcode/BitCodes.h)
     - [Release 5.0](https://github.com/llvm-mirror/llvm/blob/release_50/include/llvm/Bitcode/BitCodes.h)
     - [Release 6.0](https://github.com/llvm-mirror/llvm/blob/release_60/include/llvm/Bitcode/BitCodes.h)
   * LLVM bitcode:
     - [Release 4.0](https://github.com/llvm-mirror/llvm/blob/release_40/include/llvm/Bitcode/LLVMBitCodes.h)
     - [Release 5.0](https://github.com/llvm-mirror/llvm/blob/release_50/include/llvm/Bitcode/LLVMBitCodes.h)
     - [Release 6.0](https://github.com/llvm-mirror/llvm/blob/release_60/include/llvm/Bitcode/LLVMBitCodes.h)

## Running the tests

### `llvm-disasm-test`

To compare the behavior of `llvm-disasm` against that of `llvm-dis`:
```bash
cabal build
./dist/build/disasm-test/disasm-test ./disasm-test/tests/fun-attrs.ll
```
To see all the options,
```bash
./dist/build/disasm-test/disasm-test --help
```

If you have [Nix](https://nixos.org/nix/) installed, you can easily compare
against multiple versions of `llvm-dis`, e.g.
```bash
nix-shell --pure -p llvm_6 --run "./dist/build/disasm-test/disasm-test ./disasm-test/tests/fun-attrs.ll"
```

### `regression-test`

To compare the behavior of two different versions of `llvm-disasm`, run
```bash
cabal build
./dist/build/regression-test/regression-test --rev1=HEAD --rev2=HEAD~1
```
To see all the options,
```bash
./dist/build/regression-test/regression-test --help
```

## Travis CI build

The `.travis.yml` file is generated using
[haskell-ci](https://github.com/haskell-CI/haskell-ci).
However, we have to add the following so that it fetches the latest
`llvm-pretty` from Github.
```yml
  - "printf 'packages: llvm-pretty-bc-parser-*/*.cabal https://github.com/elliottt/llvm-pretty/archive/master.tar.gz \\n' > cabal.project"
```
