# llvm-pretty-bc-parser

Parser for the llvm bitcode format.

## Compatibility

`llvm-pretty-bc-parser` has been tested extensively against LLVM/Clang 3.8.

It is known to not be perfectly compatible with LLVM/Clang 3.9 in the presence
of debug information (see [issue
#79](https://github.com/GaloisInc/llvm-pretty-bc-parser/issues/79)), but when
compiling with out debug symbols, your mileage may vary.

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
