# Revision history for llvm-pretty-bc-parser

## 0.4.1.0 (January 2024)

* Add preliminary support for LLVM versions up through 16.
* Require building with `llvm-pretty-0.12.*`.
* Add preliminary support for parsing opaque pointers. For now,
  `llvm-pretty-bc-parser` will still fill in the types of certain instructions
  with non-opaque pointer types (e.g., the type of memory to store in a `store`
  instruction), so be wary of this if you are parsing a bitcode file that
  contains opaque pointers. See also the discussion in
  https://github.com/GaloisInc/llvm-pretty-bc-parser/issues/262.
* Improve the runtime performance of the parser.
* A variety of bugfixes. Some notable fixes include:
  * Fix a bug in which the parser would fail to parse `DIDerivedType` nodes
    produced by Apple Clang on macOS.
  * Fix a bug in which the DWARF address space field of a `DIDerivedType` node
    was parsed incorrectly.
  * Fix a bug in which constant `fcmp`/`icmp` expressions would parse their
    operands incorrectly.
