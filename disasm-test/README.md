# `disasm-test`

This test suite ensures that for each `.ll` file under the `tests` directory:

1. After using `llvm-as` to produce a `.bc` file, the `.bc` file can be parsed
   using `llvm-pretty-bc-parser`.
2. The resulting `llvm-pretty` AST can be pretty-printed back out to an `.ll`
   file using `llvm-pretty`'s pretty-printer.
3. The new `.ll` file is mostly equivalent to the original `.ll` file.

Here, "mostly equivalent" means that the two files are syntactically
equivalent, ignoring minor differences in whitespace and the order of metadata
in the metadata list.

## Conditional tests

Some of the test cases have slightly different bitcode depending on which LLVM
version is used. These test cases will have accompanying
`<test-case>.pre-llvm<version>.ll` files, where `pre-llvm<version>` indicates
that this test output is used for all LLVM versions up to (but not including)
`<version>`. Note that if a test case has multiple `pre-llvm<version>.ll`
files, then the `<version>` that is closest to the current LLVM version
(without going over) is picked.

To illustrate this with a concrete example, consider suppose we have a test
case `foo` with the following `.ll` files

* `foo.pre-llvm11.ll`
* `foo.pre-llvm13.ll`
* `foo.ll`

The following `.ll` files would be used for the following LLVM versions:

* LLVM 10: `foo.pre-llvm11.ll`
* LLVM 11: `foo.pre-llvm13.ll`
* LLVM 12: `foo.pre-llvm13.ll`
* LLVM 13 or later: `foo.ll`

There are some test cases that require a sufficiently recent LLVM version to
run. To indicate that a test should not be run on LLVMs older than `<version>`,
create a `pre-llvm<version>.ll` file with `SKIP_TEST` as the first line. The
use of `SKIP_TEST` signals that this test should be skipped when using LLVMs
older than `<version>`. Note that the test suite will not read anything past
`SKIP_TEST`, so the rest of the file can be used to document why the test is
skipped on that particular configuration.
