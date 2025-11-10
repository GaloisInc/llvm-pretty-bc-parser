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
that this test output is used for all LLVM major versions up to (but not
including) `<version>`. Note that if a test case has multiple
`pre-llvm<version>.ll` files, then the `<version>` that is closest to the current
LLVM version (without going over) is picked.

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

There are only a limited set of `pre-llvm<version>` specifications recognized:

* `pre-llvm9`
* `pre-llvm12`
* `pre-llvm13`
* `pre-llvm14`
* `pre-llvm15`
* `pre-llvm16`
* `pre-llvm17`

There are also circumstances where the LLVM syntax undergoes a significant change
in a specific release (e.g. in LLVM 19, certain LLVM intrinsic functions were
dropped in favor of DebugRecords, which are represented with completely new
syntax).  To facilitate support for this, it is also possible to use the
`post-llvm<version>` test qualifier to indicate that the associated file contains
results that should only apply to LLVM versions *after* the specified
`<version>`.  The selection process for `post-llvm<version>` files is similar to
the `pre-llvm<version>` selection described above but with a lower threshold
rather than an upper threshould.  It is also possible to use `pre-llvm<version>`
ahd `post-llvm<version>` specifications together to be very selective about which
files are selected for testing in association with specific LLVM versions.

The following `post-llvm<version>` specifications are recognized:
* `post-llvm18`
