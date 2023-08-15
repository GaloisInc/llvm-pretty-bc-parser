This directory contains various LLVM bitcode files that should be used as the
inputs to the `disasm_test` process.

# Background

There are multiple types of inputs to disasm_test:
  * `.ll` files, which are converted to bitcode files via `llvm-as`
  * `C`/`C++` files, which are converted to bitcode files via `clang`
  * raw bitcode files, found here.

As described by disasm-test, once a bitcode file is available, it is converted
back into a text `.ll` format by LLVM's `llvm-dis` and this package's parsing
library + `llvm-pretty` pretty-printing.  If that is successful, the resulting
`.ll` file from the second method is used as the new input to repeat the above
process, and the two results are compared for equivalence.

# This Directory

The contribution from *this* directory are bitcode-format files that are not--as
part of this testing--generated from a `.ll` or `C` or `C++` file.

Because the contents of this directory are not human readable (and usually
generated from some other source/toolset), this README should be extended to
include a description of what each bitcode file is intended to test and how it
was generated (even if it wasn't generated locally).

The tasty-sweet expectations will look in this directory for a `.bc` file as the
root file, with a `.ll` file as the expected file, but (as described in the
Purpose section below), the `.ll` represents the form of the `.bc` that is
obtained via this parsing library (plus `llvm-pretty` pretty-printing) and not
necessarily the original `.bc` file.  The contents of the `.ll` files here are as
described in the `disasm-test/README.md`.

# Purpose

A primary example of this type of file is a bitcode file generated from Apple's
modified CLANG/LLVM toolset; these files may contain metadata specifications that
are unique to Apple and not merged into the upstream LLVM tools.  The
`llvm-pretty-bc-parser` library should be permissive on parsing when it does not
impact the binary code executed, and thus, this library should admit the
Apple-specific bitcode even though the Apple-specific metadata will be dropped.
Since the disasm-test tests run in multiple environments, the actual
bitcode-format files from the Apple toolset is added directly here.

# Test Manifest

* `hello-world.bc` : This is a simple test to verify this set of test inputs.  It
  is actually generated from `hello-world.ll` and should therefore return
  success; it represents the benchmark for the other tests collected in this
  directory.

  Generated on Linux via: `llvm-as hello-world.ll -o hello-world.bc`
  with `llvm-as` version 11.1.0.
