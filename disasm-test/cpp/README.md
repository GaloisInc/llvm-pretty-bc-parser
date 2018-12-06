# C++ tests

The assembly files were generated using the `generate.sh` script, which details
which versions of `clang++` and `llvm-as` they are expected to work with.

Passes roundtrip:
 - `return0.ll`

Parses, but fails roundtrip:
 - `iostream.ll`
