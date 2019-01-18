# C++ tests

The assembly files were generated using the `generate.sh` script, which details
which versions of `clang++` and `llvm-as** they are expected to work with.

**Note**: This is true except for `atomicrmw.ll` which is generated from `nix-build atomicrmw.nix`.

Passes roundtrip:
 - `return0.ll`

Parses, but fails roundtrip:
 - `atomicrmw.ll`
 - `iostream.ll`
 - `templates.ll`
 - `merge.ll`
