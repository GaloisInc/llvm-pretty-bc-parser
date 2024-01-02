##> rootMatchName: p2.cpp
##> summary: failure to llvm-as the llvm-disasm results

Using this module's llvm-disasm to convert the clang++ generated bitcode into assembly (.ll), the resulting assembly is invalid for llvm-as:

    error: expected type
    define linkonce_ddr default i32 @_ZN4uorb14uorb_advertiseEPK21orb_metadata(%class.uorb* %32

This is demonstrated with llvm version 7, 11, 13, and 15: it may or may not occur
with other LLVM versions.
