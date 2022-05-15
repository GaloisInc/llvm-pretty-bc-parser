;; Adapted from https://github.com/llvm/llvm-project/blob/d5561e0a0bbd484da17d3b68ae5fedc0a057246b/llvm/test/Assembler/debug-info.ll

!7 = !DIBasicType(tag: DW_TAG_base_type, name: "name", size: 1, align: 2, encoding: DW_ATE_unsigned_char)
!15 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !7, size: 32, align: 32, dwarfAddressSpace: 1)
