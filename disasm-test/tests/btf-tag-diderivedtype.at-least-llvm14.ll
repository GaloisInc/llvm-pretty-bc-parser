;; Adapted from https://github.com/llvm/llvm-project/blob/430e22388173c96da6777fccb9735a6e8ee3ea1c/llvm/test/Bitcode/attr-btf_tag-field.ll

!1 = !DIFile(filename: "attr-btf_tag-field.c", directory: "/home/yhs/work/tests/llvm/btf_tag")
!12 = !DIBasicType(name: "int", size: 32, encoding: DW_ATE_signed)
!14 = distinct !DICompositeType(tag: DW_TAG_structure_type, name: "t1", file: !1, line: 7, size: 32, elements: !15)
!15 = !{!16}
!16 = !DIDerivedType(tag: DW_TAG_member, name: "a", scope: !14, file: !1, line: 8, baseType: !12, size: 32, annotations: !17)
!17 = !{!18, !19}
!18 = !{!"btf_tag", !"tag1"}
!19 = !{!"btf_tag", !"tag2"}
