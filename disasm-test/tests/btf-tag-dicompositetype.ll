;; https://github.com/llvm/llvm-project/blob/0b32dca12ef4d82af71f86a70c49806e5b81ead2/llvm/test/Bitcode/attr-btf_tag-dicomposite.ll

!3 = !DIFile(filename: "struct.c", directory: "/home/yhs/work/tests/llvm/btf_tag")
!6 = distinct !DICompositeType(tag: DW_TAG_structure_type, name: "t", file: !3, line: 1, size: 32, elements: !7, annotations: !10)
!7 = !{!8}
!8 = !DIDerivedType(tag: DW_TAG_member, name: "a", scope: !6, file: !3, line: 1, baseType: !9, size: 32)
!9 = !DIBasicType(name: "int", size: 32, encoding: DW_ATE_signed)
!10 = !{!11, !12}
!11 = !{!"btf_tag", !"a"}
!12 = !{!"btf_tag", !"b"}
