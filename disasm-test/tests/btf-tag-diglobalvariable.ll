;; Adapted from https://github.com/llvm/llvm-project/blob/30c288489ae51a3e0819241f367eeae6df2b09e7/llvm/test/Bitcode/attr-btf_tag-diglobalvariable.ll

!0 = !DIGlobalVariableExpression(var: !1, expr: !DIExpression())
!1 = distinct !DIGlobalVariable(name: "g1", scope: !2, file: !3, line: 7, type: !6, isLocal: false, isDefinition: true, annotations: !10)
!2 = distinct !DICompileUnit(language: DW_LANG_C99, file: !3, producer: "clang version 13.0.0 (https://github.com/llvm/llvm-project.git 47af5574a87dc298b5c6c36ff6a969c8c77c8499)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, enums: !4, globals: !5, splitDebugInlining: false, nameTableKind: None)
!3 = !DIFile(filename: "t.c", directory: "/home/yhs/work/tests/llvm/btf_tag")
!4 = !{}
!5 = !{!0}
!6 = distinct !DICompositeType(tag: DW_TAG_structure_type, name: "t1", file: !3, line: 4, size: 32, elements: !7)
!7 = !{!8}
!8 = !DIDerivedType(tag: DW_TAG_member, name: "a", scope: !6, file: !3, line: 5, baseType: !9, size: 32)
!9 = !DIBasicType(name: "int", size: 32, encoding: DW_ATE_signed)
!10 = !{!11, !12}
!11 = !{!"btf_tag", !"tag1"}
!12 = !{!"btf_tag", !"tag2"}
