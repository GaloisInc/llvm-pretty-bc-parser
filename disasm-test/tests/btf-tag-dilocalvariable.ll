;; Adapted from https://github.com/llvm/llvm-project/blob/1bebc31c617d1a0773f1d561f02dd17c5e83b23b/llvm/test/Bitcode/attr-btf_tag-parameter.ll

!0 = distinct !DICompileUnit(language: DW_LANG_C99, file: !1, producer: "clang version 13.0.0 (https://github.com/llvm/llvm-project.git c9e3139e00bcef23b236a02890b909a130d1b3d9)", isOptimized: true, runtimeVersion: 0, emissionKind: FullDebug, enums: !2, splitDebugInlining: false, nameTableKind: None)
!1 = !DIFile(filename: "func.c", directory: "/home/yhs/work/tests/llvm/btf_tag")
!2 = !{}
!8 = distinct !DISubprogram(name: "f", scope: !1, file: !1, line: 1, type: !9, scopeLine: 1, flags: DIFlagPrototyped | DIFlagAllCallsDescribed, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !0, retainedNodes: !12)
!9 = !DISubroutineType(types: !10)
!10 = !{!11, !11}
!11 = !DIBasicType(name: "int", size: 32, encoding: DW_ATE_signed)
!12 = !{!13}
!13 = !DILocalVariable(name: "a", arg: 1, scope: !8, file: !1, line: 1, type: !11, annotations: !14)
!14 = !{!15, !16}
!15 = !{!"btf_tag", !"a"}
!16 = !{!"btf_tag", !"b"}
