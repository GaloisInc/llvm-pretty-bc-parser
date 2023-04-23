;; Adapted from https://github.com/llvm/llvm-project/blob/d383df32c0d5bcdc8c160ecdd7174399aa3c5395/llvm/test/Bitcode/attr-btf_tag-disubprogram.ll

!0 = distinct !DICompileUnit(language: DW_LANG_C99, file: !1, producer: "clang version 13.0.0 (https://github.com/llvm/llvm-project.git a6dd9d402a04d53403664bbb47771f2573c7ade0)", isOptimized: true, runtimeVersion: 0, emissionKind: FullDebug, enums: !2, splitDebugInlining: false, nameTableKind: None)
!1 = !DIFile(filename: "func.c", directory: "/home/yhs/work/tests/llvm/btf_tag")
!2 = !{}
!7 = !{!"clang version 13.0.0 (https://github.com/llvm/llvm-project.git a6dd9d402a04d53403664bbb47771f2573c7ade0)"}
!8 = distinct !DISubprogram(name: "f", scope: !1, file: !1, line: 1, type: !9, scopeLine: 1, flags: DIFlagPrototyped | DIFlagAllCallsDescribed, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !0, retainedNodes: !12, annotations: !14)
!9 = !DISubroutineType(types: !10)
!10 = !{!11, !11}
!11 = !DIBasicType(name: "int", size: 32, encoding: DW_ATE_signed)
!12 = !{!13}
!13 = !DILocalVariable(name: "a", arg: 1, scope: !8, file: !1, line: 1, type: !11)
!14 = !{!15, !16}
!15 = !{!"btf_tag", !"a"}
!16 = !{!"btf_tag", !"b"}
