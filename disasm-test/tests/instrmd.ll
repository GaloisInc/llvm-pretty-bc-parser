define i32 @f(i32 %a0) {
   %1 = mul i32 %a0, 2
   %2 = mul i32 %1, 2
   %3 = mul i32 %2, 2
   br label %test
test:
   %4 = mul i32 %a0, 2, !llvm.loop !3
   br label %5, !dbg !1
; <label>:5
   %6 = mul i32 %4, 2
   %7 = mul i32 %6, 2
   %8 = mul i32 %7, 2
   %9 = mul i32 %8, 2
   %10 = mul i32 %9, 2
   %11 = mul i32 %10, 2
   %12 = mul i32 %11, 2
   br label %test, !dbg !2, !llvm.loop !3
}

!llvm.dbg.cu = !{!0}
!llvm.module.flags = !{!11}

!0 = distinct !DICompileUnit(language: DW_LANG_C99, file: !7, producer: "hand-made version 1.0", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, enums: !10, splitDebugInlining: false, nameTableKind: None)
!1 = !DILocation(line: 1, column: 1, scope: !5)
!2 = !DILocation(line: 91, column: 81, scope: !5)
!3 = distinct !{!3, !1, !4}
!4 = !DILocation(line: 9, column: 8, scope: !5)
!5 = distinct !DILexicalBlock(scope: !6, file: !1, line: 32, column: 5)
!6 = distinct !DISubprogram(name: "test", scope: !7, file: !7, line: 31, type: !9, scopeLine: 31, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !10)
!7 = !DIFile(filename: "disasm-test/tests/instrmd.ll", directory: "/where/these/tests/are")
!9 = !DISubroutineType(types: !10)
!10 = !{}
!11 = !{i32 2, !"Debug Info Version", i32 3}
