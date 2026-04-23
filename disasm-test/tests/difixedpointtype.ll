;; This test checks generation of DIFixedPointType.
;; From llvm-project llvm/test/Bitcode/fixedpoint_type.ll  (https://github.com/llvm/llvm-project/blob/llvm/test/Bitcode/fixedpoint_type.ll)

; ModuleID = 'fixedpoint_type.ll'
source_filename = "/dir/fixedpoint_type.adb"

!llvm.module.flags = !{!0, !1}
!llvm.dbg.cu = !{!2}

!0 = !{i32 2, !"Debug Info Version", i32 3}
!1 = !{i32 2, !"Dwarf Version", i32 4}
!2 = distinct !DICompileUnit(language: DW_LANG_Ada95, file: !3, producer: "GNAT/LLVM", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, enums: !4, retainedTypes: !5, globals: !4, imports: !4)
!3 = !DIFile(filename: "fixedpoint_type.adb", directory: "/dir")
!4 = !{}
!5 = !{!11, !12, !13}
!6 = distinct !DISubprogram(name: "fp", scope: !3, file: !3, line: 1, type: !7, scopeLine: 1, spFlags: DISPFlagDefinition, unit: !4, retainedNodes: !9)
!7 = !DISubroutineType(types: !8)
!8 = !{null}
!9 = !{!10}
!10 = !DILocalVariable(name: "x", scope: !6, file: !3, line: 3, type: !11, align: 32)
!11 = !DIFixedPointType(name: "fp__decimal", size: 32, align: 32, encoding: DW_ATE_signed_fixed, kind: Decimal, factor: -4)
!12 = !DIFixedPointType(name: "fp__rational", size: 32, align: 32, encoding: DW_ATE_unsigned_fixed, kind: Rational, numerator: 1234, denominator: 5678)
!13 = !DIFixedPointType(name: "fp__binary", size: 64, align: 0, encoding: DW_ATE_unsigned_fixed, kind: Binary, factor: -16)
