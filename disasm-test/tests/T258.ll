; ModuleID = 'T258.c'
source_filename = "T258.c"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

; Function Attrs: mustprogress nofree noinline norecurse nosync nounwind willreturn memory(none) uwtable
define dso_local void @f(ptr nocapture noundef %0) local_unnamed_addr #0 !dbg !10 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !16, metadata !DIExpression()), !dbg !17
  ret void, !dbg !18
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn memory(none) uwtable
define dso_local i32 @main() local_unnamed_addr #1 !dbg !19 {
  call void @llvm.dbg.assign(metadata i1 undef, metadata !23, metadata !DIExpression(), metadata !24, metadata ptr undef, metadata !DIExpression()), !dbg !25
  ret i32 0, !dbg !26
}

; Function Attrs: mustprogress nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.assign(metadata, metadata, metadata, metadata, metadata, metadata) #2

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.value(metadata, metadata, metadata) #3

attributes #0 = { mustprogress nofree noinline norecurse nosync nounwind willreturn memory(none) uwtable "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #1 = { mustprogress nofree norecurse nosync nounwind willreturn memory(none) uwtable "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #2 = { mustprogress nocallback nofree nosync nounwind speculatable willreturn memory(none) }
attributes #3 = { nocallback nofree nosync nounwind speculatable willreturn memory(none) }

!llvm.dbg.cu = !{!0}
!llvm.module.flags = !{!2, !3, !4, !5, !6, !7, !8}
!llvm.ident = !{!9}

!0 = distinct !DICompileUnit(language: DW_LANG_C11, file: !1, producer: "clang version 17.0.2 (https://github.com/llvm/llvm-project b2417f51dbbd7435eb3aaf203de24de6754da50e)", isOptimized: true, runtimeVersion: 0, emissionKind: FullDebug, splitDebugInlining: false, nameTableKind: None)
!1 = !DIFile(filename: "T258.c", directory: "/home/ryanscott/Documents/Hacking/Haskell/llvm-pretty-bc-parser/disasm-test/tests", checksumkind: CSK_MD5, checksum: "f929e28f718c84761ea699b3ede6b85d")
!2 = !{i32 7, !"Dwarf Version", i32 5}
!3 = !{i32 2, !"Debug Info Version", i32 3}
!4 = !{i32 1, !"wchar_size", i32 4}
!5 = !{i32 8, !"PIC Level", i32 2}
!6 = !{i32 7, !"PIE Level", i32 2}
!7 = !{i32 7, !"uwtable", i32 2}
!8 = !{i32 7, !"debug-info-assignment-tracking", i1 true}
!9 = !{!"clang version 17.0.2 (https://github.com/llvm/llvm-project b2417f51dbbd7435eb3aaf203de24de6754da50e)"}
!10 = distinct !DISubprogram(name: "f", scope: !1, file: !1, line: 1, type: !11, scopeLine: 1, flags: DIFlagPrototyped | DIFlagAllCallsDescribed, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !0, retainedNodes: !15)
!11 = !DISubroutineType(types: !12)
!12 = !{null, !13}
!13 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !14, size: 64)
!14 = !DIBasicType(name: "int", size: 32, encoding: DW_ATE_signed)
!15 = !{!16}
!16 = !DILocalVariable(name: "x", arg: 1, scope: !10, file: !1, line: 1, type: !13)
!17 = !DILocation(line: 0, scope: !10)
!18 = !DILocation(line: 1, column: 47, scope: !10)
!19 = distinct !DISubprogram(name: "main", scope: !1, file: !1, line: 3, type: !20, scopeLine: 3, flags: DIFlagPrototyped | DIFlagAllCallsDescribed, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !0, retainedNodes: !22)
!20 = !DISubroutineType(types: !21)
!21 = !{!14}
!22 = !{!23}
!23 = !DILocalVariable(name: "x", scope: !19, file: !1, line: 4, type: !14)
!24 = distinct !DIAssignID()
!25 = !DILocation(line: 0, scope: !19)
!26 = !DILocation(line: 6, column: 3, scope: !19)
