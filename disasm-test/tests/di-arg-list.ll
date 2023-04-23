; ModuleID = 'di-arg-list.c'
source_filename = "di-arg-list.c"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

; Function Attrs: mustprogress nofree norecurse nosync nounwind readnone uwtable willreturn
define dso_local i32 @f(i32 %0, i32 %1) local_unnamed_addr #0 !dbg !9 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !14, metadata !DIExpression()), !dbg !17
  call void @llvm.dbg.value(metadata i32 %1, metadata !15, metadata !DIExpression()), !dbg !17
  call void @llvm.dbg.value(metadata !DIArgList(i32 %0, i32 %1), metadata !16, metadata !DIExpression(DW_OP_LLVM_arg, 0, DW_OP_LLVM_arg, 1, DW_OP_plus, DW_OP_stack_value)), !dbg !17
  ret i32 0, !dbg !18
}

; Function Attrs: nofree nosync nounwind readnone speculatable willreturn
declare void @llvm.dbg.value(metadata, metadata, metadata) #1

attributes #0 = { mustprogress nofree norecurse nosync nounwind readnone uwtable willreturn "frame-pointer"="none" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #1 = { nofree nosync nounwind readnone speculatable willreturn }

!llvm.dbg.cu = !{!0}
!llvm.module.flags = !{!3, !4, !5, !6}
!llvm.ident = !{!7}
!llvm.commandline = !{!8}

!0 = distinct !DICompileUnit(language: DW_LANG_C99, file: !1, producer: "clang version 13.0.1", isOptimized: true, runtimeVersion: 0, emissionKind: FullDebug, enums: !2, splitDebugInlining: false, nameTableKind: None)
!1 = !DIFile(filename: "di-arg-list.c", directory: "/home/rscott/Documents/Hacking/Haskell/llvm-pretty-bc-parser/disasm-test/tests")
!2 = !{}
!3 = !{i32 7, !"Dwarf Version", i32 4}
!4 = !{i32 2, !"Debug Info Version", i32 3}
!5 = !{i32 1, !"wchar_size", i32 4}
!6 = !{i32 7, !"uwtable", i32 1}
!7 = !{!"clang version 13.0.1"}
!8 = !{!"/home/rscott/Software/clang+llvm-13.0.1/bin/clang-13 -S -emit-llvm -g -O1 -frecord-command-line di-arg-list.c -o di-arg-list.at-least-llvm13.ll"}
!9 = distinct !DISubprogram(name: "f", scope: !1, file: !1, line: 1, type: !10, scopeLine: 1, flags: DIFlagPrototyped | DIFlagAllCallsDescribed, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !0, retainedNodes: !13)
!10 = !DISubroutineType(types: !11)
!11 = !{!12, !12, !12}
!12 = !DIBasicType(name: "int", size: 32, encoding: DW_ATE_signed)
!13 = !{!14, !15, !16}
!14 = !DILocalVariable(name: "x", arg: 1, scope: !9, file: !1, line: 1, type: !12)
!15 = !DILocalVariable(name: "y", arg: 2, scope: !9, file: !1, line: 1, type: !12)
!16 = !DILocalVariable(name: "z", scope: !9, file: !1, line: 2, type: !12)
!17 = !DILocation(line: 0, scope: !9)
!18 = !DILocation(line: 3, column: 3, scope: !9)
