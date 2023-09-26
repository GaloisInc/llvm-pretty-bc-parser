; ModuleID = '/tmp/nix-shell.mXmHgC/cmpxchg1066927-8.bc'
source_filename = "disasm-test/tests/cmpxchg.c"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

@val = global i32 2344, align 4, !dbg !0

; Function Attrs: noinline nounwind optnone sspstrong uwtable
define i32 @do_atomic_update(i32 %0) #0 !dbg !15 {
  %2 = alloca i32, align 4
  %3 = alloca i32, align 4
  %4 = alloca i32, align 4
  %5 = alloca i8, align 1
  store i32 %0, i32* %2, align 4
  call void @llvm.dbg.declare(metadata i32* %2, metadata !18, metadata !DIExpression()), !dbg !19
  call void @llvm.dbg.declare(metadata i32* %3, metadata !20, metadata !DIExpression()), !dbg !21
  store i32 2344, i32* %3, align 4, !dbg !21
  %6 = load atomic i32, i32* %2 seq_cst, align 4, !dbg !22
  store i32 %6, i32* %4, align 4, !dbg !22
  %7 = load i32, i32* %3, align 4, !dbg !22
  %8 = load i32, i32* %4, align 4, !dbg !22
  %9 = cmpxchg weak i32* @val, i32 %7, i32 %8 seq_cst seq_cst, !dbg !22
  %10 = extractvalue { i32, i1 } %9, 0, !dbg !22
  %11 = extractvalue { i32, i1 } %9, 1, !dbg !22
  br i1 %11, label %13, label %12, !dbg !22

12:                                               ; preds = %1
  store i32 %10, i32* %3, align 4, !dbg !22
  br label %13, !dbg !22

13:                                               ; preds = %12, %1
  %14 = zext i1 %11 to i8, !dbg !22
  store i8 %14, i8* %5, align 1, !dbg !22
  %15 = load i8, i8* %5, align 1, !dbg !22
  %16 = trunc i8 %15 to i1, !dbg !22
  %17 = zext i1 %16 to i32, !dbg !22
  ret i32 %17, !dbg !23
}

; Function Attrs: nounwind readnone speculatable willreturn
declare void @llvm.dbg.declare(metadata, metadata, metadata) #1

attributes #0 = { noinline nounwind optnone sspstrong uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "frame-pointer"="all" "less-precise-fpmad"="false" "min-legal-vector-width"="0" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="true" "stack-protector-buffer-size"="4" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { nounwind readnone speculatable willreturn }

!llvm.dbg.cu = !{!2}
!llvm.module.flags = !{!10, !11, !12, !13}
!llvm.ident = !{!14}

!0 = !DIGlobalVariableExpression(var: !1, expr: !DIExpression())
!1 = distinct !DIGlobalVariable(name: "val", scope: !2, file: !3, line: 4, type: !6, isLocal: false, isDefinition: true)
!2 = distinct !DICompileUnit(language: DW_LANG_C99, file: !3, producer: "clang version 11.1.0", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, enums: !4, globals: !5, splitDebugInlining: false, nameTableKind: None)
!3 = !DIFile(filename: "disasm-test/tests/cmpxchg.c", directory: "/home/kquick/work/DFAMS/llvm-pretty-bc-parser")
!4 = !{}
!5 = !{!0}
!6 = !DIDerivedType(tag: DW_TAG_typedef, name: "atomic_int", file: !7, line: 83, baseType: !8)
!7 = !DIFile(filename: "/nix/store/4pk431ywfvw0k926blzwncnp699z3vh5-clang-wrapper-11.1.0/resource-root/include/stdatomic.h", directory: "")
!8 = !DIDerivedType(tag: DW_TAG_atomic_type, baseType: !9)
!9 = !DIBasicType(name: "int", size: 32, encoding: DW_ATE_signed)
!10 = !{i32 7, !"Dwarf Version", i32 4}
!11 = !{i32 2, !"Debug Info Version", i32 3}
!12 = !{i32 1, !"wchar_size", i32 4}
!13 = !{i32 7, !"PIC Level", i32 2}
!14 = !{!"clang version 11.1.0"}
!15 = distinct !DISubprogram(name: "do_atomic_update", scope: !3, file: !3, line: 6, type: !16, scopeLine: 6, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !2, retainedNodes: !4)
!16 = !DISubroutineType(types: !17)
!17 = !{!9, !6}
!18 = !DILocalVariable(name: "newval", arg: 1, scope: !15, file: !3, line: 6, type: !6)
!19 = !DILocation(line: 6, column: 33, scope: !15)
!20 = !DILocalVariable(name: "old_val", scope: !15, file: !3, line: 7, type: !9)
!21 = !DILocation(line: 7, column: 9, scope: !15)
!22 = !DILocation(line: 8, column: 12, scope: !15)
!23 = !DILocation(line: 8, column: 5, scope: !15)
