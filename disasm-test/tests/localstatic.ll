; ModuleID = 'localstatic.bc'
source_filename = "localstatic.c"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

@has_local_static.disptab = internal constant [3 x i8*] [i8* blockaddress(@has_local_static, %10), i8* blockaddress(@has_local_static, %17), i8* blockaddress(@has_local_static, %20)], align 16, !dbg !0
@.str = private unnamed_addr constant [6 x i8] c"= %d\0A\00", align 1

; Function Attrs: noinline nounwind optnone sspstrong uwtable
define i32 @has_local_static(i32 %0) #0 !dbg !2 {
  %2 = alloca i32, align 4
  %3 = alloca i32, align 4
  store i32 %0, i32* %2, align 4
  call void @llvm.dbg.declare(metadata i32* %2, metadata !21, metadata !DIExpression()), !dbg !22
  call void @llvm.dbg.declare(metadata i32* %3, metadata !23, metadata !DIExpression()), !dbg !24
  %4 = load i32, i32* %2, align 4, !dbg !25
  store i32 %4, i32* %3, align 4, !dbg !26
  br label %5, !dbg !27

5:                                                ; preds = %17, %1
  call void @llvm.dbg.label(metadata !28), !dbg !29
  %6 = load i32, i32* %3, align 4, !dbg !30
  %7 = sext i32 %6 to i64, !dbg !31
  %8 = getelementptr [3 x i8*], [3 x i8*]* @has_local_static.disptab, i64 0, i64 %7, !dbg !31
  %9 = load i8*, i8** %8, align 8, !dbg !31
  br label %22, !dbg !32

10:                                               ; preds = %22
  call void @llvm.dbg.label(metadata !33), !dbg !34
  %11 = load i32, i32* %3, align 4, !dbg !35
  %12 = add i32 %11, 1, !dbg !35
  store i32 %12, i32* %3, align 4, !dbg !35
  %13 = load i32, i32* %3, align 4, !dbg !36
  %14 = sext i32 %13 to i64, !dbg !37
  %15 = getelementptr [3 x i8*], [3 x i8*]* @has_local_static.disptab, i64 0, i64 %14, !dbg !37
  %16 = load i8*, i8** %15, align 8, !dbg !37
  br label %22, !dbg !38

17:                                               ; preds = %22
  call void @llvm.dbg.label(metadata !39), !dbg !40
  %18 = load i32, i32* %3, align 4, !dbg !41
  %19 = mul i32 %18, 3, !dbg !41
  store i32 %19, i32* %3, align 4, !dbg !41
  br label %5, !dbg !42

20:                                               ; preds = %22
  call void @llvm.dbg.label(metadata !43), !dbg !44
  %21 = load i32, i32* %3, align 4, !dbg !45
  ret i32 %21, !dbg !46

22:                                               ; preds = %10, %5
  %23 = phi i8* [ %9, %5 ], [ %16, %10 ]
  indirectbr i8* %23, [label %10, label %17, label %20]
}

; Function Attrs: nounwind readnone speculatable willreturn
declare void @llvm.dbg.declare(metadata, metadata, metadata) #1

; Function Attrs: nounwind readnone speculatable willreturn
declare void @llvm.dbg.label(metadata) #1

; Function Attrs: noinline nounwind optnone sspstrong uwtable
define i32 @main(i32 %0, i8** %1) #0 !dbg !47 {
  %3 = alloca i32, align 4
  %4 = alloca i8**, align 8
  store i32 %0, i32* %3, align 4
  call void @llvm.dbg.declare(metadata i32* %3, metadata !53, metadata !DIExpression()), !dbg !54
  store i8** %1, i8*** %4, align 8
  call void @llvm.dbg.declare(metadata i8*** %4, metadata !55, metadata !DIExpression()), !dbg !56
  %5 = load i32, i32* %3, align 4, !dbg !57
  %6 = call i32 @has_local_static(i32 %5), !dbg !58
  %7 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([6 x i8], [6 x i8]* @.str, i64 0, i64 0), i32 %6), !dbg !59
  ret i32 0, !dbg !60
}

declare i32 @printf(i8*, ...) #2

attributes #0 = { noinline nounwind optnone sspstrong uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "frame-pointer"="all" "less-precise-fpmad"="false" "min-legal-vector-width"="0" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="true" "stack-protector-buffer-size"="4" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { nounwind readnone speculatable willreturn }
attributes #2 = { "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "frame-pointer"="all" "less-precise-fpmad"="false" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="true" "stack-protector-buffer-size"="4" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }

!llvm.dbg.cu = !{!7}
!llvm.module.flags = !{!16, !17, !18, !19}
!llvm.ident = !{!20}

!0 = !DIGlobalVariableExpression(var: !1, expr: !DIExpression())
!1 = distinct !DIGlobalVariable(name: "disptab", scope: !2, file: !3, line: 7, type: !10, isLocal: true, isDefinition: true)
!2 = distinct !DISubprogram(name: "has_local_static", scope: !3, file: !3, line: 5, type: !4, scopeLine: 5, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !7, retainedNodes: !8)
!3 = !DIFile(filename: "localstatic.c", directory: "/home/kquick/work/DFAMS/tp241209/sources/llvm-pretty-bc-parser/disasm-test/tests")
!4 = !DISubroutineType(types: !5)
!5 = !{!6, !6}
!6 = !DIBasicType(name: "int", size: 32, encoding: DW_ATE_signed)
!7 = distinct !DICompileUnit(language: DW_LANG_C99, file: !3, producer: "clang version 11.1.0", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, enums: !8, globals: !9, splitDebugInlining: false, nameTableKind: None)
!8 = !{}
!9 = !{!0}
!10 = !DICompositeType(tag: DW_TAG_array_type, baseType: !11, size: 192, elements: !14)
!11 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !12)
!12 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !13, size: 64)
!13 = !DIDerivedType(tag: DW_TAG_const_type, baseType: null)
!14 = !{!15}
!15 = !DISubrange(count: 3)
!16 = !{i32 7, !"Dwarf Version", i32 4}
!17 = !{i32 2, !"Debug Info Version", i32 3}
!18 = !{i32 1, !"wchar_size", i32 4}
!19 = !{i32 7, !"PIC Level", i32 2}
!20 = !{!"clang version 11.1.0"}
!21 = !DILocalVariable(name: "x", arg: 1, scope: !2, file: !3, line: 5, type: !6)
!22 = !DILocation(line: 5, column: 26, scope: !2)
!23 = !DILocalVariable(name: "y", scope: !2, file: !3, line: 6, type: !6)
!24 = !DILocation(line: 6, column: 9, scope: !2)
!25 = !DILocation(line: 8, column: 9, scope: !2)
!26 = !DILocation(line: 8, column: 7, scope: !2)
!27 = !DILocation(line: 8, column: 5, scope: !2)
!28 = !DILabel(scope: !2, name: "start", file: !3, line: 9)
!29 = !DILocation(line: 9, column: 1, scope: !2)
!30 = !DILocation(line: 10, column: 19, scope: !2)
!31 = !DILocation(line: 10, column: 11, scope: !2)
!32 = !DILocation(line: 10, column: 5, scope: !2)
!33 = !DILabel(scope: !2, name: "fn1", file: !3, line: 11)
!34 = !DILocation(line: 11, column: 1, scope: !2)
!35 = !DILocation(line: 12, column: 7, scope: !2)
!36 = !DILocation(line: 13, column: 19, scope: !2)
!37 = !DILocation(line: 13, column: 11, scope: !2)
!38 = !DILocation(line: 13, column: 5, scope: !2)
!39 = !DILabel(scope: !2, name: "fn2", file: !3, line: 14)
!40 = !DILocation(line: 14, column: 1, scope: !2)
!41 = !DILocation(line: 15, column: 7, scope: !2)
!42 = !DILocation(line: 16, column: 5, scope: !2)
!43 = !DILabel(scope: !2, name: "fn3", file: !3, line: 17)
!44 = !DILocation(line: 17, column: 1, scope: !2)
!45 = !DILocation(line: 18, column: 12, scope: !2)
!46 = !DILocation(line: 18, column: 5, scope: !2)
!47 = distinct !DISubprogram(name: "main", scope: !3, file: !3, line: 21, type: !48, scopeLine: 21, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !7, retainedNodes: !8)
!48 = !DISubroutineType(types: !49)
!49 = !{!6, !6, !50}
!50 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !51, size: 64)
!51 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !52, size: 64)
!52 = !DIBasicType(name: "char", size: 8, encoding: DW_ATE_signed_char)
!53 = !DILocalVariable(name: "argc", arg: 1, scope: !47, file: !3, line: 21, type: !6)
!54 = !DILocation(line: 21, column: 14, scope: !47)
!55 = !DILocalVariable(name: "argv", arg: 2, scope: !47, file: !3, line: 21, type: !50)
!56 = !DILocation(line: 21, column: 27, scope: !47)
!57 = !DILocation(line: 22, column: 39, scope: !47)
!58 = !DILocation(line: 22, column: 22, scope: !47)
!59 = !DILocation(line: 22, column: 5, scope: !47)
!60 = !DILocation(line: 23, column: 1, scope: !47)
