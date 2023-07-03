; ModuleID = 'smallprog.c'
source_filename = "smallprog.c"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

%struct.message = type { i32, i32*, i32 }
%struct.broker = type { [5 x %struct.message], i32, i32 }

; Function Attrs: noinline nounwind optnone uwtable
define dso_local void @sp_receive(%struct.message* %0) #0 !dbg !8 {
  %2 = alloca %struct.message*, align 8
  %3 = alloca i32, align 4
  store %struct.message* %0, %struct.message** %2, align 8
  call void @llvm.dbg.declare(metadata %struct.message** %2, metadata !20, metadata !DIExpression()), !dbg !21
  %4 = load %struct.message*, %struct.message** %2, align 8, !dbg !22
  %5 = getelementptr inbounds %struct.message, %struct.message* %4, i32 0, i32 0, !dbg !23
  %6 = load i32, i32* %5, align 8, !dbg !23
  switch i32 %6, label %26 [
    i32 0, label %7
  ], !dbg !24

7:                                                ; preds = %1
  call void @llvm.dbg.declare(metadata i32* %3, metadata !25, metadata !DIExpression()), !dbg !28
  store i32 0, i32* %3, align 4, !dbg !28
  br label %8, !dbg !29

8:                                                ; preds = %22, %7
  %9 = load i32, i32* %3, align 4, !dbg !30
  %10 = load %struct.message*, %struct.message** %2, align 8, !dbg !32
  %11 = getelementptr inbounds %struct.message, %struct.message* %10, i32 0, i32 2, !dbg !33
  %12 = load i32, i32* %11, align 8, !dbg !33
  %13 = shl i32 %9, %12, !dbg !34
  %14 = icmp ne i32 %13, 0, !dbg !35
  br i1 %14, label %15, label %25, !dbg !35

15:                                               ; preds = %8
  %16 = load %struct.message*, %struct.message** %2, align 8, !dbg !36
  %17 = getelementptr inbounds %struct.message, %struct.message* %16, i32 0, i32 1, !dbg !38
  %18 = load i32*, i32** %17, align 8, !dbg !38
  %19 = load i32, i32* %3, align 4, !dbg !39
  %20 = sext i32 %19 to i64, !dbg !36
  %21 = getelementptr inbounds i32, i32* %18, i64 %20, !dbg !36
  store i32 0, i32* %21, align 4, !dbg !40
  br label %22, !dbg !41

22:                                               ; preds = %15
  %23 = load i32, i32* %3, align 4, !dbg !42
  %24 = add nsw i32 %23, 1, !dbg !42
  store i32 %24, i32* %3, align 4, !dbg !42
  br label %8, !dbg !43, !llvm.loop !44

25:                                               ; preds = %8
  br label %26, !dbg !47

26:                                               ; preds = %1, %25
  ret void, !dbg !48
}

; Function Attrs: nofree nosync nounwind readnone speculatable willreturn
declare void @llvm.dbg.declare(metadata, metadata, metadata) #1

; Function Attrs: noinline nounwind optnone uwtable
define dso_local void @broker_init(%struct.broker* %0) #0 !dbg !49 {
  %2 = alloca %struct.broker*, align 8
  store %struct.broker* %0, %struct.broker** %2, align 8
  call void @llvm.dbg.declare(metadata %struct.broker** %2, metadata !62, metadata !DIExpression()), !dbg !63
  %3 = load %struct.broker*, %struct.broker** %2, align 8, !dbg !64
  %4 = getelementptr inbounds %struct.broker, %struct.broker* %3, i32 0, i32 2, !dbg !65
  store i32 0, i32* %4, align 4, !dbg !66
  ret void, !dbg !67
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local void @broker_run(%struct.broker* %0) #0 !dbg !68 {
  %2 = alloca %struct.broker*, align 8
  %3 = alloca i32, align 4
  store %struct.broker* %0, %struct.broker** %2, align 8
  call void @llvm.dbg.declare(metadata %struct.broker** %2, metadata !69, metadata !DIExpression()), !dbg !70
  call void @llvm.dbg.declare(metadata i32* %3, metadata !71, metadata !DIExpression()), !dbg !73
  store i32 0, i32* %3, align 4, !dbg !73
  br label %4, !dbg !74

4:                                                ; preds = %16, %1
  %5 = load i32, i32* %3, align 4, !dbg !75
  %6 = load %struct.broker*, %struct.broker** %2, align 8, !dbg !77
  %7 = getelementptr inbounds %struct.broker, %struct.broker* %6, i32 0, i32 2, !dbg !78
  %8 = load i32, i32* %7, align 4, !dbg !78
  %9 = icmp slt i32 %5, %8, !dbg !79
  br i1 %9, label %10, label %19, !dbg !80

10:                                               ; preds = %4
  %11 = load %struct.broker*, %struct.broker** %2, align 8, !dbg !81
  %12 = getelementptr inbounds %struct.broker, %struct.broker* %11, i32 0, i32 0, !dbg !83
  %13 = load i32, i32* %3, align 4, !dbg !84
  %14 = sext i32 %13 to i64, !dbg !81
  %15 = getelementptr inbounds [5 x %struct.message], [5 x %struct.message]* %12, i64 0, i64 %14, !dbg !81
  call void @sp_receive(%struct.message* %15), !dbg !85
  br label %16, !dbg !86

16:                                               ; preds = %10
  %17 = load i32, i32* %3, align 4, !dbg !87
  %18 = add nsw i32 %17, 1, !dbg !87
  store i32 %18, i32* %3, align 4, !dbg !87
  br label %4, !dbg !88, !llvm.loop !89

19:                                               ; preds = %4
  ret void, !dbg !91
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i32 @main() #0 !dbg !92 {
  %1 = alloca %struct.broker, align 8
  call void @llvm.dbg.declare(metadata %struct.broker* %1, metadata !95, metadata !DIExpression()), !dbg !96
  call void @broker_init(%struct.broker* %1), !dbg !97
  call void @broker_run(%struct.broker* %1), !dbg !98
  ret i32 0, !dbg !99
}

attributes #0 = { noinline nounwind optnone uwtable "disable-tail-calls"="false" "frame-pointer"="all" "less-precise-fpmad"="false" "min-legal-vector-width"="0" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { nofree nosync nounwind readnone speculatable willreturn }

!llvm.dbg.cu = !{!0}
!llvm.module.flags = !{!3, !4, !5}
!llvm.ident = !{!6}
!llvm.commandline = !{!7}

!0 = distinct !DICompileUnit(language: DW_LANG_C99, file: !1, producer: "clang version 12.0.1", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, enums: !2, splitDebugInlining: false, nameTableKind: None)
!1 = !DIFile(filename: "smallprog.c", directory: "/home/rscott/Documents/Hacking/Haskell/llvm-pretty-bc-parser/disasm-test/tests")
!2 = !{}
!3 = !{i32 7, !"Dwarf Version", i32 4}
!4 = !{i32 2, !"Debug Info Version", i32 3}
!5 = !{i32 1, !"wchar_size", i32 4}
!6 = !{!"clang version 12.0.1"}
!7 = !{!"/home/rscott/Software/clang+llvm-12.0.1/bin/clang-12 -emit-llvm -g -S -frecord-command-line smallprog.c -o smallprog.ll"}
!8 = distinct !DISubprogram(name: "sp_receive", scope: !1, file: !1, line: 17, type: !9, scopeLine: 17, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !2)
!9 = !DISubroutineType(types: !10)
!10 = !{null, !11}
!11 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !12, size: 64)
!12 = !DIDerivedType(tag: DW_TAG_typedef, name: "message_t", file: !1, line: 9, baseType: !13)
!13 = distinct !DICompositeType(tag: DW_TAG_structure_type, name: "message", file: !1, line: 5, size: 192, elements: !14)
!14 = !{!15, !17, !19}
!15 = !DIDerivedType(tag: DW_TAG_member, name: "type", scope: !13, file: !1, line: 6, baseType: !16, size: 32)
!16 = !DIBasicType(name: "int", size: 32, encoding: DW_ATE_signed)
!17 = !DIDerivedType(tag: DW_TAG_member, name: "payload", scope: !13, file: !1, line: 7, baseType: !18, size: 64, offset: 64)
!18 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !16, size: 64)
!19 = !DIDerivedType(tag: DW_TAG_member, name: "payload_len", scope: !13, file: !1, line: 8, baseType: !16, size: 32, offset: 128)
!20 = !DILocalVariable(name: "msg", arg: 1, scope: !8, file: !1, line: 17, type: !11)
!21 = !DILocation(line: 17, column: 28, scope: !8)
!22 = !DILocation(line: 18, column: 13, scope: !8)
!23 = !DILocation(line: 18, column: 18, scope: !8)
!24 = !DILocation(line: 18, column: 5, scope: !8)
!25 = !DILocalVariable(name: "i", scope: !26, file: !1, line: 20, type: !16)
!26 = distinct !DILexicalBlock(scope: !27, file: !1, line: 20, column: 9)
!27 = distinct !DILexicalBlock(scope: !8, file: !1, line: 18, column: 24)
!28 = !DILocation(line: 20, column: 18, scope: !26)
!29 = !DILocation(line: 20, column: 14, scope: !26)
!30 = !DILocation(line: 20, column: 25, scope: !31)
!31 = distinct !DILexicalBlock(scope: !26, file: !1, line: 20, column: 9)
!32 = !DILocation(line: 20, column: 30, scope: !31)
!33 = !DILocation(line: 20, column: 35, scope: !31)
!34 = !DILocation(line: 20, column: 27, scope: !31)
!35 = !DILocation(line: 20, column: 9, scope: !26)
!36 = !DILocation(line: 21, column: 13, scope: !37)
!37 = distinct !DILexicalBlock(scope: !31, file: !1, line: 20, column: 53)
!38 = !DILocation(line: 21, column: 18, scope: !37)
!39 = !DILocation(line: 21, column: 26, scope: !37)
!40 = !DILocation(line: 21, column: 29, scope: !37)
!41 = !DILocation(line: 22, column: 9, scope: !37)
!42 = !DILocation(line: 20, column: 48, scope: !31)
!43 = !DILocation(line: 20, column: 9, scope: !31)
!44 = distinct !{!44, !35, !45, !46}
!45 = !DILocation(line: 22, column: 9, scope: !26)
!46 = !{!"llvm.loop.mustprogress"}
!47 = !DILocation(line: 23, column: 9, scope: !27)
!48 = !DILocation(line: 25, column: 1, scope: !8)
!49 = distinct !DISubprogram(name: "broker_init", scope: !1, file: !1, line: 27, type: !50, scopeLine: 27, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !2)
!50 = !DISubroutineType(types: !51)
!51 = !{null, !52}
!52 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !53, size: 64)
!53 = !DIDerivedType(tag: DW_TAG_typedef, name: "broker_t", file: !1, line: 15, baseType: !54)
!54 = distinct !DICompositeType(tag: DW_TAG_structure_type, name: "broker", file: !1, line: 11, size: 1024, elements: !55)
!55 = !{!56, !60, !61}
!56 = !DIDerivedType(tag: DW_TAG_member, name: "msgs", scope: !54, file: !1, line: 12, baseType: !57, size: 960)
!57 = !DICompositeType(tag: DW_TAG_array_type, baseType: !12, size: 960, elements: !58)
!58 = !{!59}
!59 = !DISubrange(count: 5)
!60 = !DIDerivedType(tag: DW_TAG_member, name: "msg_idx", scope: !54, file: !1, line: 13, baseType: !16, size: 32, offset: 960)
!61 = !DIDerivedType(tag: DW_TAG_member, name: "num_msgs", scope: !54, file: !1, line: 14, baseType: !16, size: 32, offset: 992)
!62 = !DILocalVariable(name: "broker", arg: 1, scope: !49, file: !1, line: 27, type: !52)
!63 = !DILocation(line: 27, column: 28, scope: !49)
!64 = !DILocation(line: 28, column: 5, scope: !49)
!65 = !DILocation(line: 28, column: 13, scope: !49)
!66 = !DILocation(line: 28, column: 22, scope: !49)
!67 = !DILocation(line: 29, column: 1, scope: !49)
!68 = distinct !DISubprogram(name: "broker_run", scope: !1, file: !1, line: 31, type: !50, scopeLine: 31, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !2)
!69 = !DILocalVariable(name: "broker", arg: 1, scope: !68, file: !1, line: 31, type: !52)
!70 = !DILocation(line: 31, column: 27, scope: !68)
!71 = !DILocalVariable(name: "i", scope: !72, file: !1, line: 32, type: !16)
!72 = distinct !DILexicalBlock(scope: !68, file: !1, line: 32, column: 5)
!73 = !DILocation(line: 32, column: 14, scope: !72)
!74 = !DILocation(line: 32, column: 10, scope: !72)
!75 = !DILocation(line: 32, column: 21, scope: !76)
!76 = distinct !DILexicalBlock(scope: !72, file: !1, line: 32, column: 5)
!77 = !DILocation(line: 32, column: 25, scope: !76)
!78 = !DILocation(line: 32, column: 33, scope: !76)
!79 = !DILocation(line: 32, column: 23, scope: !76)
!80 = !DILocation(line: 32, column: 5, scope: !72)
!81 = !DILocation(line: 33, column: 22, scope: !82)
!82 = distinct !DILexicalBlock(scope: !76, file: !1, line: 32, column: 48)
!83 = !DILocation(line: 33, column: 30, scope: !82)
!84 = !DILocation(line: 33, column: 35, scope: !82)
!85 = !DILocation(line: 33, column: 9, scope: !82)
!86 = !DILocation(line: 34, column: 5, scope: !82)
!87 = !DILocation(line: 32, column: 43, scope: !76)
!88 = !DILocation(line: 32, column: 5, scope: !76)
!89 = distinct !{!89, !80, !90, !46}
!90 = !DILocation(line: 34, column: 5, scope: !72)
!91 = !DILocation(line: 35, column: 1, scope: !68)
!92 = distinct !DISubprogram(name: "main", scope: !1, file: !1, line: 37, type: !93, scopeLine: 37, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !2)
!93 = !DISubroutineType(types: !94)
!94 = !{!16}
!95 = !DILocalVariable(name: "broker", scope: !92, file: !1, line: 38, type: !53)
!96 = !DILocation(line: 38, column: 14, scope: !92)
!97 = !DILocation(line: 39, column: 5, scope: !92)
!98 = !DILocation(line: 40, column: 5, scope: !92)
!99 = !DILocation(line: 41, column: 1, scope: !92)
