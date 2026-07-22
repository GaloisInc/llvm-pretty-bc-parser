; ModuleID = 'disasm-test/tests/varargs.c'
source_filename = "disasm-test/tests/varargs.c"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

%struct.__va_list_tag = type { i32, i32, ptr, ptr }

@.str = private unnamed_addr constant [4 x i8] c"%f\0A\00", align 1, !dbg !0

; Function Attrs: noinline nounwind optnone uwtable
define dso_local double @average(i32 noundef %0, ...) #0 !dbg !17 {
  %2 = alloca i32, align 4
  %3 = alloca [1 x %struct.__va_list_tag], align 16
  %4 = alloca double, align 8
  %5 = alloca i32, align 4
  store i32 %0, ptr %2, align 4
    #dbg_declare(ptr %2, !23, !DIExpression(), !24)
    #dbg_declare(ptr %3, !25, !DIExpression(), !42)
    #dbg_declare(ptr %4, !43, !DIExpression(), !44)
  store double 0.000000e+00, ptr %4, align 8, !dbg !44
  %6 = getelementptr inbounds [1 x %struct.__va_list_tag], ptr %3, i64 0, i64 0, !dbg !45
  call void @llvm.va_start.p0(ptr %6), !dbg !45
    #dbg_declare(ptr %5, !46, !DIExpression(), !48)
  store i32 0, ptr %5, align 4, !dbg !48
  br label %7, !dbg !49

7:                                                ; preds = %31, %1
  %8 = load i32, ptr %5, align 4, !dbg !50
  %9 = load i32, ptr %2, align 4, !dbg !52
  %10 = icmp slt i32 %8, %9, !dbg !53
  br i1 %10, label %11, label %34, !dbg !54

11:                                               ; preds = %7
  %12 = getelementptr inbounds [1 x %struct.__va_list_tag], ptr %3, i64 0, i64 0, !dbg !55
  %13 = getelementptr inbounds nuw %struct.__va_list_tag, ptr %12, i32 0, i32 0, !dbg !55
  %14 = load i32, ptr %13, align 16, !dbg !55
  %15 = icmp ule i32 %14, 40, !dbg !55
  br i1 %15, label %16, label %21, !dbg !55

16:                                               ; preds = %11
  %17 = getelementptr inbounds nuw %struct.__va_list_tag, ptr %12, i32 0, i32 3, !dbg !55
  %18 = load ptr, ptr %17, align 16, !dbg !55
  %19 = getelementptr i8, ptr %18, i32 %14, !dbg !55
  %20 = add i32 %14, 8, !dbg !55
  store i32 %20, ptr %13, align 16, !dbg !55
  br label %25, !dbg !55

21:                                               ; preds = %11
  %22 = getelementptr inbounds nuw %struct.__va_list_tag, ptr %12, i32 0, i32 2, !dbg !55
  %23 = load ptr, ptr %22, align 8, !dbg !55
  %24 = getelementptr i8, ptr %23, i32 8, !dbg !55
  store ptr %24, ptr %22, align 8, !dbg !55
  br label %25, !dbg !55

25:                                               ; preds = %21, %16
  %26 = phi ptr [ %19, %16 ], [ %23, %21 ], !dbg !55
  %27 = load i32, ptr %26, align 4, !dbg !55
  %28 = sitofp i32 %27 to double, !dbg !55
  %29 = load double, ptr %4, align 8, !dbg !57
  %30 = fadd double %29, %28, !dbg !57
  store double %30, ptr %4, align 8, !dbg !57
  br label %31, !dbg !58

31:                                               ; preds = %25
  %32 = load i32, ptr %5, align 4, !dbg !59
  %33 = add nsw i32 %32, 1, !dbg !59
  store i32 %33, ptr %5, align 4, !dbg !59
  br label %7, !dbg !60, !llvm.loop !61

34:                                               ; preds = %7
  %35 = getelementptr inbounds [1 x %struct.__va_list_tag], ptr %3, i64 0, i64 0, !dbg !64
  call void @llvm.va_end.p0(ptr %35), !dbg !64
  %36 = load double, ptr %4, align 8, !dbg !65
  %37 = load i32, ptr %2, align 4, !dbg !66
  %38 = sitofp i32 %37 to double, !dbg !66
  %39 = fdiv double %36, %38, !dbg !67
  ret double %39, !dbg !68
}

; Function Attrs: nocallback nofree nosync nounwind willreturn
declare void @llvm.va_start.p0(ptr) #1

; Function Attrs: nocallback nofree nosync nounwind willreturn
declare void @llvm.va_end.p0(ptr) #1

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i32 @main(i32 noundef %0, ptr noundef %1) #0 !dbg !69 {
  %3 = alloca i32, align 4
  %4 = alloca i32, align 4
  %5 = alloca ptr, align 8
  store i32 0, ptr %3, align 4
  store i32 %0, ptr %4, align 4
    #dbg_declare(ptr %4, !74, !DIExpression(), !75)
  store ptr %1, ptr %5, align 8
    #dbg_declare(ptr %5, !76, !DIExpression(), !77)
  %6 = call double (i32, ...) @average(i32 noundef 3, i32 noundef 1, i32 noundef 2, i32 noundef 3), !dbg !78
  %7 = call i32 (ptr, ...) @printf(ptr noundef @.str, double noundef %6), !dbg !79
  ret i32 0, !dbg !80
}

declare i32 @printf(ptr noundef, ...) #2

attributes #0 = { noinline nounwind optnone uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #1 = { nocallback nofree nosync nounwind willreturn }
attributes #2 = { "frame-pointer"="all" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }

!llvm.dbg.cu = !{!7}
!llvm.module.flags = !{!9, !10, !11, !12, !13, !14, !15}
!llvm.ident = !{!16}

!0 = !DIGlobalVariableExpression(var: !1, expr: !DIExpression())
!1 = distinct !DIGlobalVariable(scope: null, file: !2, line: 18, type: !3, isLocal: true, isDefinition: true)
!2 = !DIFile(filename: "disasm-test/tests/varargs.c", directory: "/home/kquick/work/saw-script-ada/deps/llvm-pretty-bc-parser", checksumkind: CSK_MD5, checksum: "15e5adfcff59b19f53a0a7387552fa1c")
!3 = !DICompositeType(tag: DW_TAG_array_type, baseType: !4, size: 32, elements: !5)
!4 = !DIBasicType(name: "char", size: 8, encoding: DW_ATE_signed_char)
!5 = !{!6}
!6 = !DISubrange(count: 4)
!7 = distinct !DICompileUnit(language: DW_LANG_C11, file: !2, producer: "clang version 22.1.2", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, globals: !8, splitDebugInlining: false, nameTableKind: None)
!8 = !{!0}
!9 = !{i32 7, !"Dwarf Version", i32 5}
!10 = !{i32 2, !"Debug Info Version", i32 3}
!11 = !{i32 1, !"wchar_size", i32 4}
!12 = !{i32 8, !"PIC Level", i32 2}
!13 = !{i32 7, !"PIE Level", i32 2}
!14 = !{i32 7, !"uwtable", i32 2}
!15 = !{i32 7, !"frame-pointer", i32 2}
!16 = !{!"clang version 22.1.2"}
!17 = distinct !DISubprogram(name: "average", scope: !2, file: !2, line: 4, type: !18, scopeLine: 4, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !7, retainedNodes: !22)
!18 = !DISubroutineType(types: !19)
!19 = !{!20, !21, null}
!20 = !DIBasicType(name: "double", size: 64, encoding: DW_ATE_float)
!21 = !DIBasicType(name: "int", size: 32, encoding: DW_ATE_signed)
!22 = !{}
!23 = !DILocalVariable(name: "count", arg: 1, scope: !17, file: !2, line: 4, type: !21)
!24 = !DILocation(line: 4, column: 20, scope: !17)
!25 = !DILocalVariable(name: "ap", scope: !17, file: !2, line: 5, type: !26)
!26 = !DIDerivedType(tag: DW_TAG_typedef, name: "va_list", file: !27, line: 53, baseType: !28)
!27 = !DIFile(filename: "/nix/store/h0ip0h6qp7kc2wm7mwjaglkxxbzmjri4-glibc-2.42-51-dev/include/stdio.h", directory: "", checksumkind: CSK_MD5, checksum: "c14d5295568128a31f260455322e331f")
!28 = !DIDerivedType(tag: DW_TAG_typedef, name: "__gnuc_va_list", file: !29, line: 12, baseType: !30)
!29 = !DIFile(filename: "/nix/store/vdn3b0f2hg108axhzsk9sw69ljlmhzsm-clang-wrapper-22.1.2/resource-root/include/__stdarg___gnuc_va_list.h", directory: "", checksumkind: CSK_MD5, checksum: "edb3f2eab991638e4dc94f6e55e3530f")
!30 = !DIDerivedType(tag: DW_TAG_typedef, name: "__builtin_va_list", file: !2, baseType: !31)
!31 = !DICompositeType(tag: DW_TAG_array_type, baseType: !32, size: 192, elements: !40)
!32 = distinct !DICompositeType(tag: DW_TAG_structure_type, name: "__va_list_tag", size: 192, elements: !33)
!33 = !{!34, !36, !37, !39}
!34 = !DIDerivedType(tag: DW_TAG_member, name: "gp_offset", scope: !32, file: !2, line: 5, baseType: !35, size: 32)
!35 = !DIBasicType(name: "unsigned int", size: 32, encoding: DW_ATE_unsigned)
!36 = !DIDerivedType(tag: DW_TAG_member, name: "fp_offset", scope: !32, file: !2, line: 5, baseType: !35, size: 32, offset: 32)
!37 = !DIDerivedType(tag: DW_TAG_member, name: "overflow_arg_area", scope: !32, file: !2, line: 5, baseType: !38, size: 64, offset: 64)
!38 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: null, size: 64)
!39 = !DIDerivedType(tag: DW_TAG_member, name: "reg_save_area", scope: !32, file: !2, line: 5, baseType: !38, size: 64, offset: 128)
!40 = !{!41}
!41 = !DISubrange(count: 1)
!42 = !DILocation(line: 5, column: 13, scope: !17)
!43 = !DILocalVariable(name: "sum", scope: !17, file: !2, line: 6, type: !20)
!44 = !DILocation(line: 6, column: 12, scope: !17)
!45 = !DILocation(line: 8, column: 5, scope: !17)
!46 = !DILocalVariable(name: "j", scope: !47, file: !2, line: 9, type: !21)
!47 = distinct !DILexicalBlock(scope: !17, file: !2, line: 9, column: 5)
!48 = !DILocation(line: 9, column: 14, scope: !47)
!49 = !DILocation(line: 9, column: 10, scope: !47)
!50 = !DILocation(line: 9, column: 21, scope: !51)
!51 = distinct !DILexicalBlock(scope: !47, file: !2, line: 9, column: 5)
!52 = !DILocation(line: 9, column: 25, scope: !51)
!53 = !DILocation(line: 9, column: 23, scope: !51)
!54 = !DILocation(line: 9, column: 5, scope: !47)
!55 = !DILocation(line: 10, column: 16, scope: !56)
!56 = distinct !DILexicalBlock(scope: !51, file: !2, line: 9, column: 37)
!57 = !DILocation(line: 10, column: 13, scope: !56)
!58 = !DILocation(line: 11, column: 5, scope: !56)
!59 = !DILocation(line: 9, column: 32, scope: !51)
!60 = !DILocation(line: 9, column: 5, scope: !51)
!61 = distinct !{!61, !54, !62, !63}
!62 = !DILocation(line: 11, column: 5, scope: !47)
!63 = !{!"llvm.loop.mustprogress"}
!64 = !DILocation(line: 12, column: 5, scope: !17)
!65 = !DILocation(line: 14, column: 12, scope: !17)
!66 = !DILocation(line: 14, column: 18, scope: !17)
!67 = !DILocation(line: 14, column: 16, scope: !17)
!68 = !DILocation(line: 14, column: 5, scope: !17)
!69 = distinct !DISubprogram(name: "main", scope: !2, file: !2, line: 17, type: !70, scopeLine: 17, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !7, retainedNodes: !22)
!70 = !DISubroutineType(types: !71)
!71 = !{!21, !21, !72}
!72 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !73, size: 64)
!73 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !4, size: 64)
!74 = !DILocalVariable(name: "argc", arg: 1, scope: !69, file: !2, line: 17, type: !21)
!75 = !DILocation(line: 17, column: 14, scope: !69)
!76 = !DILocalVariable(name: "argv", arg: 2, scope: !69, file: !2, line: 17, type: !72)
!77 = !DILocation(line: 17, column: 26, scope: !69)
!78 = !DILocation(line: 18, column: 20, scope: !69)
!79 = !DILocation(line: 18, column: 5, scope: !69)
!80 = !DILocation(line: 19, column: 5, scope: !69)
