; ModuleID = 'templates.bc'
source_filename = "templates.cpp"
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

; Function Attrs: nounwind readnone sspstrong uwtable
define i32 @nonzeroChar(i8 signext) local_unnamed_addr #0 !dbg !8 {
  call void @llvm.dbg.value(metadata i8 %0, metadata !14, metadata !DIExpression()), !dbg !15
  call void @llvm.dbg.value(metadata i8 %0, metadata !16, metadata !DIExpression()), !dbg !21
  %2 = icmp ne i8 %0, 0, !dbg !23
  %3 = zext i1 %2 to i32, !dbg !24
  ret i32 %3, !dbg !25
}

; Function Attrs: nounwind readnone sspstrong uwtable
define i32 @nonzeroInt(i32) local_unnamed_addr #0 !dbg !26 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !30, metadata !DIExpression()), !dbg !31
  call void @llvm.dbg.value(metadata i32 %0, metadata !32, metadata !DIExpression()), !dbg !37
  %2 = icmp ne i32 %0, 0, !dbg !39
  %3 = zext i1 %2 to i32, !dbg !40
  ret i32 %3, !dbg !41
}

; Function Attrs: nounwind readnone speculatable
declare void @llvm.dbg.value(metadata, metadata, metadata) #1

attributes #0 = { nounwind readnone sspstrong uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="4" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { nounwind readnone speculatable }

!llvm.dbg.cu = !{!0}
!llvm.module.flags = !{!3, !4, !5, !6}
!llvm.ident = !{!7}

!0 = distinct !DICompileUnit(language: DW_LANG_C_plus_plus, file: !1, producer: "clang version 6.0.1 (tags/RELEASE_601/final)", isOptimized: true, runtimeVersion: 0, emissionKind: FullDebug, enums: !2)
!1 = !DIFile(filename: "templates.cpp", directory: "/home/siddharthist/code/llvm-pretty-bc-parser/disasm-test/cpp")
!2 = !{}
!3 = !{i32 2, !"Dwarf Version", i32 4}
!4 = !{i32 2, !"Debug Info Version", i32 3}
!5 = !{i32 1, !"wchar_size", i32 4}
!6 = !{i32 7, !"PIC Level", i32 2}
!7 = !{!"clang version 6.0.1 (tags/RELEASE_601/final)"}
!8 = distinct !DISubprogram(name: "nonzeroChar", scope: !1, file: !1, line: 9, type: !9, isLocal: false, isDefinition: true, scopeLine: 9, flags: DIFlagPrototyped, isOptimized: true, unit: !0, variables: !13)
!9 = !DISubroutineType(types: !10)
!10 = !{!11, !12}
!11 = !DIBasicType(name: "int", size: 32, encoding: DW_ATE_signed)
!12 = !DIBasicType(name: "char", size: 8, encoding: DW_ATE_signed_char)
!13 = !{!14}
!14 = !DILocalVariable(name: "a", arg: 1, scope: !8, file: !1, line: 9, type: !12)
!15 = !DILocation(line: 9, column: 24, scope: !8)
!16 = !DILocalVariable(name: "a", arg: 1, scope: !17, file: !1, line: 4, type: !12)
!17 = distinct !DISubprogram(name: "nonzero<char>", linkageName: "_Z7nonzeroIcEiT_", scope: !1, file: !1, line: 4, type: !9, isLocal: false, isDefinition: true, scopeLine: 4, flags: DIFlagPrototyped, isOptimized: true, unit: !0, templateParams: !19, variables: !18)
!18 = !{!16}
!19 = !{!20}
!20 = !DITemplateTypeParameter(name: "T", type: !12)
!21 = !DILocation(line: 4, column: 15, scope: !17, inlinedAt: !22)
!22 = distinct !DILocation(line: 9, column: 36, scope: !8)
!23 = !DILocation(line: 5, column: 12, scope: !17, inlinedAt: !22)
!24 = !DILocation(line: 5, column: 10, scope: !17, inlinedAt: !22)
!25 = !DILocation(line: 9, column: 29, scope: !8)
!26 = distinct !DISubprogram(name: "nonzeroInt", scope: !1, file: !1, line: 10, type: !27, isLocal: false, isDefinition: true, scopeLine: 10, flags: DIFlagPrototyped, isOptimized: true, unit: !0, variables: !29)
!27 = !DISubroutineType(types: !28)
!28 = !{!11, !11}
!29 = !{!30}
!30 = !DILocalVariable(name: "a", arg: 1, scope: !26, file: !1, line: 10, type: !11)
!31 = !DILocation(line: 10, column: 22, scope: !26)
!32 = !DILocalVariable(name: "a", arg: 1, scope: !33, file: !1, line: 4, type: !11)
!33 = distinct !DISubprogram(name: "nonzero<int>", linkageName: "_Z7nonzeroIiEiT_", scope: !1, file: !1, line: 4, type: !27, isLocal: false, isDefinition: true, scopeLine: 4, flags: DIFlagPrototyped, isOptimized: true, unit: !0, templateParams: !35, variables: !34)
!34 = !{!32}
!35 = !{!36}
!36 = !DITemplateTypeParameter(name: "T", type: !11)
!37 = !DILocation(line: 4, column: 15, scope: !33, inlinedAt: !38)
!38 = distinct !DILocation(line: 10, column: 34, scope: !26)
!39 = !DILocation(line: 5, column: 12, scope: !33, inlinedAt: !38)
!40 = !DILocation(line: 5, column: 10, scope: !33, inlinedAt: !38)
!41 = !DILocation(line: 10, column: 27, scope: !26)
