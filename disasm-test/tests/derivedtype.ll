; ModuleID = 'derivedtype.bc'
source_filename = "derivedtype.c"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

%struct.message = type { i32, i8* }

; Function Attrs: noinline nounwind optnone sspstrong uwtable
define i32 @foo(%struct.message* %0) #0 !dbg !8 {
  %2 = alloca %struct.message*, align 8
  store %struct.message* %0, %struct.message** %2, align 8
  call void @llvm.dbg.declare(metadata %struct.message** %2, metadata !19, metadata !DIExpression()), !dbg !20
  %3 = load %struct.message*, %struct.message** %2, align 8, !dbg !21
  %4 = getelementptr inbounds %struct.message, %struct.message* %3, i32 0, i32 0, !dbg !22
  %5 = load i32, i32* %4, align 8, !dbg !22
  ret i32 %5, !dbg !23
}

; Function Attrs: nounwind readnone speculatable willreturn
declare void @llvm.dbg.declare(metadata, metadata, metadata) #1

attributes #0 = { noinline nounwind optnone sspstrong uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "frame-pointer"="all" "less-precise-fpmad"="false" "min-legal-vector-width"="0" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="4" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { nounwind readnone speculatable willreturn }

!llvm.dbg.cu = !{!0}
!llvm.module.flags = !{!3, !4, !5, !6}
!llvm.ident = !{!7}

!0 = distinct !DICompileUnit(language: DW_LANG_C99, file: !1, producer: "clang version 10.0.1 ", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, enums: !2, splitDebugInlining: false, nameTableKind: None)
!1 = !DIFile(filename: "derivedtype.c", directory: "/home/kquick/work/RISE/llvm-regr5/disasm-test/tests")
!2 = !{}
!3 = !{i32 7, !"Dwarf Version", i32 4}
!4 = !{i32 2, !"Debug Info Version", i32 3}
!5 = !{i32 1, !"wchar_size", i32 4}
!6 = !{i32 7, !"PIC Level", i32 2}
!7 = !{!"clang version 10.0.1 "}
!8 = distinct !DISubprogram(name: "foo", scope: !1, file: !1, line: 3, type: !9, scopeLine: 3, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !2)
!9 = !DISubroutineType(types: !10)
!10 = !{!11, !12}
!11 = !DIBasicType(name: "int", size: 32, encoding: DW_ATE_signed)
!12 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !13, size: 64)
!13 = distinct !DICompositeType(tag: DW_TAG_structure_type, name: "message", file: !1, line: 1, size: 128, elements: !14)
!14 = !{!15, !16}
!15 = !DIDerivedType(tag: DW_TAG_member, name: "msglen", scope: !13, file: !1, line: 1, baseType: !11, size: 32)
!16 = !DIDerivedType(tag: DW_TAG_member, name: "msgptr", scope: !13, file: !1, line: 1, baseType: !17, size: 64, offset: 64)
!17 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !18, size: 64)
!18 = !DIBasicType(name: "char", size: 8, encoding: DW_ATE_signed_char)
!19 = !DILocalVariable(name: "mptr", arg: 1, scope: !8, file: !1, line: 3, type: !12)
!20 = !DILocation(line: 3, column: 25, scope: !8)
!21 = !DILocation(line: 4, column: 12, scope: !8)
!22 = !DILocation(line: 4, column: 18, scope: !8)
!23 = !DILocation(line: 4, column: 5, scope: !8)
