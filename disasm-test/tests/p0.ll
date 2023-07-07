; ModuleID = 'p0.bc'
source_filename = "disasm-test/tests/p0.c"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

%struct.broker = type { [10 x %struct.message], i8, i8 }
%struct.message = type { i16, i8*, i8 }

@broker = external global %struct.broker, align 8
@signal = external global [200 x i8], align 16
@__const.main.msg = private unnamed_addr constant %struct.message { i16 0, i8* getelementptr inbounds ([200 x i8], [200 x i8]* @signal, i32 0, i32 0), i8 -56 }, align 8

; Function Attrs: noinline nounwind optnone sspstrong uwtable
define void @broker_init(%struct.broker* %0) #0 !dbg !21 {
  %2 = alloca %struct.broker*, align 8
  store %struct.broker* %0, %struct.broker** %2, align 8
  call void @llvm.dbg.declare(metadata %struct.broker** %2, metadata !44, metadata !DIExpression()), !dbg !45
  %3 = load %struct.broker*, %struct.broker** %2, align 8, !dbg !46
  %4 = getelementptr inbounds %struct.broker, %struct.broker* %3, i32 0, i32 2, !dbg !47
  store i8 0, i8* %4, align 1, !dbg !48
  ret void, !dbg !49
}

; Function Attrs: nounwind readnone speculatable willreturn
declare void @llvm.dbg.declare(metadata, metadata, metadata) #1

; Function Attrs: noinline nounwind optnone sspstrong uwtable
define void @broker_run(%struct.broker* %0) #0 !dbg !50 {
  %2 = alloca %struct.broker*, align 8
  %3 = alloca i8, align 1
  store %struct.broker* %0, %struct.broker** %2, align 8
  call void @llvm.dbg.declare(metadata %struct.broker** %2, metadata !51, metadata !DIExpression()), !dbg !52
  call void @llvm.dbg.declare(metadata i8* %3, metadata !53, metadata !DIExpression()), !dbg !55
  store i8 0, i8* %3, align 1, !dbg !55
  br label %4, !dbg !56

4:                                                ; preds = %18, %1
  %5 = load i8, i8* %3, align 1, !dbg !57
  %6 = zext i8 %5 to i32, !dbg !57
  %7 = load %struct.broker*, %struct.broker** %2, align 8, !dbg !59
  %8 = getelementptr inbounds %struct.broker, %struct.broker* %7, i32 0, i32 2, !dbg !60
  %9 = load i8, i8* %8, align 1, !dbg !60
  %10 = zext i8 %9 to i32, !dbg !59
  %11 = icmp slt i32 %6, %10, !dbg !61
  br i1 %11, label %12, label %21, !dbg !62

12:                                               ; preds = %4
  %13 = load %struct.broker*, %struct.broker** %2, align 8, !dbg !63
  %14 = getelementptr inbounds %struct.broker, %struct.broker* %13, i32 0, i32 0, !dbg !65
  %15 = load i8, i8* %3, align 1, !dbg !66
  %16 = zext i8 %15 to i64, !dbg !63
  %17 = getelementptr [10 x %struct.message], [10 x %struct.message]* %14, i64 0, i64 %16, !dbg !63
  call void @sp_receive(%struct.message* %17), !dbg !67
  br label %18, !dbg !68

18:                                               ; preds = %12
  %19 = load i8, i8* %3, align 1, !dbg !69
  %20 = add i8 %19, 1, !dbg !69
  store i8 %20, i8* %3, align 1, !dbg !69
  br label %4, !dbg !70, !llvm.loop !71

21:                                               ; preds = %4
  ret void, !dbg !73
}

; Function Attrs: noinline nounwind optnone sspstrong uwtable
define void @sp_receive(%struct.message* %0) #0 !dbg !74 {
  %2 = alloca %struct.message*, align 8
  store %struct.message* %0, %struct.message** %2, align 8
  call void @llvm.dbg.declare(metadata %struct.message** %2, metadata !78, metadata !DIExpression()), !dbg !79
  %3 = load %struct.message*, %struct.message** %2, align 8, !dbg !80
  %4 = getelementptr inbounds %struct.message, %struct.message* %3, i32 0, i32 0, !dbg !81
  %5 = load i16, i16* %4, align 8, !dbg !81
  %6 = zext i16 %5 to i32, !dbg !80
  switch i32 %6, label %28 [
    i32 0, label %7
    i32 1, label %14
    i32 2, label %21
  ], !dbg !82

7:                                                ; preds = %1
  %8 = load %struct.message*, %struct.message** %2, align 8, !dbg !83
  %9 = getelementptr inbounds %struct.message, %struct.message* %8, i32 0, i32 1, !dbg !85
  %10 = load i8*, i8** %9, align 8, !dbg !85
  %11 = load %struct.message*, %struct.message** %2, align 8, !dbg !86
  %12 = getelementptr inbounds %struct.message, %struct.message* %11, i32 0, i32 2, !dbg !87
  %13 = load i8, i8* %12, align 8, !dbg !87
  call void @sp_clear(i8* %10, i8 zeroext %13), !dbg !88
  br label %29, !dbg !89

14:                                               ; preds = %1
  %15 = load %struct.message*, %struct.message** %2, align 8, !dbg !90
  %16 = getelementptr inbounds %struct.message, %struct.message* %15, i32 0, i32 1, !dbg !91
  %17 = load i8*, i8** %16, align 8, !dbg !91
  %18 = load %struct.message*, %struct.message** %2, align 8, !dbg !92
  %19 = getelementptr inbounds %struct.message, %struct.message* %18, i32 0, i32 2, !dbg !93
  %20 = load i8, i8* %19, align 8, !dbg !93
  call void @sp_add_five(i8* %17, i8 zeroext %20), !dbg !94
  br label %29, !dbg !95

21:                                               ; preds = %1
  %22 = load %struct.message*, %struct.message** %2, align 8, !dbg !96
  %23 = getelementptr inbounds %struct.message, %struct.message* %22, i32 0, i32 1, !dbg !97
  %24 = load i8*, i8** %23, align 8, !dbg !97
  %25 = load %struct.message*, %struct.message** %2, align 8, !dbg !98
  %26 = getelementptr inbounds %struct.message, %struct.message* %25, i32 0, i32 2, !dbg !99
  %27 = load i8, i8* %26, align 8, !dbg !99
  call void @sp_double(i8* %24, i8 zeroext %27), !dbg !100
  br label %29, !dbg !101

28:                                               ; preds = %1
  br label %29, !dbg !102

29:                                               ; preds = %28, %21, %14, %7
  ret void, !dbg !103
}

; Function Attrs: noinline nounwind optnone sspstrong uwtable
define void @broker_publish(%struct.broker* %0, %struct.message* %1) #0 !dbg !104 {
  %3 = alloca %struct.broker*, align 8
  %4 = alloca %struct.message*, align 8
  store %struct.broker* %0, %struct.broker** %3, align 8
  call void @llvm.dbg.declare(metadata %struct.broker** %3, metadata !107, metadata !DIExpression()), !dbg !108
  store %struct.message* %1, %struct.message** %4, align 8
  call void @llvm.dbg.declare(metadata %struct.message** %4, metadata !109, metadata !DIExpression()), !dbg !110
  %5 = load %struct.broker*, %struct.broker** %3, align 8, !dbg !111
  %6 = getelementptr inbounds %struct.broker, %struct.broker* %5, i32 0, i32 0, !dbg !112
  %7 = load %struct.broker*, %struct.broker** %3, align 8, !dbg !113
  %8 = getelementptr inbounds %struct.broker, %struct.broker* %7, i32 0, i32 2, !dbg !114
  %9 = load i8, i8* %8, align 1, !dbg !115
  %10 = add i8 %9, 1, !dbg !115
  store i8 %10, i8* %8, align 1, !dbg !115
  %11 = zext i8 %9 to i64, !dbg !111
  %12 = getelementptr [10 x %struct.message], [10 x %struct.message]* %6, i64 0, i64 %11, !dbg !111
  %13 = load %struct.message*, %struct.message** %4, align 8, !dbg !116
  %14 = bitcast %struct.message* %12 to i8*, !dbg !117
  %15 = bitcast %struct.message* %13 to i8*, !dbg !117
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %14, i8* align 8 %15, i64 24, i1 false), !dbg !117
  ret void, !dbg !118
}

; Function Attrs: argmemonly nounwind willreturn
declare void @llvm.memcpy.p0i8.p0i8.i64(i8* noalias nocapture writeonly, i8* noalias nocapture readonly, i64, i1 immarg) #2

; Function Attrs: noinline nounwind optnone sspstrong uwtable
define void @sp_clear(i8* %0, i8 zeroext %1) #0 !dbg !119 {
  %3 = alloca i8*, align 8
  %4 = alloca i8, align 1
  %5 = alloca i8, align 1
  store i8* %0, i8** %3, align 8
  call void @llvm.dbg.declare(metadata i8** %3, metadata !122, metadata !DIExpression()), !dbg !123
  store i8 %1, i8* %4, align 1
  call void @llvm.dbg.declare(metadata i8* %4, metadata !124, metadata !DIExpression()), !dbg !125
  call void @llvm.dbg.declare(metadata i8* %5, metadata !126, metadata !DIExpression()), !dbg !128
  store i8 0, i8* %5, align 1, !dbg !128
  br label %6, !dbg !129

6:                                                ; preds = %17, %2
  %7 = load i8, i8* %5, align 1, !dbg !130
  %8 = zext i8 %7 to i32, !dbg !130
  %9 = load i8, i8* %4, align 1, !dbg !132
  %10 = zext i8 %9 to i32, !dbg !132
  %11 = icmp slt i32 %8, %10, !dbg !133
  br i1 %11, label %12, label %20, !dbg !134

12:                                               ; preds = %6
  %13 = load i8*, i8** %3, align 8, !dbg !135
  %14 = load i8, i8* %5, align 1, !dbg !137
  %15 = zext i8 %14 to i64, !dbg !135
  %16 = getelementptr i8, i8* %13, i64 %15, !dbg !135
  store i8 0, i8* %16, align 1, !dbg !138
  br label %17, !dbg !139

17:                                               ; preds = %12
  %18 = load i8, i8* %5, align 1, !dbg !140
  %19 = add i8 %18, 1, !dbg !140
  store i8 %19, i8* %5, align 1, !dbg !140
  br label %6, !dbg !141, !llvm.loop !142

20:                                               ; preds = %6
  ret void, !dbg !144
}

; Function Attrs: noinline nounwind optnone sspstrong uwtable
define void @sp_double(i8* %0, i8 zeroext %1) #0 !dbg !145 {
  %3 = alloca i8*, align 8
  %4 = alloca i8, align 1
  %5 = alloca i8, align 1
  store i8* %0, i8** %3, align 8
  call void @llvm.dbg.declare(metadata i8** %3, metadata !146, metadata !DIExpression()), !dbg !147
  store i8 %1, i8* %4, align 1
  call void @llvm.dbg.declare(metadata i8* %4, metadata !148, metadata !DIExpression()), !dbg !149
  call void @llvm.dbg.declare(metadata i8* %5, metadata !150, metadata !DIExpression()), !dbg !152
  store i8 0, i8* %5, align 1, !dbg !152
  br label %6, !dbg !153

6:                                                ; preds = %21, %2
  %7 = load i8, i8* %5, align 1, !dbg !154
  %8 = zext i8 %7 to i32, !dbg !154
  %9 = load i8, i8* %4, align 1, !dbg !156
  %10 = zext i8 %9 to i32, !dbg !156
  %11 = icmp slt i32 %8, %10, !dbg !157
  br i1 %11, label %12, label %24, !dbg !158

12:                                               ; preds = %6
  %13 = load i8*, i8** %3, align 8, !dbg !159
  %14 = load i8, i8* %5, align 1, !dbg !161
  %15 = zext i8 %14 to i64, !dbg !159
  %16 = getelementptr i8, i8* %13, i64 %15, !dbg !159
  %17 = load i8, i8* %16, align 1, !dbg !162
  %18 = zext i8 %17 to i32, !dbg !162
  %19 = mul i32 %18, 2, !dbg !162
  %20 = trunc i32 %19 to i8, !dbg !162
  store i8 %20, i8* %16, align 1, !dbg !162
  br label %21, !dbg !163

21:                                               ; preds = %12
  %22 = load i8, i8* %5, align 1, !dbg !164
  %23 = add i8 %22, 1, !dbg !164
  store i8 %23, i8* %5, align 1, !dbg !164
  br label %6, !dbg !165, !llvm.loop !166

24:                                               ; preds = %6
  ret void, !dbg !168
}

; Function Attrs: noinline nounwind optnone sspstrong uwtable
define void @sp_add_five(i8* %0, i8 zeroext %1) #0 !dbg !169 {
  %3 = alloca i8*, align 8
  %4 = alloca i8, align 1
  %5 = alloca i8, align 1
  store i8* %0, i8** %3, align 8
  call void @llvm.dbg.declare(metadata i8** %3, metadata !170, metadata !DIExpression()), !dbg !171
  store i8 %1, i8* %4, align 1
  call void @llvm.dbg.declare(metadata i8* %4, metadata !172, metadata !DIExpression()), !dbg !173
  call void @llvm.dbg.declare(metadata i8* %5, metadata !174, metadata !DIExpression()), !dbg !176
  store i8 0, i8* %5, align 1, !dbg !176
  br label %6, !dbg !177

6:                                                ; preds = %21, %2
  %7 = load i8, i8* %5, align 1, !dbg !178
  %8 = zext i8 %7 to i32, !dbg !178
  %9 = load i8, i8* %4, align 1, !dbg !180
  %10 = zext i8 %9 to i32, !dbg !180
  %11 = icmp slt i32 %8, %10, !dbg !181
  br i1 %11, label %12, label %24, !dbg !182

12:                                               ; preds = %6
  %13 = load i8*, i8** %3, align 8, !dbg !183
  %14 = load i8, i8* %5, align 1, !dbg !185
  %15 = zext i8 %14 to i64, !dbg !183
  %16 = getelementptr i8, i8* %13, i64 %15, !dbg !183
  %17 = load i8, i8* %16, align 1, !dbg !186
  %18 = zext i8 %17 to i32, !dbg !186
  %19 = add i32 %18, 5, !dbg !186
  %20 = trunc i32 %19 to i8, !dbg !186
  store i8 %20, i8* %16, align 1, !dbg !186
  br label %21, !dbg !187

21:                                               ; preds = %12
  %22 = load i8, i8* %5, align 1, !dbg !188
  %23 = add i8 %22, 1, !dbg !188
  store i8 %23, i8* %5, align 1, !dbg !188
  br label %6, !dbg !189, !llvm.loop !190

24:                                               ; preds = %6
  ret void, !dbg !192
}

; Function Attrs: noinline nounwind optnone sspstrong uwtable
define void @sp_send(i8* %0) #0 !dbg !193 {
  %2 = alloca i8*, align 8
  %3 = alloca %struct.message, align 8
  store i8* %0, i8** %2, align 8
  call void @llvm.dbg.declare(metadata i8** %2, metadata !196, metadata !DIExpression()), !dbg !197
  call void @llvm.dbg.declare(metadata %struct.message* %3, metadata !198, metadata !DIExpression()), !dbg !199
  %4 = getelementptr inbounds %struct.message, %struct.message* %3, i32 0, i32 0, !dbg !200
  store i16 0, i16* %4, align 8, !dbg !200
  %5 = getelementptr inbounds %struct.message, %struct.message* %3, i32 0, i32 1, !dbg !200
  %6 = load i8*, i8** %2, align 8, !dbg !201
  store i8* %6, i8** %5, align 8, !dbg !200
  %7 = getelementptr inbounds %struct.message, %struct.message* %3, i32 0, i32 2, !dbg !200
  store i8 -56, i8* %7, align 8, !dbg !200
  call void @broker_publish(%struct.broker* @broker, %struct.message* %3), !dbg !202
  ret void, !dbg !203
}

; Function Attrs: noinline nounwind optnone sspstrong uwtable
define i32 @main() #0 !dbg !204 {
  %1 = alloca %struct.message, align 8
  call void @broker_init(%struct.broker* @broker), !dbg !208
  call void @llvm.dbg.declare(metadata %struct.message* %1, metadata !209, metadata !DIExpression()), !dbg !210
  %2 = bitcast %struct.message* %1 to i8*, !dbg !210
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %2, i8* align 8 bitcast (%struct.message* @__const.main.msg to i8*), i64 24, i1 false), !dbg !210
  call void @broker_publish(%struct.broker* @broker, %struct.message* %1), !dbg !211
  %3 = getelementptr inbounds %struct.message, %struct.message* %1, i32 0, i32 0, !dbg !212
  store i16 1, i16* %3, align 8, !dbg !213
  call void @broker_publish(%struct.broker* @broker, %struct.message* %1), !dbg !214
  %4 = getelementptr inbounds %struct.message, %struct.message* %1, i32 0, i32 0, !dbg !215
  store i16 2, i16* %4, align 8, !dbg !216
  call void @broker_publish(%struct.broker* @broker, %struct.message* %1), !dbg !217
  call void @broker_run(%struct.broker* @broker), !dbg !218
  ret i32 0, !dbg !219
}

attributes #0 = { noinline nounwind optnone sspstrong uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "frame-pointer"="all" "less-precise-fpmad"="false" "min-legal-vector-width"="0" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="true" "stack-protector-buffer-size"="4" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { nounwind readnone speculatable willreturn }
attributes #2 = { argmemonly nounwind willreturn }

!llvm.dbg.cu = !{!0}
!llvm.module.flags = !{!16, !17, !18, !19}
!llvm.ident = !{!20}

!0 = distinct !DICompileUnit(language: DW_LANG_C99, file: !1, producer: "clang version 11.1.0", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, enums: !2, globals: !3, splitDebugInlining: false, nameTableKind: None)
!1 = !DIFile(filename: "disasm-test/tests/p0.c", directory: "/home/kquick/work/RISE/llvm-regr2")
!2 = !{}
!3 = !{!4, !12, !14}
!4 = !DIGlobalVariableExpression(var: !5, expr: !DIExpression(DW_OP_constu, 0, DW_OP_stack_value))
!5 = distinct !DIGlobalVariable(name: "MSG_TYPE_CLEAR", scope: !0, file: !1, line: 7, type: !6, isLocal: true, isDefinition: true)
!6 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !7)
!7 = !DIDerivedType(tag: DW_TAG_typedef, name: "uint16_t", file: !8, line: 25, baseType: !9)
!8 = !DIFile(filename: "/nix/store/rfw51dqr3qn7b6fjy8hmx6f0x3hfwbx6-glibc-2.37-8-dev/include/bits/stdint-uintn.h", directory: "")
!9 = !DIDerivedType(tag: DW_TAG_typedef, name: "__uint16_t", file: !10, line: 40, baseType: !11)
!10 = !DIFile(filename: "/nix/store/rfw51dqr3qn7b6fjy8hmx6f0x3hfwbx6-glibc-2.37-8-dev/include/bits/types.h", directory: "")
!11 = !DIBasicType(name: "unsigned short", size: 16, encoding: DW_ATE_unsigned)
!12 = !DIGlobalVariableExpression(var: !13, expr: !DIExpression(DW_OP_constu, 1, DW_OP_stack_value))
!13 = distinct !DIGlobalVariable(name: "MSG_TYPE_ADD_FIVE", scope: !0, file: !1, line: 8, type: !6, isLocal: true, isDefinition: true)
!14 = !DIGlobalVariableExpression(var: !15, expr: !DIExpression(DW_OP_constu, 2, DW_OP_stack_value))
!15 = distinct !DIGlobalVariable(name: "MSG_TYPE_DOUBLE", scope: !0, file: !1, line: 9, type: !6, isLocal: true, isDefinition: true)
!16 = !{i32 7, !"Dwarf Version", i32 4}
!17 = !{i32 2, !"Debug Info Version", i32 3}
!18 = !{i32 1, !"wchar_size", i32 4}
!19 = !{i32 7, !"PIC Level", i32 2}
!20 = !{!"clang version 11.1.0"}
!21 = distinct !DISubprogram(name: "broker_init", scope: !1, file: !1, line: 29, type: !22, scopeLine: 29, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !2)
!22 = !DISubroutineType(types: !23)
!23 = !{null, !24}
!24 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !25, size: 64)
!25 = !DIDerivedType(tag: DW_TAG_typedef, name: "broker_t", file: !1, line: 23, baseType: !26)
!26 = distinct !DICompositeType(tag: DW_TAG_structure_type, name: "broker", file: !1, line: 19, size: 1984, elements: !27)
!27 = !{!28, !42, !43}
!28 = !DIDerivedType(tag: DW_TAG_member, name: "msgs", scope: !26, file: !1, line: 20, baseType: !29, size: 1920)
!29 = !DICompositeType(tag: DW_TAG_array_type, baseType: !30, size: 1920, elements: !40)
!30 = !DIDerivedType(tag: DW_TAG_typedef, name: "message_t", file: !1, line: 17, baseType: !31)
!31 = distinct !DICompositeType(tag: DW_TAG_structure_type, name: "message", file: !1, line: 13, size: 192, elements: !32)
!32 = !{!33, !34, !39}
!33 = !DIDerivedType(tag: DW_TAG_member, name: "type", scope: !31, file: !1, line: 14, baseType: !7, size: 16)
!34 = !DIDerivedType(tag: DW_TAG_member, name: "payload", scope: !31, file: !1, line: 15, baseType: !35, size: 64, offset: 64)
!35 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !36, size: 64)
!36 = !DIDerivedType(tag: DW_TAG_typedef, name: "uint8_t", file: !8, line: 24, baseType: !37)
!37 = !DIDerivedType(tag: DW_TAG_typedef, name: "__uint8_t", file: !10, line: 38, baseType: !38)
!38 = !DIBasicType(name: "unsigned char", size: 8, encoding: DW_ATE_unsigned_char)
!39 = !DIDerivedType(tag: DW_TAG_member, name: "payload_len", scope: !31, file: !1, line: 16, baseType: !36, size: 8, offset: 128)
!40 = !{!41}
!41 = !DISubrange(count: 10)
!42 = !DIDerivedType(tag: DW_TAG_member, name: "msg_idx", scope: !26, file: !1, line: 21, baseType: !36, size: 8, offset: 1920)
!43 = !DIDerivedType(tag: DW_TAG_member, name: "num_messages", scope: !26, file: !1, line: 22, baseType: !36, size: 8, offset: 1928)
!44 = !DILocalVariable(name: "broker", arg: 1, scope: !21, file: !1, line: 29, type: !24)
!45 = !DILocation(line: 29, column: 29, scope: !21)
!46 = !DILocation(line: 30, column: 5, scope: !21)
!47 = !DILocation(line: 30, column: 13, scope: !21)
!48 = !DILocation(line: 30, column: 26, scope: !21)
!49 = !DILocation(line: 31, column: 1, scope: !21)
!50 = distinct !DISubprogram(name: "broker_run", scope: !1, file: !1, line: 33, type: !22, scopeLine: 33, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !2)
!51 = !DILocalVariable(name: "broker", arg: 1, scope: !50, file: !1, line: 33, type: !24)
!52 = !DILocation(line: 33, column: 28, scope: !50)
!53 = !DILocalVariable(name: "i", scope: !54, file: !1, line: 34, type: !36)
!54 = distinct !DILexicalBlock(scope: !50, file: !1, line: 34, column: 5)
!55 = !DILocation(line: 34, column: 18, scope: !54)
!56 = !DILocation(line: 34, column: 10, scope: !54)
!57 = !DILocation(line: 34, column: 25, scope: !58)
!58 = distinct !DILexicalBlock(scope: !54, file: !1, line: 34, column: 5)
!59 = !DILocation(line: 34, column: 29, scope: !58)
!60 = !DILocation(line: 34, column: 37, scope: !58)
!61 = !DILocation(line: 34, column: 27, scope: !58)
!62 = !DILocation(line: 34, column: 5, scope: !54)
!63 = !DILocation(line: 36, column: 22, scope: !64)
!64 = distinct !DILexicalBlock(scope: !58, file: !1, line: 34, column: 56)
!65 = !DILocation(line: 36, column: 30, scope: !64)
!66 = !DILocation(line: 36, column: 35, scope: !64)
!67 = !DILocation(line: 36, column: 9, scope: !64)
!68 = !DILocation(line: 37, column: 5, scope: !64)
!69 = !DILocation(line: 34, column: 52, scope: !58)
!70 = !DILocation(line: 34, column: 5, scope: !58)
!71 = distinct !{!71, !62, !72}
!72 = !DILocation(line: 37, column: 5, scope: !54)
!73 = !DILocation(line: 38, column: 1, scope: !50)
!74 = distinct !DISubprogram(name: "sp_receive", scope: !1, file: !1, line: 67, type: !75, scopeLine: 67, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !2)
!75 = !DISubroutineType(types: !76)
!76 = !{null, !77}
!77 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !30, size: 64)
!78 = !DILocalVariable(name: "msg", arg: 1, scope: !74, file: !1, line: 67, type: !77)
!79 = !DILocation(line: 67, column: 29, scope: !74)
!80 = !DILocation(line: 68, column: 13, scope: !74)
!81 = !DILocation(line: 68, column: 18, scope: !74)
!82 = !DILocation(line: 68, column: 5, scope: !74)
!83 = !DILocation(line: 70, column: 22, scope: !84)
!84 = distinct !DILexicalBlock(scope: !74, file: !1, line: 68, column: 24)
!85 = !DILocation(line: 70, column: 27, scope: !84)
!86 = !DILocation(line: 70, column: 36, scope: !84)
!87 = !DILocation(line: 70, column: 41, scope: !84)
!88 = !DILocation(line: 70, column: 13, scope: !84)
!89 = !DILocation(line: 71, column: 13, scope: !84)
!90 = !DILocation(line: 73, column: 25, scope: !84)
!91 = !DILocation(line: 73, column: 30, scope: !84)
!92 = !DILocation(line: 73, column: 39, scope: !84)
!93 = !DILocation(line: 73, column: 44, scope: !84)
!94 = !DILocation(line: 73, column: 13, scope: !84)
!95 = !DILocation(line: 74, column: 13, scope: !84)
!96 = !DILocation(line: 76, column: 23, scope: !84)
!97 = !DILocation(line: 76, column: 28, scope: !84)
!98 = !DILocation(line: 76, column: 37, scope: !84)
!99 = !DILocation(line: 76, column: 42, scope: !84)
!100 = !DILocation(line: 76, column: 13, scope: !84)
!101 = !DILocation(line: 77, column: 13, scope: !84)
!102 = !DILocation(line: 79, column: 13, scope: !84)
!103 = !DILocation(line: 81, column: 1, scope: !74)
!104 = distinct !DISubprogram(name: "broker_publish", scope: !1, file: !1, line: 40, type: !105, scopeLine: 40, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !2)
!105 = !DISubroutineType(types: !106)
!106 = !{null, !24, !77}
!107 = !DILocalVariable(name: "broker", arg: 1, scope: !104, file: !1, line: 40, type: !24)
!108 = !DILocation(line: 40, column: 32, scope: !104)
!109 = !DILocalVariable(name: "msg", arg: 2, scope: !104, file: !1, line: 40, type: !77)
!110 = !DILocation(line: 40, column: 52, scope: !104)
!111 = !DILocation(line: 41, column: 5, scope: !104)
!112 = !DILocation(line: 41, column: 13, scope: !104)
!113 = !DILocation(line: 41, column: 18, scope: !104)
!114 = !DILocation(line: 41, column: 26, scope: !104)
!115 = !DILocation(line: 41, column: 38, scope: !104)
!116 = !DILocation(line: 41, column: 45, scope: !104)
!117 = !DILocation(line: 41, column: 44, scope: !104)
!118 = !DILocation(line: 42, column: 1, scope: !104)
!119 = distinct !DISubprogram(name: "sp_clear", scope: !1, file: !1, line: 44, type: !120, scopeLine: 44, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !2)
!120 = !DISubroutineType(types: !121)
!121 = !{null, !35, !36}
!122 = !DILocalVariable(name: "signal", arg: 1, scope: !119, file: !1, line: 44, type: !35)
!123 = !DILocation(line: 44, column: 25, scope: !119)
!124 = !DILocalVariable(name: "signal_len", arg: 2, scope: !119, file: !1, line: 44, type: !36)
!125 = !DILocation(line: 44, column: 41, scope: !119)
!126 = !DILocalVariable(name: "i", scope: !127, file: !1, line: 45, type: !36)
!127 = distinct !DILexicalBlock(scope: !119, file: !1, line: 45, column: 5)
!128 = !DILocation(line: 45, column: 18, scope: !127)
!129 = !DILocation(line: 45, column: 10, scope: !127)
!130 = !DILocation(line: 45, column: 25, scope: !131)
!131 = distinct !DILexicalBlock(scope: !127, file: !1, line: 45, column: 5)
!132 = !DILocation(line: 45, column: 29, scope: !131)
!133 = !DILocation(line: 45, column: 27, scope: !131)
!134 = !DILocation(line: 45, column: 5, scope: !127)
!135 = !DILocation(line: 46, column: 9, scope: !136)
!136 = distinct !DILexicalBlock(scope: !131, file: !1, line: 45, column: 46)
!137 = !DILocation(line: 46, column: 16, scope: !136)
!138 = !DILocation(line: 46, column: 19, scope: !136)
!139 = !DILocation(line: 47, column: 5, scope: !136)
!140 = !DILocation(line: 45, column: 42, scope: !131)
!141 = !DILocation(line: 45, column: 5, scope: !131)
!142 = distinct !{!142, !134, !143}
!143 = !DILocation(line: 47, column: 5, scope: !127)
!144 = !DILocation(line: 48, column: 1, scope: !119)
!145 = distinct !DISubprogram(name: "sp_double", scope: !1, file: !1, line: 50, type: !120, scopeLine: 50, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !2)
!146 = !DILocalVariable(name: "signal", arg: 1, scope: !145, file: !1, line: 50, type: !35)
!147 = !DILocation(line: 50, column: 26, scope: !145)
!148 = !DILocalVariable(name: "signal_len", arg: 2, scope: !145, file: !1, line: 50, type: !36)
!149 = !DILocation(line: 50, column: 42, scope: !145)
!150 = !DILocalVariable(name: "i", scope: !151, file: !1, line: 51, type: !36)
!151 = distinct !DILexicalBlock(scope: !145, file: !1, line: 51, column: 5)
!152 = !DILocation(line: 51, column: 18, scope: !151)
!153 = !DILocation(line: 51, column: 10, scope: !151)
!154 = !DILocation(line: 51, column: 25, scope: !155)
!155 = distinct !DILexicalBlock(scope: !151, file: !1, line: 51, column: 5)
!156 = !DILocation(line: 51, column: 29, scope: !155)
!157 = !DILocation(line: 51, column: 27, scope: !155)
!158 = !DILocation(line: 51, column: 5, scope: !151)
!159 = !DILocation(line: 52, column: 9, scope: !160)
!160 = distinct !DILexicalBlock(scope: !155, file: !1, line: 51, column: 46)
!161 = !DILocation(line: 52, column: 16, scope: !160)
!162 = !DILocation(line: 52, column: 19, scope: !160)
!163 = !DILocation(line: 53, column: 5, scope: !160)
!164 = !DILocation(line: 51, column: 42, scope: !155)
!165 = !DILocation(line: 51, column: 5, scope: !155)
!166 = distinct !{!166, !158, !167}
!167 = !DILocation(line: 53, column: 5, scope: !151)
!168 = !DILocation(line: 54, column: 1, scope: !145)
!169 = distinct !DISubprogram(name: "sp_add_five", scope: !1, file: !1, line: 56, type: !120, scopeLine: 56, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !2)
!170 = !DILocalVariable(name: "signal", arg: 1, scope: !169, file: !1, line: 56, type: !35)
!171 = !DILocation(line: 56, column: 28, scope: !169)
!172 = !DILocalVariable(name: "signal_len", arg: 2, scope: !169, file: !1, line: 56, type: !36)
!173 = !DILocation(line: 56, column: 44, scope: !169)
!174 = !DILocalVariable(name: "i", scope: !175, file: !1, line: 57, type: !36)
!175 = distinct !DILexicalBlock(scope: !169, file: !1, line: 57, column: 5)
!176 = !DILocation(line: 57, column: 18, scope: !175)
!177 = !DILocation(line: 57, column: 10, scope: !175)
!178 = !DILocation(line: 57, column: 25, scope: !179)
!179 = distinct !DILexicalBlock(scope: !175, file: !1, line: 57, column: 5)
!180 = !DILocation(line: 57, column: 29, scope: !179)
!181 = !DILocation(line: 57, column: 27, scope: !179)
!182 = !DILocation(line: 57, column: 5, scope: !175)
!183 = !DILocation(line: 58, column: 9, scope: !184)
!184 = distinct !DILexicalBlock(scope: !179, file: !1, line: 57, column: 46)
!185 = !DILocation(line: 58, column: 16, scope: !184)
!186 = !DILocation(line: 58, column: 19, scope: !184)
!187 = !DILocation(line: 59, column: 5, scope: !184)
!188 = !DILocation(line: 57, column: 42, scope: !179)
!189 = !DILocation(line: 57, column: 5, scope: !179)
!190 = distinct !{!190, !182, !191}
!191 = !DILocation(line: 59, column: 5, scope: !175)
!192 = !DILocation(line: 60, column: 1, scope: !169)
!193 = distinct !DISubprogram(name: "sp_send", scope: !1, file: !1, line: 62, type: !194, scopeLine: 62, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !2)
!194 = !DISubroutineType(types: !195)
!195 = !{null, !35}
!196 = !DILocalVariable(name: "signal", arg: 1, scope: !193, file: !1, line: 62, type: !35)
!197 = !DILocation(line: 62, column: 24, scope: !193)
!198 = !DILocalVariable(name: "msg", scope: !193, file: !1, line: 63, type: !30)
!199 = !DILocation(line: 63, column: 15, scope: !193)
!200 = !DILocation(line: 63, column: 21, scope: !193)
!201 = !DILocation(line: 63, column: 38, scope: !193)
!202 = !DILocation(line: 64, column: 5, scope: !193)
!203 = !DILocation(line: 65, column: 1, scope: !193)
!204 = distinct !DISubprogram(name: "main", scope: !1, file: !1, line: 83, type: !205, scopeLine: 83, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !2)
!205 = !DISubroutineType(types: !206)
!206 = !{!207}
!207 = !DIBasicType(name: "int", size: 32, encoding: DW_ATE_signed)
!208 = !DILocation(line: 84, column: 5, scope: !204)
!209 = !DILocalVariable(name: "msg", scope: !204, file: !1, line: 86, type: !30)
!210 = !DILocation(line: 86, column: 15, scope: !204)
!211 = !DILocation(line: 87, column: 5, scope: !204)
!212 = !DILocation(line: 88, column: 9, scope: !204)
!213 = !DILocation(line: 88, column: 14, scope: !204)
!214 = !DILocation(line: 89, column: 5, scope: !204)
!215 = !DILocation(line: 90, column: 9, scope: !204)
!216 = !DILocation(line: 90, column: 14, scope: !204)
!217 = !DILocation(line: 91, column: 5, scope: !204)
!218 = !DILocation(line: 93, column: 5, scope: !204)
!219 = !DILocation(line: 94, column: 1, scope: !204)
