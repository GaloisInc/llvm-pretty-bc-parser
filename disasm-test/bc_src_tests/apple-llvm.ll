; ModuleID = 'swap.c'
source_filename = "swap.c"
target datalayout = "e-m:o-i64:64-i128:128-n32:64-S128"
target triple = "arm64-apple-macosx13.0.0"

@.str = private unnamed_addr constant [32 x i8] c"[%s] Testing with %u and %u... \00", align 1
@.str.3 = private unnamed_addr constant [7 x i8] c"Chosen\00", align 1
@.str.4 = private unnamed_addr constant [7 x i8] c"Random\00", align 1
@str = private unnamed_addr constant [7 x i8] c"FAILED\00", align 1
@str.10 = private unnamed_addr constant [3 x i8] c"OK\00", align 1
@str.11 = private unnamed_addr constant [38 x i8] c"Beginning chosen-value tests for swap\00", align 1
@str.12 = private unnamed_addr constant [35 x i8] c"Ending chosen-value tests for swap\00", align 1
@str.13 = private unnamed_addr constant [32 x i8] c"Beginning random tests for swap\00", align 1
@str.14 = private unnamed_addr constant [29 x i8] c"Ending random tests for swap\00", align 1

; Function Attrs: argmemonly mustprogress nofree norecurse nosync nounwind ssp willreturn uwtable(sync)
define void @swap(i32* nocapture noundef %0, i32* nocapture noundef %1) local_unnamed_addr #0 !dbg !14 {
  call void @llvm.dbg.value(metadata i32* %0, metadata !22, metadata !DIExpression()), !dbg !25
  call void @llvm.dbg.value(metadata i32* %1, metadata !23, metadata !DIExpression()), !dbg !25
  %3 = load i32, i32* %0, align 4, !dbg !26, !tbaa !27
  call void @llvm.dbg.value(metadata i32 %3, metadata !24, metadata !DIExpression()), !dbg !25
  %4 = load i32, i32* %1, align 4, !dbg !31, !tbaa !27
  store i32 %4, i32* %0, align 4, !dbg !32, !tbaa !27
  store i32 %3, i32* %1, align 4, !dbg !33, !tbaa !27
  ret void, !dbg !34
}

; Function Attrs: argmemonly mustprogress nocallback nofree nosync nounwind willreturn
declare void @llvm.lifetime.start.p0i8(i64 immarg, i8* nocapture) #1

; Function Attrs: argmemonly mustprogress nocallback nofree nosync nounwind willreturn
declare void @llvm.lifetime.end.p0i8(i64 immarg, i8* nocapture) #1

; Function Attrs: mustprogress nofree nosync nounwind readnone ssp willreturn uwtable(sync)
define zeroext i1 @swap_spec(i32 noundef %0, i32 noundef %1) local_unnamed_addr #2 !dbg !35 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !40, metadata !DIExpression()), !dbg !44
  call void @llvm.dbg.value(metadata i32 %1, metadata !41, metadata !DIExpression()), !dbg !44
  call void @llvm.dbg.value(metadata i32 %0, metadata !42, metadata !DIExpression()), !dbg !44
  call void @llvm.dbg.value(metadata i32 %1, metadata !43, metadata !DIExpression()), !dbg !44
  call void @llvm.dbg.value(metadata i32 %1, metadata !42, metadata !DIExpression()), !dbg !44
  call void @llvm.dbg.value(metadata i32 %0, metadata !43, metadata !DIExpression()), !dbg !44
  ret i1 true, !dbg !45
}

; Function Attrs: argmemonly mustprogress nofree norecurse nosync nounwind ssp willreturn uwtable(sync)
define void @xor_swap(i32* nocapture noundef %0, i32* nocapture noundef %1) local_unnamed_addr #0 !dbg !46 {
  call void @llvm.dbg.value(metadata i32* %0, metadata !48, metadata !DIExpression()), !dbg !50
  call void @llvm.dbg.value(metadata i32* %1, metadata !49, metadata !DIExpression()), !dbg !50
  %3 = load i32, i32* %1, align 4, !dbg !51, !tbaa !27
  %4 = load i32, i32* %0, align 4, !dbg !52, !tbaa !27
  %5 = xor i32 %4, %3, !dbg !52
  store i32 %5, i32* %0, align 4, !dbg !52, !tbaa !27
  %6 = load i32, i32* %1, align 4, !dbg !53, !tbaa !27
  %7 = xor i32 %6, %5, !dbg !53
  store i32 %7, i32* %1, align 4, !dbg !53, !tbaa !27
  %8 = load i32, i32* %0, align 4, !dbg !54, !tbaa !27
  %9 = xor i32 %8, %7, !dbg !54
  store i32 %9, i32* %0, align 4, !dbg !54, !tbaa !27
  ret void, !dbg !55
}

; Function Attrs: mustprogress nofree nosync nounwind readnone ssp willreturn uwtable(sync)
define zeroext i1 @xor_swap_spec(i32 noundef %0, i32 noundef %1) local_unnamed_addr #2 !dbg !56 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !58, metadata !DIExpression()), !dbg !64
  call void @llvm.dbg.value(metadata i32 %1, metadata !59, metadata !DIExpression()), !dbg !64
  call void @llvm.dbg.value(metadata i32 %0, metadata !60, metadata !DIExpression()), !dbg !64
  call void @llvm.dbg.value(metadata i32 %0, metadata !61, metadata !DIExpression()), !dbg !64
  call void @llvm.dbg.value(metadata i32 %1, metadata !62, metadata !DIExpression()), !dbg !64
  call void @llvm.dbg.value(metadata i32 %1, metadata !63, metadata !DIExpression()), !dbg !64
  call void @llvm.dbg.value(metadata i32 %1, metadata !60, metadata !DIExpression()), !dbg !64
  call void @llvm.dbg.value(metadata i32 %1, metadata !61, metadata !DIExpression()), !dbg !64
  call void @llvm.dbg.value(metadata i32 %0, metadata !62, metadata !DIExpression()), !dbg !64
  call void @llvm.dbg.value(metadata i32 %0, metadata !63, metadata !DIExpression()), !dbg !64
  ret i1 true, !dbg !65
}

; Function Attrs: nounwind ssp uwtable(sync)
define zeroext i1 @general_swap_spec(void (i32*, i32*)* nocapture noundef readonly %0, i32 noundef %1, i32 noundef %2) local_unnamed_addr #3 !dbg !66 {
  %4 = alloca i32, align 4
  %5 = alloca i32, align 4
  call void @llvm.dbg.value(metadata void (i32*, i32*)* %0, metadata !71, metadata !DIExpression()), !dbg !76
  call void @llvm.dbg.value(metadata i32 %1, metadata !72, metadata !DIExpression()), !dbg !76
  call void @llvm.dbg.value(metadata i32 %2, metadata !73, metadata !DIExpression()), !dbg !76
  %6 = bitcast i32* %4 to i8*, !dbg !77
  call void @llvm.lifetime.start.p0i8(i64 4, i8* nonnull %6) #12, !dbg !77
  call void @llvm.dbg.value(metadata i32 %1, metadata !74, metadata !DIExpression()), !dbg !76
  store i32 %1, i32* %4, align 4, !dbg !78, !tbaa !27
  %7 = bitcast i32* %5 to i8*, !dbg !77
  call void @llvm.lifetime.start.p0i8(i64 4, i8* nonnull %7) #12, !dbg !77
  call void @llvm.dbg.value(metadata i32 %2, metadata !75, metadata !DIExpression()), !dbg !76
  store i32 %2, i32* %5, align 4, !dbg !79, !tbaa !27
  call void @llvm.dbg.value(metadata i32* %4, metadata !74, metadata !DIExpression(DW_OP_deref)), !dbg !76
  call void @llvm.dbg.value(metadata i32* %5, metadata !75, metadata !DIExpression(DW_OP_deref)), !dbg !76
  call void %0(i32* noundef nonnull %4, i32* noundef nonnull %5) #12, !dbg !80
  %8 = load i32, i32* %4, align 4, !dbg !81, !tbaa !27
  call void @llvm.dbg.value(metadata i32 %8, metadata !74, metadata !DIExpression()), !dbg !76
  %9 = icmp eq i32 %8, %2, !dbg !82
  %10 = load i32, i32* %5, align 4, !dbg !83
  call void @llvm.dbg.value(metadata i32 %10, metadata !75, metadata !DIExpression()), !dbg !76
  %11 = icmp eq i32 %10, %1, !dbg !83
  %12 = select i1 %9, i1 %11, i1 false, !dbg !83
  call void @llvm.lifetime.end.p0i8(i64 4, i8* nonnull %7) #12, !dbg !84
  call void @llvm.lifetime.end.p0i8(i64 4, i8* nonnull %6) #12, !dbg !84
  ret i1 %12, !dbg !85
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind readnone ssp willreturn uwtable(sync)
define void @swap_broken1(i32* nocapture noundef %0, i32* nocapture noundef %1) local_unnamed_addr #4 !dbg !86 {
  call void @llvm.dbg.value(metadata i32* %0, metadata !88, metadata !DIExpression()), !dbg !91
  call void @llvm.dbg.value(metadata i32* %1, metadata !89, metadata !DIExpression()), !dbg !91
  call void @llvm.dbg.value(metadata i32 undef, metadata !90, metadata !DIExpression()), !dbg !91
  ret void, !dbg !92
}

; Function Attrs: mustprogress nofree nosync nounwind readnone ssp willreturn uwtable(sync)
define zeroext i1 @swap_broken1_spec(i32 noundef %0, i32 noundef %1) local_unnamed_addr #2 !dbg !93 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !95, metadata !DIExpression()), !dbg !99
  call void @llvm.dbg.value(metadata i32 %1, metadata !96, metadata !DIExpression()), !dbg !99
  call void @llvm.dbg.value(metadata i32 %0, metadata !97, metadata !DIExpression()), !dbg !99
  call void @llvm.dbg.value(metadata i32 %1, metadata !98, metadata !DIExpression()), !dbg !99
  call void @llvm.dbg.value(metadata i32 %0, metadata !97, metadata !DIExpression()), !dbg !99
  %3 = icmp eq i32 %0, %1, !dbg !100
  call void @llvm.dbg.value(metadata i32 %1, metadata !98, metadata !DIExpression()), !dbg !99
  ret i1 %3, !dbg !101
}

; Function Attrs: argmemonly mustprogress nofree norecurse nosync nounwind ssp willreturn uwtable(sync)
define void @swap_broken2(i32* nocapture noundef %0, i32* nocapture noundef %1) local_unnamed_addr #0 !dbg !102 {
  call void @llvm.dbg.value(metadata i32* %0, metadata !104, metadata !DIExpression()), !dbg !107
  call void @llvm.dbg.value(metadata i32* %1, metadata !105, metadata !DIExpression()), !dbg !107
  %3 = load i32, i32* %0, align 4, !dbg !108, !tbaa !27
  call void @llvm.dbg.value(metadata i32 %3, metadata !106, metadata !DIExpression()), !dbg !107
  %4 = icmp eq i32 %3, 4142351, !dbg !109
  br i1 %4, label %7, label %5, !dbg !111

5:                                                ; preds = %2
  %6 = load i32, i32* %1, align 4, !dbg !112, !tbaa !27
  store i32 %6, i32* %0, align 4, !dbg !114, !tbaa !27
  store i32 %3, i32* %1, align 4, !dbg !115, !tbaa !27
  br label %7, !dbg !116

7:                                                ; preds = %5, %2
  ret void, !dbg !117
}

; Function Attrs: mustprogress nofree nosync nounwind readnone ssp willreturn uwtable(sync)
define zeroext i1 @swap_broken2_spec(i32 noundef %0, i32 noundef %1) local_unnamed_addr #2 !dbg !118 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !120, metadata !DIExpression()), !dbg !124
  call void @llvm.dbg.value(metadata i32 %1, metadata !121, metadata !DIExpression()), !dbg !124
  call void @llvm.dbg.value(metadata i32 %0, metadata !122, metadata !DIExpression()), !dbg !124
  call void @llvm.dbg.value(metadata i32 %1, metadata !123, metadata !DIExpression()), !dbg !124
  call void @llvm.dbg.value(metadata i32* undef, metadata !104, metadata !DIExpression()), !dbg !125
  call void @llvm.dbg.value(metadata i32* undef, metadata !105, metadata !DIExpression()), !dbg !125
  call void @llvm.dbg.value(metadata i32 %0, metadata !106, metadata !DIExpression()), !dbg !125
  %3 = icmp eq i32 %0, 4142351, !dbg !127
  %4 = select i1 %3, i32 4142351, i32 %1, !dbg !128
  %5 = select i1 %3, i32 %1, i32 %0, !dbg !128
  call void @llvm.dbg.value(metadata i32 %4, metadata !122, metadata !DIExpression()), !dbg !124
  %6 = icmp eq i32 %4, %1, !dbg !129
  call void @llvm.dbg.value(metadata i32 %5, metadata !123, metadata !DIExpression()), !dbg !124
  %7 = icmp eq i32 %5, %0, !dbg !130
  %8 = and i1 %6, %7, !dbg !130
  ret i1 %8, !dbg !131
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind ssp willreturn uwtable(sync)
define void @swap_broken3(i32* nocapture noundef %0, i32* nocapture noundef %1) local_unnamed_addr #5 !dbg !132 {
  call void @llvm.dbg.value(metadata i32* %0, metadata !134, metadata !DIExpression()), !dbg !137
  call void @llvm.dbg.value(metadata i32* %1, metadata !135, metadata !DIExpression()), !dbg !137
  %3 = load i32, i32* %0, align 4, !dbg !138, !tbaa !27
  call void @llvm.dbg.value(metadata i32 %3, metadata !136, metadata !DIExpression()), !dbg !137
  %4 = icmp eq i32 %3, 0, !dbg !139
  br i1 %4, label %10, label %5, !dbg !141

5:                                                ; preds = %2
  %6 = load i32, i32* %1, align 4, !dbg !142, !tbaa !27
  %7 = shl i32 %6, 5, !dbg !143
  %8 = icmp eq i32 %3, %7, !dbg !144
  %9 = select i1 %8, i32* null, i32* %1, !dbg !145
  call void @llvm.dbg.value(metadata i32* %9, metadata !135, metadata !DIExpression()), !dbg !137
  br label %10, !dbg !145

10:                                               ; preds = %5, %2
  %11 = phi i32* [ %9, %5 ], [ %1, %2 ]
  call void @llvm.dbg.value(metadata i32* %11, metadata !135, metadata !DIExpression()), !dbg !137
  %12 = load i32, i32* %11, align 4, !dbg !146, !tbaa !27
  store i32 %12, i32* %0, align 4, !dbg !147, !tbaa !27
  store i32 %3, i32* %11, align 4, !dbg !148, !tbaa !27
  ret void, !dbg !149
}

; Function Attrs: mustprogress nofree nosync nounwind ssp willreturn uwtable(sync)
define zeroext i1 @swap_broken3_spec(i32 noundef %0, i32 noundef %1) local_unnamed_addr #6 !dbg !150 {
  %3 = alloca i32, align 4
  call void @llvm.dbg.value(metadata i32 %0, metadata !152, metadata !DIExpression()), !dbg !156
  call void @llvm.dbg.value(metadata i32 %1, metadata !153, metadata !DIExpression()), !dbg !156
  call void @llvm.dbg.value(metadata i32 %0, metadata !154, metadata !DIExpression()), !dbg !156
  %4 = bitcast i32* %3 to i8*, !dbg !157
  call void @llvm.lifetime.start.p0i8(i64 4, i8* nonnull %4), !dbg !157
  call void @llvm.dbg.value(metadata i32 %1, metadata !155, metadata !DIExpression()), !dbg !156
  store i32 %1, i32* %3, align 4, !dbg !158, !tbaa !27
  call void @llvm.dbg.value(metadata i32* %3, metadata !155, metadata !DIExpression(DW_OP_deref)), !dbg !156
  call void @llvm.dbg.value(metadata i32* undef, metadata !134, metadata !DIExpression()), !dbg !159
  call void @llvm.dbg.value(metadata i32* %3, metadata !135, metadata !DIExpression()), !dbg !159
  call void @llvm.dbg.value(metadata i32 %0, metadata !136, metadata !DIExpression()), !dbg !159
  %5 = icmp eq i32 %0, 0, !dbg !161
  %6 = shl i32 %1, 5, !dbg !162
  %7 = icmp eq i32 %6, %0, !dbg !162
  %8 = select i1 %7, i32* null, i32* %3, !dbg !162
  %9 = select i1 %5, i32* %3, i32* %8, !dbg !162
  call void @llvm.dbg.value(metadata i32* %9, metadata !135, metadata !DIExpression()), !dbg !159
  %10 = load i32, i32* %9, align 4, !dbg !163, !tbaa !27
  store i32 %0, i32* %9, align 4, !dbg !164, !tbaa !27
  call void @llvm.dbg.value(metadata i32 %10, metadata !154, metadata !DIExpression()), !dbg !156
  %11 = icmp eq i32 %10, %1, !dbg !165
  %12 = load i32, i32* %3, align 4, !dbg !166
  call void @llvm.dbg.value(metadata i32 %12, metadata !155, metadata !DIExpression()), !dbg !156
  %13 = icmp eq i32 %12, %0, !dbg !166
  %14 = select i1 %11, i1 %13, i1 false, !dbg !166
  call void @llvm.lifetime.end.p0i8(i64 4, i8* nonnull %4), !dbg !167
  ret i1 %14, !dbg !168
}

; Function Attrs: nounwind ssp uwtable(sync)
define void @test_swap_function(void (i32*, i32*)* nocapture noundef readonly %0, i8* noundef %1, i32 noundef %2, i32 noundef %3) local_unnamed_addr #3 !dbg !169 {
  %5 = alloca i32, align 4
  %6 = alloca i32, align 4
  call void @llvm.dbg.value(metadata void (i32*, i32*)* %0, metadata !175, metadata !DIExpression()), !dbg !179
  call void @llvm.dbg.value(metadata i8* %1, metadata !176, metadata !DIExpression()), !dbg !179
  call void @llvm.dbg.value(metadata i32 %2, metadata !177, metadata !DIExpression()), !dbg !179
  call void @llvm.dbg.value(metadata i32 %3, metadata !178, metadata !DIExpression()), !dbg !179
  %7 = call i32 (i8*, ...) @printf(i8* noundef nonnull dereferenceable(1) getelementptr inbounds ([32 x i8], [32 x i8]* @.str, i64 0, i64 0), i8* noundef %1, i32 noundef %2, i32 noundef %3), !dbg !180
  call void @llvm.dbg.value(metadata void (i32*, i32*)* %0, metadata !71, metadata !DIExpression()) #12, !dbg !181
  call void @llvm.dbg.value(metadata i32 %2, metadata !72, metadata !DIExpression()) #12, !dbg !181
  call void @llvm.dbg.value(metadata i32 %3, metadata !73, metadata !DIExpression()) #12, !dbg !181
  %8 = bitcast i32* %5 to i8*, !dbg !184
  call void @llvm.lifetime.start.p0i8(i64 4, i8* nonnull %8) #12, !dbg !184
  call void @llvm.dbg.value(metadata i32 %2, metadata !74, metadata !DIExpression()) #12, !dbg !181
  store i32 %2, i32* %5, align 4, !dbg !185, !tbaa !27
  %9 = bitcast i32* %6 to i8*, !dbg !184
  call void @llvm.lifetime.start.p0i8(i64 4, i8* nonnull %9) #12, !dbg !184
  call void @llvm.dbg.value(metadata i32 %3, metadata !75, metadata !DIExpression()) #12, !dbg !181
  store i32 %3, i32* %6, align 4, !dbg !186, !tbaa !27
  call void @llvm.dbg.value(metadata i32* %5, metadata !74, metadata !DIExpression(DW_OP_deref)) #12, !dbg !181
  call void @llvm.dbg.value(metadata i32* %6, metadata !75, metadata !DIExpression(DW_OP_deref)) #12, !dbg !181
  call void %0(i32* noundef nonnull %5, i32* noundef nonnull %6) #12, !dbg !187
  %10 = load i32, i32* %5, align 4, !dbg !188, !tbaa !27
  call void @llvm.dbg.value(metadata i32 %10, metadata !74, metadata !DIExpression()) #12, !dbg !181
  %11 = icmp eq i32 %10, %3, !dbg !189
  %12 = load i32, i32* %6, align 4, !dbg !190
  call void @llvm.dbg.value(metadata i32 %12, metadata !75, metadata !DIExpression()) #12, !dbg !181
  %13 = icmp eq i32 %12, %2, !dbg !190
  %14 = select i1 %11, i1 %13, i1 false, !dbg !190
  call void @llvm.lifetime.end.p0i8(i64 4, i8* nonnull %9) #12, !dbg !191
  call void @llvm.lifetime.end.p0i8(i64 4, i8* nonnull %8) #12, !dbg !191
  %15 = select i1 %14, i8* getelementptr inbounds ([3 x i8], [3 x i8]* @str.10, i64 0, i64 0), i8* getelementptr inbounds ([7 x i8], [7 x i8]* @str, i64 0, i64 0)
  %16 = call i32 @puts(i8* nonnull dereferenceable(1) %15), !dbg !192
  ret void, !dbg !193
}

; Function Attrs: nofree nounwind
declare noundef i32 @printf(i8* nocapture noundef readonly, ...) local_unnamed_addr #7

; Function Attrs: nofree nounwind ssp uwtable(sync)
define void @test_swap(i8* noundef %0, i32 noundef %1, i32 noundef %2) local_unnamed_addr #8 !dbg !194 {
  call void @llvm.dbg.value(metadata i8* %0, metadata !198, metadata !DIExpression()), !dbg !201
  call void @llvm.dbg.value(metadata i32 %1, metadata !199, metadata !DIExpression()), !dbg !201
  call void @llvm.dbg.value(metadata i32 %2, metadata !200, metadata !DIExpression()), !dbg !201
  call void @llvm.dbg.value(metadata void (i32*, i32*)* @swap, metadata !175, metadata !DIExpression()) #12, !dbg !202
  call void @llvm.dbg.value(metadata i8* %0, metadata !176, metadata !DIExpression()) #12, !dbg !202
  call void @llvm.dbg.value(metadata i32 %1, metadata !177, metadata !DIExpression()) #12, !dbg !202
  call void @llvm.dbg.value(metadata i32 %2, metadata !178, metadata !DIExpression()) #12, !dbg !202
  %4 = call i32 (i8*, ...) @printf(i8* noundef nonnull dereferenceable(1) getelementptr inbounds ([32 x i8], [32 x i8]* @.str, i64 0, i64 0), i8* noundef %0, i32 noundef %1, i32 noundef %2) #12, !dbg !204
  %5 = call i32 @puts(i8* nonnull dereferenceable(1) getelementptr inbounds ([3 x i8], [3 x i8]* @str.10, i64 0, i64 0)) #12, !dbg !205
  ret void, !dbg !207
}

; Function Attrs: nounwind ssp uwtable(sync)
define void @chosen_value_test(void (i32*, i32*)* nocapture noundef readonly %0) local_unnamed_addr #3 !dbg !208 {
  %2 = alloca i32, align 4
  %3 = alloca i32, align 4
  %4 = alloca i32, align 4
  %5 = alloca i32, align 4
  %6 = alloca i32, align 4
  %7 = alloca i32, align 4
  %8 = alloca i32, align 4
  %9 = alloca i32, align 4
  call void @llvm.dbg.value(metadata void (i32*, i32*)* %0, metadata !212, metadata !DIExpression()), !dbg !213
  call void @llvm.dbg.value(metadata void (i32*, i32*)* %0, metadata !175, metadata !DIExpression()) #12, !dbg !214
  call void @llvm.dbg.value(metadata i8* getelementptr inbounds ([7 x i8], [7 x i8]* @.str.3, i64 0, i64 0), metadata !176, metadata !DIExpression()) #12, !dbg !214
  call void @llvm.dbg.value(metadata i32 1, metadata !177, metadata !DIExpression()) #12, !dbg !214
  call void @llvm.dbg.value(metadata i32 2, metadata !178, metadata !DIExpression()) #12, !dbg !214
  %10 = call i32 (i8*, ...) @printf(i8* noundef nonnull dereferenceable(1) getelementptr inbounds ([32 x i8], [32 x i8]* @.str, i64 0, i64 0), i8* noundef getelementptr inbounds ([7 x i8], [7 x i8]* @.str.3, i64 0, i64 0), i32 noundef 1, i32 noundef 2) #12, !dbg !216
  call void @llvm.dbg.value(metadata void (i32*, i32*)* %0, metadata !71, metadata !DIExpression()) #12, !dbg !217
  call void @llvm.dbg.value(metadata i32 1, metadata !72, metadata !DIExpression()) #12, !dbg !217
  call void @llvm.dbg.value(metadata i32 2, metadata !73, metadata !DIExpression()) #12, !dbg !217
  %11 = bitcast i32* %8 to i8*, !dbg !219
  call void @llvm.lifetime.start.p0i8(i64 4, i8* nonnull %11) #12, !dbg !219
  call void @llvm.dbg.value(metadata i32 1, metadata !74, metadata !DIExpression()) #12, !dbg !217
  store i32 1, i32* %8, align 4, !dbg !220, !tbaa !27
  %12 = bitcast i32* %9 to i8*, !dbg !219
  call void @llvm.lifetime.start.p0i8(i64 4, i8* nonnull %12) #12, !dbg !219
  call void @llvm.dbg.value(metadata i32 2, metadata !75, metadata !DIExpression()) #12, !dbg !217
  store i32 2, i32* %9, align 4, !dbg !221, !tbaa !27
  call void @llvm.dbg.value(metadata i32* %8, metadata !74, metadata !DIExpression(DW_OP_deref)) #12, !dbg !217
  call void @llvm.dbg.value(metadata i32* %9, metadata !75, metadata !DIExpression(DW_OP_deref)) #12, !dbg !217
  call void %0(i32* noundef nonnull %8, i32* noundef nonnull %9) #12, !dbg !222
  %13 = load i32, i32* %8, align 4, !dbg !223, !tbaa !27
  call void @llvm.dbg.value(metadata i32 %13, metadata !74, metadata !DIExpression()) #12, !dbg !217
  %14 = icmp eq i32 %13, 2, !dbg !224
  %15 = load i32, i32* %9, align 4, !dbg !225
  call void @llvm.dbg.value(metadata i32 %15, metadata !75, metadata !DIExpression()) #12, !dbg !217
  %16 = icmp eq i32 %15, 1, !dbg !225
  %17 = select i1 %14, i1 %16, i1 false, !dbg !225
  call void @llvm.lifetime.end.p0i8(i64 4, i8* nonnull %12) #12, !dbg !226
  call void @llvm.lifetime.end.p0i8(i64 4, i8* nonnull %11) #12, !dbg !226
  %18 = select i1 %17, i8* getelementptr inbounds ([3 x i8], [3 x i8]* @str.10, i64 0, i64 0), i8* getelementptr inbounds ([7 x i8], [7 x i8]* @str, i64 0, i64 0)
  %19 = call i32 @puts(i8* nonnull dereferenceable(1) %18) #12, !dbg !227
  call void @llvm.dbg.value(metadata void (i32*, i32*)* %0, metadata !175, metadata !DIExpression()) #12, !dbg !228
  call void @llvm.dbg.value(metadata i8* getelementptr inbounds ([7 x i8], [7 x i8]* @.str.3, i64 0, i64 0), metadata !176, metadata !DIExpression()) #12, !dbg !228
  call void @llvm.dbg.value(metadata i32 2429, metadata !177, metadata !DIExpression()) #12, !dbg !228
  call void @llvm.dbg.value(metadata i32 98423, metadata !178, metadata !DIExpression()) #12, !dbg !228
  %20 = call i32 (i8*, ...) @printf(i8* noundef nonnull dereferenceable(1) getelementptr inbounds ([32 x i8], [32 x i8]* @.str, i64 0, i64 0), i8* noundef getelementptr inbounds ([7 x i8], [7 x i8]* @.str.3, i64 0, i64 0), i32 noundef 2429, i32 noundef 98423) #12, !dbg !230
  call void @llvm.dbg.value(metadata void (i32*, i32*)* %0, metadata !71, metadata !DIExpression()) #12, !dbg !231
  call void @llvm.dbg.value(metadata i32 2429, metadata !72, metadata !DIExpression()) #12, !dbg !231
  call void @llvm.dbg.value(metadata i32 98423, metadata !73, metadata !DIExpression()) #12, !dbg !231
  %21 = bitcast i32* %6 to i8*, !dbg !233
  call void @llvm.lifetime.start.p0i8(i64 4, i8* nonnull %21) #12, !dbg !233
  call void @llvm.dbg.value(metadata i32 2429, metadata !74, metadata !DIExpression()) #12, !dbg !231
  store i32 2429, i32* %6, align 4, !dbg !234, !tbaa !27
  %22 = bitcast i32* %7 to i8*, !dbg !233
  call void @llvm.lifetime.start.p0i8(i64 4, i8* nonnull %22) #12, !dbg !233
  call void @llvm.dbg.value(metadata i32 98423, metadata !75, metadata !DIExpression()) #12, !dbg !231
  store i32 98423, i32* %7, align 4, !dbg !235, !tbaa !27
  call void @llvm.dbg.value(metadata i32* %6, metadata !74, metadata !DIExpression(DW_OP_deref)) #12, !dbg !231
  call void @llvm.dbg.value(metadata i32* %7, metadata !75, metadata !DIExpression(DW_OP_deref)) #12, !dbg !231
  call void %0(i32* noundef nonnull %6, i32* noundef nonnull %7) #12, !dbg !236
  %23 = load i32, i32* %6, align 4, !dbg !237, !tbaa !27
  call void @llvm.dbg.value(metadata i32 %23, metadata !74, metadata !DIExpression()) #12, !dbg !231
  %24 = icmp eq i32 %23, 98423, !dbg !238
  %25 = load i32, i32* %7, align 4, !dbg !239
  call void @llvm.dbg.value(metadata i32 %25, metadata !75, metadata !DIExpression()) #12, !dbg !231
  %26 = icmp eq i32 %25, 2429, !dbg !239
  %27 = select i1 %24, i1 %26, i1 false, !dbg !239
  call void @llvm.lifetime.end.p0i8(i64 4, i8* nonnull %22) #12, !dbg !240
  call void @llvm.lifetime.end.p0i8(i64 4, i8* nonnull %21) #12, !dbg !240
  %28 = select i1 %27, i8* getelementptr inbounds ([3 x i8], [3 x i8]* @str.10, i64 0, i64 0), i8* getelementptr inbounds ([7 x i8], [7 x i8]* @str, i64 0, i64 0), !dbg !241
  %29 = call i32 @puts(i8* nonnull dereferenceable(1) %28) #12, !dbg !242
  call void @llvm.dbg.value(metadata void (i32*, i32*)* %0, metadata !175, metadata !DIExpression()) #12, !dbg !243
  call void @llvm.dbg.value(metadata i8* getelementptr inbounds ([7 x i8], [7 x i8]* @.str.3, i64 0, i64 0), metadata !176, metadata !DIExpression()) #12, !dbg !243
  call void @llvm.dbg.value(metadata i32 8347853, metadata !177, metadata !DIExpression()) #12, !dbg !243
  call void @llvm.dbg.value(metadata i32 0, metadata !178, metadata !DIExpression()) #12, !dbg !243
  %30 = call i32 (i8*, ...) @printf(i8* noundef nonnull dereferenceable(1) getelementptr inbounds ([32 x i8], [32 x i8]* @.str, i64 0, i64 0), i8* noundef getelementptr inbounds ([7 x i8], [7 x i8]* @.str.3, i64 0, i64 0), i32 noundef 8347853, i32 noundef 0) #12, !dbg !245
  call void @llvm.dbg.value(metadata void (i32*, i32*)* %0, metadata !71, metadata !DIExpression()) #12, !dbg !246
  call void @llvm.dbg.value(metadata i32 8347853, metadata !72, metadata !DIExpression()) #12, !dbg !246
  call void @llvm.dbg.value(metadata i32 0, metadata !73, metadata !DIExpression()) #12, !dbg !246
  %31 = bitcast i32* %4 to i8*, !dbg !248
  call void @llvm.lifetime.start.p0i8(i64 4, i8* nonnull %31) #12, !dbg !248
  call void @llvm.dbg.value(metadata i32 8347853, metadata !74, metadata !DIExpression()) #12, !dbg !246
  store i32 8347853, i32* %4, align 4, !dbg !249, !tbaa !27
  %32 = bitcast i32* %5 to i8*, !dbg !248
  call void @llvm.lifetime.start.p0i8(i64 4, i8* nonnull %32) #12, !dbg !248
  call void @llvm.dbg.value(metadata i32 0, metadata !75, metadata !DIExpression()) #12, !dbg !246
  store i32 0, i32* %5, align 4, !dbg !250, !tbaa !27
  call void @llvm.dbg.value(metadata i32* %4, metadata !74, metadata !DIExpression(DW_OP_deref)) #12, !dbg !246
  call void @llvm.dbg.value(metadata i32* %5, metadata !75, metadata !DIExpression(DW_OP_deref)) #12, !dbg !246
  call void %0(i32* noundef nonnull %4, i32* noundef nonnull %5) #12, !dbg !251
  %33 = load i32, i32* %4, align 4, !dbg !252, !tbaa !27
  call void @llvm.dbg.value(metadata i32 %33, metadata !74, metadata !DIExpression()) #12, !dbg !246
  %34 = icmp eq i32 %33, 0, !dbg !253
  %35 = load i32, i32* %5, align 4, !dbg !254
  call void @llvm.dbg.value(metadata i32 %35, metadata !75, metadata !DIExpression()) #12, !dbg !246
  %36 = icmp eq i32 %35, 8347853, !dbg !254
  %37 = select i1 %34, i1 %36, i1 false, !dbg !254
  call void @llvm.lifetime.end.p0i8(i64 4, i8* nonnull %32) #12, !dbg !255
  call void @llvm.lifetime.end.p0i8(i64 4, i8* nonnull %31) #12, !dbg !255
  %38 = select i1 %37, i8* getelementptr inbounds ([3 x i8], [3 x i8]* @str.10, i64 0, i64 0), i8* getelementptr inbounds ([7 x i8], [7 x i8]* @str, i64 0, i64 0), !dbg !256
  %39 = call i32 @puts(i8* nonnull dereferenceable(1) %38) #12, !dbg !257
  call void @llvm.dbg.value(metadata void (i32*, i32*)* %0, metadata !175, metadata !DIExpression()) #12, !dbg !258
  call void @llvm.dbg.value(metadata i8* getelementptr inbounds ([7 x i8], [7 x i8]* @.str.3, i64 0, i64 0), metadata !176, metadata !DIExpression()) #12, !dbg !258
  call void @llvm.dbg.value(metadata i32 5, metadata !177, metadata !DIExpression()) #12, !dbg !258
  call void @llvm.dbg.value(metadata i32 5, metadata !178, metadata !DIExpression()) #12, !dbg !258
  %40 = call i32 (i8*, ...) @printf(i8* noundef nonnull dereferenceable(1) getelementptr inbounds ([32 x i8], [32 x i8]* @.str, i64 0, i64 0), i8* noundef getelementptr inbounds ([7 x i8], [7 x i8]* @.str.3, i64 0, i64 0), i32 noundef 5, i32 noundef 5) #12, !dbg !260
  call void @llvm.dbg.value(metadata void (i32*, i32*)* %0, metadata !71, metadata !DIExpression()) #12, !dbg !261
  call void @llvm.dbg.value(metadata i32 5, metadata !72, metadata !DIExpression()) #12, !dbg !261
  call void @llvm.dbg.value(metadata i32 5, metadata !73, metadata !DIExpression()) #12, !dbg !261
  %41 = bitcast i32* %2 to i8*, !dbg !263
  call void @llvm.lifetime.start.p0i8(i64 4, i8* nonnull %41) #12, !dbg !263
  call void @llvm.dbg.value(metadata i32 5, metadata !74, metadata !DIExpression()) #12, !dbg !261
  store i32 5, i32* %2, align 4, !dbg !264, !tbaa !27
  %42 = bitcast i32* %3 to i8*, !dbg !263
  call void @llvm.lifetime.start.p0i8(i64 4, i8* nonnull %42) #12, !dbg !263
  call void @llvm.dbg.value(metadata i32 5, metadata !75, metadata !DIExpression()) #12, !dbg !261
  store i32 5, i32* %3, align 4, !dbg !265, !tbaa !27
  call void @llvm.dbg.value(metadata i32* %2, metadata !74, metadata !DIExpression(DW_OP_deref)) #12, !dbg !261
  call void @llvm.dbg.value(metadata i32* %3, metadata !75, metadata !DIExpression(DW_OP_deref)) #12, !dbg !261
  call void %0(i32* noundef nonnull %2, i32* noundef nonnull %3) #12, !dbg !266
  %43 = load i32, i32* %2, align 4, !dbg !267, !tbaa !27
  call void @llvm.dbg.value(metadata i32 %43, metadata !74, metadata !DIExpression()) #12, !dbg !261
  %44 = icmp eq i32 %43, 5, !dbg !268
  %45 = load i32, i32* %3, align 4, !dbg !269
  call void @llvm.dbg.value(metadata i32 %45, metadata !75, metadata !DIExpression()) #12, !dbg !261
  %46 = icmp eq i32 %45, 5, !dbg !269
  %47 = select i1 %44, i1 %46, i1 false, !dbg !269
  call void @llvm.lifetime.end.p0i8(i64 4, i8* nonnull %42) #12, !dbg !270
  call void @llvm.lifetime.end.p0i8(i64 4, i8* nonnull %41) #12, !dbg !270
  %48 = select i1 %47, i8* getelementptr inbounds ([3 x i8], [3 x i8]* @str.10, i64 0, i64 0), i8* getelementptr inbounds ([7 x i8], [7 x i8]* @str, i64 0, i64 0), !dbg !271
  %49 = call i32 @puts(i8* nonnull dereferenceable(1) %48) #12, !dbg !272
  ret void, !dbg !273
}

; Function Attrs: nounwind ssp uwtable(sync)
define void @random_value_test(void (i32*, i32*)* nocapture noundef readonly %0) local_unnamed_addr #3 !dbg !274 {
  %2 = alloca i32, align 4
  %3 = alloca i32, align 4
  call void @llvm.dbg.value(metadata void (i32*, i32*)* %0, metadata !276, metadata !DIExpression()), !dbg !280
  %4 = call i64 @time(i64* noundef null) #12, !dbg !281
  %5 = trunc i64 %4 to i32, !dbg !281
  call void @srand(i32 noundef %5) #12, !dbg !282
  call void @llvm.dbg.value(metadata i32 0, metadata !277, metadata !DIExpression()), !dbg !283
  %6 = bitcast i32* %2 to i8*
  %7 = bitcast i32* %3 to i8*
  br label %9, !dbg !284

8:                                                ; preds = %9
  ret void, !dbg !285

9:                                                ; preds = %1, %9
  %10 = phi i32 [ 0, %1 ], [ %21, %9 ]
  call void @llvm.dbg.value(metadata i32 %10, metadata !277, metadata !DIExpression()), !dbg !283
  %11 = call i32 @rand() #12, !dbg !286
  %12 = call i32 @rand() #12, !dbg !289
  call void @llvm.dbg.value(metadata void (i32*, i32*)* %0, metadata !175, metadata !DIExpression()) #12, !dbg !290
  call void @llvm.dbg.value(metadata i8* getelementptr inbounds ([7 x i8], [7 x i8]* @.str.4, i64 0, i64 0), metadata !176, metadata !DIExpression()) #12, !dbg !290
  call void @llvm.dbg.value(metadata i32 %11, metadata !177, metadata !DIExpression()) #12, !dbg !290
  call void @llvm.dbg.value(metadata i32 %12, metadata !178, metadata !DIExpression()) #12, !dbg !290
  %13 = call i32 (i8*, ...) @printf(i8* noundef nonnull dereferenceable(1) getelementptr inbounds ([32 x i8], [32 x i8]* @.str, i64 0, i64 0), i8* noundef getelementptr inbounds ([7 x i8], [7 x i8]* @.str.4, i64 0, i64 0), i32 noundef %11, i32 noundef %12) #12, !dbg !292
  call void @llvm.dbg.value(metadata void (i32*, i32*)* %0, metadata !71, metadata !DIExpression()) #12, !dbg !293
  call void @llvm.dbg.value(metadata i32 %11, metadata !72, metadata !DIExpression()) #12, !dbg !293
  call void @llvm.dbg.value(metadata i32 %12, metadata !73, metadata !DIExpression()) #12, !dbg !293
  call void @llvm.lifetime.start.p0i8(i64 4, i8* nonnull %6) #12, !dbg !295
  call void @llvm.dbg.value(metadata i32 %11, metadata !74, metadata !DIExpression()) #12, !dbg !293
  store i32 %11, i32* %2, align 4, !dbg !296, !tbaa !27
  call void @llvm.lifetime.start.p0i8(i64 4, i8* nonnull %7) #12, !dbg !295
  call void @llvm.dbg.value(metadata i32 %12, metadata !75, metadata !DIExpression()) #12, !dbg !293
  store i32 %12, i32* %3, align 4, !dbg !297, !tbaa !27
  call void @llvm.dbg.value(metadata i32* %2, metadata !74, metadata !DIExpression(DW_OP_deref)) #12, !dbg !293
  call void @llvm.dbg.value(metadata i32* %3, metadata !75, metadata !DIExpression(DW_OP_deref)) #12, !dbg !293
  call void %0(i32* noundef nonnull %2, i32* noundef nonnull %3) #12, !dbg !298
  %14 = load i32, i32* %2, align 4, !dbg !299, !tbaa !27
  call void @llvm.dbg.value(metadata i32 %14, metadata !74, metadata !DIExpression()) #12, !dbg !293
  %15 = icmp eq i32 %14, %12, !dbg !300
  %16 = load i32, i32* %3, align 4, !dbg !301
  call void @llvm.dbg.value(metadata i32 %16, metadata !75, metadata !DIExpression()) #12, !dbg !293
  %17 = icmp eq i32 %16, %11, !dbg !301
  %18 = select i1 %15, i1 %17, i1 false, !dbg !301
  call void @llvm.lifetime.end.p0i8(i64 4, i8* nonnull %7) #12, !dbg !302
  call void @llvm.lifetime.end.p0i8(i64 4, i8* nonnull %6) #12, !dbg !302
  %19 = select i1 %18, i8* getelementptr inbounds ([3 x i8], [3 x i8]* @str.10, i64 0, i64 0), i8* getelementptr inbounds ([7 x i8], [7 x i8]* @str, i64 0, i64 0)
  %20 = call i32 @puts(i8* nonnull dereferenceable(1) %19) #12, !dbg !303
  %21 = add nuw nsw i32 %10, 1, !dbg !304
  call void @llvm.dbg.value(metadata i32 %21, metadata !277, metadata !DIExpression()), !dbg !283
  %22 = icmp eq i32 %21, 100, !dbg !305
  br i1 %22, label %8, label %9, !dbg !284, !llvm.loop !306
}

declare !dbg !310 void @srand(i32 noundef) local_unnamed_addr #9

declare !dbg !315 i64 @time(i64* noundef) local_unnamed_addr #9

declare !dbg !325 i32 @rand() local_unnamed_addr #9

; Function Attrs: nounwind ssp uwtable(sync)
define i32 @main() local_unnamed_addr #3 !dbg !328 {
  %1 = call i32 @puts(i8* nonnull dereferenceable(1) getelementptr inbounds ([38 x i8], [38 x i8]* @str.11, i64 0, i64 0)), !dbg !329
  call void @llvm.dbg.value(metadata void (i32*, i32*)* @swap, metadata !212, metadata !DIExpression()) #12, !dbg !330
  call void @llvm.dbg.value(metadata void (i32*, i32*)* @swap, metadata !175, metadata !DIExpression()) #12, !dbg !332
  call void @llvm.dbg.value(metadata i8* getelementptr inbounds ([7 x i8], [7 x i8]* @.str.3, i64 0, i64 0), metadata !176, metadata !DIExpression()) #12, !dbg !332
  call void @llvm.dbg.value(metadata i32 1, metadata !177, metadata !DIExpression()) #12, !dbg !332
  call void @llvm.dbg.value(metadata i32 2, metadata !178, metadata !DIExpression()) #12, !dbg !332
  %2 = call i32 (i8*, ...) @printf(i8* noundef nonnull dereferenceable(1) getelementptr inbounds ([32 x i8], [32 x i8]* @.str, i64 0, i64 0), i8* noundef getelementptr inbounds ([7 x i8], [7 x i8]* @.str.3, i64 0, i64 0), i32 noundef 1, i32 noundef 2) #12, !dbg !334
  %3 = call i32 @puts(i8* nonnull dereferenceable(1) getelementptr inbounds ([3 x i8], [3 x i8]* @str.10, i64 0, i64 0)) #12, !dbg !335
  call void @llvm.dbg.value(metadata void (i32*, i32*)* @swap, metadata !175, metadata !DIExpression()) #12, !dbg !336
  call void @llvm.dbg.value(metadata i8* getelementptr inbounds ([7 x i8], [7 x i8]* @.str.3, i64 0, i64 0), metadata !176, metadata !DIExpression()) #12, !dbg !336
  call void @llvm.dbg.value(metadata i32 2429, metadata !177, metadata !DIExpression()) #12, !dbg !336
  call void @llvm.dbg.value(metadata i32 98423, metadata !178, metadata !DIExpression()) #12, !dbg !336
  %4 = call i32 (i8*, ...) @printf(i8* noundef nonnull dereferenceable(1) getelementptr inbounds ([32 x i8], [32 x i8]* @.str, i64 0, i64 0), i8* noundef getelementptr inbounds ([7 x i8], [7 x i8]* @.str.3, i64 0, i64 0), i32 noundef 2429, i32 noundef 98423) #12, !dbg !338
  %5 = call i32 @puts(i8* nonnull dereferenceable(1) getelementptr inbounds ([3 x i8], [3 x i8]* @str.10, i64 0, i64 0)) #12, !dbg !339
  call void @llvm.dbg.value(metadata void (i32*, i32*)* @swap, metadata !175, metadata !DIExpression()) #12, !dbg !340
  call void @llvm.dbg.value(metadata i8* getelementptr inbounds ([7 x i8], [7 x i8]* @.str.3, i64 0, i64 0), metadata !176, metadata !DIExpression()) #12, !dbg !340
  call void @llvm.dbg.value(metadata i32 8347853, metadata !177, metadata !DIExpression()) #12, !dbg !340
  call void @llvm.dbg.value(metadata i32 0, metadata !178, metadata !DIExpression()) #12, !dbg !340
  %6 = call i32 (i8*, ...) @printf(i8* noundef nonnull dereferenceable(1) getelementptr inbounds ([32 x i8], [32 x i8]* @.str, i64 0, i64 0), i8* noundef getelementptr inbounds ([7 x i8], [7 x i8]* @.str.3, i64 0, i64 0), i32 noundef 8347853, i32 noundef 0) #12, !dbg !342
  %7 = call i32 @puts(i8* nonnull dereferenceable(1) getelementptr inbounds ([3 x i8], [3 x i8]* @str.10, i64 0, i64 0)) #12, !dbg !343
  call void @llvm.dbg.value(metadata void (i32*, i32*)* @swap, metadata !175, metadata !DIExpression()) #12, !dbg !344
  call void @llvm.dbg.value(metadata i8* getelementptr inbounds ([7 x i8], [7 x i8]* @.str.3, i64 0, i64 0), metadata !176, metadata !DIExpression()) #12, !dbg !344
  call void @llvm.dbg.value(metadata i32 5, metadata !177, metadata !DIExpression()) #12, !dbg !344
  call void @llvm.dbg.value(metadata i32 5, metadata !178, metadata !DIExpression()) #12, !dbg !344
  %8 = call i32 (i8*, ...) @printf(i8* noundef nonnull dereferenceable(1) getelementptr inbounds ([32 x i8], [32 x i8]* @.str, i64 0, i64 0), i8* noundef getelementptr inbounds ([7 x i8], [7 x i8]* @.str.3, i64 0, i64 0), i32 noundef 5, i32 noundef 5) #12, !dbg !346
  %9 = call i32 @puts(i8* nonnull dereferenceable(1) getelementptr inbounds ([3 x i8], [3 x i8]* @str.10, i64 0, i64 0)) #12, !dbg !347
  %10 = call i32 @putchar(i32 10), !dbg !348
  %11 = call i32 @puts(i8* nonnull dereferenceable(1) getelementptr inbounds ([35 x i8], [35 x i8]* @str.12, i64 0, i64 0)), !dbg !349
  %12 = call i32 @puts(i8* nonnull dereferenceable(1) getelementptr inbounds ([32 x i8], [32 x i8]* @str.13, i64 0, i64 0)), !dbg !350
  call void @llvm.dbg.value(metadata void (i32*, i32*)* @swap, metadata !276, metadata !DIExpression()) #12, !dbg !351
  %13 = call i64 @time(i64* noundef null) #12, !dbg !353
  %14 = trunc i64 %13 to i32, !dbg !353
  call void @srand(i32 noundef %14) #12, !dbg !354
  call void @llvm.dbg.value(metadata i32 0, metadata !277, metadata !DIExpression()) #12, !dbg !355
  br label %15, !dbg !356

15:                                               ; preds = %15, %0
  %16 = phi i32 [ 0, %0 ], [ %21, %15 ]
  call void @llvm.dbg.value(metadata i32 %16, metadata !277, metadata !DIExpression()) #12, !dbg !355
  %17 = call i32 @rand() #12, !dbg !357
  %18 = call i32 @rand() #12, !dbg !358
  call void @llvm.dbg.value(metadata void (i32*, i32*)* @swap, metadata !175, metadata !DIExpression()) #12, !dbg !359
  call void @llvm.dbg.value(metadata i8* getelementptr inbounds ([7 x i8], [7 x i8]* @.str.4, i64 0, i64 0), metadata !176, metadata !DIExpression()) #12, !dbg !359
  call void @llvm.dbg.value(metadata i32 %17, metadata !177, metadata !DIExpression()) #12, !dbg !359
  call void @llvm.dbg.value(metadata i32 %18, metadata !178, metadata !DIExpression()) #12, !dbg !359
  %19 = call i32 (i8*, ...) @printf(i8* noundef nonnull dereferenceable(1) getelementptr inbounds ([32 x i8], [32 x i8]* @.str, i64 0, i64 0), i8* noundef getelementptr inbounds ([7 x i8], [7 x i8]* @.str.4, i64 0, i64 0), i32 noundef %17, i32 noundef %18) #12, !dbg !361
  %20 = call i32 @puts(i8* nonnull dereferenceable(1) getelementptr inbounds ([3 x i8], [3 x i8]* @str.10, i64 0, i64 0)) #12, !dbg !362
  %21 = add nuw nsw i32 %16, 1, !dbg !363
  call void @llvm.dbg.value(metadata i32 %21, metadata !277, metadata !DIExpression()) #12, !dbg !355
  %22 = icmp eq i32 %21, 100, !dbg !364
  br i1 %22, label %23, label %15, !dbg !356, !llvm.loop !365

23:                                               ; preds = %15
  %24 = call i32 @puts(i8* nonnull dereferenceable(1) getelementptr inbounds ([29 x i8], [29 x i8]* @str.14, i64 0, i64 0)), !dbg !367
  ret i32 0, !dbg !368
}

; Function Attrs: nocallback nofree nosync nounwind readnone speculatable willreturn
declare void @llvm.dbg.value(metadata, metadata, metadata) #10

; Function Attrs: nofree nounwind
declare noundef i32 @puts(i8* nocapture noundef readonly) local_unnamed_addr #11

; Function Attrs: nofree nounwind
declare noundef i32 @putchar(i32 noundef) local_unnamed_addr #11

attributes #0 = { argmemonly mustprogress nofree norecurse nosync nounwind ssp willreturn uwtable(sync) "frame-pointer"="non-leaf" "min-legal-vector-width"="0" "no-trapping-math"="true" "probe-stack"="__chkstk_darwin" "stack-protector-buffer-size"="8" "target-cpu"="apple-m1" "target-features"="+aes,+crc,+crypto,+dotprod,+fp-armv8,+fp16fml,+fullfp16,+lse,+neon,+ras,+rcpc,+rdm,+sha2,+sha3,+sm4,+v8.5a,+zcm,+zcz" }
attributes #1 = { argmemonly mustprogress nocallback nofree nosync nounwind willreturn }
attributes #2 = { mustprogress nofree nosync nounwind readnone ssp willreturn uwtable(sync) "frame-pointer"="non-leaf" "min-legal-vector-width"="0" "no-trapping-math"="true" "probe-stack"="__chkstk_darwin" "stack-protector-buffer-size"="8" "target-cpu"="apple-m1" "target-features"="+aes,+crc,+crypto,+dotprod,+fp-armv8,+fp16fml,+fullfp16,+lse,+neon,+ras,+rcpc,+rdm,+sha2,+sha3,+sm4,+v8.5a,+zcm,+zcz" }
attributes #3 = { nounwind ssp uwtable(sync) "frame-pointer"="non-leaf" "min-legal-vector-width"="0" "no-trapping-math"="true" "probe-stack"="__chkstk_darwin" "stack-protector-buffer-size"="8" "target-cpu"="apple-m1" "target-features"="+aes,+crc,+crypto,+dotprod,+fp-armv8,+fp16fml,+fullfp16,+lse,+neon,+ras,+rcpc,+rdm,+sha2,+sha3,+sm4,+v8.5a,+zcm,+zcz" }
attributes #4 = { mustprogress nofree norecurse nosync nounwind readnone ssp willreturn uwtable(sync) "frame-pointer"="non-leaf" "min-legal-vector-width"="0" "no-trapping-math"="true" "probe-stack"="__chkstk_darwin" "stack-protector-buffer-size"="8" "target-cpu"="apple-m1" "target-features"="+aes,+crc,+crypto,+dotprod,+fp-armv8,+fp16fml,+fullfp16,+lse,+neon,+ras,+rcpc,+rdm,+sha2,+sha3,+sm4,+v8.5a,+zcm,+zcz" }
attributes #5 = { mustprogress nofree norecurse nosync nounwind ssp willreturn uwtable(sync) "frame-pointer"="non-leaf" "min-legal-vector-width"="0" "no-trapping-math"="true" "probe-stack"="__chkstk_darwin" "stack-protector-buffer-size"="8" "target-cpu"="apple-m1" "target-features"="+aes,+crc,+crypto,+dotprod,+fp-armv8,+fp16fml,+fullfp16,+lse,+neon,+ras,+rcpc,+rdm,+sha2,+sha3,+sm4,+v8.5a,+zcm,+zcz" }
attributes #6 = { mustprogress nofree nosync nounwind ssp willreturn uwtable(sync) "frame-pointer"="non-leaf" "min-legal-vector-width"="0" "no-trapping-math"="true" "probe-stack"="__chkstk_darwin" "stack-protector-buffer-size"="8" "target-cpu"="apple-m1" "target-features"="+aes,+crc,+crypto,+dotprod,+fp-armv8,+fp16fml,+fullfp16,+lse,+neon,+ras,+rcpc,+rdm,+sha2,+sha3,+sm4,+v8.5a,+zcm,+zcz" }
attributes #7 = { nofree nounwind "frame-pointer"="non-leaf" "no-trapping-math"="true" "probe-stack"="__chkstk_darwin" "stack-protector-buffer-size"="8" "target-cpu"="apple-m1" "target-features"="+aes,+crc,+crypto,+dotprod,+fp-armv8,+fp16fml,+fullfp16,+lse,+neon,+ras,+rcpc,+rdm,+sha2,+sha3,+sm4,+v8.5a,+zcm,+zcz" }
attributes #8 = { nofree nounwind ssp uwtable(sync) "frame-pointer"="non-leaf" "min-legal-vector-width"="0" "no-trapping-math"="true" "probe-stack"="__chkstk_darwin" "stack-protector-buffer-size"="8" "target-cpu"="apple-m1" "target-features"="+aes,+crc,+crypto,+dotprod,+fp-armv8,+fp16fml,+fullfp16,+lse,+neon,+ras,+rcpc,+rdm,+sha2,+sha3,+sm4,+v8.5a,+zcm,+zcz" }
attributes #9 = { "frame-pointer"="non-leaf" "no-trapping-math"="true" "probe-stack"="__chkstk_darwin" "stack-protector-buffer-size"="8" "target-cpu"="apple-m1" "target-features"="+aes,+crc,+crypto,+dotprod,+fp-armv8,+fp16fml,+fullfp16,+lse,+neon,+ras,+rcpc,+rdm,+sha2,+sha3,+sm4,+v8.5a,+zcm,+zcz" }
attributes #10 = { nocallback nofree nosync nounwind readnone speculatable willreturn }
attributes #11 = { nofree nounwind }
attributes #12 = { nounwind }

!llvm.module.flags = !{!0, !1, !2, !3, !4, !5, !6, !7, !8, !9, !10}
!llvm.dbg.cu = !{!11}
!llvm.ident = !{!13}

!0 = !{i32 2, !"SDK Version", [2 x i32] [i32 13, i32 3]}
!1 = !{i32 7, !"Dwarf Version", i32 4}
!2 = !{i32 2, !"Debug Info Version", i32 3}
!3 = !{i32 1, !"wchar_size", i32 4}
!4 = !{i32 8, !"branch-target-enforcement", i32 0}
!5 = !{i32 8, !"sign-return-address", i32 0}
!6 = !{i32 8, !"sign-return-address-all", i32 0}
!7 = !{i32 8, !"sign-return-address-with-bkey", i32 0}
!8 = !{i32 7, !"PIC Level", i32 2}
!9 = !{i32 7, !"uwtable", i32 1}
!10 = !{i32 7, !"frame-pointer", i32 1}
!11 = distinct !DICompileUnit(language: DW_LANG_C99, file: !12, producer: "Apple clang version 14.0.3 (clang-1403.0.22.14.1)", isOptimized: true, runtimeVersion: 0, emissionKind: FullDebug, splitDebugInlining: false, nameTableKind: None, sysroot: "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk", sdk: "MacOSX.sdk")
!12 = !DIFile(filename: "swap.c", directory: "/Users/mccleeary/dev/examples/intro")
!13 = !{!"Apple clang version 14.0.3 (clang-1403.0.22.14.1)"}
!14 = distinct !DISubprogram(name: "swap", scope: !12, file: !12, line: 9, type: !15, scopeLine: 9, flags: DIFlagPrototyped | DIFlagAllCallsDescribed, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !11, retainedNodes: !21)
!15 = !DISubroutineType(types: !16)
!16 = !{null, !17, !17}
!17 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !18, size: 64)
!18 = !DIDerivedType(tag: DW_TAG_typedef, name: "uint32_t", file: !19, line: 31, baseType: !20)
!19 = !DIFile(filename: "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include/_types/_uint32_t.h", directory: "")
!20 = !DIBasicType(name: "unsigned int", size: 32, encoding: DW_ATE_unsigned)
!21 = !{!22, !23, !24}
!22 = !DILocalVariable(name: "x", arg: 1, scope: !14, file: !12, line: 9, type: !17)
!23 = !DILocalVariable(name: "y", arg: 2, scope: !14, file: !12, line: 9, type: !17)
!24 = !DILocalVariable(name: "tmp", scope: !14, file: !12, line: 10, type: !18)
!25 = !DILocation(line: 0, scope: !14)
!26 = !DILocation(line: 10, column: 20, scope: !14)
!27 = !{!28, !28, i64 0}
!28 = !{!"int", !29, i64 0}
!29 = !{!"omnipotent char", !30, i64 0}
!30 = !{!"Simple C/C++ TBAA"}
!31 = !DILocation(line: 11, column: 10, scope: !14)
!32 = !DILocation(line: 11, column: 8, scope: !14)
!33 = !DILocation(line: 12, column: 8, scope: !14)
!34 = !DILocation(line: 13, column: 1, scope: !14)
!35 = distinct !DISubprogram(name: "swap_spec", scope: !12, file: !12, line: 15, type: !36, scopeLine: 15, flags: DIFlagPrototyped | DIFlagAllCallsDescribed, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !11, retainedNodes: !39)
!36 = !DISubroutineType(types: !37)
!37 = !{!38, !18, !18}
!38 = !DIBasicType(name: "_Bool", size: 8, encoding: DW_ATE_boolean)
!39 = !{!40, !41, !42, !43}
!40 = !DILocalVariable(name: "a", arg: 1, scope: !35, file: !12, line: 15, type: !18)
!41 = !DILocalVariable(name: "b", arg: 2, scope: !35, file: !12, line: 15, type: !18)
!42 = !DILocalVariable(name: "x", scope: !35, file: !12, line: 16, type: !18)
!43 = !DILocalVariable(name: "y", scope: !35, file: !12, line: 16, type: !18)
!44 = !DILocation(line: 0, scope: !35)
!45 = !DILocation(line: 18, column: 5, scope: !35)
!46 = distinct !DISubprogram(name: "xor_swap", scope: !12, file: !12, line: 21, type: !15, scopeLine: 21, flags: DIFlagPrototyped | DIFlagAllCallsDescribed, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !11, retainedNodes: !47)
!47 = !{!48, !49}
!48 = !DILocalVariable(name: "x", arg: 1, scope: !46, file: !12, line: 21, type: !17)
!49 = !DILocalVariable(name: "y", arg: 2, scope: !46, file: !12, line: 21, type: !17)
!50 = !DILocation(line: 0, scope: !46)
!51 = !DILocation(line: 22, column: 11, scope: !46)
!52 = !DILocation(line: 22, column: 8, scope: !46)
!53 = !DILocation(line: 23, column: 8, scope: !46)
!54 = !DILocation(line: 24, column: 8, scope: !46)
!55 = !DILocation(line: 25, column: 1, scope: !46)
!56 = distinct !DISubprogram(name: "xor_swap_spec", scope: !12, file: !12, line: 27, type: !36, scopeLine: 27, flags: DIFlagPrototyped | DIFlagAllCallsDescribed, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !11, retainedNodes: !57)
!57 = !{!58, !59, !60, !61, !62, !63}
!58 = !DILocalVariable(name: "a", arg: 1, scope: !56, file: !12, line: 27, type: !18)
!59 = !DILocalVariable(name: "b", arg: 2, scope: !56, file: !12, line: 27, type: !18)
!60 = !DILocalVariable(name: "a1", scope: !56, file: !12, line: 28, type: !18)
!61 = !DILocalVariable(name: "a2", scope: !56, file: !12, line: 28, type: !18)
!62 = !DILocalVariable(name: "b1", scope: !56, file: !12, line: 29, type: !18)
!63 = !DILocalVariable(name: "b2", scope: !56, file: !12, line: 29, type: !18)
!64 = !DILocation(line: 0, scope: !56)
!65 = !DILocation(line: 32, column: 5, scope: !56)
!66 = distinct !DISubprogram(name: "general_swap_spec", scope: !12, file: !12, line: 36, type: !67, scopeLine: 36, flags: DIFlagPrototyped | DIFlagAllCallsDescribed, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !11, retainedNodes: !70)
!67 = !DISubroutineType(types: !68)
!68 = !{!38, !69, !18, !18}
!69 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !15, size: 64)
!70 = !{!71, !72, !73, !74, !75}
!71 = !DILocalVariable(name: "fun", arg: 1, scope: !66, file: !12, line: 36, type: !69)
!72 = !DILocalVariable(name: "a", arg: 2, scope: !66, file: !12, line: 36, type: !18)
!73 = !DILocalVariable(name: "b", arg: 3, scope: !66, file: !12, line: 36, type: !18)
!74 = !DILocalVariable(name: "x", scope: !66, file: !12, line: 37, type: !18)
!75 = !DILocalVariable(name: "y", scope: !66, file: !12, line: 37, type: !18)
!76 = !DILocation(line: 0, scope: !66)
!77 = !DILocation(line: 37, column: 5, scope: !66)
!78 = !DILocation(line: 37, column: 14, scope: !66)
!79 = !DILocation(line: 37, column: 21, scope: !66)
!80 = !DILocation(line: 38, column: 5, scope: !66)
!81 = !DILocation(line: 39, column: 12, scope: !66)
!82 = !DILocation(line: 39, column: 14, scope: !66)
!83 = !DILocation(line: 39, column: 19, scope: !66)
!84 = !DILocation(line: 40, column: 1, scope: !66)
!85 = !DILocation(line: 39, column: 5, scope: !66)
!86 = distinct !DISubprogram(name: "swap_broken1", scope: !12, file: !12, line: 42, type: !15, scopeLine: 42, flags: DIFlagPrototyped | DIFlagAllCallsDescribed, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !11, retainedNodes: !87)
!87 = !{!88, !89, !90}
!88 = !DILocalVariable(name: "x", arg: 1, scope: !86, file: !12, line: 42, type: !17)
!89 = !DILocalVariable(name: "y", arg: 2, scope: !86, file: !12, line: 42, type: !17)
!90 = !DILocalVariable(name: "tmp", scope: !86, file: !12, line: 43, type: !18)
!91 = !DILocation(line: 0, scope: !86)
!92 = !DILocation(line: 46, column: 1, scope: !86)
!93 = distinct !DISubprogram(name: "swap_broken1_spec", scope: !12, file: !12, line: 48, type: !36, scopeLine: 48, flags: DIFlagPrototyped | DIFlagAllCallsDescribed, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !11, retainedNodes: !94)
!94 = !{!95, !96, !97, !98}
!95 = !DILocalVariable(name: "a", arg: 1, scope: !93, file: !12, line: 48, type: !18)
!96 = !DILocalVariable(name: "b", arg: 2, scope: !93, file: !12, line: 48, type: !18)
!97 = !DILocalVariable(name: "x", scope: !93, file: !12, line: 49, type: !18)
!98 = !DILocalVariable(name: "y", scope: !93, file: !12, line: 49, type: !18)
!99 = !DILocation(line: 0, scope: !93)
!100 = !DILocation(line: 51, column: 14, scope: !93)
!101 = !DILocation(line: 51, column: 5, scope: !93)
!102 = distinct !DISubprogram(name: "swap_broken2", scope: !12, file: !12, line: 55, type: !15, scopeLine: 55, flags: DIFlagPrototyped | DIFlagAllCallsDescribed, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !11, retainedNodes: !103)
!103 = !{!104, !105, !106}
!104 = !DILocalVariable(name: "x", arg: 1, scope: !102, file: !12, line: 55, type: !17)
!105 = !DILocalVariable(name: "y", arg: 2, scope: !102, file: !12, line: 55, type: !17)
!106 = !DILocalVariable(name: "tmp", scope: !102, file: !12, line: 56, type: !18)
!107 = !DILocation(line: 0, scope: !102)
!108 = !DILocation(line: 56, column: 20, scope: !102)
!109 = !DILocation(line: 57, column: 12, scope: !110)
!110 = distinct !DILexicalBlock(scope: !102, file: !12, line: 57, column: 9)
!111 = !DILocation(line: 57, column: 9, scope: !102)
!112 = !DILocation(line: 58, column: 14, scope: !113)
!113 = distinct !DILexicalBlock(scope: !110, file: !12, line: 57, column: 24)
!114 = !DILocation(line: 58, column: 12, scope: !113)
!115 = !DILocation(line: 59, column: 12, scope: !113)
!116 = !DILocation(line: 60, column: 5, scope: !113)
!117 = !DILocation(line: 61, column: 1, scope: !102)
!118 = distinct !DISubprogram(name: "swap_broken2_spec", scope: !12, file: !12, line: 63, type: !36, scopeLine: 63, flags: DIFlagPrototyped | DIFlagAllCallsDescribed, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !11, retainedNodes: !119)
!119 = !{!120, !121, !122, !123}
!120 = !DILocalVariable(name: "a", arg: 1, scope: !118, file: !12, line: 63, type: !18)
!121 = !DILocalVariable(name: "b", arg: 2, scope: !118, file: !12, line: 63, type: !18)
!122 = !DILocalVariable(name: "x", scope: !118, file: !12, line: 64, type: !18)
!123 = !DILocalVariable(name: "y", scope: !118, file: !12, line: 64, type: !18)
!124 = !DILocation(line: 0, scope: !118)
!125 = !DILocation(line: 0, scope: !102, inlinedAt: !126)
!126 = distinct !DILocation(line: 65, column: 5, scope: !118)
!127 = !DILocation(line: 57, column: 12, scope: !110, inlinedAt: !126)
!128 = !DILocation(line: 57, column: 9, scope: !102, inlinedAt: !126)
!129 = !DILocation(line: 66, column: 14, scope: !118)
!130 = !DILocation(line: 66, column: 19, scope: !118)
!131 = !DILocation(line: 66, column: 5, scope: !118)
!132 = distinct !DISubprogram(name: "swap_broken3", scope: !12, file: !12, line: 70, type: !15, scopeLine: 70, flags: DIFlagPrototyped | DIFlagAllCallsDescribed, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !11, retainedNodes: !133)
!133 = !{!134, !135, !136}
!134 = !DILocalVariable(name: "x", arg: 1, scope: !132, file: !12, line: 70, type: !17)
!135 = !DILocalVariable(name: "y", arg: 2, scope: !132, file: !12, line: 70, type: !17)
!136 = !DILocalVariable(name: "tmp", scope: !132, file: !12, line: 71, type: !18)
!137 = !DILocation(line: 0, scope: !132)
!138 = !DILocation(line: 71, column: 20, scope: !132)
!139 = !DILocation(line: 72, column: 9, scope: !140)
!140 = distinct !DILexicalBlock(scope: !132, file: !12, line: 72, column: 9)
!141 = !DILocation(line: 72, column: 12, scope: !140)
!142 = !DILocation(line: 72, column: 22, scope: !140)
!143 = !DILocation(line: 72, column: 25, scope: !140)
!144 = !DILocation(line: 72, column: 18, scope: !140)
!145 = !DILocation(line: 72, column: 9, scope: !132)
!146 = !DILocation(line: 75, column: 10, scope: !132)
!147 = !DILocation(line: 75, column: 8, scope: !132)
!148 = !DILocation(line: 76, column: 8, scope: !132)
!149 = !DILocation(line: 77, column: 1, scope: !132)
!150 = distinct !DISubprogram(name: "swap_broken3_spec", scope: !12, file: !12, line: 79, type: !36, scopeLine: 79, flags: DIFlagPrototyped | DIFlagAllCallsDescribed, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !11, retainedNodes: !151)
!151 = !{!152, !153, !154, !155}
!152 = !DILocalVariable(name: "a", arg: 1, scope: !150, file: !12, line: 79, type: !18)
!153 = !DILocalVariable(name: "b", arg: 2, scope: !150, file: !12, line: 79, type: !18)
!154 = !DILocalVariable(name: "x", scope: !150, file: !12, line: 80, type: !18)
!155 = !DILocalVariable(name: "y", scope: !150, file: !12, line: 80, type: !18)
!156 = !DILocation(line: 0, scope: !150)
!157 = !DILocation(line: 80, column: 5, scope: !150)
!158 = !DILocation(line: 80, column: 21, scope: !150)
!159 = !DILocation(line: 0, scope: !132, inlinedAt: !160)
!160 = distinct !DILocation(line: 81, column: 5, scope: !150)
!161 = !DILocation(line: 72, column: 9, scope: !140, inlinedAt: !160)
!162 = !DILocation(line: 72, column: 12, scope: !140, inlinedAt: !160)
!163 = !DILocation(line: 75, column: 10, scope: !132, inlinedAt: !160)
!164 = !DILocation(line: 76, column: 8, scope: !132, inlinedAt: !160)
!165 = !DILocation(line: 82, column: 14, scope: !150)
!166 = !DILocation(line: 82, column: 19, scope: !150)
!167 = !DILocation(line: 83, column: 1, scope: !150)
!168 = !DILocation(line: 82, column: 5, scope: !150)
!169 = distinct !DISubprogram(name: "test_swap_function", scope: !12, file: !12, line: 87, type: !170, scopeLine: 87, flags: DIFlagPrototyped | DIFlagAllCallsDescribed, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !11, retainedNodes: !174)
!170 = !DISubroutineType(types: !171)
!171 = !{null, !69, !172, !18, !18}
!172 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !173, size: 64)
!173 = !DIBasicType(name: "char", size: 8, encoding: DW_ATE_signed_char)
!174 = !{!175, !176, !177, !178}
!175 = !DILocalVariable(name: "fun", arg: 1, scope: !169, file: !12, line: 87, type: !69)
!176 = !DILocalVariable(name: "descr", arg: 2, scope: !169, file: !12, line: 87, type: !172)
!177 = !DILocalVariable(name: "x", arg: 3, scope: !169, file: !12, line: 87, type: !18)
!178 = !DILocalVariable(name: "y", arg: 4, scope: !169, file: !12, line: 87, type: !18)
!179 = !DILocation(line: 0, scope: !169)
!180 = !DILocation(line: 88, column: 5, scope: !169)
!181 = !DILocation(line: 0, scope: !66, inlinedAt: !182)
!182 = distinct !DILocation(line: 89, column: 9, scope: !183)
!183 = distinct !DILexicalBlock(scope: !169, file: !12, line: 89, column: 9)
!184 = !DILocation(line: 37, column: 5, scope: !66, inlinedAt: !182)
!185 = !DILocation(line: 37, column: 14, scope: !66, inlinedAt: !182)
!186 = !DILocation(line: 37, column: 21, scope: !66, inlinedAt: !182)
!187 = !DILocation(line: 38, column: 5, scope: !66, inlinedAt: !182)
!188 = !DILocation(line: 39, column: 12, scope: !66, inlinedAt: !182)
!189 = !DILocation(line: 39, column: 14, scope: !66, inlinedAt: !182)
!190 = !DILocation(line: 39, column: 19, scope: !66, inlinedAt: !182)
!191 = !DILocation(line: 40, column: 1, scope: !66, inlinedAt: !182)
!192 = !DILocation(line: 0, scope: !183)
!193 = !DILocation(line: 94, column: 1, scope: !169)
!194 = distinct !DISubprogram(name: "test_swap", scope: !12, file: !12, line: 96, type: !195, scopeLine: 96, flags: DIFlagPrototyped | DIFlagAllCallsDescribed, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !11, retainedNodes: !197)
!195 = !DISubroutineType(types: !196)
!196 = !{null, !172, !18, !18}
!197 = !{!198, !199, !200}
!198 = !DILocalVariable(name: "descr", arg: 1, scope: !194, file: !12, line: 96, type: !172)
!199 = !DILocalVariable(name: "x", arg: 2, scope: !194, file: !12, line: 96, type: !18)
!200 = !DILocalVariable(name: "y", arg: 3, scope: !194, file: !12, line: 96, type: !18)
!201 = !DILocation(line: 0, scope: !194)
!202 = !DILocation(line: 0, scope: !169, inlinedAt: !203)
!203 = distinct !DILocation(line: 97, column: 5, scope: !194)
!204 = !DILocation(line: 88, column: 5, scope: !169, inlinedAt: !203)
!205 = !DILocation(line: 90, column: 9, scope: !206, inlinedAt: !203)
!206 = distinct !DILexicalBlock(scope: !183, file: !12, line: 89, column: 39)
!207 = !DILocation(line: 98, column: 1, scope: !194)
!208 = distinct !DISubprogram(name: "chosen_value_test", scope: !12, file: !12, line: 100, type: !209, scopeLine: 100, flags: DIFlagPrototyped | DIFlagAllCallsDescribed, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !11, retainedNodes: !211)
!209 = !DISubroutineType(types: !210)
!210 = !{null, !69}
!211 = !{!212}
!212 = !DILocalVariable(name: "fun", arg: 1, scope: !208, file: !12, line: 100, type: !69)
!213 = !DILocation(line: 0, scope: !208)
!214 = !DILocation(line: 0, scope: !169, inlinedAt: !215)
!215 = distinct !DILocation(line: 101, column: 5, scope: !208)
!216 = !DILocation(line: 88, column: 5, scope: !169, inlinedAt: !215)
!217 = !DILocation(line: 0, scope: !66, inlinedAt: !218)
!218 = distinct !DILocation(line: 89, column: 9, scope: !183, inlinedAt: !215)
!219 = !DILocation(line: 37, column: 5, scope: !66, inlinedAt: !218)
!220 = !DILocation(line: 37, column: 14, scope: !66, inlinedAt: !218)
!221 = !DILocation(line: 37, column: 21, scope: !66, inlinedAt: !218)
!222 = !DILocation(line: 38, column: 5, scope: !66, inlinedAt: !218)
!223 = !DILocation(line: 39, column: 12, scope: !66, inlinedAt: !218)
!224 = !DILocation(line: 39, column: 14, scope: !66, inlinedAt: !218)
!225 = !DILocation(line: 39, column: 19, scope: !66, inlinedAt: !218)
!226 = !DILocation(line: 40, column: 1, scope: !66, inlinedAt: !218)
!227 = !DILocation(line: 0, scope: !183, inlinedAt: !215)
!228 = !DILocation(line: 0, scope: !169, inlinedAt: !229)
!229 = distinct !DILocation(line: 102, column: 5, scope: !208)
!230 = !DILocation(line: 88, column: 5, scope: !169, inlinedAt: !229)
!231 = !DILocation(line: 0, scope: !66, inlinedAt: !232)
!232 = distinct !DILocation(line: 89, column: 9, scope: !183, inlinedAt: !229)
!233 = !DILocation(line: 37, column: 5, scope: !66, inlinedAt: !232)
!234 = !DILocation(line: 37, column: 14, scope: !66, inlinedAt: !232)
!235 = !DILocation(line: 37, column: 21, scope: !66, inlinedAt: !232)
!236 = !DILocation(line: 38, column: 5, scope: !66, inlinedAt: !232)
!237 = !DILocation(line: 39, column: 12, scope: !66, inlinedAt: !232)
!238 = !DILocation(line: 39, column: 14, scope: !66, inlinedAt: !232)
!239 = !DILocation(line: 39, column: 19, scope: !66, inlinedAt: !232)
!240 = !DILocation(line: 40, column: 1, scope: !66, inlinedAt: !232)
!241 = !DILocation(line: 89, column: 9, scope: !169, inlinedAt: !229)
!242 = !DILocation(line: 0, scope: !183, inlinedAt: !229)
!243 = !DILocation(line: 0, scope: !169, inlinedAt: !244)
!244 = distinct !DILocation(line: 103, column: 5, scope: !208)
!245 = !DILocation(line: 88, column: 5, scope: !169, inlinedAt: !244)
!246 = !DILocation(line: 0, scope: !66, inlinedAt: !247)
!247 = distinct !DILocation(line: 89, column: 9, scope: !183, inlinedAt: !244)
!248 = !DILocation(line: 37, column: 5, scope: !66, inlinedAt: !247)
!249 = !DILocation(line: 37, column: 14, scope: !66, inlinedAt: !247)
!250 = !DILocation(line: 37, column: 21, scope: !66, inlinedAt: !247)
!251 = !DILocation(line: 38, column: 5, scope: !66, inlinedAt: !247)
!252 = !DILocation(line: 39, column: 12, scope: !66, inlinedAt: !247)
!253 = !DILocation(line: 39, column: 14, scope: !66, inlinedAt: !247)
!254 = !DILocation(line: 39, column: 19, scope: !66, inlinedAt: !247)
!255 = !DILocation(line: 40, column: 1, scope: !66, inlinedAt: !247)
!256 = !DILocation(line: 89, column: 9, scope: !169, inlinedAt: !244)
!257 = !DILocation(line: 0, scope: !183, inlinedAt: !244)
!258 = !DILocation(line: 0, scope: !169, inlinedAt: !259)
!259 = distinct !DILocation(line: 104, column: 5, scope: !208)
!260 = !DILocation(line: 88, column: 5, scope: !169, inlinedAt: !259)
!261 = !DILocation(line: 0, scope: !66, inlinedAt: !262)
!262 = distinct !DILocation(line: 89, column: 9, scope: !183, inlinedAt: !259)
!263 = !DILocation(line: 37, column: 5, scope: !66, inlinedAt: !262)
!264 = !DILocation(line: 37, column: 14, scope: !66, inlinedAt: !262)
!265 = !DILocation(line: 37, column: 21, scope: !66, inlinedAt: !262)
!266 = !DILocation(line: 38, column: 5, scope: !66, inlinedAt: !262)
!267 = !DILocation(line: 39, column: 12, scope: !66, inlinedAt: !262)
!268 = !DILocation(line: 39, column: 14, scope: !66, inlinedAt: !262)
!269 = !DILocation(line: 39, column: 19, scope: !66, inlinedAt: !262)
!270 = !DILocation(line: 40, column: 1, scope: !66, inlinedAt: !262)
!271 = !DILocation(line: 89, column: 9, scope: !169, inlinedAt: !259)
!272 = !DILocation(line: 0, scope: !183, inlinedAt: !259)
!273 = !DILocation(line: 106, column: 1, scope: !208)
!274 = distinct !DISubprogram(name: "random_value_test", scope: !12, file: !12, line: 108, type: !209, scopeLine: 108, flags: DIFlagPrototyped | DIFlagAllCallsDescribed, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !11, retainedNodes: !275)
!275 = !{!276, !277}
!276 = !DILocalVariable(name: "fun", arg: 1, scope: !274, file: !12, line: 108, type: !69)
!277 = !DILocalVariable(name: "i", scope: !278, file: !12, line: 111, type: !279)
!278 = distinct !DILexicalBlock(scope: !274, file: !12, line: 111, column: 5)
!279 = !DIBasicType(name: "int", size: 32, encoding: DW_ATE_signed)
!280 = !DILocation(line: 0, scope: !274)
!281 = !DILocation(line: 109, column: 11, scope: !274)
!282 = !DILocation(line: 109, column: 5, scope: !274)
!283 = !DILocation(line: 0, scope: !278)
!284 = !DILocation(line: 111, column: 5, scope: !278)
!285 = !DILocation(line: 114, column: 1, scope: !274)
!286 = !DILocation(line: 112, column: 43, scope: !287)
!287 = distinct !DILexicalBlock(scope: !288, file: !12, line: 111, column: 36)
!288 = distinct !DILexicalBlock(scope: !278, file: !12, line: 111, column: 5)
!289 = !DILocation(line: 112, column: 51, scope: !287)
!290 = !DILocation(line: 0, scope: !169, inlinedAt: !291)
!291 = distinct !DILocation(line: 112, column: 9, scope: !287)
!292 = !DILocation(line: 88, column: 5, scope: !169, inlinedAt: !291)
!293 = !DILocation(line: 0, scope: !66, inlinedAt: !294)
!294 = distinct !DILocation(line: 89, column: 9, scope: !183, inlinedAt: !291)
!295 = !DILocation(line: 37, column: 5, scope: !66, inlinedAt: !294)
!296 = !DILocation(line: 37, column: 14, scope: !66, inlinedAt: !294)
!297 = !DILocation(line: 37, column: 21, scope: !66, inlinedAt: !294)
!298 = !DILocation(line: 38, column: 5, scope: !66, inlinedAt: !294)
!299 = !DILocation(line: 39, column: 12, scope: !66, inlinedAt: !294)
!300 = !DILocation(line: 39, column: 14, scope: !66, inlinedAt: !294)
!301 = !DILocation(line: 39, column: 19, scope: !66, inlinedAt: !294)
!302 = !DILocation(line: 40, column: 1, scope: !66, inlinedAt: !294)
!303 = !DILocation(line: 0, scope: !183, inlinedAt: !291)
!304 = !DILocation(line: 111, column: 32, scope: !288)
!305 = !DILocation(line: 111, column: 23, scope: !288)
!306 = distinct !{!306, !284, !307, !308, !309}
!307 = !DILocation(line: 113, column: 5, scope: !278)
!308 = !{!"llvm.loop.mustprogress"}
!309 = !{!"llvm.loop.unroll.disable"}
!310 = !DISubprogram(name: "srand", scope: !311, file: !311, line: 164, type: !312, flags: DIFlagPrototyped, spFlags: DISPFlagOptimized, retainedNodes: !314)
!311 = !DIFile(filename: "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include/stdlib.h", directory: "")
!312 = !DISubroutineType(types: !313)
!313 = !{null, !20}
!314 = !{}
!315 = !DISubprogram(name: "time", scope: !316, file: !316, line: 118, type: !317, flags: DIFlagPrototyped, spFlags: DISPFlagOptimized, retainedNodes: !314)
!316 = !DIFile(filename: "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include/time.h", directory: "")
!317 = !DISubroutineType(types: !318)
!318 = !{!319, !324}
!319 = !DIDerivedType(tag: DW_TAG_typedef, name: "time_t", file: !320, line: 31, baseType: !321)
!320 = !DIFile(filename: "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include/sys/_types/_time_t.h", directory: "")
!321 = !DIDerivedType(tag: DW_TAG_typedef, name: "__darwin_time_t", file: !322, line: 98, baseType: !323)
!322 = !DIFile(filename: "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include/arm/_types.h", directory: "")
!323 = !DIBasicType(name: "long", size: 64, encoding: DW_ATE_signed)
!324 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !319, size: 64)
!325 = !DISubprogram(name: "rand", scope: !311, file: !311, line: 162, type: !326, flags: DIFlagPrototyped, spFlags: DISPFlagOptimized, retainedNodes: !314)
!326 = !DISubroutineType(types: !327)
!327 = !{!279}
!328 = distinct !DISubprogram(name: "main", scope: !12, file: !12, line: 116, type: !326, scopeLine: 116, flags: DIFlagAllCallsDescribed, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !11, retainedNodes: !314)
!329 = !DILocation(line: 117, column: 5, scope: !328)
!330 = !DILocation(line: 0, scope: !208, inlinedAt: !331)
!331 = distinct !DILocation(line: 118, column: 5, scope: !328)
!332 = !DILocation(line: 0, scope: !169, inlinedAt: !333)
!333 = distinct !DILocation(line: 101, column: 5, scope: !208, inlinedAt: !331)
!334 = !DILocation(line: 88, column: 5, scope: !169, inlinedAt: !333)
!335 = !DILocation(line: 90, column: 9, scope: !206, inlinedAt: !333)
!336 = !DILocation(line: 0, scope: !169, inlinedAt: !337)
!337 = distinct !DILocation(line: 102, column: 5, scope: !208, inlinedAt: !331)
!338 = !DILocation(line: 88, column: 5, scope: !169, inlinedAt: !337)
!339 = !DILocation(line: 90, column: 9, scope: !206, inlinedAt: !337)
!340 = !DILocation(line: 0, scope: !169, inlinedAt: !341)
!341 = distinct !DILocation(line: 103, column: 5, scope: !208, inlinedAt: !331)
!342 = !DILocation(line: 88, column: 5, scope: !169, inlinedAt: !341)
!343 = !DILocation(line: 90, column: 9, scope: !206, inlinedAt: !341)
!344 = !DILocation(line: 0, scope: !169, inlinedAt: !345)
!345 = distinct !DILocation(line: 104, column: 5, scope: !208, inlinedAt: !331)
!346 = !DILocation(line: 88, column: 5, scope: !169, inlinedAt: !345)
!347 = !DILocation(line: 90, column: 9, scope: !206, inlinedAt: !345)
!348 = !DILocation(line: 119, column: 5, scope: !328)
!349 = !DILocation(line: 120, column: 5, scope: !328)
!350 = !DILocation(line: 122, column: 5, scope: !328)
!351 = !DILocation(line: 0, scope: !274, inlinedAt: !352)
!352 = distinct !DILocation(line: 123, column: 5, scope: !328)
!353 = !DILocation(line: 109, column: 11, scope: !274, inlinedAt: !352)
!354 = !DILocation(line: 109, column: 5, scope: !274, inlinedAt: !352)
!355 = !DILocation(line: 0, scope: !278, inlinedAt: !352)
!356 = !DILocation(line: 111, column: 5, scope: !278, inlinedAt: !352)
!357 = !DILocation(line: 112, column: 43, scope: !287, inlinedAt: !352)
!358 = !DILocation(line: 112, column: 51, scope: !287, inlinedAt: !352)
!359 = !DILocation(line: 0, scope: !169, inlinedAt: !360)
!360 = distinct !DILocation(line: 112, column: 9, scope: !287, inlinedAt: !352)
!361 = !DILocation(line: 88, column: 5, scope: !169, inlinedAt: !360)
!362 = !DILocation(line: 90, column: 9, scope: !206, inlinedAt: !360)
!363 = !DILocation(line: 111, column: 32, scope: !288, inlinedAt: !352)
!364 = !DILocation(line: 111, column: 23, scope: !288, inlinedAt: !352)
!365 = distinct !{!365, !356, !366, !308, !309}
!366 = !DILocation(line: 113, column: 5, scope: !278, inlinedAt: !352)
!367 = !DILocation(line: 124, column: 5, scope: !328)
!368 = !DILocation(line: 127, column: 1, scope: !328)
