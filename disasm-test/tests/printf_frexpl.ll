; ModuleID = 'lib/printf-frexpl.c'
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

; Function Attrs: nounwind uwtable
define x86_fp80 @printf_frexpl(x86_fp80 %x, i32* nocapture %expptr) #0 {
  %exponent = alloca i32, align 4
  %_cw = alloca i16, align 2
  %_ncw = alloca i16, align 2
  %_ncw1 = alloca i16, align 2
  call void asm sideeffect "fnstcw $0", "=*m,~{dirflag},~{fpsr},~{flags}"(i16* %_cw) #2, !srcloc !1
  %1 = load i16* %_cw, align 2, !tbaa !2
  %2 = or i16 %1, 768
  store i16 %2, i16* %_ncw, align 2, !tbaa !2
  call void asm sideeffect "fldcw $0", "*m,~{dirflag},~{fpsr},~{flags}"(i16* %_ncw) #2, !srcloc !6
  %3 = call x86_fp80 @frexpl(x86_fp80 %x, i32* %exponent) #2
  %4 = fadd x86_fp80 %3, %3
  %5 = load i32* %exponent, align 4, !tbaa !7
  %6 = add nsw i32 %5, -1
  store i32 %6, i32* %exponent, align 4, !tbaa !7
  %7 = icmp slt i32 %5, -16381
  br i1 %7, label %8, label %11

; <label>:8                                       ; preds = %0
  %9 = add nsw i32 %5, 16381
  %10 = call x86_fp80 @ldexpl(x86_fp80 %4, i32 %9) #2
  store i32 -16382, i32* %exponent, align 4, !tbaa !7
  br label %11

; <label>:11                                      ; preds = %8, %0
  %12 = phi i32 [ -16382, %8 ], [ %6, %0 ]
  %.0 = phi x86_fp80 [ %10, %8 ], [ %4, %0 ]
  store i16 %1, i16* %_ncw1, align 2, !tbaa !2
  call void asm sideeffect "fldcw $0", "*m,~{dirflag},~{fpsr},~{flags}"(i16* %_ncw1) #2, !srcloc !9
  store i32 %12, i32* %expptr, align 4, !tbaa !7
  ret x86_fp80 %.0
}

; Function Attrs: nounwind
declare x86_fp80 @frexpl(x86_fp80, i32* nocapture) #1

; Function Attrs: nounwind
declare x86_fp80 @ldexpl(x86_fp80, i32) #1

attributes #0 = { nounwind uwtable "less-precise-fpmad"="false" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { nounwind "less-precise-fpmad"="false" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #2 = { nounwind }

!llvm.ident = !{!0}

!0 = !{!"clang version 3.6.2 (tags/RELEASE_362/final)"}
!1 = !{i32 -2146987380}
!2 = !{!3, !3, i64 0}
!3 = !{!"short", !4, i64 0}
!4 = !{!"omnipotent char", !5, i64 0}
!5 = !{!"Simple C/C++ TBAA"}
!6 = !{i32 -2146987172}
!7 = !{!8, !8, i64 0}
!8 = !{!"int", !4, i64 0}
!9 = !{i32 -2146986812}
