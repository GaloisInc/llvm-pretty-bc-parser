; ModuleID = 'test.c'
source_filename = "test.c"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i32 @test1(i32 %0) #0 {
  %2 = alloca i32, align 4
  %3 = alloca i32, align 4
  store i32 %0, i32* %3, align 4
  %4 = load i32, i32* %3, align 4
  callbr void asm sideeffect "testl $0, $0; jne ${1:l};", "r,X,X,~{dirflag},~{fpsr},~{flags}"(i32 %4, i8* blockaddress(@test1, %7), i8* blockaddress(@test1, %6)) #1
          to label %5 [label %7, label %6], !srcloc !2

5:                                                ; preds = %1
  store i32 0, i32* %2, align 4
  br label %8

6:                                                ; preds = %1
  store i32 0, i32* %2, align 4
  br label %8

7:                                                ; preds = %1
  store i32 1, i32* %2, align 4
  br label %8

8:                                                ; preds = %7, %6, %5
  %9 = load i32, i32* %2, align 4
  ret i32 %9
}

attributes #0 = { noinline nounwind optnone uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "frame-pointer"="all" "less-precise-fpmad"="false" "min-legal-vector-width"="0" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { nounwind }

!llvm.module.flags = !{!0}
!llvm.ident = !{!1}

!0 = !{i32 1, !"wchar_size", i32 4}
!1 = !{!"clang version 10.0.0-4ubuntu1 "}
!2 = !{i32 265}
