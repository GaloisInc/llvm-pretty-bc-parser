; MSVC C++ SEH: catchswitch with "unwind to caller", the 3-arg MSVC
; catchpad form [TypeInfo, Flags, CatchObject], catchret, and a
; "funclet" operand bundle on a call inside the catch handler.
target triple = "x86_64-pc-windows-msvc"

declare void @may_throw()
declare void @log(i32)
declare i32 @__CxxFrameHandler3(...)

define void @test_catch() personality ptr @__CxxFrameHandler3 {
entry:
  invoke void @may_throw()
          to label %cont unwind label %cs

cont:                                             ; preds = %entry, %catch
  ret void

cs:                                               ; preds = %entry
  %0 = catchswitch within none [label %catch] unwind to caller

catch:                                            ; preds = %cs
  %1 = catchpad within %0 [ptr null, i32 64, ptr null]
  call void @log(i32 1) [ "funclet"(token %1) ]
  catchret from %1 to label %cont
}
