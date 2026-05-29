; MSVC C++ SEH: cleanuppad with no arguments, cleanupret with
; "unwind to caller", and a "funclet" operand bundle on a call
; inside the cleanup handler.
target triple = "x86_64-pc-windows-msvc"

declare void @may_throw()
declare void @log(i32)
declare i32 @__CxxFrameHandler3(...)

define void @test_cleanup() personality ptr @__CxxFrameHandler3 {
entry:
  invoke void @may_throw()
          to label %cont unwind label %cleanup

cont:                                             ; preds = %entry
  ret void

cleanup:                                          ; preds = %entry
  %0 = cleanuppad within none []
  call void @log(i32 2) [ "funclet"(token %0) ]
  cleanupret from %0 unwind to caller
}
