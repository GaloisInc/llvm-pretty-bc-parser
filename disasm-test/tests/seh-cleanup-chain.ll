; MSVC C++ SEH cleanup chain: two sibling cleanuppads (both
; `within none`) where the first cleanupret has an explicit
; `unwind label %X` pointing at the second.  Canonical pattern for
; chained C++ destructors under MSVC EH.
target triple = "x86_64-pc-windows-msvc"

declare void @may_throw()
declare void @log(i32)
declare i32 @__CxxFrameHandler3(...)

define void @test_cleanup_chain() personality ptr @__CxxFrameHandler3 {
entry:
  invoke void @may_throw()
          to label %cont unwind label %cu1

cont:                                             ; preds = %entry
  ret void

cu1:                                              ; preds = %entry
  %0 = cleanuppad within none []
  call void @log(i32 6) [ "funclet"(token %0) ]
  cleanupret from %0 unwind label %cu2

cu2:                                              ; preds = %cu1
  %1 = cleanuppad within none []
  call void @log(i32 7) [ "funclet"(token %1) ]
  cleanupret from %1 unwind to caller
}
