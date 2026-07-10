; MSVC C++ SEH nested funclets: catchswitch with an explicit
; "unwind label", a cleanuppad parented to a catchpad token (the
; `within %1` clause), a "funclet" operand bundle on an invoke
; (bundle precedes the to/unwind clauses), and a cleanupret with
; explicit unwind label.
target triple = "x86_64-pc-windows-msvc"

declare void @may_throw()
declare void @log(i32)
declare i32 @__CxxFrameHandler3(...)

define void @test_nested() personality ptr @__CxxFrameHandler3 {
entry:
  invoke void @may_throw()
          to label %cont unwind label %cs1

cont:                                             ; preds = %entry, %catchend
  ret void

cs1:                                              ; preds = %entry
  %0 = catchswitch within none [label %catch1] unwind label %outer_cleanup

catch1:                                           ; preds = %cs1
  %1 = catchpad within %0 [ptr null, i32 64, ptr null]
  call void @log(i32 3) [ "funclet"(token %1) ]
  invoke void @may_throw() [ "funclet"(token %1) ]
          to label %catch_done unwind label %catch_cleanup

catch_done:                                       ; preds = %catch1
  catchret from %1 to label %catchend

catch_cleanup:                                    ; preds = %catch1
  %2 = cleanuppad within %1 []
  call void @log(i32 4) [ "funclet"(token %2) ]
  cleanupret from %2 unwind label %outer_cleanup

catchend:                                         ; preds = %catch_done
  br label %cont

outer_cleanup:                                    ; preds = %cs1, %catch_cleanup
  %3 = cleanuppad within none []
  call void @log(i32 5) [ "funclet"(token %3) ]
  cleanupret from %3 unwind to caller
}
