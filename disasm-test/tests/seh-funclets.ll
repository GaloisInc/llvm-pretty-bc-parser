; Windows SEH funclet opcodes: catchswitch, catchpad, cleanuppad,
; catchret, cleanupret.  These are emitted by clang-cl and other
; MSVC-target builds when lowering C++ exception handling, and they
; were not previously supported by the bitcode parser.
;
; We use an Itanium personality (rather than the MSVC
; @__CxxFrameHandler3) so that the IR verifier does not require
; "funclet" operand bundles on inner calls.  This lets the test focus
; on the funclet opcodes themselves without dragging in operand
; bundle parsing.
target triple = "x86_64-pc-linux-gnu"

declare void @may_throw()
declare i32 @__gxx_personality_v0(...)

; Exercises catchswitch (with one handler, unwind to caller),
; catchpad (with one argument), and catchret.
define void @test_catch() personality ptr @__gxx_personality_v0 {
entry:
  invoke void @may_throw()
          to label %cont unwind label %cs

cont:                                             ; preds = %entry, %catch
  ret void

cs:                                               ; preds = %entry
  %0 = catchswitch within none [label %catch] unwind to caller

catch:                                            ; preds = %cs
  %1 = catchpad within %0 [ptr null]
  catchret from %1 to label %cont
}

; Exercises cleanuppad with no arguments and cleanupret with
; "unwind to caller".
define void @test_cleanup() personality ptr @__gxx_personality_v0 {
entry:
  invoke void @may_throw()
          to label %cont unwind label %cleanup

cont:                                             ; preds = %entry
  ret void

cleanup:                                          ; preds = %entry
  %0 = cleanuppad within none []
  cleanupret from %0 unwind to caller
}

; Exercises catchswitch with an explicit default unwind destination,
; catchpad with multiple arguments, a nested cleanuppad whose parent
; is a catchpad token, and cleanupret with an explicit unwind label.
define void @test_nested() personality ptr @__gxx_personality_v0 {
entry:
  invoke void @may_throw()
          to label %cont unwind label %cs1

cont:                                             ; preds = %entry, %catchend
  ret void

cs1:                                              ; preds = %entry
  %0 = catchswitch within none [label %catch1] unwind label %cleanup

catch1:                                           ; preds = %cs1
  %1 = catchpad within %0 [ptr null, i32 0, ptr null]
  catchret from %1 to label %catchend

catchend:                                         ; preds = %catch1
  br label %cont

cleanup:                                          ; preds = %cs1, %nested_cleanup
  %2 = cleanuppad within none []
  cleanupret from %2 unwind to caller

; A cleanup whose parent is a catchpad token.  Unreachable from
; entry, but the IR is well-formed and exercises cleanupret with an
; explicit unwind label as well as cleanuppad with a non-"none"
; parent.
nested_cleanup:                                   ; No predecessors
  %3 = cleanuppad within %1 []
  cleanupret from %3 unwind label %cleanup
}
