; Windows SEH funclet opcodes (catchswitch, catchpad, catchret,
; cleanuppad, cleanupret) and the "funclet" operand bundles that the
; IR verifier requires on every inner call/invoke inside a funclet
; when the MSVC C++ EH personality (@__CxxFrameHandler3) is in use.
;
; Coverage:
;   * catchswitch with `unwind to caller`           (test_catch)
;   * catchswitch with explicit `unwind label %X`   (test_nested)
;   * catchpad with the 3-arg MSVC C++ form
;     [TypeInfo, Flags, CatchObject]                (test_catch, test_nested)
;   * cleanuppad with `within none`                 (test_cleanup, test_nested, test_cleanup_chain)
;   * cleanuppad with non-none parent token
;     (cleanup nested inside a catchpad)            (test_nested)
;   * cleanupret with `unwind to caller`            (test_cleanup, test_nested)
;   * cleanupret with explicit `unwind label %X`    (test_nested, test_cleanup_chain)
;   * "funclet" operand bundle on a call            (every funclet body below)
;   * "funclet" operand bundle on an invoke
;     (bundle precedes the `to`/`unwind` clauses)   (test_nested)
;   * `personality ptr @__CxxFrameHandler3` clause  (every define below)
;
; A complementary test using a real clang-cl-generated bitcode
; artifact lives at disasm-test/bc_src_tests/windows-seh-funclets.bc,
; which compares ASTs (not text) and exercises the parser on bitcode
; produced by an unmodified MSVC toolchain.
target triple = "x86_64-pc-windows-msvc"

declare void @may_throw()
declare void @log(i32)
declare i32 @__CxxFrameHandler3(...)

; Exercises catchswitch (unwind to caller), catchpad, catchret, and
; a "funclet" bundle on the inner @log call inside the catch handler.
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

; Exercises cleanuppad with no arguments, cleanupret with "unwind to
; caller", and a "funclet" bundle on the inner @log call inside the
; cleanup handler.
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

; Exercises catchswitch with an explicit unwind destination, the
; 3-arg catchpad, an inner invoke (with a "funclet" bundle) whose
; unwind edge lands in a cleanuppad nested *within* the catchpad
; token, and a separate outer cleanuppad reached via the catchswitch
; unwind edge.
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

outer_cleanup:                                    ; preds = %cs1
  %3 = cleanuppad within none []
  call void @log(i32 5) [ "funclet"(token %3) ]
  cleanupret from %3 unwind to caller
}

; Exercises a chain of two sibling cleanuppads (both `within none`)
; where the first cleanupret has an explicit `unwind label %X`
; pointing at the second.  This is the canonical pattern for nested
; C++ destructors under MSVC EH and complements the
; cleanup-nested-inside-catch chain in test_nested above.
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
