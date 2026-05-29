; Pretty-printed form of windows-seh-funclets.bc as produced by this parser
; + llvm-pretty.  This file is for human reference only -- the test runner
; in disasm-test/Main.hs (runRawBCTest) does NOT compare against this text;
; it round-trips the .bc through parseBC -> processLL and compares the two
; resulting ASTs.  See disasm-test/bc_src_tests/README.md.
;
; Provenance for the companion .bc:
;   * Source: $TEMP/windows-seh-funclets.cpp (a small C++ program written
;     for this PR -- see the README entry).
;   * Compiled with clang-cl 20.1.6 (x86_64-pc-windows-msvc) at /EHsc, no /O2:
;       clang-cl /EHsc /clang:-emit-llvm /clang:-S windows-seh-funclets.cpp
;   * Assembled to bitcode with LLVM 21 llvm-as (the rustup toolchain):
;       <rustlib>/bin/llvm-as.exe windows-seh-funclets.ll \
;         -o windows-seh-funclets.bc
;
; What this exercises end-to-end through bitcode:
;   * MSVC SEH funclet opcodes: catchswitch / catchpad / catchret /
;     cleanuppad / cleanupret  (the PR #365 work).
;   * `[ "funclet"(token %N) ]` operand bundles on inner calls inside the
;     catch and cleanup funclets  (the operand-bundle work added in this PR).
;   * The `__CxxFrameHandler3` MSVC C++ EH personality clause on a Define.
source_filename = "C:\Users\AMELIA~1\AppData\Local\Temp\windows-seh-funclets.cpp"
target triple = "x86_64-pc-windows-unknown-"
target datalayout = "e-m:w-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-f80:128-n8:16:32:64-S128"
%struct.G = type { i32 }
declare default void @"?may_throw@@YAXXZ"()
declare default i32 @__CxxFrameHandler3(...)
declare default void @"?log_failure@@YAXH@Z"(i32)
declare default void @llvm.trap()
declare default void @__std_terminate()
define default i32 @"?test_catch_only@@YAHXZ"() personality i32(...)* @__CxxFrameHandler3 {
; <label>: 0
  %1 = alloca i32, align 4
  invoke void @"?may_throw@@YAXXZ"() to label %6 unwind label %2
; <label>: 2
  %3 = catchswitch within none [label %4] unwind to caller
; <label>: 4
  %5 = catchpad within %3 [ptr null, i32 64, ptr null]
  call void @"?log_failure@@YAXH@Z"(i32 1) ["funclet"(token %5)]
  store i32 1, i32* %1, align 4
  catchret from %5 to label %7
; <label>: 6
  store i32 0, i32* %1, align 4
  br label %9
; <label>: 7
  br label %9
; <label>: 8
  call void @llvm.trap()
  unreachable
; <label>: 9
  %10 = load i32, i32* %1, align 4
  ret i32 %10
}
define default i32 @"?test_cleanup_only@@YAHXZ"() personality i32(...)* @__CxxFrameHandler3 {
; <label>: 0
  %1 = alloca %struct.G, align 4
  %2 = getelementptr nuw inbounds %struct.G, %struct.G* %1, i32 0, i32 0
  store i32 2, i32* %2, align 4
  invoke void @"?may_throw@@YAXXZ"() to label %3 unwind label %4
; <label>: 3
  call void @"??1G@?1??test_cleanup_only@@YAHXZ@QEAA@XZ"(%struct.G* %1)
  ret i32 0
; <label>: 4
  %5 = cleanuppad within none []
  call void @"??1G@?1??test_cleanup_only@@YAHXZ@QEAA@XZ"(%struct.G* %1) ["funclet"(token %5)]
  cleanupret from %5 unwind to caller
}
define internal default void @"??1G@?1??test_cleanup_only@@YAHXZ@QEAA@XZ"(ptr %0) personality i32(...)* @__CxxFrameHandler3 {
; <label>: 1
  %2 = alloca ptr, align 8
  store ptr %0, ptr* %2, align 8
  %3 = load ptr, ptr* %2, align 8
  %4 = getelementptr nuw inbounds %struct.G, ptr %3, i32 0, i32 0
  %5 = load i32, i32* %4, align 4
  invoke void @"?log_failure@@YAXH@Z"(i32 %5) to label %6 unwind label %7
; <label>: 6
  ret void
; <label>: 7
  %8 = cleanuppad within none []
  call void @__std_terminate() ["funclet"(token %8)]
  unreachable
}
!llvm.ident = !{!6}
!llvm.linker.options = !{!0, !1}
!llvm.module.flags = !{!2, !3, !4, !5}
!0 = !{!"/DEFAULTLIB:libcmt.lib"}
!1 = !{!"/DEFAULTLIB:oldnames.lib"}
!2 = !{i32 1, !"wchar_size", i32 2}
!3 = !{i32 8, !"PIC Level", i32 2}
!4 = !{i32 7, !"uwtable", i32 2}
!5 = !{i32 1, !"MaxTLSAlign", i32 65536}
!6 = !{!"clang version 20.1.6"}
