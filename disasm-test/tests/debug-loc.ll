; ModuleID = 'test.c'
target datalayout = "e-m:o-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-apple-macosx10.10.0"

; Function Attrs: nounwind ssp uwtable
define i32 @f(i32 %c) #0 {
entry:
  %c.addr = alloca i32, align 4
  store i32 %c, i32* %c.addr, align 4
  call void @llvm.dbg.declare(metadata i32* %c.addr, metadata !13, metadata !14), !dbg !15
  %0 = load i32* %c.addr, align 4, !dbg !16
  %add = add nsw i32 %0, 1, !dbg !16
  ret i32 %add, !dbg !16
}

; Function Attrs: nounwind readnone
declare void @llvm.dbg.declare(metadata, metadata, metadata) #1

attributes #0 = { nounwind ssp uwtable "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { nounwind readnone }

!llvm.dbg.cu = !{!0}
!llvm.module.flags = !{!9, !10, !11}
!llvm.ident = !{!12}

!0 = !{!"0x11\0012\00clang version 3.6.1 (tags/RELEASE_361/final)\000\00\000\00\001", !1, !2, !2, !3, !2, !2} ; [ DW_TAG_compile_unit ] [/Users/trevor/Work/verifier/llvm-pretty-bc-parser/test.c] [DW_LANG_C99]
!1 = !{!"test.c", !"/Users/trevor/Work/verifier/llvm-pretty-bc-parser"}
!2 = !{}
!3 = !{!4}
!4 = !{!"0x2e\00f\00f\00\002\000\001\000\000\00256\000\002", !1, !5, !6, null, i32 (i32)* @f, null, null, !2} ; [ DW_TAG_subprogram ] [line 2] [def] [f]
!5 = !{!"0x29", !1}                               ; [ DW_TAG_file_type ] [/Users/trevor/Work/verifier/llvm-pretty-bc-parser/test.c]
!6 = !{!"0x15\00\000\000\000\000\000\000", null, null, null, !7, null, null, null} ; [ DW_TAG_subroutine_type ] [line 0, size 0, align 0, offset 0] [from ]
!7 = !{!8, !8}
!8 = !{!"0x24\00int\000\0032\0032\000\000\005", null, null} ; [ DW_TAG_base_type ] [int] [line 0, size 32, align 32, offset 0, enc DW_ATE_signed]
!9 = !{i32 2, !"Dwarf Version", i32 2}
!10 = !{i32 2, !"Debug Info Version", i32 2}
!11 = !{i32 1, !"PIC Level", i32 2}
!12 = !{!"clang version 3.6.1 (tags/RELEASE_361/final)"}
!13 = !{!"0x101\00c\0016777218\000", !4, !5, !8}  ; [ DW_TAG_arg_variable ] [c] [line 2]
!14 = !{!"0x102"}                                 ; [ DW_TAG_expression ]
!15 = !MDLocation(line: 2, column: 11, scope: !4)
!16 = !MDLocation(line: 3, column: 12, scope: !4)
!17 = !MDLocation(line: 3, column: 5, scope: !4)
