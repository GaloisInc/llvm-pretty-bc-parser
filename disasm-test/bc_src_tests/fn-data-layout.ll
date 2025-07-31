; A regression test for #292. This ensures that we can parse data layout strings
; that specify function pointer alignment (i.e., the Fn32 part in the string
; below).
target datalayout = "e-m:o-i64:64-i128:128-n32:64-S128-Fn32"
