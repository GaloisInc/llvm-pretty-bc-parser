
; This test exposes a strange problem in which our parser was ignoring arguments
; to the phi instruction if they failed to parse.  The case where they were
; failing was exposed by having one pair of value and label be made up of
; implicit identifiers.
define void @test(i32 %a) {
	br label %test

test:
	%1 = phi i32 [ %a, %0 ], [ %4, %3 ]
	%2 = icmp eq i32 %a, 0
	br i1 %2, label %exit, label %3

; label %3
	%4 = sub i32 %1, 1
	br label %test

exit:
	ret void
}
