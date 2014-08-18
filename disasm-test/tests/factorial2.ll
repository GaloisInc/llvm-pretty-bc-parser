; ModuleID = 'test.bc'

define i32 @factorial(i32 %a0) {
  br label %test

test:                                             ; preds = %incr, %0
  %1 = phi i32 [ %a0, %0 ], [ %6, %4 ]
  %2 = phi i32 [ 1, %0 ], [ %5, %4 ]
  %3 = icmp ule i32 %2, 1
  br i1 %3, label %exit, label %4

; <label>:4
  %5 = mul i32 %2, %1
  %6 = sub i32 %1, 1
  br label %test

exit:                                             ; preds = %test
  ret i32 %2
}
