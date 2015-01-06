
define void @f() {

  br label %bar

foo:
  %1 = add i32 %2, 10
  br label %exit

bar:
  %2 = add i32 0, 10
  br label %foo

exit:
  ret void

}
