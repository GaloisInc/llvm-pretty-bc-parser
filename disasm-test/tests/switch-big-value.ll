
define i32 @f(i512 %x) {
  switch i512 %x, label %default [
    i512  9223372036854775808, label %isOne
    i512 19223372036854775808, label %isOne
  ]

default:
  ret i32 0

isOne:
  ret i32 1
}
