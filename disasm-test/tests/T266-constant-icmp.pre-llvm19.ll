@global_var = external constant [1 x i8]

define i64 @h() {
  br i1 icmp ne (i32 ptrtoint (ptr @global_var to i32), i32 1), label %pc_1, label %pc_1
pc_1:
  ret i64 0
}
