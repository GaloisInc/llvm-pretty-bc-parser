define i8** @constexpr() {
  ret i8** getelementptr inbounds ({ [4 x i8*], [4 x i8*] }, { [4 x i8*], [4 x i8*] }* null, i32 0, inrange i32 1, i32 2)
}
