define void @test_zext(i32 %a) {
  %res1 = zext nneg i32 %a to i64
  %res2 = zext i32 %a to i64
  ret void
}
