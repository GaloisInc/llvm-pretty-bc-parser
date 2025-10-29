define void @test_trunc(i64 %a) {
  %res1 = trunc nuw i64 %a to i32
  %res2 = trunc nsw i64 %a to i32
  %res3 = trunc nuw nsw i64 %a to i32
  %res4 = trunc nsw nuw i64 %a to i32
  %res5 = trunc i64 %a to i32
  ret void
}
