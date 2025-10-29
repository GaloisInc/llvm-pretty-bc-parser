define void @test_uitofp(i32 %a) {
  %res1 = uitofp nneg i32 %a to float
  %res2 = uitofp i32 %a to float
  ret void
}
