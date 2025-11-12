define void @test_icmp(i32 %a, i32 %b) {
  %res1 = icmp samesign ult i32 %a, %b
  %res2 = icmp ult i32 %a, %b
  ret void
}
