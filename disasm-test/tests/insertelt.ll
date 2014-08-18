
define <4 x i8> @f(i8 %a, i8 %b, i8 %c, i8 %d) {
  %1 = insertelement <4 x i8> undef, i8 %a, i32 0
  %2 = insertelement <4 x i8> %1,    i8 %b, i32 1
  %3 = insertelement <4 x i8> %2,    i8 %c, i32 2
  %4 = insertelement <4 x i8> %3,    i8 %d, i32 3

  ret <4 x i8> %3
}
