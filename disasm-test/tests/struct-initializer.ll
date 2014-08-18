
%0 = type { i32, i8, [3 x i8] }
%1 = type { i32, [6 x i8], [2 x i8] }

@struct_test.b = internal constant %0 { i32 99, i8 122, [3 x i8] undef }, align 4
@struct_test_two.x = internal constant %1 { i32 1, [6 x i8] c"fredd\00", [2 x i8] undef }, align 4
