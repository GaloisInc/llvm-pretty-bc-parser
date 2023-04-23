%struct.RT = type { i8, [10 x [20 x i32]], i8 }
%struct.ST = type { i32, double, %struct.RT }

@.s = private constant %struct.ST zeroinitializer

define ptr @foo() {
entry:
  ret ptr getelementptr inbounds (%struct.ST, ptr @.s, i64 1, i32 2, i32 1, i64 5, i64 13)
}
