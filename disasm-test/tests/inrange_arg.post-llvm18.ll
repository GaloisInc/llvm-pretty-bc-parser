; ModuleID = 'kwq_18.bc'
source_filename = "disasm-test/tests/inrange_arg.pre-llvm19.ll"

define ptr @constexpr() {
  ret ptr getelementptr inbounds ({ [4 x ptr], [4 x ptr] }, ptr null, i32 0, i32 1, i32 2)
  ret ptr getelementptr inbounds nuw inrange(-8, 8) (i8, ptr null, i64 16)
}
