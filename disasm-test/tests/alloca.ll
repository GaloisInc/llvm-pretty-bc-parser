; ModuleID = 'alloca.bc'

%aaaa = type [1 x i32]

define i32 @f() {
  %arr = alloca %aaaa, align 4
  %ptr = getelementptr %aaaa* %arr, i32 0, i32 0
  store i32 0, i32* %ptr
  ret i32 0
}
