define void @f(i32 %x) {
    ret void
}

define void @g() {
    %p = alloca ptr
    store ptr @f, ptr %p
    %f = load ptr, ptr %p
    call void (i32) %f(i32 42)
    ret void
}
