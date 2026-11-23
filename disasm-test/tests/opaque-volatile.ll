define i32 @volatile(ptr %p, i32 %v) {
    %a = load volatile i32, ptr %p, align 4
    store volatile i32 %v, ptr %p, align 4
    %b = load atomic volatile i32, ptr %p seq_cst, align 4
    store atomic volatile i32 %v, ptr %p seq_cst, align 4
    %c = load i32, ptr %p, align 4
    store i32 %v, ptr %p, align 4
    ret i32 %a
}
