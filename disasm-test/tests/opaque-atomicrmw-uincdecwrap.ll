define void @atomicrmw(ptr %a, i32 %i) {
    %b15 = atomicrmw uinc_wrap ptr %a, i32 %i acquire
    %b16 = atomicrmw udec_wrap ptr %a, i32 %i acquire
    ret void
}
