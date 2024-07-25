define void @atomicrmw(i32* %a, i32 %i) {
    %b15 = atomicrmw uinc_wrap i32* %a, i32 %i acquire
    %b16 = atomicrmw udec_wrap i32* %a, i32 %i acquire
    ret void
}
