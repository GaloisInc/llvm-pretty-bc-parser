define void @atomicrmw(i32* %a, i32 %i) {
    %b = atomicrmw add i32* %a, i32 %i acquire
    ret void
}
