define void @atomicrmw(ptr %a, i32 %i) {
    %b = atomicrmw add ptr %a, i32 %i acquire
    ret void
}
