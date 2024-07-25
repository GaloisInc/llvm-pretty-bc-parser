define void @atomicrmw(i32* %a, i32 %i) {
    %b0  = atomicrmw xchg i32* %a, i32 %i acquire
    %b1  = atomicrmw add  i32* %a, i32 %i acquire
    %b2  = atomicrmw sub  i32* %a, i32 %i acquire
    %b3  = atomicrmw and  i32* %a, i32 %i acquire
    %b4  = atomicrmw nand i32* %a, i32 %i acquire
    %b5  = atomicrmw or   i32* %a, i32 %i acquire
    %b6  = atomicrmw xor  i32* %a, i32 %i acquire
    %b7  = atomicrmw max  i32* %a, i32 %i acquire
    %b8  = atomicrmw min  i32* %a, i32 %i acquire
    %b9  = atomicrmw umax i32* %a, i32 %i acquire
    %b10 = atomicrmw umin i32* %a, i32 %i acquire
    ret void
}
