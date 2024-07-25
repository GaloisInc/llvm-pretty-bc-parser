define void @atomicrmw(ptr %a1, i32 %i, ptr %a2, float %f) {
    %b0  = atomicrmw xchg ptr %a1, i32   %i acquire
    %b1  = atomicrmw add  ptr %a1, i32   %i acquire
    %b2  = atomicrmw sub  ptr %a1, i32   %i acquire
    %b3  = atomicrmw and  ptr %a1, i32   %i acquire
    %b4  = atomicrmw nand ptr %a1, i32   %i acquire
    %b5  = atomicrmw or   ptr %a1, i32   %i acquire
    %b6  = atomicrmw xor  ptr %a1, i32   %i acquire
    %b7  = atomicrmw max  ptr %a1, i32   %i acquire
    %b8  = atomicrmw min  ptr %a1, i32   %i acquire
    %b9  = atomicrmw umax ptr %a1, i32   %i acquire
    %b10 = atomicrmw umin ptr %a1, i32   %i acquire
    %b11 = atomicrmw fadd ptr %a2, float %f acquire
    %b12 = atomicrmw fsub ptr %a2, float %f acquire
    %b13 = atomicrmw fmax ptr %a2, float %f acquire
    %b14 = atomicrmw fmin ptr %a2, float %f acquire
    ret void
}
