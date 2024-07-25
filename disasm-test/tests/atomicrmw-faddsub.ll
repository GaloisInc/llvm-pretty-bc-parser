define void @atomicrmw(float* %a, float %f) {
    %b11 = atomicrmw fadd float* %a, float %f acquire
    %b12 = atomicrmw fsub float* %a, float %f acquire
    ret void
}
