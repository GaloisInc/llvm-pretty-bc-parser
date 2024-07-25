define void @atomicrmw(float* %a, float %f) {
    %b13 = atomicrmw fmax float* %a, float %f acquire
    %b14 = atomicrmw fmin float* %a, float %f acquire
    ret void
}
