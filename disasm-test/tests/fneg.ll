define double @real_fneg(double %X) {
        %Y = fneg double %X               ; <double> [#uses=1]
        ret double %Y
}

define double @real_fneg_constant() {
        %Y = fneg double -2.0             ; <double> [#uses=1]
        ret double %Y
}

define float @real_fnegf(float %X) {
        %Y = fneg float %X                ; <float> [#uses=1]
        ret float %Y
}
