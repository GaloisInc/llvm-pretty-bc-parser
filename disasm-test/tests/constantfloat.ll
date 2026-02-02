; half-floats
@hf0 = dso_local global half 0xH0000, align 2
@hf1 = dso_local global half 0xH3C00, align 2
@hf2 = dso_local global half 0xH4000, align 2
@hf025 = dso_local global half 0xH3A00, align 2
@hfinf = dso_local global half 0xH7C00, align 2
@hfnan = dso_local global half 0xHFC55, align 2

; single-floats
;
; Do not attempt to test NaNs; it breaks the test infrastructure.
; Because we load these into the Haskell float type, a NaN is not
; equal to itself, and then the test both (a) fails and then (b)
; prints a diff of what you got wrong... with no visible differences,
; which is highly confusing.
;
; Note: apparently the proper behavior for printing single-precision
; as hex is to promote to double and print that. This is also why the
; hex values below are doubles.
@f0 = dso_local global float 0.0, align 4
@f1 = dso_local global float 1.0, align 4
@f2 = dso_local global float 2.0, align 4
@f025 = dso_local global float 0.25, align 4
@finf = dso_local global float 0x7FF0000000000000, align 4
;@fnan = dso_local global float 0xFFF5555540000000, align 4

; double-floats
;
; Don't test NaNs for the same reason as singles.
@d0 = dso_local global double 0.0, align 8
@d1 = dso_local global double 1.0, align 8
@d2 = dso_local global double 2.0, align 8
@d025 = dso_local global double 0.25, align 8
@dinf = dso_local global double 0x7FF0000000000000, align 8
;@dnan = dso_local global double 0xFFF5555555555555, align 8

; x86 80-bit floats
;
; These are always printed as hex so there's not much point chasing
; after specific values; just get zero and nonzero.
@fp80_a = dso_local global x86_fp80 0xK00000000000000000000, align 16
@fp80_b = dso_local global x86_fp80 0xKF0F0F0F0F0F0F0F0F0F0, align 16

; quad floats
;
; These are always printed as hex so there's not much point chasing
; after specific values; just get zero and nonzero.
@fp128_a = dso_local global fp128 0xL00000000000000000000000000000000, align 16
@fp128_b = dso_local global fp128 0xLF0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0, align 16

; PowerPC pair-of-doubles floats
;
; These are always printed as hex so there's not much point chasing
; after specific values; just get zero and nonzero. Avoid making
; either component a NaN.
@fp128ppc_a = dso_local global ppc_fp128 0xM00000000000000000000000000000000, align 16
@fp128ppc_b = dso_local global ppc_fp128 0xMF0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0, align 16
