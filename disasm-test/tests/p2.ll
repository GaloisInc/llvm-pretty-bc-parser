; ModuleID = 'p2.bc'
source_filename = "disasm-test/tests/p2.cpp"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

%class.uorb = type { i8 }
%struct.gyro_status = type { double, double, double }
%struct.angular_accel = type { double }
%struct.actuator_control = type { i32, double, double }

@main_uorb = local_unnamed_addr global %class.uorb zeroinitializer, align 1
@vehicle_angular_acc_handle = local_unnamed_addr global i32 0, align 4
@fw_att_control_handle = local_unnamed_addr global i32 0, align 4
@airship_att_handle1 = local_unnamed_addr global i32 0, align 4
@airship_att_handle2 = local_unnamed_addr global i32 0, align 4
@airship_att_handle3 = local_unnamed_addr global i32 0, align 4

; Function Attrs: norecurse nounwind readnone sspstrong uwtable
define void @_Z17vehicle_gps_startv() local_unnamed_addr #0 {
  ret void
}

; Function Attrs: nounwind readnone sspstrong uwtable
define void @_Z18vehicle_gps_updatev() local_unnamed_addr #1 {
  ret void
}

; Function Attrs: norecurse nounwind readnone sspstrong uwtable
define void @_Z16vehicle_gps_stopv() local_unnamed_addr #0 {
  ret void
}

; Function Attrs: nofree norecurse nounwind sspstrong uwtable writeonly
define void @_Z25vehicle_angular_acc_startv() local_unnamed_addr #2 {
  store i32 1, i32* @vehicle_angular_acc_handle, align 4, !tbaa !3
  ret void
}

; Function Attrs: norecurse nounwind readnone sspstrong uwtable
define void @_Z24vehicle_angular_acc_stopv() local_unnamed_addr #0 {
  ret void
}

; Function Attrs: nofree norecurse nounwind sspstrong uwtable
define void @_Z21compute_angular_accelP11gyro_statusP13angular_accel(%struct.gyro_status* nocapture readonly %0, %struct.angular_accel* nocapture %1) local_unnamed_addr #3 {
  %3 = getelementptr inbounds %struct.gyro_status, %struct.gyro_status* %0, i64 0, i32 0
  %4 = load double, double* %3, align 8, !tbaa !7
  %5 = fmul double %4, 1.000000e+02
  %6 = getelementptr inbounds %struct.gyro_status, %struct.gyro_status* %0, i64 0, i32 1
  %7 = load double, double* %6, align 8, !tbaa !10
  %8 = fadd double %5, %7
  %9 = getelementptr inbounds %struct.gyro_status, %struct.gyro_status* %0, i64 0, i32 2
  %10 = load double, double* %9, align 8, !tbaa !11
  %11 = fsub double %8, %10
  %12 = getelementptr inbounds %struct.angular_accel, %struct.angular_accel* %1, i64 0, i32 0
  store double %11, double* %12, align 8, !tbaa !12
  ret void
}

; Function Attrs: nounwind readonly sspstrong uwtable
define void @_Z26vehicle_angular_acc_updatev() local_unnamed_addr #4 {
  ret void
}

; Function Attrs: norecurse nounwind readnone sspstrong uwtable
define void @_Z10gyro_startv() local_unnamed_addr #0 {
  ret void
}

; Function Attrs: nofree norecurse nounwind sspstrong uwtable
define void @_Z9read_gyroP11gyro_status(%struct.gyro_status* nocapture %0) local_unnamed_addr #3 {
  %2 = load volatile double, double* inttoptr (i64 5592400 to double*), align 16, !tbaa !14
  %3 = getelementptr inbounds %struct.gyro_status, %struct.gyro_status* %0, i64 0, i32 0
  store double %2, double* %3, align 8, !tbaa !7
  %4 = load volatile double, double* inttoptr (i64 5592416 to double*), align 32, !tbaa !14
  %5 = getelementptr inbounds %struct.gyro_status, %struct.gyro_status* %0, i64 0, i32 1
  store double %4, double* %5, align 8, !tbaa !10
  %6 = load volatile double, double* inttoptr (i64 5592432 to double*), align 16, !tbaa !14
  %7 = getelementptr inbounds %struct.gyro_status, %struct.gyro_status* %0, i64 0, i32 2
  store double %6, double* %7, align 8, !tbaa !11
  ret void
}

; Function Attrs: nounwind sspstrong uwtable
define i32 @_Z23gyro_calc_inconsistencyP11gyro_status(%struct.gyro_status* nocapture readnone %0) local_unnamed_addr #5 {
  %2 = tail call i32 @rand() #8
  %3 = icmp slt i32 %2, 100
  %4 = zext i1 %3 to i32
  ret i32 %4
}

; Function Attrs: nounwind
declare i32 @rand() local_unnamed_addr #6

; Function Attrs: nounwind sspstrong uwtable
define void @_Z11gyro_updatev() local_unnamed_addr #5 {
  %1 = load volatile double, double* inttoptr (i64 5592400 to double*), align 16, !tbaa !14
  %2 = load volatile double, double* inttoptr (i64 5592416 to double*), align 32, !tbaa !14
  %3 = load volatile double, double* inttoptr (i64 5592432 to double*), align 16, !tbaa !14
  %4 = tail call i32 @rand() #8
  ret void
}

; Function Attrs: nofree norecurse nounwind sspstrong uwtable writeonly
define void @_Z20fw_att_control_startv() local_unnamed_addr #2 {
  store i32 1, i32* @fw_att_control_handle, align 4, !tbaa !3
  ret void
}

; Function Attrs: nofree norecurse nounwind sspstrong uwtable
define void @_Z19compute_att_controlP13angular_accelP16actuator_control(%struct.angular_accel* nocapture readonly %0, %struct.actuator_control* nocapture %1) local_unnamed_addr #3 {
  %3 = getelementptr inbounds %struct.actuator_control, %struct.actuator_control* %1, i64 0, i32 0
  store i32 1, i32* %3, align 8, !tbaa !15
  %4 = bitcast %struct.angular_accel* %0 to i64*
  %5 = load i64, i64* %4, align 8, !tbaa !12
  %6 = getelementptr inbounds %struct.actuator_control, %struct.actuator_control* %1, i64 0, i32 1
  %7 = bitcast double* %6 to i64*
  store i64 %5, i64* %7, align 8, !tbaa !17
  %8 = bitcast i64 %5 to double
  %9 = fmul double %8, 1.000000e+01
  %10 = getelementptr inbounds %struct.actuator_control, %struct.actuator_control* %1, i64 0, i32 2
  store double %9, double* %10, align 8, !tbaa !18
  ret void
}

; Function Attrs: nounwind readonly sspstrong uwtable
define void @_Z21fw_att_control_updatev() local_unnamed_addr #4 {
  ret void
}

; Function Attrs: norecurse nounwind readnone sspstrong uwtable
define void @_Z8cam_initv() local_unnamed_addr #0 {
  ret void
}

; Function Attrs: nounwind readnone sspstrong uwtable
define void @_Z11cam_capturev() local_unnamed_addr #1 {
  ret void
}

; Function Attrs: nofree norecurse nounwind sspstrong uwtable writeonly
define void @_Z17airship_att_startv() local_unnamed_addr #2 {
  store i32 1, i32* @airship_att_handle1, align 4, !tbaa !3
  store i32 1, i32* @airship_att_handle2, align 4, !tbaa !3
  store i32 1, i32* @airship_att_handle3, align 4, !tbaa !3
  ret void
}

; Function Attrs: norecurse nounwind readnone sspstrong uwtable
define void @_Z16airship_att_stopv() local_unnamed_addr #0 {
  ret void
}

; Function Attrs: nounwind sspstrong uwtable
define void @_Z18airship_att_updatev() local_unnamed_addr #5 {
  %1 = tail call i32 @rand() #8
  ret void
}

; Function Attrs: nounwind readonly sspstrong uwtable
define void @_Z23airship_att_recalibratev() local_unnamed_addr #4 {
  ret void
}

; Function Attrs: norecurse nounwind readnone sspstrong uwtable
define void @_Z7px4_logv() local_unnamed_addr #0 {
  ret void
}

; Function Attrs: nofree norecurse nounwind sspstrong uwtable writeonly
define void @_Z13subscribe_allv() local_unnamed_addr #2 {
  store i32 1, i32* @vehicle_angular_acc_handle, align 4, !tbaa !3
  store i32 1, i32* @fw_att_control_handle, align 4, !tbaa !3
  store i32 1, i32* @airship_att_handle1, align 4, !tbaa !3
  store i32 1, i32* @airship_att_handle2, align 4, !tbaa !3
  store i32 1, i32* @airship_att_handle3, align 4, !tbaa !3
  ret void
}

; Function Attrs: nounwind sspstrong uwtable
define void @_Z17run_control_cyclev() local_unnamed_addr #5 {
  %1 = load volatile double, double* inttoptr (i64 5592400 to double*), align 16, !tbaa !14
  %2 = load volatile double, double* inttoptr (i64 5592416 to double*), align 32, !tbaa !14
  %3 = load volatile double, double* inttoptr (i64 5592432 to double*), align 16, !tbaa !14
  %4 = tail call i32 @rand() #8
  %5 = tail call i32 @rand() #8
  ret void
}

; Function Attrs: norecurse noreturn nounwind sspstrong uwtable
define i32 @main(i32 %0, i8** nocapture readnone %1) local_unnamed_addr #7 {
  store i32 1, i32* @vehicle_angular_acc_handle, align 4, !tbaa !3
  store i32 1, i32* @fw_att_control_handle, align 4, !tbaa !3
  store i32 1, i32* @airship_att_handle1, align 4, !tbaa !3
  store i32 1, i32* @airship_att_handle2, align 4, !tbaa !3
  store i32 1, i32* @airship_att_handle3, align 4, !tbaa !3
  br label %3

3:                                                ; preds = %2, %3
  %4 = load volatile double, double* inttoptr (i64 5592400 to double*), align 16, !tbaa !14
  %5 = load volatile double, double* inttoptr (i64 5592416 to double*), align 32, !tbaa !14
  %6 = load volatile double, double* inttoptr (i64 5592432 to double*), align 16, !tbaa !14
  %7 = tail call i32 @rand() #8
  %8 = tail call i32 @rand() #8
  br label %3
}

attributes #0 = { norecurse nounwind readnone sspstrong uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "frame-pointer"="none" "less-precise-fpmad"="false" "min-legal-vector-width"="0" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="true" "stack-protector-buffer-size"="4" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { nounwind readnone sspstrong uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "frame-pointer"="none" "less-precise-fpmad"="false" "min-legal-vector-width"="0" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="true" "stack-protector-buffer-size"="4" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #2 = { nofree norecurse nounwind sspstrong uwtable writeonly "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "frame-pointer"="none" "less-precise-fpmad"="false" "min-legal-vector-width"="0" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="true" "stack-protector-buffer-size"="4" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #3 = { nofree norecurse nounwind sspstrong uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "frame-pointer"="none" "less-precise-fpmad"="false" "min-legal-vector-width"="0" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="true" "stack-protector-buffer-size"="4" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #4 = { nounwind readonly sspstrong uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "frame-pointer"="none" "less-precise-fpmad"="false" "min-legal-vector-width"="0" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="true" "stack-protector-buffer-size"="4" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #5 = { nounwind sspstrong uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "frame-pointer"="none" "less-precise-fpmad"="false" "min-legal-vector-width"="0" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="true" "stack-protector-buffer-size"="4" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #6 = { nounwind "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "frame-pointer"="none" "less-precise-fpmad"="false" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="true" "stack-protector-buffer-size"="4" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #7 = { norecurse noreturn nounwind sspstrong uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "frame-pointer"="none" "less-precise-fpmad"="false" "min-legal-vector-width"="0" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="true" "stack-protector-buffer-size"="4" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #8 = { nounwind }

!llvm.module.flags = !{!0, !1}
!llvm.ident = !{!2}

!0 = !{i32 1, !"wchar_size", i32 4}
!1 = !{i32 7, !"PIC Level", i32 2}
!2 = !{!"clang version 11.1.0"}
!3 = !{!4, !4, i64 0}
!4 = !{!"int", !5, i64 0}
!5 = !{!"omnipotent char", !6, i64 0}
!6 = !{!"Simple C++ TBAA"}
!7 = !{!8, !9, i64 0}
!8 = !{!"_ZTS11gyro_status", !9, i64 0, !9, i64 8, !9, i64 16}
!9 = !{!"double", !5, i64 0}
!10 = !{!8, !9, i64 8}
!11 = !{!8, !9, i64 16}
!12 = !{!13, !9, i64 0}
!13 = !{!"_ZTS13angular_accel", !9, i64 0}
!14 = !{!9, !9, i64 0}
!15 = !{!16, !4, i64 0}
!16 = !{!"_ZTS16actuator_control", !4, i64 0, !9, i64 8, !9, i64 16}
!17 = !{!16, !9, i64 8}
!18 = !{!16, !9, i64 16}
