
declare void @llvm.dbg.declare(metadata, metadata, metadata)

define void @f(i32 %x) {
  %y = add i32 %x, 20
  call void @llvm.dbg.declare( metadata i32 %x, metadata !{ !"x" }, metadata !{
!"0x102" })
  ret void
}
