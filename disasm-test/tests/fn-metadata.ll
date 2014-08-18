
declare void @llvm.dbg.declare(metadata, metadata)

define void @f(i32 %x) {
  %y = add i32 %x, 20
  call void @llvm.dbg.declare( metadata !{ i32 %x }, metadata !{ metadata !"x" } )
  ret void
}
