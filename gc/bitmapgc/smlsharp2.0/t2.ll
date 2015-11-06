
declare void @stackmap(i64, i32, ...)
define void @caller_meta_leaf() {
entry:
  %metadata = alloca i64, i32 3, align 8
  store i64 11, i64* %metadata
  store i64 12, i64* %metadata
  store i64 13, i64* %metadata
  call void(i64, i32, ...)* @stackmap(i64 4, i32 0, i64* %metadata)
  call void(i64, i32, ...) @llvm.experimental.stackmap(i64 4, i32 0, i64* %metadata)
  ret void
}


declare void @llvm.experimental.stackmap(i64, i32, ...)
declare void @llvm.experimental.patchpoint.void(i64, i32, i8*, i32, ...)
declare i64 @llvm.experimental.patchpoint.i64(i64, i32, i8*, i32, ...)

