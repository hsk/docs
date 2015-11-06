declare void @stackmap(i64, i32, ...)

define i32 @main() {
 call void (i64, i32, ...)* @stackmap(i64 4, i32 0)

  call void (i32, ...)* @sumf(i32 11)
  ret i32 0
}

declare void @sumf(i32, ...)

