  
declare void @printInt(i32)
define i32 @main() {
  %s0 = sub i32 44, 2
  call void @printInt(i32 %s0)
  ret i32 0
}

