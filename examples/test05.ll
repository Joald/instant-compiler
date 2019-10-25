  
declare void @printInt(i32)
define i32 @main() {
  %s0 = sub i32 1, 1
  %s1 = sub i32 1, 1
  %s2 = sub i32 1, 1
  %s3 = sub i32 1, 1
  %s4 = sub i32 1, 1
  %s5 = sub i32 1, 1
  %s6 = sub i32 1, 1
  %s7 = sub i32 1, 1
  %s8 = sub i32 1, 1
  %s9 = sub i32 1, 1
  %s10 = sub i32 1, 1
  %s11 = sub i32 1, 1
  %s12 = sub i32 1, 1
  %s13 = sub i32 1, 1
  %s14 = sub i32 1, 1
  %s15 = sub i32 1, 1
  %s16 = sub i32 1, 1
  %s17 = sub i32 1, 1
  %s18 = sub i32 1, 1
  %s19 = add i32 %s17, %s18
  %s20 = add i32 %s16, %s19
  %s21 = add i32 %s15, %s20
  %s22 = add i32 %s14, %s21
  %s23 = add i32 %s13, %s22
  %s24 = add i32 %s12, %s23
  %s25 = add i32 %s11, %s24
  %s26 = add i32 %s10, %s25
  %s27 = add i32 %s9, %s26
  %s28 = add i32 %s8, %s27
  %s29 = add i32 %s7, %s28
  %s30 = add i32 %s6, %s29
  %s31 = add i32 %s5, %s30
  %s32 = add i32 %s4, %s31
  %s33 = add i32 %s3, %s32
  %s34 = add i32 %s2, %s33
  %s35 = add i32 %s1, %s34
  %s36 = add i32 %s0, %s35
  %s37 = add i32 1, %s36
  call void @printInt(i32 %s37)
  ret i32 0
}

