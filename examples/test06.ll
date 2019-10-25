  
declare void @printInt(i32)
define i32 @main() {
  %s0 = alloca i32
  store i32 0, i32* %s0
  %s1 = alloca i32
  store i32 1, i32* %s1
  %s2 = alloca i32
  store i32 0, i32* %s2
  %s3 = alloca i32
  store i32 1, i32* %s3
  %s4 = alloca i32
  store i32 0, i32* %s4
  %s5 = alloca i32
  store i32 1, i32* %s5
  %s6 = alloca i32
  store i32 0, i32* %s6
  %s7 = alloca i32
  store i32 1, i32* %s7
  %s8 = load i32, i32* %s0
  %s9 = load i32, i32* %s1
  %s10 = mul i32 %s8, %s9
  %s11 = load i32, i32* %s2
  %s12 = load i32, i32* %s3
  %s13 = mul i32 %s11, %s12
  %s14 = load i32, i32* %s4
  %s15 = load i32, i32* %s5
  %s16 = load i32, i32* %s6
  %s17 = load i32, i32* %s7
  %s18 = add i32 %s16, %s17
  %s19 = add i32 %s15, %s18
  %s20 = add i32 %s14, %s19
  %s21 = add i32 %s13, %s20
  %s22 = add i32 %s10, %s21
  call void @printInt(i32 %s22)
  store i32 1, i32* %s0
  store i32 2, i32* %s1
  store i32 1, i32* %s2
  store i32 2, i32* %s3
  store i32 1, i32* %s4
  store i32 2, i32* %s5
  store i32 1, i32* %s6
  store i32 2, i32* %s7
  %s23 = alloca i32
  store i32 1, i32* %s23
  %s24 = alloca i32
  store i32 2, i32* %s24
  %s25 = alloca i32
  store i32 1, i32* %s25
  %s26 = alloca i32
  store i32 2, i32* %s26
  %s27 = alloca i32
  store i32 1, i32* %s27
  %s28 = alloca i32
  store i32 2, i32* %s28
  %s29 = load i32, i32* %s0
  %s30 = mul i32 2, %s29
  %s31 = load i32, i32* %s1
  %s32 = sdiv i32 %s31, 2
  %s33 = load i32, i32* %s2
  %s34 = load i32, i32* %s3
  %s35 = load i32, i32* %s4
  %s36 = load i32, i32* %s5
  %s37 = load i32, i32* %s6
  %s38 = load i32, i32* %s7
  %s39 = load i32, i32* %s23
  %s40 = load i32, i32* %s24
  %s41 = sdiv i32 %s40, 2
  %s42 = load i32, i32* %s25
  %s43 = load i32, i32* %s26
  %s44 = load i32, i32* %s27
  %s45 = load i32, i32* %s28
  %s46 = add i32 %s44, %s45
  %s47 = add i32 %s43, %s46
  %s48 = add i32 %s42, %s47
  %s49 = add i32 %s41, %s48
  %s50 = add i32 %s39, %s49
  %s51 = add i32 %s38, %s50
  %s52 = add i32 %s37, %s51
  %s53 = add i32 %s36, %s52
  %s54 = add i32 %s35, %s53
  %s55 = add i32 %s34, %s54
  %s56 = add i32 %s33, %s55
  %s57 = add i32 %s32, %s56
  %s58 = add i32 %s30, %s57
  %s59 = sdiv i32 %s58, 10
  call void @printInt(i32 %s59)
  ret i32 0
}

