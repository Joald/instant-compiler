  
declare void @printInt(i32)
define i32 @main() {
  %s0 = alloca i32
  store i32 1, i32* %s0
  %s1 = alloca i32
  store i32 2, i32* %s1
  %s2 = load i32, i32* %s1
  %s3 = load i32, i32* %s0
  %s4 = load i32, i32* %s0
  %s5 = load i32, i32* %s0
  %s6 = load i32, i32* %s0
  %s7 = load i32, i32* %s0
  %s8 = load i32, i32* %s0
  %s9 = load i32, i32* %s0
  %s10 = load i32, i32* %s0
  %s11 = load i32, i32* %s0
  %s12 = load i32, i32* %s0
  %s13 = load i32, i32* %s1
  %s14 = load i32, i32* %s0
  %s15 = load i32, i32* %s0
  %s16 = load i32, i32* %s0
  %s17 = load i32, i32* %s0
  %s18 = load i32, i32* %s0
  %s19 = load i32, i32* %s0
  %s20 = load i32, i32* %s0
  %s21 = load i32, i32* %s0
  %s22 = load i32, i32* %s0
  %s23 = load i32, i32* %s0
  %s24 = load i32, i32* %s0
  %s25 = load i32, i32* %s0
  %s26 = load i32, i32* %s0
  %s27 = load i32, i32* %s0
  %s28 = load i32, i32* %s0
  %s29 = load i32, i32* %s0
  %s30 = load i32, i32* %s0
  %s31 = load i32, i32* %s0
  %s32 = load i32, i32* %s0
  %s33 = load i32, i32* %s1
  %s34 = add i32 %s32, %s33
  %s35 = add i32 1, %s34
  %s36 = add i32 %s31, %s35
  %s37 = add i32 %s30, %s36
  %s38 = add i32 1, %s37
  %s39 = add i32 %s29, %s38
  %s40 = add i32 %s28, %s39
  %s41 = add i32 1, %s40
  %s42 = add i32 %s27, %s41
  %s43 = add i32 %s26, %s42
  %s44 = add i32 %s25, %s43
  %s45 = add i32 %s24, %s44
  %s46 = add i32 1, %s45
  %s47 = add i32 %s23, %s46
  %s48 = add i32 %s22, %s47
  %s49 = add i32 %s21, %s48
  %s50 = add i32 %s20, %s49
  %s51 = add i32 %s19, %s50
  %s52 = add i32 %s18, %s51
  %s53 = add i32 %s17, %s52
  %s54 = add i32 %s16, %s53
  %s55 = add i32 %s15, %s54
  %s56 = add i32 %s14, %s55
  %s57 = add i32 1, %s56
  %s58 = add i32 %s13, %s57
  %s59 = add i32 %s12, %s58
  %s60 = add i32 %s11, %s59
  %s61 = add i32 %s10, %s60
  %s62 = add i32 1, %s61
  %s63 = add i32 %s9, %s62
  %s64 = add i32 %s8, %s63
  %s65 = add i32 %s7, %s64
  %s66 = add i32 %s6, %s65
  %s67 = add i32 %s5, %s66
  %s68 = add i32 1, %s67
  %s69 = add i32 %s4, %s68
  %s70 = add i32 %s3, %s69
  %s71 = add i32 %s2, %s70
  call void @printInt(i32 %s71)
  ret i32 0
}

