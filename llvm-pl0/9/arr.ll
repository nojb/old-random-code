define void @main() {
entry:
  %a = alloca [10 x i32]
  %a.1 = bitcast [10 x i32]* %a to i32*
  %tmp = getelementptr i32* %a.1, i32 4
  %b = load i32* %tmp
  ret void
}
