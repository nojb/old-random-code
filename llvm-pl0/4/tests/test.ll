declare i32 @_print(i32)

define void @f(i32* %x) {
  store i32 123, i32* %x
  ret void
}

define void @main() {
  %x = alloca i32
  store i32 111, i32* %x
  call void @f(i32* %x)
  %y = load i32* %x
  %1 = call i32 @_print(i32 %y)
  ret void
}
