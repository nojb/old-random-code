@G = external global i32

declare i32 @_WriteInt(i32)

define void @main() {
entry:
  %g = load i32* @G
  %0 = call i32 @_WriteInt(i32 %g)
  ret void
}
