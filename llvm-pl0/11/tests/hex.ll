; ModuleID = 'm'

define void @main() {
entry:
  %WriteHex = call i32 @_WriteHex(i32 3870)       ; <i32> [#uses=0]
  ret void
}

declare i32 @_WriteHex(i32)
