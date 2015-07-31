; ModuleID = 'm'

%0 = type { i32 }

@a = global %0 undef                              ; <%0*> [#uses=2]
@b = global %0 undef                              ; <%0*> [#uses=2]

define void @main() {
entry:
  store i32 123, i32* getelementptr inbounds (%0* @a, i32 0, i32 0)
  store i32 244, i32* getelementptr inbounds (%0* @b, i32 0, i32 0)
  %a = load %0* @a                                ; <%0> [#uses=1]
  store %0 %a, %0* @b
  store i32 45, i32* getelementptr inbounds (%0* @a, i32 0, i32 0)
  %elt = load i32* getelementptr inbounds (%0* @b, i32 0, i32 0) ; <i32> [#uses=1]
  %WriteInt = call i32 @_WriteInt(i32 %elt)       ; <i32> [#uses=0]
  %WriteLn = call i32 @_WriteLn()                 ; <i32> [#uses=0]
  ret void
}

declare i32 @_WriteInt(i32)

declare i32 @_WriteLn()
