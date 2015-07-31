define void @main() {
entry:
  %f = call i32 @f(i32 10)                        ; <i32> [#uses=0]
  ret void
}

define i32 @f(i32 %a) {
entry:
  %a1 = alloca i32                                ; <i32*> [#uses=2]
  store i32 %a, i32* %a1
  %b = alloca i32                                 ; <i32*> [#uses=1]
  %g = call i32 @g(i32* %b, i32* %a1)             ; <i32> [#uses=0]
  ret i32 0
}

define i32 @g(i32* %b, i32* %a) {
entry:
  %a1 = load i32* %a                              ; <i32> [#uses=1]
  store i32 %a1, i32* %b
  %z = call i32 @_print(i32 %a1)
  ret i32 0
}

declare i32 @_print(i32)
