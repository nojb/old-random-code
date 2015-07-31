
define void @main() {
entry:
  %x = alloca i32                                 ; <i32*> [#uses=3]
  %y = alloca i32                                 ; <i32*> [#uses=2]
  store i32 10, i32* %x
  %fact = call i32 @fact(i32* %x, i32* %y)        ; <i32> [#uses=0]
  %x1 = load i32* %x                              ; <i32> [#uses=1]
  %0 = call i32 @_print(i32 %x1)                  ; <i32> [#uses=0]
  %y2 = load i32* %y                              ; <i32> [#uses=1]
  %1 = call i32 @_print(i32 %y2)                  ; <i32> [#uses=0]
  ret void
}

define i32 @fact(i32* %x, i32* %y) {
entry:
  %f = alloca i32                                 ; <i32*> [#uses=4]
  store i32 1, i32* %f
  br label %test

after:                                            ; preds = %test
  %f5 = load i32* %f                              ; <i32> [#uses=1]
  store i32 %f5, i32* %y
  ret i32 0

body:                                             ; preds = %test
  %f2 = load i32* %f                              ; <i32> [#uses=1]
  %x3 = load i32* %x                              ; <i32> [#uses=1]
  %multmp = mul i32 %f2, %x3                      ; <i32> [#uses=1]
  store i32 %multmp, i32* %f
  %x4 = load i32* %x                              ; <i32> [#uses=1]
  %subtmp = sub i32 %x4, 1                        ; <i32> [#uses=1]
  store i32 %subtmp, i32* %x
  br label %test

test:                                             ; preds = %body, %entry
  %x1 = load i32* %x                              ; <i32> [#uses=1]
  %cmptmp = icmp sgt i32 %x1, 0                   ; <i1> [#uses=1]
  br i1 %cmptmp, label %body, label %after
}

declare i32 @_print(i32)
