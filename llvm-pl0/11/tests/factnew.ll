define void @main() {
entry:
  %f = alloca i32                                 ; <i32*> [#uses=2]
  %fact = call i32 @fact(i32 10, i32* %f)         ; <i32> [#uses=0]
  %f1 = load i32* %f                              ; <i32> [#uses=1]
  %0 = call i32 @_print(i32 %f1)                  ; <i32> [#uses=0]
  ret void
}

define i32 @fact(i32 %n, i32* %f) {
entry:
  %n1 = alloca i32                                ; <i32*> [#uses=2]
  store i32 %n, i32* %n1
  %i = alloca i32                                 ; <i32*> [#uses=5]
  %n2 = load i32* %n1                             ; <i32> [#uses=1]
  store i32 %n2, i32* %i
  store i32 1, i32* %f
  br label %test

after:                                            ; preds = %test
  ret i32 0

body:                                             ; preds = %test
  %f4 = load i32* %f                              ; <i32> [#uses=1]
  %i5 = load i32* %i                              ; <i32> [#uses=1]
  %multmp = mul i32 %f4, %i5                      ; <i32> [#uses=1]
  store i32 %multmp, i32* %f
  %i6 = load i32* %i                              ; <i32> [#uses=1]
  %subtmp = sub i32 %i6, 1                        ; <i32> [#uses=1]
  store i32 %subtmp, i32* %i
  br label %test

test:                                             ; preds = %body, %entry
  %i3 = load i32* %i                              ; <i32> [#uses=1]
  %cmptmp = icmp sgt i32 %i3, 0                   ; <i1> [#uses=1]
  br i1 %cmptmp, label %body, label %after
}

declare i32 @_print(i32)
