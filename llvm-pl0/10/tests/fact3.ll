; ModuleID = 'fact'

define void @main() {
entry:
  %result = alloca i32                            ; <i32*> [#uses=4]
  %counter = alloca i32                           ; <i32*> [#uses=5]
  store i32 1, i32* %result
  store i32 10, i32* %counter
  br label %test

after:                                            ; preds = %test
  ret void

body:                                             ; preds = %test
  %result2 = load i32* %result                    ; <i32> [#uses=1]
  %counter3 = load i32* %counter                  ; <i32> [#uses=1]
  %tmp4 = mul i32 %result2, %counter3             ; <i32> [#uses=1]
  store i32 %tmp4, i32* %result
  %counter5 = load i32* %counter                  ; <i32> [#uses=1]
  %tmp6 = sub i32 %counter5, 1                    ; <i32> [#uses=1]
  store i32 %tmp6, i32* %counter
  %result7 = load i32* %result                    ; <i32> [#uses=1]
  %WriteInt = call i32 @_WriteInt(i32 %result7)   ; <i32> [#uses=0]
  %WriteLn = call i32 @_WriteLn()                 ; <i32> [#uses=0]
  br label %test

test:                                             ; preds = %body, %entry
  %counter1 = load i32* %counter                  ; <i32> [#uses=1]
  %tmp = icmp sgt i32 %counter1, 0                ; <i1> [#uses=1]
  br i1 %tmp, label %body, label %after
}

declare i32 @_WriteInt(i32)

declare i32 @_WriteLn()
