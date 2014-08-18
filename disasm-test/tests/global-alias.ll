
%azaz = type { i32, i8 }

define i32 @f() {
	%a = alloca %azaz, align 4
	%ptr = getelementptr %azaz* %a, i32 0, i32 0
	store i32 42, i32* %ptr
	%x = load i32* %ptr
	ret i32 %x
}
