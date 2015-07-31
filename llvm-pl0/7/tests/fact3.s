	.cstring
LC0:
	.ascii "%d\12\0"
	.text
.globl _main
_main:
	pushl	%ebp
	movl	%esp, %ebp
	pushl	%ebx
	subl	$36, %esp
	call	L6
"L00000000001$pb":
L6:
	popl	%ebx
	movl	$10, -12(%ebp)
	movl	$1, -20(%ebp)
	jmp	L2
L3:
	movl	-20(%ebp), %eax
	imull	-12(%ebp), %eax
	movl	%eax, -20(%ebp)
	decl	-12(%ebp)
L2:
	cmpl	$0, -12(%ebp)
	jg	L3
	movl	-20(%ebp), %eax
	movl	%eax, -16(%ebp)
	movl	-16(%ebp), %eax
	movl	%eax, 4(%esp)
	leal	LC0-"L00000000001$pb"(%ebx), %eax
	movl	%eax, (%esp)
	call	_printf
	movl	$0, %eax
	addl	$36, %esp
	popl	%ebx
	leave
	ret
	.subsections_via_symbols
