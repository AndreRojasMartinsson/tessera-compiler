.text
.globl main
main:
	pushq %rbp
	movq %rsp, %rbp
	movq 24(%rsi), %rsi
	leaq fmt(%rip), %rdi
	callq printf
	movl $0, %eax
	leave
	ret
/* end function main */

.data
.align 8
fmt:
	.ascii "Hi: %s\n"
	.byte 0
/* end data */

