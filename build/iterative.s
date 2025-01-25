.data
.align 8
main.36:
	.ascii "ASSERTION: Left != Right.\nLeft: %d\nRight: %d\n"
	.byte 0
/* end data */

.data
.align 8
main.39:
	.ascii "ASSERTION: Left != Right.\nLeft: %d\nRight: %d\n"
	.byte 0
/* end data */

.data
.align 8
main.42:
	.ascii "ASSERTION: Left != Right.\nLeft: %d\nRight: %d\n"
	.byte 0
/* end data */

.data
.align 8
main.45:
	.ascii "ASSERTION: Left != Right.\nLeft: %d\nRight: %d\n"
	.byte 0
/* end data */

.text
fib_iter:
	pushq %rbp
	movq %rsp, %rbp
	movq %rdi, %rax
	cmpq $1, %rax
	jle .Lbb8
	subq $16, %rsp
	movq %rsp, %rsi
	subq $16, %rsp
	movq %rsp, %r8
	subq $16, %rsp
	movq %rsp, %r9
	subq $16, %rsp
	movq %rsp, %r10
	movq $0, (%r10)
	movq $1, (%r9)
	movq $0, (%r8)
	movq $2, (%rsi)
	movq %rax, %rdi
	movl $2, %edx
	movl $0, %eax
	movl $1, %eax
	movl $0, %ecx
.Lbb3:
	movq %rax, %r11
	xchgq %rcx, %rax
	cmpq %rdi, %rdx
	jg .Lbb6
	addq %rcx, %rax
	movq %rax, (%r10)
	movq %rcx, (%r8)
	movq %rax, (%r9)
	addq $1, %rdx
	movq %rdx, (%rsi)
	jmp .Lbb3
.Lbb6:
	movq %r11, %rax
	movq %rbp, %rsp
	subq $0, %rsp
	leave
	ret
.Lbb8:
	movq %rbp, %rsp
	subq $0, %rsp
	leave
	ret
/* end function fib_iter */

.text
.globl main
main:
	pushq %rbp
	movq %rsp, %rbp
	sub $8, %rsp
	pushq %rbx
	pushq %r12
	pushq %r13
	movl $5, %edi
	callq fib_iter
	movq %rax, %r13
	movl $20, %edi
	callq fib_iter
	movq %rax, %r12
	movl $45, %edi
	callq fib_iter
	movq %rax, %rbx
	movl $65, %edi
	callq fib_iter
	movq %r13, %rsi
	cmpq $5, %rsi
	jnz .Lbb20
	movq %r12, %rsi
	cmpq $6765, %rsi
	jnz .Lbb19
	movq %rbx, %rsi
	cmpq $1134903170, %rsi
	jnz .Lbb18
	movq %rax, %rsi
	movq $17167680177565, %rax
	cmpq %rax, %rsi
	jnz .Lbb17
	movl $0, %eax
	popq %r13
	popq %r12
	popq %rbx
	leave
	ret
.Lbb17:
	movq $17167680177565, %rdx
	leaq main.45(%rip), %rdi
	callq printf
	movl $1, %eax
	popq %r13
	popq %r12
	popq %rbx
	leave
	ret
.Lbb18:
	movl $1134903170, %edx
	leaq main.42(%rip), %rdi
	callq printf
	movl $1, %eax
	popq %r13
	popq %r12
	popq %rbx
	leave
	ret
.Lbb19:
	movl $6765, %edx
	leaq main.39(%rip), %rdi
	callq printf
	movl $1, %eax
	popq %r13
	popq %r12
	popq %rbx
	leave
	ret
.Lbb20:
	movl $5, %edx
	leaq main.36(%rip), %rdi
	callq printf
	movl $1, %eax
	popq %r13
	popq %r12
	popq %rbx
	leave
	ret
/* end function main */

