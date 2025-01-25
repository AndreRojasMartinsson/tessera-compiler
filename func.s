.text
.globl std..math..addf
std..math..addf:
	pushq %rbp
	movq %rsp, %rbp
	addsd %xmm1, %xmm0
	leave
	ret
/* end function std..math..addf */

.text
.globl main
main:
	pushq %rbp
	movq %rsp, %rbp
	movl $13, %esi
	leaq fmt(%rip), %rdi
	callq printf
	movsd .Lfp0(%rip), %xmm1
	movsd .Lfp0(%rip), %xmm0
	callq std..math..addf
	leaq fmt_2(%rip), %rdi
	callq printf
	movl $0, %eax
	leave
	ret
/* end function main */

.data
.align 8
fmt:
	.ascii "5 + 8 = %d\n"
	.byte 0
/* end data */

.data
.align 8
fmt_2:
	.ascii "7.5 + 7.5 = %f\n"
	.byte 0
/* end data */

/* floating point constants */
.data
.align 8
.Lfp0:
	.int 0
	.int 1075707904 /* 7.500000 */
