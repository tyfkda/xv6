	.globl setjmp
	.globl longjmp

setjmp:
	# rdi: jmp_buf
	movq 0(%rsp), %rax	# Return address

	# Save old callee-save registers
	movq %rbp, 0(%rdi)
	movq %rbx, 8(%rdi)
	movq %r11, 16(%rdi)
	movq %r12, 24(%rdi)
	movq %r13, 32(%rdi)
	movq %r14, 40(%rdi)
	movq %r15, 48(%rdi)
	movq %rsp, 56(%rdi)
	movq %rax, 64(%rdi)

	xor %eax, %eax
	ret

longjmp:
	# rdi: jmp_buf
	# rsi: result

	# Restore
	movq 0(%rdi), %rbp
	movq 8(%rdi), %rbx
	movq 16(%rdi), %r11
	movq 24(%rdi), %r12
	movq 32(%rdi), %r13
	movq 40(%rdi), %r14
	movq 48(%rdi), %r15
	movq 56(%rdi), %rsp
	movq 64(%rdi), %rax
	movq %rax, 0(%rsp)	# Writeback return address

	movl %esi, %eax

	ret
