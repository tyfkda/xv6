	.globl setjmp
	.globl longjmp

setjmp:
	movl 4(%esp), %edx	# jmp_buf
	movl 0(%esp), %eax	# Return address

	# Save old callee-save registers
	movl %ebp, 0(%edx)
	movl %ebx, 4(%edx)
	movl %esi, 8(%edx)
	movl %edi, 12(%edx)
	movl %esp, 16(%edx)
	movl %eax, 20(%edx)

	xor %eax, %eax
	ret

longjmp:
	movl 4(%esp), %edx	# jmp_buf
	movl 8(%esp), %eax	# result

	# Restore
	movl 0(%edx), %ebp
	movl 4(%edx), %ebx
	movl 8(%edx), %esi
	movl 12(%edx), %edi
	movl 16(%edx), %esp
	movl 20(%edx), %edx
	movl %edx, 0(%esp)	# Writeback return address

	ret
