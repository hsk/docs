	.section	__TEXT,__text,regular,pure_instructions
	.globl	_test1
	.align	4, 0x90
_test1:                                 ## @test1
	.cfi_startproc
## BB#0:                                ## %entry
	pushq	%r14
Ltmp3:
	.cfi_def_cfa_offset 16
	pushq	%rbx
Ltmp4:
	.cfi_def_cfa_offset 24
	pushq	%rax
Ltmp5:
	.cfi_def_cfa_offset 32
Ltmp6:
	.cfi_offset %rbx, -24
Ltmp7:
	.cfi_offset %r14, -16
	movq	%rsi, %r14
	movabsq	$5518974240767171908, %rbx ## imm = 0x4C9755D4C9755D44
	addq	%rdi, %rbx
	adcq	$7, %r14
	movl	$1, %edi
	movl	$240, %esi
	callq	_tackmap
	movq	%rbx, %rax
	movq	%r14, %rdx
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	ret
	.cfi_endproc


.subsections_via_symbols
