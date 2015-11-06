	.section	__TEXT,__text,regular,pure_instructions
	.globl	_main
	.align	4, 0x90
_main:                                  ## @main
	.cfi_startproc
## BB#0:
	pushq	%rax
Ltmp1:
	.cfi_def_cfa_offset 16
	movl	$4, %edi
	xorl	%esi, %esi
	xorl	%eax, %eax
	callq	_stackmap
	movl	$11, %edi
	xorl	%eax, %eax
	callq	_sumf
	xorl	%eax, %eax
	popq	%rdx
	ret
	.cfi_endproc


.subsections_via_symbols
