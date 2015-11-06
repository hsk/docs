	.section	__TEXT,__text,regular,pure_instructions
	.macosx_version_min 13, 4
	.globl	_heap_find
	.align	4, 0x90
_heap_find:                             ## @heap_find
	.cfi_startproc
## BB#0:
	subl	$20, %esp
Ltmp0:
	.cfi_def_cfa_offset 24
	movl	24(%esp), %eax
	movl	%eax, 8(%esp)
	movl	L_heap_list$non_lazy_ptr, %eax
	jmp	LBB0_1
	.align	4, 0x90
LBB0_4:                                 ##   in Loop: Header=BB0_1 Depth=1
	movl	(%esp), %eax
LBB0_1:                                 ## =>This Inner Loop Header: Depth=1
	movl	(%eax), %eax
	movl	%eax, (%esp)
	cmpl	$0, (%esp)
	je	LBB0_5
## BB#2:                                ##   in Loop: Header=BB0_1 Depth=1
	movl	(%esp), %eax
	cmpl	8(%esp), %eax
	jne	LBB0_4
## BB#3:
	movl	$1, 16(%esp)
	jmp	LBB0_6
LBB0_5:
	movl	$0, 16(%esp)
LBB0_6:
	movl	16(%esp), %eax
	addl	$20, %esp
	retl
	.cfi_endproc

	.globl	_gc_mark_object
	.align	4, 0x90
_gc_mark_object:                        ## @gc_mark_object
	.cfi_startproc
## BB#0:
	pushl	%esi
Ltmp1:
	.cfi_def_cfa_offset 8
	subl	$56, %esp
Ltmp2:
	.cfi_def_cfa_offset 64
Ltmp3:
	.cfi_offset %esi, -8
	movl	64(%esp), %eax
	movl	%eax, 48(%esp)
	addl	$-12, %eax
	movl	%eax, 40(%esp)
	movl	%eax, 4(%esp)
	movl	$L_.str, (%esp)
	calll	_printf
	movl	40(%esp), %eax
	movl	%eax, (%esp)
	calll	_heap_find
	testl	%eax, %eax
	je	LBB1_18
## BB#1:
	movl	40(%esp), %eax
	cmpb	$0, 9(%eax)
	jne	LBB1_18
## BB#2:
	movl	40(%esp), %eax
	movb	$1, 9(%eax)
	movl	40(%esp), %eax
	movzbl	8(%eax), %eax
	cmpl	$3, %eax
	je	LBB1_10
## BB#3:
	cmpl	$2, %eax
	jne	LBB1_4
## BB#17:
	movl	$L_.str2, (%esp)
	calll	_printf
	movl	48(%esp), %eax
	movl	(%eax), %eax
	movl	%eax, (%esp)
	calll	_gc_mark_object
	movl	48(%esp), %eax
	movl	4(%eax), %eax
	movl	%eax, (%esp)
	calll	_gc_mark_object
	jmp	LBB1_18
LBB1_10:
	movl	40(%esp), %eax
	movl	4(%eax), %eax
	movl	%eax, %ecx
	sarl	$31, %ecx
	shrdl	$3, %ecx, %eax
	shrl	$3, %ecx
	movl	%ecx, 36(%esp)
	movl	%eax, 32(%esp)
	movl	%ecx, 8(%esp)
	movl	%eax, 4(%esp)
	movl	$L_.str3, (%esp)
	calll	_printf
	movl	32(%esp), %eax
	shll	$3, %eax
	addl	48(%esp), %eax
	movl	%eax, 24(%esp)
	movl	32(%esp), %eax
	movl	36(%esp), %ecx
	movl	%ecx, 8(%esp)
	movl	%eax, 4(%esp)
	movl	$L_.str1, (%esp)
	calll	_printf
	movl	$0, 16(%esp)
	jmp	LBB1_11
	.align	4, 0x90
LBB1_15:                                ##   in Loop: Header=BB1_11 Depth=1
	movl	16(%esp), %eax
	movl	48(%esp), %ecx
	movl	(%ecx,%eax,4), %eax
	movl	%eax, (%esp)
	calll	_gc_mark_object
	incl	16(%esp)
LBB1_11:                                ## =>This Inner Loop Header: Depth=1
	movl	16(%esp), %eax
	movl	%eax, %ecx
	sarl	$31, %ecx
	cmpl	32(%esp), %eax
	setae	%al
	cmpl	36(%esp), %ecx
	setge	%cl
	je	LBB1_13
## BB#12:                               ##   in Loop: Header=BB1_11 Depth=1
	movb	%cl, %al
LBB1_13:                                ##   in Loop: Header=BB1_11 Depth=1
	testb	%al, %al
	jne	LBB1_18
## BB#14:                               ##   in Loop: Header=BB1_11 Depth=1
	movl	16(%esp), %ecx
	movl	%ecx, %eax
	andl	$-8, %eax
	movl	24(%esp), %edx
	andb	$7, %cl
	movl	$1, %esi
                                        ## kill: CL<def> CL<kill> ECX<kill>
	shll	%cl, %esi
	movl	%esi, %ecx
	sarl	$31, %ecx
	andl	(%edx,%eax), %esi
	andl	4(%edx,%eax), %ecx
	orl	%esi, %ecx
	jne	LBB1_15
## BB#16:                               ##   in Loop: Header=BB1_11 Depth=1
	movl	16(%esp), %eax
	movl	%eax, 4(%esp)
	movl	$L_.str4, (%esp)
	calll	_printf
	incl	16(%esp)
	jmp	LBB1_11
LBB1_4:
	testl	%eax, %eax
	jne	LBB1_18
## BB#5:
	movl	40(%esp), %eax
	movl	4(%eax), %eax
	movl	%eax, %ecx
	sarl	$31, %ecx
	shrdl	$3, %ecx, %eax
	shrl	$3, %ecx
	movl	%ecx, 36(%esp)
	movl	%eax, 32(%esp)
	movl	%ecx, 8(%esp)
	movl	%eax, 4(%esp)
	movl	$L_.str1, (%esp)
	calll	_printf
	movl	$0, 20(%esp)
	jmp	LBB1_6
	.align	4, 0x90
LBB1_9:                                 ##   in Loop: Header=BB1_6 Depth=1
	movl	20(%esp), %eax
	movl	48(%esp), %ecx
	movl	(%ecx,%eax,4), %eax
	movl	%eax, (%esp)
	calll	_gc_mark_object
	incl	20(%esp)
LBB1_6:                                 ## =>This Inner Loop Header: Depth=1
	movl	20(%esp), %eax
	movl	%eax, %ecx
	sarl	$31, %ecx
	cmpl	32(%esp), %eax
	setae	%al
	cmpl	36(%esp), %ecx
	setge	%cl
	je	LBB1_8
## BB#7:                                ##   in Loop: Header=BB1_6 Depth=1
	movb	%cl, %al
LBB1_8:                                 ##   in Loop: Header=BB1_6 Depth=1
	testb	%al, %al
	je	LBB1_9
LBB1_18:
	addl	$56, %esp
	popl	%esi
	retl
	.cfi_endproc

	.globl	_gc_add_frame_map
	.align	4, 0x90
_gc_add_frame_map:                      ## @gc_add_frame_map
	.cfi_startproc
## BB#0:
	subl	$12, %esp
Ltmp4:
	.cfi_def_cfa_offset 16
	movl	16(%esp), %eax
	movl	%eax, 8(%esp)
	movzwl	(%eax), %eax
	testb	$1, %al
	jne	LBB2_2
## BB#1:
	movl	L_gc_frame_map_list$non_lazy_ptr, %eax
	movl	(%eax), %ecx
	movl	8(%esp), %edx
	movl	%ecx, 16(%edx)
	movl	8(%esp), %ecx
	movl	%ecx, (%eax)
	addl	$12, %esp
	retl
LBB2_2:
	movl	8(%esp), %eax
	movzwl	(%eax), %eax
	movl	%eax, 4(%esp)
	movl	$L_.str5, (%esp)
	calll	_printf
	movl	$0, (%esp)
	calll	_exit
	.cfi_endproc

	.globl	_get_stack_top
	.align	4, 0x90
_get_stack_top:                         ## @get_stack_top
	.cfi_startproc
## BB#0:
	subl	$12, %esp
Ltmp5:
	.cfi_def_cfa_offset 16
	leal	8(%esp), %eax
	movl	%eax, (%esp)
	leal	12(%esp), %eax
	movl	%eax, (%esp)
	movl	12(%esp), %eax
	addl	$12, %esp
	retl
	.cfi_endproc

	.globl	_gc_mark_find_frame_map
	.align	4, 0x90
_gc_mark_find_frame_map:                ## @gc_mark_find_frame_map
	.cfi_startproc
## BB#0:
	subl	$20, %esp
Ltmp6:
	.cfi_def_cfa_offset 24
	movl	24(%esp), %eax
	movl	%eax, 8(%esp)
	movl	L_gc_frame_map_list$non_lazy_ptr, %eax
	movl	(%eax), %eax
	jmp	LBB4_1
	.align	4, 0x90
LBB4_5:                                 ##   in Loop: Header=BB4_1 Depth=1
	movl	(%esp), %eax
	movl	16(%eax), %eax
LBB4_1:                                 ## =>This Inner Loop Header: Depth=1
	movl	%eax, (%esp)
	cmpl	$0, (%esp)
	je	LBB4_6
## BB#2:                                ##   in Loop: Header=BB4_1 Depth=1
	movl	(%esp), %eax
	movl	4(%eax), %eax
	cmpl	8(%esp), %eax
	ja	LBB4_5
## BB#3:                                ##   in Loop: Header=BB4_1 Depth=1
	movl	8(%esp), %eax
	movl	(%esp), %ecx
	cmpl	8(%ecx), %eax
	ja	LBB4_5
## BB#4:
	movl	(%esp), %eax
	movl	%eax, 16(%esp)
	jmp	LBB4_7
LBB4_6:
	movl	$0, 16(%esp)
LBB4_7:
	movl	16(%esp), %eax
	addl	$20, %esp
	retl
	.cfi_endproc

	.globl	_gc_mark_find_frame
	.align	4, 0x90
_gc_mark_find_frame:                    ## @gc_mark_find_frame
	.cfi_startproc
## BB#0:
	subl	$28, %esp
Ltmp7:
	.cfi_def_cfa_offset 32
	movl	36(%esp), %eax
	movl	32(%esp), %ecx
	movl	%ecx, 16(%esp)
	movl	%eax, 8(%esp)
	movl	16(%esp), %eax
	movl	12(%eax), %eax
	movl	%eax, (%esp)
	jmp	LBB5_1
	.align	4, 0x90
LBB5_4:                                 ##   in Loop: Header=BB5_1 Depth=1
	addl	$12, (%esp)
LBB5_1:                                 ## =>This Inner Loop Header: Depth=1
	cmpl	$0, (%esp)
	je	LBB5_5
## BB#2:                                ##   in Loop: Header=BB5_1 Depth=1
	movl	(%esp), %eax
	movl	12(%eax), %eax
	cmpl	8(%esp), %eax
	jbe	LBB5_4
## BB#3:
	movl	(%esp), %eax
	movl	%eax, 24(%esp)
	jmp	LBB5_6
LBB5_5:
	movl	$0, 24(%esp)
LBB5_6:
	movl	24(%esp), %eax
	addl	$28, %esp
	retl
	.cfi_endproc

	.globl	_gc_mark_frame
	.align	4, 0x90
_gc_mark_frame:                         ## @gc_mark_frame
	.cfi_startproc
## BB#0:
	subl	$28, %esp
Ltmp8:
	.cfi_def_cfa_offset 32
	movl	36(%esp), %eax
	movl	32(%esp), %ecx
	movl	%ecx, 24(%esp)
	movl	%eax, 16(%esp)
	movl	$0, 12(%esp)
	jmp	LBB6_1
	.align	4, 0x90
LBB6_7:                                 ##   in Loop: Header=BB6_1 Depth=1
	incl	12(%esp)
LBB6_1:                                 ## =>This Loop Header: Depth=1
                                        ##     Child Loop BB6_3 Depth 2
	movl	24(%esp), %eax
	movzwl	4(%eax), %eax
	cmpl	%eax, 12(%esp)
	jge	LBB6_8
## BB#2:                                ##   in Loop: Header=BB6_1 Depth=1
	movl	12(%esp), %eax
	movl	24(%esp), %ecx
	movl	8(%ecx), %ecx
	movl	(%ecx,%eax,4), %eax
	movl	%eax, 8(%esp)
	movl	12(%esp), %eax
	shll	$5, %eax
	movl	%eax, 4(%esp)
	jmp	LBB6_3
	.align	4, 0x90
LBB6_6:                                 ##   in Loop: Header=BB6_3 Depth=2
	shrl	8(%esp)
	incl	4(%esp)
LBB6_3:                                 ##   Parent Loop BB6_1 Depth=1
                                        ## =>  This Inner Loop Header: Depth=2
	cmpl	$0, 8(%esp)
	je	LBB6_7
## BB#4:                                ##   in Loop: Header=BB6_3 Depth=2
	testb	$1, 8(%esp)
	je	LBB6_6
## BB#5:                                ##   in Loop: Header=BB6_3 Depth=2
	movl	4(%esp), %eax
	movl	16(%esp), %ecx
	movl	(%ecx,%eax,4), %eax
	movl	%eax, (%esp)
	calll	_gc_mark_object
	jmp	LBB6_6
LBB6_8:
	addl	$28, %esp
	retl
	.cfi_endproc

	.globl	_gc_mark
	.align	4, 0x90
_gc_mark:                               ## @gc_mark
	.cfi_startproc
## BB#0:
	pushl	%esi
Ltmp9:
	.cfi_def_cfa_offset 8
	subl	$56, %esp
Ltmp10:
	.cfi_def_cfa_offset 64
Ltmp11:
	.cfi_offset %esi, -8
	movl	68(%esp), %eax
	movl	64(%esp), %ecx
	movl	%ecx, 48(%esp)
	movl	%eax, 40(%esp)
	movl	L_gc_top_ptr$non_lazy_ptr, %esi
	.align	4, 0x90
LBB7_1:                                 ## =>This Inner Loop Header: Depth=1
	movl	48(%esp), %eax
	movl	4(%eax), %eax
	movl	%eax, 40(%esp)
	movl	48(%esp), %eax
	movl	(%eax), %eax
	movl	%eax, 48(%esp)
	movl	40(%esp), %eax
	movl	%eax, (%esp)
	calll	_gc_mark_find_frame_map
	movl	%eax, 32(%esp)
	testl	%eax, %eax
	je	LBB7_4
## BB#2:                                ##   in Loop: Header=BB7_1 Depth=1
	movl	32(%esp), %eax
	movl	40(%esp), %ecx
	movl	%ecx, 4(%esp)
	movl	%eax, (%esp)
	calll	_gc_mark_find_frame
	movl	%eax, 24(%esp)
	testl	%eax, %eax
	je	LBB7_4
## BB#3:                                ##   in Loop: Header=BB7_1 Depth=1
	movl	24(%esp), %eax
	movzwl	4(%eax), %eax
	movl	%eax, 4(%esp)
	movl	$L_.str6, (%esp)
	calll	_printf
	movl	32(%esp), %eax
	movzwl	(%eax), %eax
	movl	$-2, %ecx
	subl	%eax, %ecx
	shll	$2, %ecx
	addl	48(%esp), %ecx
	movl	%ecx, 16(%esp)
	movl	24(%esp), %eax
	movl	%ecx, 4(%esp)
	movl	%eax, (%esp)
	calll	_gc_mark_frame
LBB7_4:                                 ##   in Loop: Header=BB7_1 Depth=1
	movl	48(%esp), %eax
	cmpl	(%esi), %eax
	jb	LBB7_1
## BB#5:
	addl	$56, %esp
	popl	%esi
	retl
	.cfi_endproc

	.globl	_gc_sweep
	.align	4, 0x90
_gc_sweep:                              ## @gc_sweep
	.cfi_startproc
## BB#0:
	pushl	%esi
Ltmp12:
	.cfi_def_cfa_offset 8
	subl	$24, %esp
Ltmp13:
	.cfi_def_cfa_offset 32
Ltmp14:
	.cfi_offset %esi, -8
	movl	L_heap_list$non_lazy_ptr, %eax
	movl	%eax, 16(%esp)
	movl	L_heap_num$non_lazy_ptr, %esi
	jmp	LBB8_1
	.align	4, 0x90
LBB8_4:                                 ##   in Loop: Header=BB8_1 Depth=1
	movl	16(%esp), %eax
	movl	(%eax), %eax
	movb	$0, 9(%eax)
	movl	16(%esp), %eax
	movl	(%eax), %eax
	movl	%eax, 16(%esp)
LBB8_1:                                 ## =>This Inner Loop Header: Depth=1
	movl	16(%esp), %eax
	cmpl	$0, (%eax)
	je	LBB8_5
## BB#2:                                ##   in Loop: Header=BB8_1 Depth=1
	movl	16(%esp), %eax
	movl	(%eax), %eax
	cmpb	$0, 9(%eax)
	jne	LBB8_4
## BB#3:                                ##   in Loop: Header=BB8_1 Depth=1
	movl	16(%esp), %eax
	movl	(%eax), %eax
	movl	%eax, 8(%esp)
	movl	(%eax), %eax
	movl	16(%esp), %ecx
	movl	%eax, (%ecx)
	movl	8(%esp), %eax
	movl	%eax, (%esp)
	calll	_free
	decl	(%esi)
	jmp	LBB8_1
LBB8_5:
	addl	$24, %esp
	popl	%esi
	retl
	.cfi_endproc

	.globl	_gc_collect
	.align	4, 0x90
_gc_collect:                            ## @gc_collect
	.cfi_startproc
## BB#0:
	pushl	%esi
Ltmp15:
	.cfi_def_cfa_offset 8
	subl	$24, %esp
Ltmp16:
	.cfi_def_cfa_offset 32
Ltmp17:
	.cfi_offset %esi, -8
	movl	L_heap_num$non_lazy_ptr, %esi
	movl	(%esi), %eax
	movl	%eax, 12(%esp)
	calll	_get_stack_top
	movl	%eax, (%esp)
	movl	$0, 4(%esp)
	calll	_gc_mark
	calll	_gc_sweep
	movl	12(%esp), %eax
	addl	%eax, %eax
	movl	L_heap_max$non_lazy_ptr, %ecx
	movl	%eax, (%ecx)
	movl	12(%esp), %eax
	movl	(%esi), %ecx
	subl	%ecx, %eax
	movl	%ecx, 8(%esp)
	movl	%eax, 4(%esp)
	movl	$L_.str7, (%esp)
	calll	_printf
	addl	$24, %esp
	popl	%esi
	retl
	.cfi_endproc

	.globl	_gc_alloc
	.align	4, 0x90
_gc_alloc:                              ## @gc_alloc
	.cfi_startproc
## BB#0:
	pushl	%esi
Ltmp18:
	.cfi_def_cfa_offset 8
	subl	$24, %esp
Ltmp19:
	.cfi_def_cfa_offset 32
Ltmp20:
	.cfi_offset %esi, -8
	movl	36(%esp), %eax
	movl	32(%esp), %ecx
	movl	%ecx, 20(%esp)
	movl	%eax, 16(%esp)
	movl	L_heap_num$non_lazy_ptr, %esi
	movl	(%esi), %eax
	movl	L_heap_max$non_lazy_ptr, %ecx
	cmpl	(%ecx), %eax
	jne	LBB10_2
## BB#1:
	calll	_gc_collect
LBB10_2:
	movl	16(%esp), %eax
	movl	%eax, %ecx
	sarl	$31, %ecx
	addl	$16, %eax
	adcl	$0, %ecx
	movl	%ecx, 4(%esp)
	movl	%eax, (%esp)
	calll	_malloc
	movl	%eax, 8(%esp)
	movl	%eax, 4(%esp)
	movl	$L_.str8, (%esp)
	calll	_printf
	movb	20(%esp), %al
	movl	8(%esp), %ecx
	movb	%al, 8(%ecx)
	movl	L_heap_list$non_lazy_ptr, %eax
	movl	(%eax), %ecx
	movl	8(%esp), %edx
	movl	%ecx, (%edx)
	movl	8(%esp), %ecx
	movl	%ecx, (%eax)
	movl	8(%esp), %eax
	movb	$0, 9(%eax)
	movl	16(%esp), %eax
	movl	8(%esp), %ecx
	movl	%eax, 4(%ecx)
	incl	(%esi)
	movl	8(%esp), %eax
	addl	$12, %eax
	addl	$24, %esp
	popl	%esi
	retl
	.cfi_endproc

	.globl	_gc_alloc_int
	.align	4, 0x90
_gc_alloc_int:                          ## @gc_alloc_int
	.cfi_startproc
## BB#0:
	subl	$28, %esp
Ltmp21:
	.cfi_def_cfa_offset 32
	movl	32(%esp), %eax
	movl	%eax, 24(%esp)
	movl	$4, 4(%esp)
	movl	$1, (%esp)
	calll	_gc_alloc
	movl	%eax, 16(%esp)
	movl	%eax, 4(%esp)
	movl	$L_.str9, (%esp)
	calll	_printf
	movl	24(%esp), %eax
	movl	16(%esp), %ecx
	movl	%eax, (%ecx)
	movl	16(%esp), %eax
	addl	$28, %esp
	retl
	.cfi_endproc

	.globl	_gc_init
	.align	4, 0x90
_gc_init:                               ## @gc_init
	.cfi_startproc
## BB#0:
	subl	$12, %esp
Ltmp22:
	.cfi_def_cfa_offset 16
	calll	_get_stack_top
	movl	L_gc_top_ptr$non_lazy_ptr, %ecx
	movl	%eax, (%ecx)
	movl	L_heap_list$non_lazy_ptr, %eax
	movl	$0, (%eax)
	movl	L_heap_num$non_lazy_ptr, %eax
	movl	$0, (%eax)
	movl	L_heap_max$non_lazy_ptr, %eax
	movl	$8, (%eax)
	addl	$12, %esp
	retl
	.cfi_endproc

	.globl	_gc_free
	.align	4, 0x90
_gc_free:                               ## @gc_free
	.cfi_startproc
## BB#0:
	subl	$12, %esp
Ltmp23:
	.cfi_def_cfa_offset 16
	calll	_gc_collect
	addl	$12, %esp
	retl
	.cfi_endproc

	.globl	_test
	.align	4, 0x90
_test:                                  ## @test
	.cfi_startproc
## BB#0:
	subl	$28, %esp
Ltmp24:
	.cfi_def_cfa_offset 32
	jmpl	*_test.start_ptr
Ltmp25:                                 ## Block address taken
LBB14_1:
	movl	$_test.f, (%esp)
	calll	_gc_add_frame_map
	movl	$Ltmp26, _test.start_ptr
Ltmp26:                                 ## Block address taken
LBB14_2:
	leal	16(%esp), %eax
	movl	%eax, 4(%esp)
	movl	$L_.str10, (%esp)
	calll	_printf
	movl	$16, 4(%esp)
	movl	$0, (%esp)
	calll	_gc_alloc
	movl	%eax, 16(%esp)
Ltmp27:                                 ## Block address taken
LBB14_3:
	calll	_gc_collect
Ltmp28:                                 ## Block address taken
LBB14_4:
	calll	_gc_collect
Ltmp29:                                 ## Block address taken
LBB14_5:
	calll	_gc_collect
	addl	$28, %esp
	retl
	.cfi_endproc

	.globl	_test2
	.align	4, 0x90
_test2:                                 ## @test2
	.cfi_startproc
## BB#0:
	subl	$28, %esp
Ltmp30:
	.cfi_def_cfa_offset 32
	jmpl	*_test2.start_ptr
Ltmp31:                                 ## Block address taken
LBB15_1:
	movl	$_test2.f, (%esp)
	calll	_gc_add_frame_map
	movl	$Ltmp32, _test2.start_ptr
Ltmp32:                                 ## Block address taken
LBB15_2:
	movl	$16, 4(%esp)
	movl	$0, (%esp)
	calll	_gc_alloc
	movl	%eax, 16(%esp)
	movl	$16, 4(%esp)
	movl	$0, (%esp)
	calll	_gc_alloc
	movl	%eax, 20(%esp)
Ltmp33:                                 ## Block address taken
LBB15_3:
	calll	_gc_collect
Ltmp34:                                 ## Block address taken
LBB15_4:
	addl	$28, %esp
	retl
	.cfi_endproc

	.globl	_test3
	.align	4, 0x90
_test3:                                 ## @test3
	.cfi_startproc
## BB#0:
	subl	$44, %esp
Ltmp35:
	.cfi_def_cfa_offset 48
	jmpl	*_test3.start_ptr
Ltmp36:                                 ## Block address taken
LBB16_1:
	movl	$_test3.f, (%esp)
	calll	_gc_add_frame_map
	movl	$Ltmp37, _test3.start_ptr
Ltmp37:                                 ## Block address taken
LBB16_2:
	leal	16(%esp), %eax
	movl	%eax, 4(%esp)
	movl	$L_.str11, (%esp)
	calll	_printf
	movl	$16, 4(%esp)
	movl	$2, (%esp)
	calll	_gc_alloc
	movl	%eax, 16(%esp)
	movl	$10, (%esp)
	calll	_gc_alloc_int
	movl	16(%esp), %ecx
	movl	%eax, (%ecx)
	movl	$20, (%esp)
	calll	_gc_alloc_int
	movl	16(%esp), %ecx
	movl	%eax, 4(%ecx)
Ltmp38:                                 ## Block address taken
LBB16_3:
	movl	$16, 4(%esp)
	movl	$0, (%esp)
	calll	_gc_alloc
	movl	%eax, 20(%esp)
	movl	$30, (%esp)
	calll	_gc_alloc_int
	movl	20(%esp), %ecx
	movl	%eax, (%ecx)
	movl	$40, (%esp)
	calll	_gc_alloc_int
	movl	20(%esp), %ecx
	movl	%eax, 4(%ecx)
Ltmp39:                                 ## Block address taken
LBB16_4:
	movl	$8, 4(%esp)
	movl	$1, (%esp)
	calll	_gc_alloc
	movl	%eax, 24(%esp)
	movl	$50, (%eax)
	movl	24(%esp), %eax
	movl	$60, 4(%eax)
	movl	16(%esp), %eax
	movl	(%eax), %eax
	movl	(%eax), %ecx
	movl	%ecx, 8(%esp)
	movl	%eax, 4(%esp)
	movl	$L_.str12, (%esp)
	calll	_printf
	movl	16(%esp), %eax
	movl	4(%eax), %eax
	movl	(%eax), %ecx
	movl	%ecx, 8(%esp)
	movl	%eax, 4(%esp)
	movl	$L_.str13, (%esp)
	calll	_printf
	movl	20(%esp), %eax
	movl	(%eax), %eax
	movl	(%eax), %ecx
	movl	%ecx, 8(%esp)
	movl	%eax, 4(%esp)
	movl	$L_.str14, (%esp)
	calll	_printf
	movl	20(%esp), %eax
	movl	4(%eax), %eax
	movl	(%eax), %ecx
	movl	%ecx, 8(%esp)
	movl	%eax, 4(%esp)
	movl	$L_.str15, (%esp)
	calll	_printf
	movl	24(%esp), %eax
	movl	(%eax), %ecx
	movl	%ecx, 8(%esp)
	movl	%eax, 4(%esp)
	movl	$L_.str16, (%esp)
	calll	_printf
	movl	24(%esp), %eax
	movl	4(%eax), %ecx
	addl	$4, %eax
	movl	%ecx, 8(%esp)
	movl	%eax, 4(%esp)
	movl	$L_.str17, (%esp)
	calll	_printf
	calll	_gc_collect
Ltmp40:                                 ## Block address taken
LBB16_5:
	addl	$44, %esp
	retl
	.cfi_endproc

	.globl	_test_int
	.align	4, 0x90
_test_int:                              ## @test_int
	.cfi_startproc
## BB#0:
	subl	$28, %esp
Ltmp41:
	.cfi_def_cfa_offset 32
	movl	32(%esp), %eax
	movl	%eax, 24(%esp)
	jmpl	*_test_int.start_ptr
	.align	4, 0x90
Ltmp42:                                 ## Block address taken
LBB17_3:                                ## =>This Inner Loop Header: Depth=1
	movl	$_test_int.f, (%esp)
	calll	_gc_add_frame_map
	movl	$Ltmp43, _test_int.start_ptr
	movl	$Ltmp43, %eax
	jmpl	*%eax
Ltmp43:                                 ## Block address taken
LBB17_1:
	movl	24(%esp), %eax
	movl	%eax, (%esp)
	calll	_gc_alloc_int
	movl	%eax, 16(%esp)
	movl	%eax, 8(%esp)
Ltmp44:                                 ## Block address taken
LBB17_2:
	movl	8(%esp), %eax
	addl	$28, %esp
	retl
	.cfi_endproc

	.globl	_test_record
	.align	4, 0x90
_test_record:                           ## @test_record
	.cfi_startproc
## BB#0:
	subl	$28, %esp
Ltmp45:
	.cfi_def_cfa_offset 32
	jmpl	*_test_record.start_ptr
	.align	4, 0x90
Ltmp46:                                 ## Block address taken
LBB18_3:                                ## =>This Inner Loop Header: Depth=1
	movl	$_test_record.f, (%esp)
	calll	_gc_add_frame_map
	movl	$Ltmp47, _test_record.start_ptr
	movl	$Ltmp47, %eax
	jmpl	*%eax
Ltmp47:                                 ## Block address taken
LBB18_1:
	movl	$25, 4(%esp)
	movl	$3, (%esp)
	calll	_gc_alloc
	movl	%eax, 16(%esp)
	movl	$0, 4(%eax)
	movl	$10, (%eax)
	movl	$20, (%esp)
	calll	_gc_alloc_int
	movl	16(%esp), %ecx
	movl	%eax, 4(%ecx)
	movl	$30, (%esp)
	calll	_test_int
	movl	16(%esp), %ecx
	movl	%eax, 8(%ecx)
	movl	16(%esp), %eax
	movl	$0, 28(%eax)
	movl	$6, 24(%eax)
	calll	_gc_collect
Ltmp48:                                 ## Block address taken
LBB18_2:
	addl	$28, %esp
	retl
	.cfi_endproc

	.globl	_main
	.align	4, 0x90
_main:                                  ## @main
	.cfi_startproc
## BB#0:
	subl	$28, %esp
Ltmp49:
	.cfi_def_cfa_offset 32
	movl	$0, 24(%esp)
	calll	_gc_init
	calll	_test
	calll	_gc_free
	movl	$L_.str18, (%esp)
	calll	_printf
	calll	_gc_init
	calll	_test2
	calll	_gc_free
	movl	$L_.str18, (%esp)
	calll	_printf
	calll	_gc_init
	calll	_test3
	calll	_gc_free
	movl	$L_.str18, (%esp)
	calll	_printf
	calll	_gc_init
	calll	_test_record
	calll	_gc_free
	movl	$0, 16(%esp)
	movl	$16, 12(%esp)
	movl	$0, 8(%esp)
	movl	$4, 4(%esp)
	movl	$L_.str19, (%esp)
	calll	_printf
	xorl	%eax, %eax
	addl	$28, %esp
	retl
	.cfi_endproc

	.comm	_heap_list,4,3          ## @heap_list
	.section	__TEXT,__cstring,cstring_literals
L_.str:                                 ## @.str
	.asciz	"mark %p\n"

L_.str1:                                ## @.str1
	.asciz	"size=%ld\n"

L_.str2:                                ## @.str2
	.asciz	"PAIR\n"

L_.str3:                                ## @.str3
	.asciz	"RECORD size=%ld\n"

L_.str4:                                ## @.str4
	.asciz	"skip %d\n"

L_.str5:                                ## @.str5
	.asciz	"frame size error %d\n"

	.comm	_gc_frame_map_list,4,3  ## @gc_frame_map_list
L_.str6:                                ## @.str6
	.asciz	"find frame bitmap_size=%d\n"

	.comm	_gc_top_ptr,4,3         ## @gc_top_ptr
	.comm	_heap_num,4,2           ## @heap_num
	.comm	_heap_max,4,2           ## @heap_max
L_.str7:                                ## @.str7
	.asciz	"Collected %d objects, %d remaining.\n"

L_.str8:                                ## @.str8
	.asciz	"gc_alloc %p\n"

L_.str9:                                ## @.str9
	.asciz	"int ptr %p\n"

	.section	__DATA,__data
	.align	3                       ## @test.start_ptr
_test.start_ptr:
	.long	Ltmp25

	.section	__TEXT,__cstring,cstring_literals
L_.str10:                               ## @.str10
	.asciz	"frame[1]=%p\n"

	.section	__DATA,__data
	.align	2                       ## @test.bitmap
_test.bitmap:
	.long	1                       ## 0x1
	.long	0                       ## 0x0

	.align	4                       ## @test.frames
_test.frames:
	.long	Ltmp27
	.short	1                       ## 0x1
	.space	2
	.long	_test.bitmap
	.long	Ltmp28
	.short	1                       ## 0x1
	.space	2
	.long	_test.bitmap
	.long	Ltmp29
	.short	0                       ## 0x0
	.space	2
	.long	_test.bitmap
	.long	Ltmp25
	.short	0                       ## 0x0
	.space	2
	.long	0

	.align	3                       ## @test.f
_test.f:
	.short	2                       ## 0x2
	.space	2
	.long	_test
	.long	Ltmp25
	.long	_test.frames
	.long	0

	.align	3                       ## @test2.start_ptr
_test2.start_ptr:
	.long	Ltmp31

	.align	2                       ## @test2.bitmap
_test2.bitmap:
	.long	1                       ## 0x1

	.align	4                       ## @test2.frames
_test2.frames:
	.long	Ltmp33
	.short	1                       ## 0x1
	.space	2
	.long	_test2.bitmap
	.long	Ltmp34
	.short	1                       ## 0x1
	.space	2
	.long	_test2.bitmap
	.long	Ltmp31
	.short	0                       ## 0x0
	.space	2
	.long	0

	.align	3                       ## @test2.f
_test2.f:
	.short	2                       ## 0x2
	.space	2
	.long	_test2
	.long	Ltmp31
	.long	_test2.frames
	.long	0

	.align	3                       ## @test3.start_ptr
_test3.start_ptr:
	.long	Ltmp36

	.section	__TEXT,__cstring,cstring_literals
L_.str11:                               ## @.str11
	.asciz	"test frame=%p\n"

L_.str12:                               ## @.str12
	.asciz	"data1 = %p %d\n"

L_.str13:                               ## @.str13
	.asciz	"data2 = %p %d\n"

L_.str14:                               ## @.str14
	.asciz	"data3 = %p %d\n"

L_.str15:                               ## @.str15
	.asciz	"data4 = %p %d\n"

L_.str16:                               ## @.str16
	.asciz	"data5 = %p %d\n"

L_.str17:                               ## @.str17
	.asciz	"data6 = %p %d\n"

	.section	__DATA,__data
	.align	2                       ## @test3.bitmap
_test3.bitmap:
	.long	1                       ## 0x1
	.long	3                       ## 0x3
	.long	6                       ## 0x6

	.align	4                       ## @test3.frames
_test3.frames:
	.long	Ltmp38
	.short	1                       ## 0x1
	.space	2
	.long	_test3.bitmap
	.long	Ltmp39
	.short	1                       ## 0x1
	.space	2
	.long	_test3.bitmap+4
	.long	Ltmp40
	.short	1                       ## 0x1
	.space	2
	.long	_test3.bitmap+8
	.long	Ltmp36
	.short	0                       ## 0x0
	.space	2
	.long	0

	.align	3                       ## @test3.f
_test3.f:
	.short	4                       ## 0x4
	.space	2
	.long	_test3
	.long	Ltmp36
	.long	_test3.frames
	.long	0

	.align	3                       ## @test_int.start_ptr
_test_int.start_ptr:
	.long	Ltmp42

	.align	2                       ## @test_int.bitmap
_test_int.bitmap:
	.long	1                       ## 0x1
	.long	1                       ## 0x1

	.align	4                       ## @test_int.frames
_test_int.frames:
	.long	Ltmp44
	.short	1                       ## 0x1
	.space	2
	.long	_test_int.bitmap
	.long	Ltmp42
	.short	1                       ## 0x1
	.space	2
	.long	_test_int.bitmap+8

	.align	3                       ## @test_int.f
_test_int.f:
	.short	2                       ## 0x2
	.space	2
	.long	_test_int
	.long	Ltmp42
	.long	_test_int.frames
	.long	0

	.align	3                       ## @test_record.start_ptr
_test_record.start_ptr:
	.long	Ltmp46

	.align	2                       ## @test_record.bitmap
_test_record.bitmap:
	.long	1                       ## 0x1
	.long	1                       ## 0x1

	.align	4                       ## @test_record.frames
_test_record.frames:
	.long	Ltmp48
	.short	1                       ## 0x1
	.space	2
	.long	_test_record.bitmap
	.long	Ltmp46
	.short	1                       ## 0x1
	.space	2
	.long	_test_record.bitmap+8

	.align	3                       ## @test_record.f
_test_record.f:
	.short	2                       ## 0x2
	.space	2
	.long	_test_record
	.long	Ltmp46
	.long	_test_record.frames
	.long	0

	.section	__TEXT,__cstring,cstring_literals
L_.str18:                               ## @.str18
	.asciz	"---\n"

L_.str19:                               ## @.str19
	.asciz	"sizeof type %ld header %ld\n"


	.section	__IMPORT,__pointers,non_lazy_symbol_pointers
L_gc_frame_map_list$non_lazy_ptr:
	.indirect_symbol	_gc_frame_map_list
	.long	0
L_gc_top_ptr$non_lazy_ptr:
	.indirect_symbol	_gc_top_ptr
	.long	0
L_heap_list$non_lazy_ptr:
	.indirect_symbol	_heap_list
	.long	0
L_heap_max$non_lazy_ptr:
	.indirect_symbol	_heap_max
	.long	0
L_heap_num$non_lazy_ptr:
	.indirect_symbol	_heap_num
	.long	0

.subsections_via_symbols
