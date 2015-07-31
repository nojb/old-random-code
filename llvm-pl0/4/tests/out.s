	.section	__TEXT,__text,regular,pure_instructions
	.globl	_main
	.align	4, 0x90
_main:                                  ## @main
Leh_func_begin1:
## BB#0:                                ## %entry
	subl	$28, %esp
Llabel1:
	movl	$10, 24(%esp)
	leal	20(%esp), %eax
	movl	%eax, 4(%esp)
	leal	24(%esp), %eax
	movl	%eax, (%esp)
	call	_fact
	movl	24(%esp), %eax
	movl	%eax, (%esp)
	call	__print
	movl	20(%esp), %eax
	movl	%eax, (%esp)
	call	__print
	addl	$28, %esp
	ret
Leh_func_end1:

	.globl	_fact
	.align	4, 0x90
_fact:                                  ## @fact
Leh_func_begin2:
## BB#0:                                ## %entry
	subl	$4, %esp
Llabel2:
	movl	$1, (%esp)
	movl	12(%esp), %eax
	movl	8(%esp), %ecx
	jmp	LBB2_2
	.align	4, 0x90
LBB2_1:                                 ## %body
                                        ##   in Loop: Header=BB2_2 Depth=1
	movl	(%esp), %edx
	imull	(%ecx), %edx
	movl	%edx, (%esp)
	decl	(%ecx)
LBB2_2:                                 ## %test
                                        ## =>This Inner Loop Header: Depth=1
	cmpl	$0, (%ecx)
	jg	LBB2_1
## BB#3:                                ## %after
	movl	(%esp), %ecx
	movl	%ecx, (%eax)
	xorl	%eax, %eax
	addl	$4, %esp
	ret
Leh_func_end2:

	.section	__TEXT,__eh_frame,coalesced,no_toc+strip_static_syms+live_support
EH_frame0:
Lsection_eh_frame:
Leh_frame_common:
	.set	Lset1eh,Leh_frame_common_end-Leh_frame_common_begin
	.long	Lset1eh                 ## Length of Common Information Entry
Leh_frame_common_begin:
	.long	0                       ## CIE Identifier Tag
	.byte	1                       ## DW_CIE_VERSION
	.asciz	 "zR"
                                        ## CIE Augmentation
	.byte	1                       ## CIE Code Alignment Factor
	.byte	124                     ## CIE Data Alignment Factor
	.byte	8
                                        ## CIE Return Address Column
	.byte	1                       ## Augmentation Size
	.byte	16                      ## FDE Encoding = pcrel
	.byte	12                      ## CFA_def_cfa
	.byte	5                       ## Register
	.byte	4                       ## Offset
	.byte	136                     ## DW_CFA_offset + Reg (8)
	.byte	1                       ## Offset
	.align	2
Leh_frame_common_end:

	.globl	_main.eh
_main.eh:
	.set	Lset2eh,Leh_frame_end1-Leh_frame_begin1
	.long	Lset2eh                 ## Length of Frame Information Entry
Leh_frame_begin1:
	.long	Leh_frame_begin1-Leh_frame_common ## FDE CIE offset
	.long	Leh_func_begin1-.       ## FDE initial location
	.set	Lset3eh,Leh_func_end1-Leh_func_begin1
	.long	Lset3eh                 ## FDE address range
	.byte	0                       ## Augmentation size
	.byte	4                       ## CFA_advance_loc4
	.set	Lset4eh,Llabel1-Leh_func_begin1
	.long	Lset4eh
	.byte	14                      ## CFA_def_cfa_offset
	.byte	32                      ## Offset
	.align	2
Leh_frame_end1:

	.globl	_fact.eh
_fact.eh = 0
.no_dead_strip _fact.eh


.subsections_via_symbols
