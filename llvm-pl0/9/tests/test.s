	.section	__TEXT,__text,regular,pure_instructions
	.globl	_f
	.align	4, 0x90
_f:                                     ## @f
Leh_func_begin1:
## BB#0:
	movl	4(%esp), %eax
	movl	$123, (%eax)
	ret
Leh_func_end1:

	.globl	_main
	.align	4, 0x90
_main:                                  ## @main
Leh_func_begin2:
## BB#0:
	subl	$12, %esp
Llabel1:
	movl	$111, 8(%esp)
	leal	8(%esp), %eax
	movl	%eax, (%esp)
	call	_f
	movl	8(%esp), %eax
	movl	%eax, (%esp)
	call	__print
	addl	$12, %esp
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

	.globl	_f.eh
_f.eh = 0
.no_dead_strip _f.eh

	.globl	_main.eh
_main.eh:
	.set	Lset2eh,Leh_frame_end2-Leh_frame_begin2
	.long	Lset2eh                 ## Length of Frame Information Entry
Leh_frame_begin2:
	.long	Leh_frame_begin2-Leh_frame_common ## FDE CIE offset
	.long	Leh_func_begin2-.       ## FDE initial location
	.set	Lset3eh,Leh_func_end2-Leh_func_begin2
	.long	Lset3eh                 ## FDE address range
	.byte	0                       ## Augmentation size
	.byte	4                       ## CFA_advance_loc4
	.set	Lset4eh,Llabel1-Leh_func_begin2
	.long	Lset4eh
	.byte	14                      ## CFA_def_cfa_offset
	.byte	16                      ## Offset
	.align	2
Leh_frame_end2:


.subsections_via_symbols
