	.section	__TEXT,__text,regular,pure_instructions
	.globl	_main
	.align	4, 0x90
_main:                                  ## @main
Leh_func_begin1:
## BB#0:                                ## %entry
	subl	$12, %esp
Llabel1:
	movl	L_G$non_lazy_ptr, %eax
	movl	(%eax), %eax
	movl	%eax, (%esp)
	call	__WriteInt
	addl	$12, %esp
	ret
Leh_func_end1:

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
	.byte	16                      ## Offset
	.align	2
Leh_frame_end1:


	.section	__IMPORT,__pointers,non_lazy_symbol_pointers
L_G$non_lazy_ptr:
.indirect_symbol _G
	.long	0

.subsections_via_symbols
