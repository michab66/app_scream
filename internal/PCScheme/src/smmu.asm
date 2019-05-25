	name	SMMU
	title	Scheme Memory Management Utilities
	page	62,132
;							=====> SMMU.ASM
;****************************************************************
;*	  TIPC Scheme '84 Memory Management Utilities           *
;*								*
;* (C) Copyright 1985, 1987 by Texas Instruments Incorporated.	*
;*		      All rights reserved.			*
;*								*
;* Author:		Terry Caudill				*
;* Date written:	18 March 1986				*
;* History:							*
;*   rb  4/ 5/87 "getbase" returns a page's swap state in carry *
;*		 (for compatibility with PCSEXT and PCSEXP)	*
;****************************************************************
	include schemed.equ
	include schemed.ref

DOS	 equ	021h

DGROUP	group	data
PGROUP	group	prog

data	segment word public 'DATA'
	assume	ds:DGROUP
	extrn	page0:byte, page4:byte, page5:byte, page6:byte
	extrn	page7:byte, page8:byte

	extrn	_top:word, _paras:word,first_pa:word,first_dos:word

	public	GC_ING
GC_ING	dw	0

data	ends


prog	segment byte public 'PROG'
	assume	cs:PGROUP

;;======================================================================
;;
;;	Get page base address of page
;;
;; On exit, carry is clear to indicate page is always in memory
;; (for compatibility with extended and expanded versions of this routine)
;;
;;======================================================================

	public	getbase
getbase proc	near
	push	BP
	mov	BP,SP
	mov	BX,word ptr [BP+4]
	mov	AX,word ptr [BX+pagetabl] ;; Get table indicator
	clc				  ;; page always avail in conv. memory
	pop	BP
	ret

getbase endp

;;======================================================================
;;
;; InitMem()
;;	Compute the best page size, but not smaller than MIN_PAGESIZE
;;
;;======================================================================




	public	InitMem
InitMem proc	near
	push	BP
	sub	SP,2			;; Local storage
	mov	BP,SP

	mov	BX,DS
	mov	ES,BX			;; Ensure ES = DS

;; Convert offset within pagetabl[0] into paragraph address

	mov	DI,offset pagetabl
	mov	AX,word ptr [DI]
	mov	CX,4
	shr	AX,CL
	add	AX,BX
	mov	word ptr [DI],AX

;; Same for pagetabl[4] through pagetabl[8]

	mov	DX,5
	mov	DI,offset pagetabl[8]
EmmP$0:
	mov	AX,word ptr [DI]
	shr	AX,CL
	add	AX,BX
	mov	word ptr [DI],AX
	add	DI,2
	dec	DX
	jnz	EmmP$0

;; Allocate all the memory that DOS will give us.

	mov	BX,0FFFFh		;; first ask for too much
	mov	AH,048h
	int	DOS			;; DOS gets an error, but tells us
					;;   in BX how much we CAN get
	mov	AH,048h
	int	DOS			;; reissue allocation request
	mov	first_dos,AX		;; save address for returning it to DOS
	mov	first_pa,AX		;; save address for Scheme heap

;; Compute the best page size, but not smaller than MIN_PAGESIZE

	mov	AX,_paras		;; max number of paragraphs
	sub	AX,first_pa		;;    subtract first paragragh
	xor	DX,DX			;;    get ready for divide
	mov	CX,NUMPAGES-PreAlloc	;; CX <= number heap allocated pages
	idiv	CX			;; AX <= paras-per-page

	mov	DX,(MIN_PAGESIZE shr 4)
	cmp	AX,DX			;; If paras-per-page < MIN_PAGESIZE/16
	jge	EmmP$05 		;;   then
	mov	AX,DX			;;     paras-per-page = MIN_PAGESIZE/16
EmmP$05:
	mov	[BP],AX 		;; Save paras-per-page

;; Pagesize = (paras-per-page * 16)

	mov	CX,4
	shl	AX,CL
	mov	pagesize,AX
	mov	SI,AX

;; Initialize page management table

	xor	CX,CX			;; Keep number of pages in CX
	mov	DX,nextpage
	mov	freepage,DX		;; freepage = nextpage
	mov	AX,first_pa		;; AX <= next paragraph
	mov	DI,_paras		;; DI <= (_paras - paras per page)
	sub	DI,[BP]
EmmP$1:
	cmp	DI,AX			;; Did we reach it
	jb	EmmP$2			;; Yes...no more
	cmp	DX,NUMPAGES		;; See if we have filled the table
	jae	EmmP$2
	mov	BX,DX
	shl	BX,1
	mov	word ptr [BX+pagetabl],AX
	mov	word ptr [BX+psize],SI
	and	word ptr [BX+attrib],not NOMEMORY
	inc	DX
	mov	word ptr [BX+pagelink],DX
	mov	word ptr [BX+nextcell],0
	inc	CX			;; page_count++
	add	AX,[BP] 		;; nextpara = nextpara + para per page
	jmp	EmmP$1
EmmP$2:
	mov	nextpage,DX		;; nextpage = lastpage
	mov	lastpage,DX
	mov	AX,CX
	pop	BP
	pop	BP
	ret

InitMem endp

prog	ends

	end
