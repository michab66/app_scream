	name	EXPSMMU
	title	Scheme Memory Management Utilities for Expanded Memory
	page	62,132
;							=====> EXPSMMU.ASM
;****************************************************************
;*	  TIPC Scheme '84 Memory Management Utilities           *
;*								*
;*    (C) Copyright 1985 by Texas Instruments Incorporated.	*
;*		      All rights reserved.			*
;*								*
;* Author:		Terry Caudill				*
;* Date written:	18 March 1986				*
;* Modifications:						*
;*   tc 3/16/87 Better error handling for mapping errors and	*
;*		fix to requiring page frame on 64k boundary	*
;*   rb  4/5/87 "getbase" modified to return a page's swap      *
;*		 status in the carry bit			*
;****************************************************************
	  include schemed.equ
	  include schemed.ref
	  include schemed.mac

DOS	 	equ	021h
EMM_DSR 	equ	67h	;; EMM DSR Interrupt

;; EMM DSR Function Requests

EMM_Status	equ	40h	;; Get status of EMM
EMM_FrameAddr	equ	41h	;; Get segment of page frame
EMM_PageCount	equ	42h	;; How many pages available
EMM_Allocate	equ	43h	;; Allocate pages
EMM_MapPage	equ	44h	;; Map page into page frame
EMM_Dealloc	equ	45h	;; Deallocate PCS'S expanded mem pages

DGROUP	group	data
PGROUP	group	prog

data	segment word public 'DATA'
	assume	ds:DGROUP
	extrn	page0:byte, page4:byte, page5:byte, page6:byte
	extrn	page7:byte, page8:byte

	extrn	_top:word, _paras:word,first_pa:word,first_dos:word

Emm_Handle	dw 0		;; Handle returned by EMM
PageFrame	dw 0		;; Segment address for EMM Mapping

EmmAvail	db 0		;; Emm available
	public	FirstEmmPage
FirstEmmPage	db 0		;; First page number of Expanded Memory
	public	EmmPageNum,EmmPage,CodeIn
EmmPageNum	db 2		;; Emm Physical page number to map
EmmPage equ	$
EmmPage0	db 0		;; Table to map Emm Physical page
EmmPage1	db 0		;;  to actual pagetable offset
EmmPage2	db 0
CodeIn		db 0		;; Code block currently mapped
	public	GC_ING
GC_ING		dw 0

EmmDeviceName	db "EMMXXXX0"
m_ems_er	db "[VM FATAL ERROR] Expanded Memory Manager error "
		db 38h
p_errnum	db 30h
		db 0Ah,0

data	ends


prog	segment byte public 'PROG'
	assume	cs:PGROUP
	public	_MMU,_%MMU
	public	_%MMU0,_%MMU1,_MMUCB
	public	gcclean
	public	getbase
	public	InitMem
	public	rlsexp

	extrn	print_an:near	;; print_and_exit (truncated to 8 chars)

;;======================================================================
;;
;; _MMU - Take page passed on stack, and return its paragraph address
;;	  on the stack. If page in conventional memory, just get its
;;	  paragraph address from pagetabl. If in expanded memory and
;;	  already mapped in, return the PageFrame, otherwise request
;;	  the EMM to map the page into the PageFrame.
;;
;;	  NOTE: If an expanded memory page is requested which is greater
;;		than the normal page size, Emm Pages 0 and 1 are loaded
;;		automatically and address of page 0 returned.
;;
;;======================================================================

;**************************************************************************
;									  *
;			   W A R N I N G				  *
;  Any references to data normally addressed by the data segment register *
;  should be prefixed with SS: (segment override) because the DS register *
;  may not contain the address of the current data segment.		  *
;									  *
;**************************************************************************


_MMU	proc	near			;; Normal Entry from PROG segment
	push	BP
	mov	BP,SP			;; Make stack accessable
	push	BX
	mov	BX,word ptr [bp+4]	;; BX <= Page number
	cmp	BL,SS:FirstEmmPage	;; Page in real memory?
	jb	_MMUPageRet0		;; Yes..return
_MMU$0:
	push	AX			;; Save caller's regs

	mov	AX,2			;; DX <= Emm Physical page #
	cmp	BL,SS:EmmPage2		;; Mapped in Emm page 2?
	je	_MMU$00 		;; Yes ...jump
	dec	AX
	cmp	BL,SS:EmmPage1		;; Mapped in Emm page 1?
	je	_MMU$00 		;; Yes ...jump
	dec	AX
	cmp	BL,SS:EmmPage0		;; Mapped in Emm page 0?
	jne	_MMU$01 		;; Yes ...jump
_MMU$00:
	mov	SS:EmmPageNum,AL	;; Mark as last page mapped
	jmp	_MMUP$10

; If large page object, load 2 consecutive pages
_MMU$01:
	cmp	[SS:psize+BX],MIN_PAGESIZE ;; Normal sized page?
	je	_MMU$1			;; Yes...jump
	pop	AX			;; Restore AX register
	mov	SS:EmmPageNum,0 	;; Map Page 0 with 1st page
	push	BX			;; Push Page number
	call	_MMUPage		;; Go map it
	inc	SS:EmmPageNum		;; Map Page 1 with 2nd page
	add	BX,2			;; Get next page number
	push	BX			;; Push as argument
	call	_MMUPage		;; Go map it
	pop	BX			;; Ignore Para address of 2nd page
	pop	BX			;; Return Para address of 1st page
	jmp	_MMUPageRet


; Page not currently mapped - Lets map it
_MMU$1:
	mov	AL,SS:EmmPageNum	;; Last Emm physical page mapped
	inc	AL			;; Get next
	cmp	AL,3			;;
	jl	_MMU$2			;; If code block page
	xor	AL,AL			;;  then wrap to zero
_MMU$2:
	mov	SS:EmmPageNum,AL	;; Update Emm Page last mapped
	jmp	_MMUP$1

_MMU	endp

;;======================================================================
;;
;; _MMUPage - Load Expanded page number specified in EmmPageNum.
;;	      Emm Page 3 should only be used for the currently
;;	      executing code block (via LoadCode macro).
;;
;;    NOTE: EmmPageNum must be set before this routine is called.
;;
;;======================================================================

_MMUPage proc	near
	push	BP
	mov	BP,SP
	push	BX
	mov	BX,word ptr [bp+4]	;; Get page to map
	cmp	BL,SS:FirstEmmPage	;; Page in real memory?
	jae	_MMUP$0 		;; No...go map it
	cmp	SS:EmmPageNum,3 	;; Loading a code block?
	jne	_MMUPageRet0		;; No...return page
	mov	SS:CodeIn,BL		;; Note code block
_MMUPageRet0:
	mov	BX,word ptr [BX+SS:pagetabl]
_MMUPageRet:
	mov	word ptr [bp+4],BX	     ;; return it
	pop	BX
	pop	BP
	ret
_MMUP$0:
	push	AX
	xor	AH,AH
	mov	AL,SS:EmmPageNum	;; Get page number to map
_MMUP$1:
	xchg	AX,BX			;; Note page number in table
	mov	byte ptr [SS:EmmPage+BX],AL
	xchg	AX,BX

;;	Map Page from Expanded memory

	push	AX			;; Save accross call
	push	DX
	mov	AH,EMM_MapPage		;; Map Page Function
	sub	BL,SS:FirstEmmPage	;; Convert page to map
	shr	BX,1			;;   to EMM Logical Page
	mov	DX,SS:Emm_Handle	;; EMM Handle
	int	EMM_DSR
	pop	DX			;; Restore saved regs
	pop	BX
	or	AH,AH			;; Error doing map page?
	jnz	Emm_Fatal_Map		;; Yes, fatal
	mov	AX,BX			;; restore AX
_MMUP$10:
	mov	BX,SS:PageFrame 	;; Get current page frame
	shl	AL,1			;; Convert to offset
	shl	AL,1
	add	BH,AL			;;   and add to page frame
	pop	AX
	jmp	_MMUPageRet

Emm_Fatal_Map:
	jmp	Emm_Fatal_Error

_MMUPage endp

;;======================================================================
;;
;; Alternate Entry points
;;
;;======================================================================

;; Return Paragraph address of page number

_%MMU	proc	far			;; Entry from PROGX segment
	push	AX
	call	_MMU
	pop	AX
	ret
_%MMU	endp

;; Load Emm Page 0 - Called from garbage compactor

_%MMU0	proc	far			;; Entry from PROGX segment
	push	AX
	mov	SS:EmmPageNum,0
	call	_MMUPAGE
	pop	AX
	ret
_%MMU0	endp

;; Load Emm Page 1 - Called from garbage compactor

_%MMU1	proc	far			;; Entry from PROGX segment
	push	AX
	mov	SS:EmmPageNum,1
	call	_MMUPAGE
	pop	AX
	ret
_%MMU1	endp

;; Load Code Block into Emm Page 3 - Entry from PROG segment

_MMUCB	proc	near
	mov	SS:EmmPageNum,3
	jmp	_MMUPage

_MMUCB	endp


;**************************************************************************
;									  *
;			   W A R N I N G				  *
;  Any above references to data normally addressed by the data segment	  *
;  register should be prefixed with SS: (segment override) because the	  *
;  DS register may not contain the address of the current data segment.   *
;									  *
;**************************************************************************


;;======================================================================
;;
;;	Get page base address without forcing a page fault.
;;	For debugging purposes only (SDUMP.C)....
;;
;; On exit, carry set if page is swapped out, else it's clear (used by XLI).
;;
;;======================================================================

getbase proc	near
	push	BP
	mov	BP,SP
	push	BX
	mov	BX,word ptr [BP+4]
	cmp	BL,SS:FirstEmmPage
	jae	gc_00
	mov	AX,word ptr [BX+SS:pagetabl] ;; Get paragraph address
	clc
	jmp	gb_quit
gc_00:
	mov	AX,2
	cmp	BL,SS:EmmPage0
	je	gb_5
	dec	AX
	cmp	BL,SS:EmmPage1
	je	gb_5
	dec	AX
	cmp	BL,SS:EmmPage2
	je	gb_5
	dec	AX
	cmp	BL,SS:CodeIn
	stc
	jne	gb_quit
	mov	AX,3
gb_5:
	shl	AL,1
	shl	AL,1
	or	AL,byte ptr [SS:PageFrame+1]
	xchg	AL,AH
	clc
gb_quit:
	pop	BX
	pop	BP
	ret

getbase endp

;;======================================================================
;;
;; exppage()
;;	This routine returns the first emm page number
;;
;;======================================================================
	public	exppage
exppage proc	near
	xor	AH,AH
	mov	AL,FirstEmmPage
	shr	AL,1
	ret
exppage endp

;;======================================================================
;;
;; gcclean()
;;	This routine must be called after garbage collection and
;;	compaction to clean up the pagetabl and EmmPage table.
;;
;;======================================================================
gcclean proc	near
	mov	byte ptr EmmPageNum,0	;; Reset EmmPage indicator
	mov	word ptr EmmPage,0
	mov	byte ptr EmmPage2,0
	ret
gcclean endp

;;======================================================================
;;
;; InitMem()
;;	Check to see if expanded memory manager is present and set up
;;	the memory tables. Return the total number of pages (excluding
;;	the dedicated ones) we've been able to allocate.
;;
;;======================================================================
Lcl_DS_Save	dw	data		;; Local copy of data segment

InitMem proc	near
	mov	BX,DS
	mov	CS:Lcl_DS_Save,DS	;; Save DS for manager above
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

;; Compute first page paragraph address
;; (In the process, allocate all the memory that DOS will give us.)

	mov	BX,0FFFFh		;; first ask for too much
	mov	AH,048h
	int	DOS			;; DOS gets an error, but tells us
					;;   in BX how much we CAN get
	mov	AH,048h
	int	DOS			;; reissue allocation request
	mov	first_dos,AX		;; save address for returning it to DOS
	add	AX,(MIN_PAGESIZE shr 4) - 1	  ;; Move to page boundary
	and	AX,not ((MIN_PAGESIZE shr 4) - 1)
	mov	first_pa,AX		;; first page paragraph address

;; Initialize page management table with pages available in real memory

	mov	DX,nextpage
	mov	freepage,DX		;; freepage = nextpage
	mov	DI,_paras		;; Get maximum number of paragraphs
	sub	DI,(MIN_PAGESIZE shr 4) ;; Get address of last paragraph
	xor	CX,CX			;; Keep number of pages in CX
EmmP$1:
	cmp	DI,AX			;; Did we reach it
	jb	EmmP$2			;; Yes...no more
	cmp	DX,NUMPAGES		;; See if we have filled the table
	jae	EmmP$2
	mov	BX,DX
	shl	BX,1
	mov	word ptr [BX+pagetabl],AX
	and	word ptr [BX+attrib],not NOMEMORY
	inc	DX
	mov	word ptr [BX+pagelink],DX
	mov	word ptr [BX+nextcell],0
	inc	CX			;; page_count++
	add	AX,(MIN_PAGESIZE shr 4)
	jmp	EmmP$1
EmmP$2:
	push	CX			;; Save # real memory pages
	shl	DX,1
	mov	FirstEmmPage,DL 	;; Save first exp mem page number

	mov	AH,35H			;; Get Interrupt Vector
	mov	AL,67H			;; "Vector"
	int	21H
	mov	DI,000AH		;; ES:DI points to device name field
	lea	SI,EmmDeviceName	;; DS:SI points to device name
	mov	CX,8
	cld
	repe	CMPSB			;; Compare the two strings
	je	EmmPres 		;; Jump if EMM present
	mov	ES,CS:Lcl_DS_Save	;; Restore ES
	xor	BX,BX			;; No EMM pages available
	jmp	EmmP$2A 		;; Skip talking to Emm Manager
EmmPres:
	mov	ES,CS:Lcl_DS_Save	;; Restore ES
	mov	AH,EMM_FrameAddr	;; Get Page Frame Address
	int	EMM_DSR
	or	AH,AH
	jnz	Emm_Fatal_Error
EmmP$:
	mov	PageFrame,BX		;; Save page frame address

	mov	AH,EMM_PageCount	;; Get Unallocated Pages Count
	int	EMM_DSR 		;;   (returned in BX)
	or	AH,AH
	jnz	EMM_Fatal_Error
EmmP$2A:
	cmp	BX,0			;; Are there any pages available?
	je	EmmP$2B 		;; No,	jump
	mov	EmmAvail,1		;; Yes, note pages available
EmmP$2B:
	mov	AX,BX			;; Number exp mem pages available
	xor	DX,DX
	mov	DL,FirstEmmPage 	;; Restore first exp mem page
	shr	DX,1			;; Convert to number
	xor	CX,CX			;; Page count

;; Why was this here?	mov	SI,PageFrame

EmmP$3:
	cmp	CX,AX			;; Last expanded memory page?
	je	EmmP$4			;; Yes...no more
	cmp	DX,NUMPAGES		;; Filled the table?
	jae	EmmP$4			;; Yes...no more
	mov	BX,DX
	shl	BX,1
	mov	word ptr [BX+pagetabl],0
	and	word ptr [BX+attrib],not NOMEMORY
	inc	DX
	mov	word ptr [BX+pagelink],DX
	mov	word ptr [BX+nextcell],0
	inc	CX
	jmp	EmmP$3

EmmP$4:
	mov	nextpage,DX		;; nextpage = lastpage
	mov	lastpage,DX		;; 
	jcxz	EmmP$Ret		;; Return if no pages allocated

	mov	AH,EMM_Allocate 	;; Allocate Pages
	mov	BX,CX			;; Number of pages
	int	EMM_DSR
	or	AH,AH
	jnz	Emm_Fatal_Error
	mov	emm_handle,DX		;; Save Handle returned
EmmP$Ret:
	mov	AX,CX			;; Get extended memory count
	pop	CX			;; Retrieve real memory count
	add	AX,CX			;;	and return combination
	ret

Emm_Fatal_Error:
	mov	BX,DS			;; Lattice needs ES=DS
	mov	ES,BX
	and	AH,0Fh			;; isolate low order nibble of error
	add	AH,'0'                  ;; convert to ascii
	cmp	AH,'9'                  ;; is it 0-9?
	jbe	Emm_Fat01		;; yes, jump
	add	AH,'A'-'9'-1            ;; add fudge factor for A-F
Emm_Fat01:
	mov	byte ptr ss:p_errnum,AH ;; Set error indicator
	lea	BX,ss:m_ems_er		;; Fatal Error Message
	push	BX
	C_call	print_an


InitMem endp


;;======================================================================
;;
;;  rlsexp - Release Expanded Memory Pages
;;
;;======================================================================
rlsexp	proc	near
	cmp	EmmAvail,0		;; Emm being used?
	je	rlsret			;; No,	Return
	mov	AH,EMM_Dealloc		;; Yes, Deallocate pages
	mov	DX,EMM_Handle
	int	EMM_DSR
rlsret:
	ret
rlsexp	endp

prog	ends

	end
