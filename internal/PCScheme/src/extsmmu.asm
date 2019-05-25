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
;* Author:		Herman Schuurman			*
;* Date written:	26 August 1985				*
;* Last change: 	17 September 1985			*
;* History:							*
;*   rb  4/ 5/87 "getbase" returns in carry flag a page's       *
;*		 swap state					*
;****************************************************************
	.286c			;; Utilize the expanded 80286 instruction set
	include pcmake.equ
	include schemed.equ
	include schemed.ref
	include schemed.mac

DOS	 equ	021h
ExtAlloc equ	99		;; # extended mem pages to allocate initially
				;; (99 effectively removes barrier)

DGROUP	group	data
PGROUP	group	prog

data	segment word public 'DATA'
	assume	ds:DGROUP
	extrn	page0:byte, page4:byte, page5:byte, page6:byte
	extrn	page7:byte, page8:byte

	extrn	_top:word, _paras:word,first_pa:word,first_dos:word

;;	Age table

agetable label word
	dw	NUMPAGES dup (0)

AllocPag dw	0		;; Allocated number of pages
;;
;;  The following EQUates give the special bits within the page table,
;;  mainly used for the CLOCK algorithm. Note that these equates are
;;  also defined in SBIGEXT.C if modified.

SWAPPED equ	00000001b	;; Page is currently in extended memory
FIXED	equ	10000000b	;; Fixed in memory (long pages)

PageBuf  dw	SWAPPED 	;; Current available swap page (default 0)

	public	VMCycle
VMCycle dw	0		;; Current VM cycle (modulo 65536)

;;	public	FAULTS
;;FAULTS  dw	0		;; Number of page faults

	public	GC_ING
GC_ING	dw	0		;; Indicate whether garbage collecting

m_lck_er db	"[VM FATAL ERROR] Memory lock error - no page to swap",0Ah,0
m_pag_er db	"[VM FATAL ERROR] Memory paging error number "
p_errnum db	30h
	 db	0Ah,0

;;	Extended memory support structures....

DESC	struc			;; Data segment descriptor
DESCLimit dw	MIN_PAGESIZE	;; Segment limit (length)
DESCBaseL db	0		;; Physical address - bits 7..0
DESCBaseM dw	0		;; Physical address - bits 23..8
	  db	0		;; Access rights byte
	  dw	0		;; Intel reserved....
DESC	ends

;;======================================================================
;;
;;	The GDT passed to INT 15h function 87h, is organized as follows :
;;
;;				.-----------.
;;				V	    |
;;	[ES:SI] --> +00 .---------------.   |
;;			|     Dummy	|   |
;;		    +08 |---------------|   |
;;			|    GDT Loc	|---'
;;		    +10 |---------------|
;;			|  Source GDT	|
;;		    +18 |---------------|
;;			|  Target GDT	|
;;		    +20 |---------------|
;;			| BIOS code seg |
;;		    +28 |---------------|
;;			| Stack segment |
;;			`---------------'
;;
;;======================================================================

GDT	label	byte		;; Begin of global descriptor table
	DESC	<>		;; Dummy descriptor

	DESC	<>		;; GDT descriptor

Source	DESC	<,,,93h,>	;; Source area descriptor

Target	DESC	<,,,93h,>	;; Target area descriptor

	DESC	<>		;; BIOS code segment descriptor

	DESC	<>		;; Stack segment descriptor

data	ends

prog	segment byte public 'PROG'
	assume	cs:PGROUP
	public	_MMU,_%MMU
;; The following are here so link edit won't find urevolved refs
	public	_%MMU0,_%MMU1,_MMUCB
	public	getbase
	public	InitMem

	extrn	print_an:near	;; print_and_exit (truncated to 8 chars)

;;======================================================================
;;
;; _MMU - Get page indicated on stack into real memory,
;;	and return the paragraph address of it on the stack...
;;
;;======================================================================
Lcl_DS_Save dw	data			;; Saved Data Segment

_%MMU	proc	far			;; Entry from PROGX segment
_%MMU0:
_%MMU1:
	push	AX
	call	_MMU
	pop	AX
	ret
_%MMU	endp

_MMU	proc	near			;; Normal Entry from PROG segment
_MMUCB:
	push	BP			;; Make stack accessable
	mov	BP,SP
	push	DS			;; Save Caller's DS
	mov	DS,CS:Lcl_DS_Save	;; and make our's available
	push	AX
	push	BX
	mov	BX,word ptr [bp+4]	  ;; Get pagetabl offset
	mov	AX,word ptr pagetabl+[BX] ;; Get (new) table indicator
	cmp	BX,PreAlloc*2		  ;; If one of dedicated pages
	jb	M_RetPage		  ;;   then jump
	test	byte ptr [pagetabl+BX],SWAPPED ;; If in extended memory
	jne	M_Swap			       ;;   then go swap it in

;;	Update age and return para address

M_Ret:
	inc	VMCycle 		;; Time stamp
	jnz	M_Ret01 		;; On overflow
	call	PgSweep 		;;   Go sweep entire pagetabl
M_Ret01:
	mov	AX,VMCycle		  ;; Get time stamp
	mov	word ptr agetable+[BX],AX ;; Place in ageing table

	mov	AX,word ptr pagetabl+[BX] ;; Get paragraph address
	xor	AL,AL
M_RetPage:
	mov	word ptr [BP+4],AX	  ;; Set return value
	pop	BX
	pop	AX
	pop	DS
	pop	BP
	ret

;;	Retrieve page from extended memory

M_Swap:
	pusha				;; Save all registers
	push	ES			;;	including ES
	push	AX			;; Save page number on stack
	push	BX			;; Save the page table entry
	call	FndPage 		;; Find a page for swapping
	pop	DI			;; Retrieve final destination
	mov	AX,PageBuf		;; Set swapped page address
	xchg	pagetabl+[BX],AX	;; Get the current page contents
	xor	AL,AL			;; Remove attribute bits
	mov	pagetabl+[DI],AX
	mov	BX,PageBuf		;; Get the page buffer address
	shr	BX,2			;; Adjust the page base address
	add	BH,10h			;;	and raise above 1MByte
	shr	AX,4			;; Create a correct address
	push	AX			;; Save source as next destination
	call	MovePage		;; Swap old page out
	pop	BX			;; Set next destination
	pop	AX			;;	and old source
	mov	PageBuf,AX		;; Set new swap page
	shr	AX,2
	add	AH,10h
	call	MovePage		;; Swap new page in
	pop	ES			;; Restore all registers
	popa				;;	including ES

;;	inc	FAULTS			;; update page fault count

	jmp	M_Ret

_MMU	endp

;;======================================================================
;;
;;  PgSweep - page table clocked sweep routine.
;;	This routine cleans up the current page table after a full
;;	reference cycle (253 counts).
;;
;;======================================================================

	public	PgSweep
PgSweep proc near
	push	AX
	push	BX
	push	CX
	mov	BX,offset agetable[PreAlloc*2] ;; Don't bother with the
	mov	CX,AllocPag		;;	dedicated pages in the table
	xor	AX,AX			;; Clear AX register
PgSwp$0:
	mov	AL,byte ptr [BX+1]	;; Get the current high byte
	mov	word ptr [BX],AX
	add	BX,2
	loop	PgSwp$0 		;; Continue with next sweep
	mov	VMCycle,100h		;; Set next cycle
	pop	CX
	pop	BX
	pop	AX
	ret

PgSweep endp

;;======================================================================
;;
;; FndPage - Find a swappable page in the page table.
;;	This routine scans the page table (non-dedicated pages only),
;;	for swappable pages.  The least recently used page NOT USED
;;	IN THE CURRENT VM INSTRUCTION is selected...
;;
;;	As an added bonus, the current code page can not be swapped
;;	either.....
;;
;;======================================================================

FndPage proc	near
	mov	BX,cb_pag		;; Get entry into current code page
	cmp	BX,PreAlloc*2		;; Check against permanent pages
	jb	FndPag$1		;; Don't worry...it'll stay around
	cmp	pagetabl+[BX],FIXED	;; Check for fixed page
	jbe	FndPag$1		;;	which will stay too
	mov	AX,VMCycle		;; Set to current cycle
	mov	agetable+[BX],AX	;; Try to keep page in memory
FndPag$1:
	mov	BX,PreAlloc*2		;; Don't bother with the
	mov	CX,AllocPag		;;	dedicated pages in the table
	xor	DX,DX			;; Set initial distance

FndPag$2:
	test	byte ptr [BX+pagetabl],FIXED+SWAPPED
	jne	FndPag$3		;; Fixed,Swapped,Noswap pages are exempt
	mov	AX,VMCycle		;; Check against current cycle
	sub	AX,agetable+[BX]
	cmp	DX,AX
	jae	FndPag$3		;; Already found a better page
	mov	SI,BX			;; Save the page address
	mov	DX,AX			;;	and its value
FndPag$3:
	add	BX,2
	loop	FndPag$2		;; Continue with next sweep

;;	Completed the sweep..the most desirable page should
;;	be in SI now, unless DX is still 0....

	cmp	DX,0			;; See if we found a page
	je	FndPag$4		;; No...error
	mov	BX,SI			;; Return its number
	ret

	public	FndPag$4

FndPag$4:
	lea	BX,m_lck_er		;; Indicate a lock error
FatalError:
	push	BX			;; Save the error message
	mov	AX,DS
	mov	ES,AX			;; Make sure ES is Ok...
	C_call	print_an		;; Print the message and quit

FndPage endp

;;======================================================================
;;
;;	Get page base address without forcing a page fault.
;;	For debugging purposes only (SDUMP.C)....
;;
;; On exit, set carry if page is swapped out, else clear carry (used by XLI)
;;
;;======================================================================

getbase proc	near
	push	BP
	mov	BP,SP
	mov	BX,word ptr [BP+4]
	mov	AX,word ptr [BX+pagetabl] ;; Get table indicator

	test	AX,SWAPPED		; is page swapped out?
	jz	getb_10 		; no, jump
	stc				; page is swapped out, set carry
	jmp	short getb_20
getb_10: clc				; page is in memory, clear carry

getb_20: pop	 BP
	ret

getbase endp

;;======================================================================
;;
;;	Swap page to extended memory
;;	Used in FIND_BIG_BLOCK in SBIGMEM.C
;;
;;======================================================================

	public	move_pag
move_pag proc  near
	push	BP
	mov	BP,SP
	pusha				;; Save all registers
	push	ES			;;   including ES

	mov	DI,[BP+6]		   ;; Extended memory page to swap
	mov	AX,word ptr pagetabl+[DI]  ;; AX <= Extended memory address

	mov	BX,[BP+4]		;; Real memory page to swap
	xchg	pagetabl+[BX],AX	;; Update its pagetabl entry
	xor	AL,AL			;; AX <= para address of page to swap
	push	DI
	push	AX

	mov	BX,word ptr [BX+pagetabl]  ;; Extended page address (destination)
	shr	BX,2			   ;; Adjust page base address
	add	BH,10h			   ;;	and raise above 1mb address
	shr	AX,4			   ;; Real page address (source)
	call	MovePage		   ;; Move it

	pop	AX			   ;; Reload paragraph address
	or	AL,FIXED		   ;; Fixed attribute
	pop	DI			   ;; Reload page number
	mov	word ptr pagetabl+[DI],AX  ;; Update pagetabl entry

	pop	ES			;; Restore all regs
	popa				;;   including ES

	pop	BP			;; restore base ptr
	ret

move_pag endp

	subttl	Extended memory support
	page
;;======================================================================
;;
;;	Extended memory I/O routine
;;
;;	Source address is in AX, destination in BX.
;;	The high byte of each register contains the upper 8 bits of
;;	the real address (bits 16..23).  The low byte contains the
;;	next 8 bits of the real address (bits 8..15)...
;;
;;======================================================================

MovePage proc near
	mov	SI,SS
	mov	CX,SP			; Save the original stack in SI:CX
	cli
	mov	DX,CS
	mov	SS,DX
	mov	SP,offset PGROUP:ExtMemStack
	sti
	push	SI
	push	CX			; Save old stack info
	mov	Source.DESCBaseM,AX
	mov	Target.DESCBaseM,BX
	mov	CX,MIN_PAGESIZE/2		;; Reduce pagesize to word count
	push	DS
	pop	ES
	mov	SI,offset DGROUP:GDT
	mov	AH,87h			; Perform a block move
	int	15h

					; kludge to fix hanging keyboard
	mov	AL,0AEh 		; ensure keyboard enabled
	out	64h,AL			; output to 8042 controller

	pop	CX
	pop	BX
	cli
	mov	ss,BX			; Restore the original stack
	mov	sp,CX
	sti
	jz	MovRet			; If successful, return
	or	AH,AH			; Return status non-zero?
	jnz	MovePage$1		;   Yes...error
MovRet:
	ret

;;	Error detected durin paging ....as fatal as can be....

MovePage$1:
	or	p_errnum,AH		; Set error indicator
	lea	BX,m_pag_er		; Load up Error message
	jmp	FatalError		; Abort

MovePage endp


;;======================================================================
;;
;; InitMem()
;;	Initialize all the memory tables correctly.  Return the
;;	total number of pages (excluding the dedicated ones) we've
;;	been able to allocate.
;;
;;======================================================================

InitMem proc	near
	mov	BX,DS
	mov	CS:Lcl_DS_Save,BX	;; Save DS for manager above
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
	mov	first_pa,AX			  ;; first page paragraph address


;; Initialize page management table with pages available in real memory

	mov	DX,nextpage
	mov	freepage,DX		;; freepage = nextpage
	mov	DI,_paras		;; Get maximum number of paragraphs
	sub	DI,(MIN_PAGESIZE shr 4) ;; Get address of last paragraph
	xor	CX,CX			;; Keep number of pages in CX
InitM$1:
	cmp	DI,AX			;; Did we reach it
	jb	InitM$2 		;; Yes...no more
	cmp	DX,NUMPAGES		;; See if we have filled the table
	jae	InitM$2
	mov	BX,DX
	shl	BX,1
	mov	word ptr [BX+pagetabl],AX
	and	word ptr [BX+attrib],not NOMEMORY
	inc	DX
	mov	word ptr [BX+pagelink],DX
	mov	word ptr [BX+nextcell],0
	inc	CX			;; page_count++
	add	AX,(MIN_PAGESIZE shr 4)
	jmp	InitM$1

;;
;; At this time, DX <= next avail page number, CX <= current page count
;;
;; Now Lets see if this is a 286 machine
;;

InitM$2:
	mov	nextpage,DX		;; Save next available page
	xor	AX,AX

	mov	BX,PC_MAKE		;; Get pc type
	cmp	BX,1			;; Is it TIPC?
	jne	InitM$20		;;   No, go check for 286/386
	push	DS			;;   Yes,lets check for a Bus Pro
	mov	DS,AX			;;       DS <= 0 for addressing low mem
	mov	BX,DS:word ptr [01A2h]	;;       Checkout vector 68 bytes 2 & 3
	pop	DS
	add	BL,BH
	cmp	BL,0F0h 		;;       If AL==F0 then TIPC=Business Pro
	je	InitM$21
	jne	InitM$Ret
InitM$20:
	cmp	BX,IBMAT		;; Is it IBM AT?
					;;   (includes XT/286, PS/2-50,-60)
	je	InitM$21		;; yes, jump
	cmp	BX,IBM80		;; Is it IBM PS/2 Model 80?
	jne	InitM$Ret		;; no, jump

;; Fill out rest of page table with extended memory pages. Only allocate
;; the first 512kb of extended memory; the rest is allocated but marked
;; marked as unallocated in the page tables (ie, ATTRIB and PAGELINK). This
;; will force the memory allocation to work (at least initially) in real
;; memory and the first 512k of extended memory until an "out of memory".
;; At that time, NEXTPAGE will be updated, and some more pages in extended
;; memory will then be marked as allocated (ie, ATTRIB and PAGELINK). This
;; scenario will be repeated until all of extended memory is actually used.
;; The upper limit will be help in LASTPAGE. Also see out_of_memory in
;; SMEMORY.C
;;
;; This should help performance for those applications which generate a
;; lot of garbage, but don't have to use the full extent of the extended
;; memory.

InitM$21:
	push	CX			;; Save current count
	mov	AH,88h			;; Get number of contiguous 1k
	int	15h			;;  blocks starting at 1MByte
	add	ax,((MIN_PAGESIZE shr 10) - 1)
	and	ax,not ((MIN_PAGESIZE shr 10) - 1)
	xor	DX,DX
	mov	CX,(MIN_PAGESIZE shr 10);; Number 1K blocks per page
	idiv	CX			;; Reduce to # of pages
	mov	DX,nextpage		;; Retrieve next available page number
	mov	CX,0101h		;; Count the extended pages
	xor	DI,DI
InitM$3:
	dec	AX			;; Check for last extended memory page
	jle	InitM$4 		;; Yes...no more
	cmp	DX,NUMPAGES		;; See if we have filled the table
	jae	InitM$4
	mov	BX,DX			   ;; DX = page number
	shl	BX,1			   ;; BX = page table offset
	inc	DX			   ;; DX = next page number
	mov	word ptr [BX+pagetabl],CX  ;; Page's address
	mov	word ptr [BX+nextcell],0   ;; Nextcell in page = 0
	cmp	CH,ExtAlloc		   ;; 512kb allocated?
	jb	InitM$33		   ;;  below, mark as allocated
	ja	InitM$35		   ;;  above, skip allocation
	mov     DI,DX		   	   ;;  equal, EXT MEM LIMIT
InitM$33:
	and	word ptr [BX+attrib],not NOMEMORY
	mov	word ptr [BX+pagelink],DX  ;; No,  update pagelink info
InitM$35:
	inc	CH			   ;; Next extended memory page
	jmp	InitM$3 		   ;; Go allocate next page

;; At this time, DX <= last page number, CH <= # extended memory pages


InitM$4:
	mov	lastpage,DX		; last page number
	mov	nextpage,DX		; default nextpage to lastpage
	or	DI,DI			; Did we get our extended mem limit?
	jz	InitM$45		;  no,  lastpage=nextpage, jump
	mov	nextpage,DI		;  yes, lets use that limit
InitM$45:
	xor	AH,AH
	mov	AL,CH			; Get extended memory count
	dec	AX			; Don't count the swapping page
	pop	CX			; Retrieve real memory count
InitM$Ret:
	add	AX,CX			; Total Page count
	mov	AllocPag,AX		; Save allocated pages for later
	ret

InitMem endp

;;======================================================================
;;
;;	Temporary stack during extended memory operations...
;;
;;======================================================================

	db	10 dup ("ExtStack")
ExtMemStack label word			;; Extended memory support stack

prog	ends

	end
