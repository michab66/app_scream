	name	PROSMMU
	title	Scheme Memory Management Utilities for Protected Mode
	page	62,132
;							=====> PROSMMU.ASM
;****************************************************************
;*	  TIPC Scheme '84 Memory Management Utilities           *
;*								*
;*    (C) Copyright 1985 by Texas Instruments Incorporated.	*
;*		      All rights reserved.			*
;*								*
;* Author:		Terry Caudill				*
;* Date written:	17 Feb 1987  				*
;****************************************************************
	include schemed.equ
	include schemed.ref
	include schemed.mac

DOS	equ	021h

DGROUP	group	data
PGROUP	group	prog

data	segment word public 'DATA'
	assume	ds:DGROUP
	extrn	page0:byte, page4:byte, page5:byte, page6:byte
	extrn	page7:byte, page8:byte

	extrn	_top:word, _paras:word,first_pa:word,first_dos:word

	public	scheme_heap,gc_ing
scheme_heap dw	0			;selector for entire scheme heapspace
gc_ing	    dw	0			;denotes when gc is taking place

sub_segerr  db  "Error allocating data segment",0Ah,0
alloc_err   db  "Unable to allocate memory for scheme heap",0Ah,0

data	ends


prog	segment byte public 'PROG'
	assume	cs:PGROUP

;;======================================================================
;;
;;	Get page base address of page
;;
;;======================================================================

	public	getbase
getbase proc	near
	push	BP
	mov	BP,SP
	mov	BX,word ptr [BP+4]
	mov	AX,word ptr [BX+pagetabl] ;; Get table indicator
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
	sub	SP,2			;; Allocate loacl storage
	mov	BP,SP

	mov	word ptr [bp+0],0	;; number of pages allocated

	mov	bx,ds
	mov	es,bx			;; ensure ES = DS

;; The first eight pagetable entries contain offsets to data within
;; the local data segment. These offsets must be changed to to 
;; selectors so that they can be accessed as any other scheme object.
	
;; Convert offset within pagetabl[0] into paragraph address

	mov	di,offset pagetabl

	xor	si,si			
	mov	bx,word ptr [di]	;; si:bx = offset within DS
	xor	cx,cx
	mov	dx,16			;; cx:dx = length
	mov	ax,0E801h		;; Create Data Window
	int	DOS
	jc	subsegerr
	mov	word ptr [di],ax	;; move selector into pagetabl

;; Now convert pagetabl[4] through pagetabl[8]

	mov	dx,5			;; dx = # entries to modify
	mov	di,offset pagetabl[8]
EmmP$0:
	push	dx
	mov	bx,word ptr [di]	;; si:bx = offset within ds
	xor	cx,cx			;; cx:dx = length 
	mov	dx,0600h		;;   (big enough for largest page)
	mov	ax,0E801h		;; Create Data Window
	int	DOS
	pop	dx			;; get # entries
	jc	subsegerr
	mov	word ptr [di],ax	;; move selector into pagetabl
	add	di,2			;; address next pagetable entry
	dec	dx			;; any remaining?
	jnz	EmmP$0			;;  yes,  loop
	jmp	around_error
subsegerr:
	lea	bx,sub_segerr
	jmp	FatalError

around_error:
;; Now we must allocate the remaining memory (to approx. 4mb), and fill
;; the remaining pagetabl entries with selectors to address each page.

;; ask for too much memory, # bytes available will be returned in cx:dx
	mov	cx,0ffffh
	mov	dx,0ffffh		;; cx:dx = # bytes requested
	mov	ax,0E800h		;; create data segment
	int	021h			;; extended dos function from AIA

	mov	ax,dx
	mov	dx,cx			;; dx:ax = # bytes available
	push	ax
	push	dx			;; save for later

;; calculate #paragraphs available
	mov	cx,4			;; cx = shift count
make_para:
	shr	dx,1
	rcr	ax,1
	loop	make_para
;; lets make the pagesize a multiple of 2000h bytes (this is so that when
;; pages must be merged to hold large objects, there will be no wasted
;; space).
	mov	cx,NUMPAGES-PreAlloc	;; cx = number pagetabl entrys available
	idiv	cx
	mov	bx,ax			;; bx = # paras per page
	mov	ax,200h			
	cmp	ax,bx			;; if #paras/page < 200 paras
	jge	make_pagesize		;;  round to 200, jump
	mov	ax,400h			;; if #paras/page < 400 paras
	cmp	ax,bx			;;  round to 400, jump
	jge	make_pagesize
	mov	ax,7FFh	     		;; default pagesize to 7FF0 bytes
;; change the paras used in calculations above to bytes
make_pagesize:
	mov	cx,4
	shl	ax,cl			;; dx = number bytes per page
	mov	pagesize,ax		;; save away in pagesize
;; divide the # bytes available by the #bytes/page to see how many 
;; pages will be required. max = NUMPAGES-PreAlloc
	mov	cx,ax			;; cx = # bytes/page
	pop	dx
	pop	ax			;; dx:ax = # bytes available
	idiv	cx			;; ax = # pages required
	cmp	ax,NUMPAGES-PreAlloc    ;; do we exceed number avail page?
	jle	Emmp$0a			;;  no,  jump
	mov	ax,NUMPAGES-PreAlloc	;;  yes, just fill the table
Emmp$0a:
	xor	dx,dx
	mul	cx			;; dx:ax = total memory rquirements

;; Allocate only enough memory for the pagetable. Initially allocate just
;; one segment large enough to hold all the scheme heap. 
	push	cx			;;tempsave bytes/page
	mov	cx,dx
	mov	dx,ax			;; cx:dx = length
	mov	ax,0E800h		;; Create Data Segment
	int	DOS
	pop	bx			;;restore bytes/page
	jnc	Emmp$0b
allocerr:
	lea	bx,alloc_err
	jmp	FatalError
Emmp$0b:
	mov	scheme_heap,ax		;; save selector to scheme heap

;; Now allocate multiple "windows" within this larger segment. The pages
;; may overlap so that we can merge pages to hold objects that are larger
;; than a single page.	In AI Archiects terminology, we will allocate
;; "pages" of 8000h (large enough for our largest object) with a "stride"
;; of our desired pagesize (this causes overlap every pagesize number of
;; bytes. The call below will return a selector to the starting page,
;; and the number of selectors necessary to cover the entire segment.
;;
;; Warning: ds register does not address our data segment below
;;
	mov	ds,ax			;; ds = large segment
	xor	cx,cx
	mov	dx,08000h		;; cx:dx = size of each page
	xor	si,si			;; si:bx = stride
	push	bx			;; save page size
	mov	AX,0EA00h		;; Allocate Multiple Windows
	int	DOS			;; extended Dos func from AIA
	pop	si			;; restore page size
	push	ss
	pop	ds
	jc	allocerr		;; if error, exit
;;
;; Warning: ds register does not address our data segment above
;;
;; ax = first selector, bx = number of selectors, si=page size
;; loop number of selector times, filling the pagetabl with
;; selectors to the memory. selectors are 8 bytes in length
;; so bump each selector by 8 to get to the next one.

	mov	dx,nextpage		;; Next page table entry
	mov	freepage,dx		;; is also next free page
	mov	first_pa,dx		;; save for rlsexp, sbid
	mov	cx,bx			;; cx = number of pages to fill
	jcxz    Emmp$2			;; if no pages, jump
EmmP$1:
	mov	bx,dx
	shl	bx,1				    ;; bx = page index
	mov	word ptr ss:pagetabl[BX],AX	    ;; Save selector in table
	and	word ptr ss:attrib[BX],not NOMEMORY ;; mark as allocated
	mov	word ptr ss:psize[BX],si	    ;; note its size
	inc	dx				    ;;   and update for next page
	mov	word ptr ss:pagelink[BX],dx	    ;; update page link
	mov	word ptr ss:nextcell[BX],0	    ;; clear free chain pointer
	inc	word ptr [bp+0]			    ;; page_count++
	add	ax,8				    ;; next selector
	loop	EmmP$1				    ;; get next selector
EmmP$2:
	mov	nextpage,dx		;; set up nextpage
	mov	lastpage,dx		;; lastpage = nextpage
	pop	ax			;; return number pages allocated
	pop	bp			
	ret
FatalError:
	mov	ax,ss			;; ensure ds=es=ss
	mov	ds,ax
	mov	es,ax
	push	bx			;; push error message
	C_call  print_an		;; print message and quit
InitMem endp

;;======================================================================
;;
;;  rlsexp - Release Dos Allocated Pages
;;
;;======================================================================
	public  rlsexp
rlsexp	proc	near
	push	ES
	push	BP
	mov	BP,SP
	mov	es,scheme_heap		;; es = slector for scheme heap
	mov	AX,4900h		;; free allocated memory
	int	DOS			;; do it
	pop	BP
	pop	ES
	ret
rlsexp	endp

prog	ends

	end
