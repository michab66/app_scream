;                                                       =====> BASICIO.ASM
;********************************************************
;*     	        Scheme Runtime Support         	        *
;*     	      Memory Allocation Routines                *
;*	      for Variable Length Objects		*
;*                                      		*
;*    	      (C) Copyright 1985 by Texas       	*
;*     	        Instruments Incorporated.        	*
;*               All rights reserved.         		*
;*			                                *
;* Date Written:  31 December 1987      		*
;* Last Modification:                   		*
;********************************************************
          page    60,132
	  include memtype.equ
          include scheme.equ

SMALL_SIZE equ	  1024    		;space in page not worth searching

DGROUP	  group   data
data	  segment word public 'DATA'
	  assume  DS:DGROUP
data	  ends

PGROUP	  group   prog
prog	  segment byte public 'PROG'
	  assume  CS:PGROUP

	  extrn	  alloc_bi:near,alloc_pa:near,garbage:near,gcsquish:near
	  extrn	  out_of_m:near

;;;ALLOC_BLOCK
;;;
;;;  calling sequence:  alloc_block(reg, type, size)
;;;
;;;  local storage:	int temp_ret,*last_page,page,str_size
;;;
al_args struc
temp_ret  dw	?   		;return address from srch_block
last_page dw	?          	;address of pagelink chain
page	  dw    ?     		;page # of candidate
str_size  dw	?	 	;actual size of object
al_bp	  dw	?		;callers bp
          dw	?		;callers es
	  dw	?		;return address
ret_reg	  dw	?      		;register for return value
ob_typ	  dw	?     		;type of object to find
ob_siz	  dw	?      		;size of object to find
al_args ends

	 public alloc_bl
alloc_bl proc near

	push	es
	push	bp 
	sub	sp,al_bp
	mov	bp,sp 

;if object is string, check for small string and adjust size appropriately
	mov	ax,[bp].ob_siz			;get object size
	mov	[bp].str_size,ax		;and save for later
	cmp	[BP].ob_typ,STRTYPE		;is it a string?
	jnz	al005 				; no,  jump
	cmp	ax,PTRSIZE			;is it a small string?
	jge	al005				; no,  jump
	mov	[bp].ob_siz,PTRSIZE		;size = PTRSIZE
al005:
	add	[bp].ob_siz,BLK_OVHD		;size += BLK_OVHD

;search page type chain for block
	call	srch_block	
	jc	al050				;jump if block found

; Didn't find a block, test for a large block
	mov	ax,[BP].ob_siz
	cmp	ax,pagesize			;requested size > pagesize?
	jb	al010				; no, jump

	public try_big
try_big:

;try to allocate a big block
	mov	si,[bp].ret_reg
	mov	word ptr [si+02],NIL_PAGE*2	;clear ret reg in case of GC
	push	ax 				;size
	push	[BP].ob_typ			;type
	push	si 				;return reg
	call	alloc_bi 			;Allocate Big Block
	mov	sp,bp 
	jmp	al050 				;return to caller

;block not found in allocated pages, try to allocate a new page
al010:
	push	[bp].ob_typ			;type
	call	alloc_pa 			;Allocate new page
	mov	sp,bp 
	mov	[bp].page,ax			;update page 
	cmp	ax,END_LIST			;did we succeed?
	jnz	al040				; yes, jump
;no more pages, try a garbage collection, then search the pages again
;for a free block
	mov	si,[bp].ret_reg
	mov	word ptr [si+02],NIL_PAGE*2	;clear reg before GC
	call	garbage 			;do garbage collection
	call	srch_block			;search for block again
	jc	al050				;return on success
;
; Still couldn't find a block large enough, try to allocate a new page once
; again (since we just did a garbage collection).
;
	push	[BP].ob_typ			;type
	call	alloc_pa 			;Allocate a new page
	mov	sp,bp 
	mov	[bp].page,AX			;save page number
	cmp	ax,END_LIST			;did we succeed?
	jnz	al040 				; yes, jump
; We're getting desperate now. Try a collection with compaction, then try to
; allocate a new page for the object
	mov	si,[bp].ret_reg
	mov	word ptr [si+02],0		;clear for possible GC
	call	gcsquish 	    		;Compact memory
	push	[bp].ob_typ			;type
	call	alloc_pa 			;Allocate a new page
	mov	sp,bp 
	mov	[bp].page,ax 
	cmp	ax,END_LIST			;Did we succeed?
	jz	alloc_err			; no, out of memory
;at this point, a new page has been allocated; get a block from it
al040:
	push	[bp].page			;page
	push	[bp].ob_siz			;size
	push	[bp].ob_typ			;type
	push	[bp].ret_reg			;return reg
	call	find_block			;Allocate a Block 
	mov	sp,bp 
	jnc	alloc_err
;
; We have found a block, set up the header and return
;
al050:
	cmp	[bp].ob_typ,STRTYPE
	jnz	alloc_ret
	cmp	[bp].str_size,PTRSIZE
	jge	alloc_ret
;for small strings, put the negative value for object length
	push	es
	mov	si,[bp].ret_reg
	mov	bx,[si+02]			;bx = page
	mov	si,[si]    			;si = displacement
	LoadPage es,bx
	mov	cx,[bp].str_size			
	sub	cx,PTRSIZE			;cx = size - PTRSIZE
	mov	word ptr es:[si+1],cx		;replace object length
	pop	es				;restore extra segment
alloc_ret:
	add	sp,al_bp			;remove local data
	pop	bp 				;restore base pointer
	pop	es				;restore extra segment
	ret	 				;return to caller
	public  alloc_err
alloc_err:
	call	out_of_m 			;out of memory
	jmp	alloc_ret			;control will not return here


; SRCH_BLOCK - Search through all the pages of a given type looking for a 
;	       block large enough to fill the size request.
;
; Upon Entry:  All local storage and args to ALLOC_BLOCK are used. Do
;	       not modify BP.	
;
; Upon Exit:   Carry Flag set, ret_reg will contain the page:disp of the block.
;              Carry Flag clear, ret_reg will contain page of -1
;
	   public srch_block
srch_block label near
	pop	[bp].temp_ret			;save return value

	mov	bx,[bp].ob_typ			;bx = object type
	shl	bx,1 				;make into table index
	mov	si,bx
	add	bx,offset pagelist		;bx = address of pagelist[type]
	mov	[bp].last_page,bx		;save in last_page
	mov	ax,pagelist[si]			;ax = page number for this type
	cmp	ax,END_LIST			;any pages to search?
	clc					;carry clear = failure
	jz	srch_end			; no, skip loop
srch_loop:
	mov	[bp].page,ax   			;save page number for later

	push	ax				;page number
	push	[bp].ob_siz			;size of object
	push	[bp].ob_typ			;type of object
	push	[bp].ret_reg			;register to return value in
	call	find_block			;look for free space in page
	mov	sp,bp				;dump args off stack
	jc	srch_end			;carry set = success
;
; Block not found within current page.
;
	mov	si,[bp].page			;get page number
	shl	si,1				; and make into index
	cmp	[bp].ob_siz,SMALL_SiZE		;size <= SMALL_SIZE?
	jg	sr10		 		; no,  jump
; less than small_size space is left within the page; this isn't worth searching 
; again, so update the last position in the chain (last_page) to point to the
; next page in the chain.
	mov	ax,pagelink[si]			;get next page link
	mov	di,[bp].last_page
	mov	[di],ax				;*last_page = pagelink[page] 
sr10:
; update last_page to contain the address of the next position in the chain,
; and get the next page from pagelink[page].
	mov	bx,offset pagelink		;bx = address of pagelink table
	add	bx,si 				;bx = address of pagelink[page]
	mov	[bp].last_page,bx 		;save in last_page
	mov	ax,pagelink[si]			;get next page number
	cmp	ax,END_LIST			;reached end of chain?
	jne	srch_loop			; no, continue search for block
	clc					;carry clear = failure
srch_end:
	jmp	[bp].temp_ret			;return to caller

alloc_bl endp

;;;FIND_BLOCK
;;;
;;;  calling sequence:  find_block(reg, type, size, page)
;;;
;;;  Upon Exit: carry flag set: reg contains page:displ of new block
;;;		carry flag clr: reg contains page of -1
;;;
fb_args struc
     	   dw	?		;callers bp
	   dw	?		;return address
r_reg	   dw	?      		;register for return value
bl_typ	   dw	?     		;block type
bl_siz	   dw	?     		;block size
bl_pag	   dw	?     		;page number
fb_args    ends

	   public find_block	
find_block proc near
	push	bp 
	mov	bp,sp

	mov	si,[bp].r_reg			;get return register
	mov	Word Ptr [si+02],-1		;default to block not found

	mov	si,[bp].bl_pag			;get page number
	shl	si,1 				;si = page index
	LoadPage es,si				;es => page

; lets see if there's space in the free pool of this block
	mov	bx,nextcell[si]			;bx = next cell in page
  	cmp	bx,END_LIST			;if no more space
	jz	fb015 				;  then jump
	mov	ax,es:[bx+1]			;ax = free pool size
	mov	dx,[bp].bl_siz			;get size required
	cmp	ax,dx 				;if not enough space in pool
	jl	fb015				; then jump

; allocate a block from the free pool.
; ax = free pool size, bx = displacement, dx = object size
	mov	cx,[bp].bl_typ			;cx = type of object
	mov	byte ptr es:[bx],cl		;store type of new object
	mov	word ptr es:[bx+1],dx		;store size of new object
	mov	di,bx				;cx = displacement
	add	di,dx				;di = new displacement
	mov	cx,psize[si]			;get page size
	sub	cx,BLK_OVHD 			; and subtract block overhead
	cmp	cx,di				;next displ still in page?
	jb	fb010				; no, jump
	mov	byte ptr es:[di],FREETYPE	;mark next area as free
	sub	ax,dx				;ax = pool size -  object size
	mov	word ptr es:[di+1],ax		;update free pool size
	mov	nextcell[si],di			;update nextcell chain
	jmp	fb045 				;return to caller
fb010:
	mov	nextcell[si],END_LIST		;nextcell[page] = END_LIST
	jmp	fb045 	   			;return to caller

; A block was not found in the free pool. Search the entire block for a fragment
; to satisfy the request.
fb015:
	xor	bx,bx				;bx = displacement
	mov	cx,psize[si]
	sub	cx,[bp].bl_siz			;cx = displacement threshold
	cmp	cx,bx				;threshhold >= displacement?
	clc					;zero flag not set = failure
	jl	fb050				;return with no block found

;the following loop requires bx=displacement, cx=threshold, dx=free size
fb020:
	mov	dx,word ptr es:[bx+1]		;dx = size of object
	cmp	byte ptr es:[bx],FREETYPE	;is next area free?
	jz	fb035				; yes, jump
fb025:	mov	ax,BLK_OVHD+PTRSIZE		;ax = ovhd for small string
	test	dx,dx 				;if size negative
	js	fb030				; then jump
	mov	ax,dx				; else ax = size of object
fb030:	add	bx,ax        			;displacement += size
	cmp	cx,bx				;if disp <= threshhold
	jge	fb020				; then go look at next object
	clc					;zero flag not set = failure
	jmp	fb050				;return with no block found

;we have found a free space in the block; if not big enough then jump back
;into loop above, otherwise allocate the new storage
fb035:
	mov	ax,[bp].bl_siz
	cmp	ax,dx				;compare size to free size
	jl	fb025				;if less, return to loop
	jnz	fb040				;if not equal, jump
; we found an exact match
	mov	ax,[bp].bl_typ
	mov	byte ptr es:[bx],al		;just update the type field
	jmp	fb045 				;and return to caller
fb040:	
	mov	di,dx
	sub	di,BLK_OVHD			;di = free size - block overhead
	cmp	di,ax         			;can object fit into free space?
	jle	fb025 				; no, return to loop
; we can fit into a larger block, split block to allocate storage
	mov	cx,[bp].bl_typ			;cx = type of object
	mov	byte ptr es:[bx],cl		;store type of new object
	mov	word ptr es:[bx+1],ax		;store size of new object
;ax=new object size, bx=disp, dx= free size
	mov	di,bx
	add	di,ax 				;di = new displacement
	mov	cx,dx				
	sub	cx,ax 				;cx = free size - new size
	mov	byte ptr es:[di],FREETYPE	;mark next area as free
	mov	word ptr es:[di+1],cx		;update next area free size
;
; block found; return page,disp in return register.
; si = page index, bx = displacement
fb045:
	mov	ax,si				;ax = page index
	mov	si,[bp].r_reg			;si = address of return reg
	mov	[si+02],ax 			;put page index  in register
	mov	[si],bx 			;put diplacement in register
	stc					;carry set = success
fb050:
	pop	bp 				;restore base pointer
	ret	 				;return to caller

find_block endp	

prog	ends
	END
