;							=====> SGCSWEEP.ASM
;***************************************
;*   TIPC Scheme '84 Runtime Support   *
;*   Garbage Collector - Sweep Phase   *
;*				       *
;*  (C) Copyright 1984,1985 by Texas   *
;*     Instruments Incorporated.       *
;*	  All rights reserved.	       *
;*				       *
;* Date Written:  April 1984	       *
;* Last Modification:  06 January 1986 *
;***************************************
	  include scheme.equ

DGROUP	  group   data
data	  segment word public 'DATA'
	  assume  DS:DGROUP
m_fix_er  db	  "[VM INTERNAL ERROR] swpage: logical page not found",LF,0
data	  ends

PGROUP	  group   prog
prog	  segment byte public 'PROG'
	  assume  CS:PGROUP
	  public  gcsweep
gcsweep   proc	  near
	  push	  BP
	  mov	  BP,SP
;     Initialize similar page type chain headers
	  push	  ES		   ; save the caller's ES register
	  mov	  AX,DS 	   ; set ES to point to the current
	  mov	  ES,AX 	   ;  data segment
	  mov	  AX,END_LIST	   ; load the end of list indicator
	  mov	  CX,NUMTYPES	   ; load table length
	  mov	  DI,offset pagelist ; load table address
	  cld			   ; move direction = forward
rep	  stosw 		   ; initialize the pagelist table
	  pop	  ES		   ; restore the caller's ES
;     Process all except the "special" non-garbage collected pages
;	  mov	  DX,DEDPAGES-1 	       ;;;;  mov dx,NUMPAGES
;     Increment loop index, test for completion
;gcsloop:  inc	  DX			       ;;;;  dec dx
;	  cmp	  DX,NUMPAGES		       ;;;;  cmp dx,DEDPAGES-1
;	  jl	  gcsl010		       ;;;;  ja  gcsl010

	  mov	  DX,NUMPAGES
 gcsloop:  dec	  DX
	  cmp	  DX,DEDPAGES-1
	  ja	  gcsl010

	  pop	  BP
	  ret
gcsl010:  push	  DX
	  call	  swpage	   ; "sweep" the page (GC it)
	  pop	  DX
	  mov	  BX,DX 	   ; copy current page number
	  sal	  BX,1		   ; double for use as index
	  test	  attrib+[BX],NOMEMORY ; is page frame allocated?
	  jnz	  gcsloop	   ; if not, skip list update
	  mov	  AX,DX 	   ; copy current page number
	  mov	  SI,ptype+[BX]    ; move current page's type to SI
	  xchg	  pagelist+[SI],AX ; pagelist[type] <- page
	  mov	  pagelink+[BX],AX ; pagelink[page] <- old pagelist[type]
	  jmp	  short gcsloop
gcsweep   endp

arguments struc
page_len  dw	  ?		   ; page boundary (length - fudge factor)
args_BP   dw	  ?		   ; Caller's BP
	  dw	  ?		   ; Return address
page_no   dw	  ?
arguments ends

;     Test the current page to see if it's been allocated
	  public  swpage
swpage	  proc	  near
	  push	  BP
	  sub	  SP,offset args_BP ; reserve local storage
	  mov	  BP,SP
	  push	  ES		   ; save caller's ES
	  mov	  BX,[BP].page_no
	  sal	  BX,1		   ; double page number for index
	  test	  DGROUP:attrib+[BX],NOMEMORY ; allocated?
	  jz	  swp020	   ; if not allocated, loop
swpfix: 			   ; Fixnums are handled as immediates
swpchar:			   ; Characters are handled as immediates
swpfree:			   ; Why are we processing a free page?
swpref: 			   ; Ref cells no longer exist?
swpret:   pop	  ES
	  add	  SP,offset args_BP ; drop local storage from stack
	  pop	  BP
	  ret
swp020:
;     Dispatch on the type of data stored in this page
	  mov	  DI,DGROUP:ptype+[BX]	  ; load data type for this page
	  cmp	  DI,FREETYPE*2    ; Ignore free pages			[HS]
	  jz	  swpfree	   ;	to relieve the swapper...	[HS]
	  LoadPage ES,BX	   ; define base paragraph for this page[HS]
	  mov	  DI,CS:btable+[DI]
	  jmp	  DI

;     Process List Cells (and other fixed length pointer objects)
swplist:  mov	  AX,LISTSIZE
swpl010:  xor	  SI,SI 	   ; SI <- 0
	  xor	  DI,DI 	   ; zero referenced cell counter
	  mov	  CX,END_LIST	   ; load end of list marker
	  mov	  DX,-1 	   ; marker for unused cell header
	  push	  BX		   ; save page number index
	  mov	  BX,psize+[BX]    ; load page length and
	  sub	  BX,AX 	   ;  adjust for boundary check
swpl020:  markedp ES:[SI].list_gc,swpl030 ; branch, if marked
;     add cell to free list
	  mov	  ES:[SI].car,CX
	  mov	  ES:[SI].car_page,DL ; make page=FF for unused cell
	  mov	  CX,SI
	  jmp	  short swpl040
;     clear GC bit
swpl030:  and	  byte ptr ES:[SI].list_gc,NOT_GC_BI ; clear GC "marked" bit
	  inc	  DI		   ; increment referenced cell counter
;     increment cell pointer and test for end of page
swpl040:  add	  SI,AX
	  cmp	  SI,BX 	   ; test for end of page
	  jbe	  swpl020
;     end of page-- update free list header and process next page
	  pop	  BX		   ; restore page table index
	  mov	  DGROUP:nextcell+[BX],CX
	  cmp	  DI,0		   ; any referenced cells in this page?
	  jne	  swpret	   ; if ref'd cells in page, branch
	  mov	  ptype+[BX],FREETYPE*2 ; mark empty page as free
	  mov	  attrib+[BX],0
	  jmp	  short swpret

;     Process Page of Flonums
swpflo:   mov	  AX,FLOSIZE	   ; load size of a single flonum
	  xor	  SI,SI 	   ; SI <- 0
	  xor	  DI,DI 	   ; zero referenced cell counter
	  mov	  CX,END_LIST	   ; load end of list marker
	  mov	  DX,-1 	   ; marker for unused cell header
	  push	  BX		   ; save page number index
	  mov	  BX,psize+[BX]    ; load page length and
	  sub	  BX,AX 	   ;  adjust for boundary check
swpf020:  cmp	  ES:[SI].flo_type,DL ; tag = free?
	  je	  swpf025	   ; if a non-allocated cell, jump
	  markedp ES:[SI].flo_gc,swpf030 ; branch, if marked
;     add flonum to free list
	  mov	  ES:[SI].car_page,DL ; make page=FF for unused cell
swpf025:  mov	  ES:[SI].car,CX
	  mov	  CX,SI
	  jmp	  short swpf040
;     clear GC bit
swpf030:  and	  byte ptr ES:[SI].flo_gc,NOT_GC_BI ; clear GC "marked" bit
	  inc	  DI		   ; increment referenced cell counter
;     increment cell pointer and test for end of page
swpf040:  add	  SI,AX
	  cmp	  SI,BX 	   ; test for end of page
	  jbe	  swpf020
;     end of page-- update free list header and process next page
	  pop	  BX		   ; restore page table index
	  mov	  DGROUP:nextcell+[BX],CX
	  cmp	  DI,0		   ; any referenced cells in this page?
	  jne	  swpf050	   ; if ref'd cells in page, branch
	  mov	  ptype+[BX],FREETYPE*2 ; mark empty page as free
	  mov	  attrib+[BX],0
swpf050:  jmp	  swpret

;     Process variable length data object
swpbig:
swpsym:
swpstr:
swpary:
swpclos:
swpcont:
swpcode:
swpenv:
	  xor	  SI,SI
	  mov	  DI,-1
	  push	  BX		   ; save page table index
	  mov	  BX,psize+[BX]    ; load size of current page and
	  sub	  BX,PTRSIZE	   ;  adjust for boundary check
swpvloop: mov	  DX,ES:[SI].vec_len ; load length of current object
	  cmp	  DX,0
	  jge	  swp001
	  mov	  DX,BLK_OVHD+PTRSIZE
swp001:   markedp ES:[SI].vec_gc,swpv020 ; branch if object referenced
;     Object not referenced-- can we combine with previous free area?
	  cmp	  DI,0
	  jge	  swpv010	   ; If prev obj free, branch
;     Object not referenced, but previous area was
	  mov	  ES:[SI].vec_type,FREETYPE ; Mark this object as free
	  cmp	  ES:[SI].vec_len,0
	  jge	  swp002
	  mov	  ES:[SI].vec_len,BLK_OVHD+PTRSIZE
swp002:   mov	  DI,SI 	   ; Record this fact for next iteration
	  jmp	  short swpvnxt
;     Object was not referenced and can be combined with prev free area
swpv010:  add	  ES:[DI].vec_len,DX ; add length into previous free obj
	  jmp	  short swpvnxt
;     Object was referenced
swpv020:  and	  ES:[SI].vec_gc,NOT_GC_BI ; clear gc bit
	  mov	  DI,-1 	   ; Remember last object was referenced
;     Processing of current object finished-- add length and iterate
swpvnxt:  add	  SI,DX 	   ; Increment area pointer by block length
	  cmp	  SI,BX 	   ; Last object in block?
	  jb	  swpvloop	   ; Branch, if more space
;     Processing of this page finished-- update next free area pointer
swppfin:  pop	  BX		   ; Restore page table index
	  cmp	  DI,-1
	  je	  swpv030	   ; If last block not free, skip it
	  sub	  SI,psize+[BX]    ; Adjust in case last byte of page
	  neg	  SI		   ;  not accounted for
	  add	  ES:[DI].vec_len,SI
	  mov	  nextcell+[BX],DI ; Update free pool header
	  cmp	  DI,0		   ; is page empty?
	  jne	  swpv040	   ; if not, jump
	  mov	  ptype+[BX],FREETYPE*2 ; mark page as being free
	  mov	  attrib+[BX],0
	  mov	  AX,psize+[BX]
	  cmp	  AX,PAGESIZE	   ; is page larger than default page size?
	  ja	  fix_big	   ; if a "large" page, must fix memory tables
	  jmp	  swpret
swpv030:  mov	  nextcell+[BX],END_LIST ; Indicate no free pool
swpv040:  jmp	  swpret

;     Process page of ports-- close any open files before salvaging memory
swpport:
	  xor	  SI,SI
	  mov	  DI,-1
	  push	  BX		   ; save page table index
	  mov	  BX,psize+[BX]    ; load size of current page and
	  sub	  BX,PTRSIZE	   ;  adjust for boundary check
swpploop: mov	  DX,ES:[SI].pt_len ; load length of current object
	  markedp ES:[SI].port_gc,swpp020 ; branch if object referenced
	  cmp	  ES:[SI].pt_type,FREETYPE
	  je	  not_file
;     Object not referenced-- is it an open file?
	  test	  ES:[SI].pt_pflgs,WINDOW+STRIO
				   ; is this a file or a window?
	  jnz	  not_file	   ; if a window, don't bother with close (jump)
	  test	  ES:[SI].pt_pflgs,OPEN ; is file opened?
	  jz	  not_open	   ; if not open, skip close (jump)
;     Close the file
	  push	  BX		   ; save BX across call
	  mov	  BX,ES:[SI].pt_handl ; load handle
	  push	  BX		   ;  and push as argument
	  extrn   zclose:near
	  call	  zclose
	  pop	  BX		   ; drop argument off stack
	  pop	  BX		   ; restore register BX
not_file:
not_open:
;     Object not referenced-- can we combine with previous free area?
	  cmp	  DI,0
	  jge	  swpp010	   ; If prev obj free, branch
;     Object not referenced, but previous area was
	  mov	  ES:[SI].pt_type,FREETYPE ; Mark this object as free
	  mov	  DI,SI 	   ; Record this fact for next iteration
	  jmp	  short swppnxt
;     Object was not referenced and can be combined with prev free area
swpp010:  add	  ES:[DI].pt_len,DX ; add length into previous free obj
	  jmp	  short swppnxt
;     Object was referenced
swpp020:  and	  ES:[SI].port_gc,NOT_GC_BI ; clear gc bit
	  mov	  DI,-1 	   ; Remember last object was referenced
;     Processing of current object finished-- add length and iterate
swppnxt:  add	  SI,DX 	   ; Increment area pointer by block length
	  cmp	  SI,BX 	   ; Last object in block?
	  jb	  swpploop	   ; Branch, if more space
	  jmp	  swppfin	   ; complete processing

public fix_big
;     Restore memory management tables due to release of large page
fix_big label near
	  mov	  AX,PAGESIZE	   ; update page size of large page to
	  xchg	  AX,psize+[BX]    ;  the default page size
	  LoadPage DX,BX	   ; load para address of large page
IFDEF EXTMEM
	  and	  pagetabl+[BX],0FF00h
ENDIF
IFDEF PROMEM
	  mov	  CX,8		   ; amount to get to next selector
ELSE
	  mov	  CX,PAGESIZE	   ; CX <- PAGESIZE/16
	  shr	  CX,1
	  shr	  CX,1
	  shr	  CX,1
	  shr	  CX,1
ENDIF
	  mov	  BX,PAGESIZE
fix_lop:  sub	  AX,PAGESIZE	   ; decrease extended page size by one page
	  jbe	  fix_ret	   ; if all pages fixed, return
	  add	  DX,CX 	   ; compute pointer to next physical page
	  mov	  SI,DEDPAGES*2    ; initialize page table index
fix_more: push	  BX
	  LoadPage BX,SI	   ; is this the page we're looking for?
	  cmp	  DX,BX
	  pop	  BX
	  je	  fix_fnd	   ; if so, jump
	  inc	  SI		   ; increment the page table index
	  inc	  SI		   ;  twice
	  cmp	  SI,NUMPAGES*2    ; more pages?
	  jl	  fix_more	   ; if so, jump
	  lea	  BX,m_fix_er	   ; error-- loop should not exit
	  push	  BX
	  mov	  AX,DS 	   ; set TIPC register ES for call to
	  mov	  ES,AX 	   ;  Lattice C routines
	  C_call  print_an	   ; print error message and exit
fix_fnd:  mov	  psize+[SI],BX    ; reset page size to default
	  mov	  attrib+[SI],0    ; reset "no memory" bit in attribute table
IFDEF EXTMEM
	  and	  pagetabl+[SI],0FF00h	; strip attributes
ENDIF
	  mov	  ptype+[SI],FREETYPE*2 ; mark page as free
	  jmp	  short fix_lop    ; continue to free extended pages
fix_ret:  jmp	  swpret	   ; all pages released-- return
;     Branch table for processing each data type
btable	  dw	  swplist	   ; [0] List cells
	  dw	  swpfix	   ; [1] Fixnums
	  dw	  swpflo	   ; [2] Flonums
	  dw	  swpbig	   ; [3] Bignums
	  dw	  swpsym	   ; [4] Symbols
	  dw	  swpstr	   ; [5] Strings
	  dw	  swpary	   ; [6] Arrays
	  dw	  swpcont	   ; [7] Continuations
	  dw	  swpclos	   ; [8] Closures
	  dw	  swpfree	   ; [9] Free space (unallocated)
	  dw	  swpcode	   ; [10] Code
	  dw	  swpref	   ; [11] Reference cells
	  dw	  swpport	   ; [12] Port data objects
	  dw	  swpchar	   ; [13] Characters
	  dw	  swpenv	   ; [14] Environments

swpage	  endp
prog	  ends
	  end

