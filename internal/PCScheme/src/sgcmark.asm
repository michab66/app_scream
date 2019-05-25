;***************************************
;*   TIPC Scheme '84 Runtime Support   *
;*   Garbage Collection - Mark Phase   *
;*				       *
;*  (C) Copyright 1984,1985 by Texas   *
;*     Instruments Incorporated.       *
;*	  All rights reserved.	       *
;*				       *
;* Date Written:  April 1984	       *
;* Last Modification:  06 January 1986 *
;***************************************
	  include scheme.equ

arguments struc
	  dw	  ?		   ; Caller's BP
	  dw	  ?		   ; Return address
page_idx  dw	  ?		   ; Page number of pointer
pointer   dw	  ?		   ; Displacement of pointer
arguments ends

DGROUP	  group   data
data	  segment word public 'DATA'
	  assume  DS:DGROUP
	  extrn	  _base:word	   ; base address of the TIPC runtime stack
sum_bt	  dw	  sum_list	   ; [0] List cells
	  dw	  sum_fix	   ; [1] Fixnums
	  dw	  sum_flo	   ; [2] Flonums
	  dw	  sum_big	   ; [3] Bignums
	  dw	  sum_sym	   ; [4] Symbols
	  dw	  sum_str	   ; [5] Strings
	  dw	  sum_ary	   ; [6] Arrays
	  dw	  sum_cont	   ; [7] Continuations
	  dw	  sum_clos	   ; [8] Closures
	  dw	  sum_free	   ; [9] Free page
	  dw	  sum_code	   ; [10] Code page
	  dw	  sum_free	   ; [11] (Formerly, Reference cells)
	  dw	  sum_port	   ; [12] Port data objects
	  dw	  sum_char	   ; [13] Characters
	  dw	  sum_env	   ; [14] Environments
;     Branch table for pointer classification
branchtab dw	  gcmlist	   ; [0] List cells
	  dw	  gcmfix	   ; [1] Fixnums
	  dw	  gcmflo	   ; [2] Flonums
	  dw	  gcmbig	   ; [3] Bignums
	  dw	  gcmsym	   ; [4] Symbols
	  dw	  gcmstr	   ; [5] Strings
	  dw	  gcmary	   ; [6] Arrays
	  dw	  gcmcont	   ; [7] Continuations
	  dw	  gcmclos	   ; [8] Closures
	  dw	  gcmfree	   ; [9] Free page
	  dw	  gcmcode	   ; [10] Code page
	  dw	  gcmfree	   ; [11] (Formerly, Reference cells)
	  dw	  gcmport	   ; [12] Port data objects
	  dw	  gcmchar	   ; [13] Characters
	  dw	  gcmenv	   ; [14] Environments

m_oops	  db	  "[VM INTERNAL ERROR] sum_spac: infinite loop page %d",LF,0
m_format  db	  "[VM INTERNAL ERROR] sgcmark: invalid pointer: %x:%04x "
	  db	  "(unadjusted)",LF,0
m_overfl  db	  "[VM FATAL ERROR] Stack overflow during GC",LF,0
DS_addr	  dw	  DGROUP
data	  ends

PGROUP	  group   prog
prog	  segment byte public 'PROG'

	  assume  CS:PGROUP

	  public  garbage
garbage	  proc	  near
	  push	  ES
	  mov	  ES,DS_addr
	  C_call  garbage1
	  pop	  ES
	  ret
garbage   endp

mark	  proc	  near
;     ***error-- bad pointer found-- report error***
gcmfix: 			   ; Fixnums are immediates
gcmchar:			   ; Characters are immediates
gcmfree:			   ; Why are we collecting in a free page?
bad_ptr:
	  push	  AX
	  mov	  AX,offset m_format ; load address of format text
	  push	  DX		   ; save the return address
	  pushm   <SI,BX,AX>	   ; push arguments to printf
	  C_call  printf,,Load_ES  ; print error message
	  add	  SP,WORDINCR*3    ; drop arguments from stack
	  C_call  force_de	   ; go into debug mode
	  pop	  DX		   ; restore the return address
	  pop	  AX
	  jmp	  gcmret	   ; go on as if nothing happened

	  public  gcmark
gcmark:	  pop	  DX		   ; unload return address
	  pop	  BX		   ; fetch page number (x 2)
	  mov	  AX,BX		   ; save in AX
	  pop	  SI		   ; fetch displacement
	  push	  DX		   ; save return address
	  push	  ES		   ; save ES
	  mov	  DX,offset pgroup:gcmarkret
	  jmp	  gcm_tr
gcmarkret:
	  pop	  ES
	  pop	  DX
	  jmp	  DX		   ; return


;     see if pointer is to one of the "special" non-collected pages
gcm_tr:	  cmp	  BX,DEDPAGES*PAGEINCR ; check for non-gc'ed pages
	  jge	  gcm_go	   ; if not one of the special pages, jump
	  jmp	  DX		   ; return
;
gcm_go:	  push	  AX		   ; Preserve the page number
;     load pointer offset into ES:; displacement into SI
	  test	  BX,0FF01h	   ; valid pointer?
	  jnz	  bad_ptr	   ; if so, error (jump)
	  LoadPage ES,BX
	  mov	  AX,BX		   ; Use AX to store page number
;     classify pointer according to data type
	  mov	  DI,ptype+[BX]	   ; load data type*2
	  cmp	  DI,NUMTYPES*2    ; valid page type?
	  jae	  bad_ptr	   ; if not, error (jump)
	  jmp	  branchtab+[DI]

;     Process symbol or port
gcmport:
gcmsym:   markedp ES:[SI].sym_gc,gcmret ; already marked? if so, return (jump)
	  or	  byte ptr ES:[SI].sym_gc,GC_BIT ; mark symbol/port as seen
	  mov	  BL,ES:[SI].sym_page ; fetch pointer from symbol/port object
	  mov	  SI,ES:[SI].sym_disp
	  pop	  AX		   ; restore saved page number
	  LoadPage ES,AX	   ; Get Page address
	  jmp	  gcm_tr	   ; make a tail recursive call to gcmark

;     Process List Cell-- If marked, skip rest of processing
gcmlist:  markedp ES:[SI].list_gc,gcmret ; if marked, jump to return
;      Call gcmark with CAR of list cell
	  or	  byte ptr ES:[SI].list_gc,GC_BIT ; "mark" as referenced
	  mov	  BL,ES:[SI].car_page ; load page number of car field
	  cmp	  BX,DEDPAGES*PAGEINCR ; check for non-gc'ed pages
	  jl	  gcmls_ok	   ; if one of the special pages, jump
;     Test for TIPC stack overflow
	  push	  AX
	  mov	  AX,SP		   ; copy the current stack top pointer
	  sub	  AX,_base	   ;  and compute number of bytes remaining
	  cmp	  AX,64		   ; enough space to continue?
	  pop	  AX
	  jb	  stk_ovfl	   ; if not enough room, abort (jump)
;     Mark expression pointed to by the car field
	  push	  SI		   ; save offset of list cell
	  push	  DX		   ; save the previous return address
	  mov	  DX,offset PGROUP:gcmls_rt ; Load the return address
	  mov	  SI,ES:[SI].car   ; Load car field pointer
	  and	  SI,07FFFh	   ; Clear out the GC bit
	  jmp	  gcm_go	   ; Call gcmark recursively
gcmls_rt: 
	  pop	  DX		   ; Restore previous return address
	  pop	  SI		   ; Restore offset of list cell
;      Call gcmark tail recursively with CDR of list cell
gcmls_ok: mov	  BL,ES:[SI].cdr_page ; load the pointer contained in the
	  mov	  SI,ES:[SI].cdr   ;  cdr field
	  pop	  AX		   ; restore saved page 
	  LoadPage ES,AX	   ; Get Page address
	  jmp	  gcm_tr	   ; call gcmark tail recursively

;     TIPC stack overflow-- Abort
stk_ovfl: mov	  AX,offset m_overfl ; load address of error message text
	  push	  AX		   ;  and push it as an argument to printf
	  C_call  printf,,Load_ES  ; print the error message
	  C_call  getch		   ; wait for any key to be pressed
	  C_call  exit		   ; return to MS-DOS

;     Return to caller
gcmret:   pop	  AX		   ; restore saved page
	  LoadPage ES,AX	   ; Get Page address
	  jmp	  DX		   ; return to caller


;     Process reference to variable length data object or flonum
gcmflo:
gcmbig:
gcmstr:
	  or	  byte ptr ES:[SI].vec_gc,GC_BIT
	  pop	  AX		   ; restore saved page
	  LoadPage ES,AX	   ; Get Page address
	  jmp	  DX		   ; return
	
;     Process Code Block
gcmcode:  markedp ES:[SI].cod_gc,gcmret ; If already processed, return
	  or	  byte ptr ES:[SI].cod_gc,GC_BIT
	  mov	  CX,ES:[SI].cod_entr ; load entry point offset as counter
	  jmp	  gcmlop1

;     Process Variable Length Object Containing Pointers
gcmary:
gcmclos:
gcmcont:
gcmenv:
	  markedp ES:[SI].vec_gc,gcmret ; If already processed, jump to return
	  or	  byte ptr ES:[SI].vec_gc,GC_BIT ; mark as referenced
	  mov	  CX,ES:[SI].vec_len
	  cmp	  CX,PTRSIZE	   ; test for zero length vector
	  jle	  gcmret	   ; if no elements, jump
;      Test the size of the TIPC stack to insure room to continue
gcmlop1:  push	  AX
	  mov	  AX,SP		   ; load the current stack top pointer
	  sub	  AX,_base	   ;  and compute the number of bytes remaining
	  cmp	  AX,64		   ; are there at least 64 bytes left?
	  pop	  AX
	  jb	  stk_ovfl	   ; if not enough room, abort (jump)
;      Call gcmark with pointer in this object
	  push	  DX		   ; Save previous return address
	  mov	  DX,offset PGROUP:gcml_ret ; Load return address into DX
gcmloop:  add	  SI,PTRSIZE	   ; Increment address for next pointer
	  push	  CX		   ; Save counter across calls
	  push	  SI		   ; Save curr offset into vector (or whatever)
	  mov	  BL,ES:[SI].car_page ; load next element pointer from array,
	  mov	  SI,ES:[SI].car   ;  closure, etc.
	  jmp	  gcm_tr	   ; call gcmark recursively
gcml_ret: pop	  SI		   ; Restore current offset
	  pop	  CX		   ; Restore iteration count
	  sub	  CX,PTRSIZE	   ; Decrement counter
	  cmp	  CX,PTRSIZE	   ;  and test for completion
	  jg	  gcmloop	   ; Loop through all pointers in object
	  pop	  DX		   ; Restore previous return address
	  pop	  AX		   ; Restore saved page
	  LoadPage ES,AX	   ; Get Page address
	  jmp	  DX		   ; Return

mark	  endp

sum_args  struc
	  dw	  ?		   ; caller's ES
	  dw	  ?		   ; caller's BP
	  dw	  ?		   ; return address
sum_vctr  dw	  ?		   ; pointer to summation vector (for results)
sum_args  ends

	  public  sum_spac
sum_spac  proc	  near
	  push	  BP		   ; save the caller's BP on entry
	  push	  ES		   ; save the caller's ES
	  mov	  BP,SP 	   ; update BP

;     initialize
	  mov	  DI,[BP].sum_vctr ; load address of result vector
	  xor	  BX,BX 	   ; start with zero-th page

;     top of loop-- look at next page
sum_loop: xor	  AX,AX 	   ; clear the free space counter
	  cmp	  BX,DEDPAGES*PAGEINCR
	  jl	  sum_end
	  test	  attrib+[BX],NOMEMORY ; is page allocated?
	  jnz	  sum_end	   ; if not, skip it (branch)
	  cmp	  ptype+[BX],FREETYPE*2
	  je	  sum_free	   ; Ignore free pages			[TC]
	  LoadPage ES,BX	   ; load current paragraph's base address
	  mov	  SI,ptype+[BX]    ; load type of current page
	  jmp	  sum_bt+[SI]	   ; branch on page type

;     add up unused list cells
sum_list: mov	  CX,LISTSIZE	   ; load size of list cell data object
sum_l1st: mov	  SI,nextcell+[BX] ; load list cell free storage chain header
sum_lnxt: cmp	  SI,END_LIST	   ; end of list?
	  je	  sum_end	   ; if so, we're through here
	  add	  AX,CX 	   ; increment the free list cell counter
	  jo	  sum_oops	   ; if overflow, we're stuck in a loop
	  mov	  SI,ES:[SI].car   ; follow free cell chain
	  jmp	  sum_lnxt	   ; keep following linked list

;      add up unused variable length things
sum_big:
sum_sym:
sum_str:
sum_clos:
sum_cont:
sum_ary:
sum_code:
sum_port:
sum_env:
	  mov	  SI,0		   ; initialize pointer into page
	  mov	  CX,psize+[BX]    ; load size of current page
	  sub	  CX,PTRSIZE	   ; adjust size for page boundary check
sum_vnxt: cmp	  SI,CX 	   ; through with this page?
	  ja	  sum_end	   ; if so, branch
	  mov	  DX,ES:[SI].vec_len ; load block length
	  cmp	  DX,0		   ;;; check for small string
	  jge	  sum_010
	  mov	  DX,BLK_OVHD+PTRSIZE ;;; get the exact length
sum_010:  cmp	  ES:[SI].vec_type,FREETYPE ; free block?
	  jne	  sum_used	   ; if so, branch around add
	  add	  AX,DX 	   ; add in number of free bytes
sum_used: add	  SI,DX 	   ; update pointer to next block in page
	  jmp	  sum_vnxt	   ; look at next block

sum_free: mov	  AX,psize+[BX]    ; load size of free page

sum_fix:
sum_char:
sum_end:  mov	  [DI],AX	   ; store number of free bytes (AX)
	  add	  DI,2		   ; increment array index
	  add	  BX,2		   ; increment page index
	  cmp	  BX,NUMPAGES*2    ; test for completion
	  jl	  sum_loop	   ; if more pages, jump

sum_ret:  pop     ES		   ; restore caller's ES
	  pop	  BP		   ; restore caller's BP
	  ret			   ; return to caller

;     add up unused flonums
sum_flo:  mov	  CX,FLOSIZE	   ; load size of flonum
	  jmp	  sum_l1st	   ; process assuming linked list allocation

sum_oops: shr	  BX,1
	  lea	  SI,m_oops
	  pushm   <BX,SI>
	  mov	  AX,DS
	  mov	  ES,AX
	  C_call  printf
	  C_call  exit

sum_spac  endp

prog	  ends
	  end

