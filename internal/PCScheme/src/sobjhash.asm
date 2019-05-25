;							=====> SOBJHASH.ASM
;***************************************
;*     TIPC Scheme  Runtime Support    *
;*	 Object Hashing Routines       *
;*				       *
;*    (C) Copyright 1985 by Texas      *
;*     Instruments Incorporated.       *
;*	  All rights reserved.	       *
;*				       *
;* Date Written:  25 June 1985	       *
;* Last Modification:  3 November 1985 *
;***************************************
	  include scheme.equ

DGROUP	  group   data
XGROUP	  group   PROGX
PGROUP	  group   prog

data	  segment word public 'DATA'
	  assume  DS:DGROUP
obj_cntr  dw	  OHT_SIZE dup (1)
branchtab dw	  ogc_list	   ; [0] List cells
	  dw	  ogc_mark	   ; [1] Fixnums
	  dw	  ogc_var	   ; [2] Flonums
	  dw	  ogc_var	   ; [3] Bignums
	  dw	  ogc_var	   ; [4] Symbols
	  dw	  ogc_var	   ; [5] Strings
	  dw	  ogc_var	   ; [6] Arrays
	  dw	  ogc_var	   ; [7] Continuations
	  dw	  ogc_var	   ; [8] Closures
	  dw	  ogc_mark	   ; [9] Free page
	  dw	  ogc_var	   ; [10] Code page
	  dw	  ogc_mark	   ; [11] Reference cells <not anymore>
	  dw	  ogc_var	   ; [12] Port data objects
	  dw	  ogc_mark	   ; [13] Characters
	  dw	  ogc_var	   ; [14] Environments
ret_sav1  dw	  0		   ; return address save area
ret_sav2  dw	  0		   ; return address save area
data	  ends

prog	  segment byte public 'PROG'
	  assume  CS:PGROUP

;************************************************************************
;*		    Far Linkage to "lookup" Routine			*
;************************************************************************
%lookup   proc	  far
	  extrn	  lookup:near
	  call	  lookup
	  ret
%lookup	  endp

;************************************************************************
;*		      Far Linkage to "cons" Routine			*
;************************************************************************
	  public  %cons
%cons	  proc	  far
	  pop	  ret_sav1
	  pop	  ret_sav2
	  mov	  AX,DS		   ; make ES point to the data segment
	  mov	  ES,AX
	  extrn	  cons:near
	  call	  cons
	  push	  ret_sav2
	  push	  ret_sav1
	  ret
%cons	  endp

;************************************************************************
;*		   Far Linkage to "alloc_block" Routine			*
;************************************************************************
	  public  %allocbl
%allocbl  proc	  far
	  pop	  ret_sav1
	  pop	  ret_sav2
	  mov	  AX,DS		   ; make ES point to the data segment
	  mov	  ES,AX
	  extrn	  alloc_bl:near
	  call	  alloc_bl
	  push	  ret_sav2
	  push	  ret_sav1
	  ret
%allocbl  endp
prog	  ends

PROGX	  segment byte public 'PROGX'
	  assume  CS:XGROUP
;************************************************************************
;*			      Object Hash				*
;************************************************************************
oh_args	  struc
oh_key	  dw	  ?		   ; computed hash key
oh_key3	  dw	  ?		   ; computed hash key * 3
oh_disp	  dw	  ?		   ; page number component of a pointer
oh_page	  dw	  ?		   ; displacement component of a pointer
oh_reg	  dw	  ?		   ; pointer to argument register (s=d)
oh_ctr	  dw	  ?		   ; bucket's current counter value
oh_ctag	  db	  SPECFIX*2,?	   ; tag for counter
oh_BP	  dw	  ?		   ; caller's BP
	  dw	  ?		   ; caller's SI
	  dw	  ?		   ; caller's ES
	  dd	  ?		   ; return address (far call)
	  dw	  ?		   ; return address (near call)
oh_args	  ends

%objhash  proc	  far
	  lods	  byte ptr ES:[SI] ; fetch operand of object-hash
	  push	  ES		   ; save the caller's ES register
	  push	  SI		   ; save the location counter
	  push	  BP		   ; save the caller's BP register
	  sub	  SP,offset oh_BP  ; allocate local storage
	  mov	  BP,SP		   ; establish local addressability
;     load argument and compute hash index
	  mov	  BX,AX		   ; copy dest=src register number to BX
	  add	  BX,offset reg0   ;  and compute the register's address
	  mov	  [BP].oh_reg,BX   ; save the register address
;;;
;;;   Note:  computing of hash value turned off 'cause relocation of
;;;	     pointers screws things up.  For now, all objects will
;;;	     hash to a key of zero.  (JCJ 2 OCT 85)
;;;	  mov	  DX,[BX].C_page   ; load the argument's page number
;;;	  mov	  AX,[BX].C_disp   ; load the argument's displacement
;;;	  mov	  CL,AH		   ; copy high byte of displacement
;;;	  xor	  AH,AH
;;;	  xor	  CH,CH
;;;	  add	  AX,CX
;;;	  add	  AX,DX
;;;	  mov	  CX,OHT_SIZE	   ; load the hash table size for divisor
;;;	  cwd			   ; convert dividend to double word
;;;	  div	  CX		   ; divide hash value by table size
  xor  DX,DX ; ***TEMPORARY*** Load a hash key of zero
;;;
	  mov	  [BP].oh_key,DX   ; save computed hash key
	  mov	  SI,DX
	  shl	  DX,1
	  add	  SI,DX		   ; SI <- hash_key * 3
	  mov	  [BP].oh_key3,SI

;     if entries exist at this hash level, search bucket for object
	  cmp	  obj_ht+[SI],0    ; anyone home in this bucket?
	  je	  oh_nf		   ; if no entries exist, jump

;     call "lookup" to search a-list
	  mov	  AX,[BX].C_disp   ; reload object's displacement
	  mov	  DX,[BX].C_page   ;  and page for a-list search
	  xor	  BX,BX
	  mov	  BL,obj_ht+[SI]
   	  %LoadPage ES,BX
;;;	  mov	  ES,pagetabl+[BX]
	  mov	  SI,word ptr obj_ht+[SI]+1
	  call	  %lookup	   ; search the a-list
	  cmp	  BL,0
	  je	  oh_nf

;     object found in hash bucket's chain-- return it
	  mov	  AX,ES:[DI].cdr   ; load the hash counter
	  mov	  [BP].oh_ctr,AX   ;  and save it in 'oh_ctr'
	  jmp	  short oh_ret	   ; return hash value

;     make a new entry in the current hash bucket
oh_nf:	  mov	  DI,[BP].oh_key
	  shl	  DI,1		   ; multiply hash value by 2 for index
	  mov	  AX,obj_cntr+[DI] ; load obj hash counter for this bucket
	  inc	  obj_cntr+[DI]    ; increment the obj hash counter
	  mov	  [BP].oh_ctag,SPECFIX*2 ; convert hash counter to a fixnum
	  mov	  [BP].oh_ctr,AX   ;  pointer
	  lea	  BX,[BP].oh_ctr   ; load hash counter's "reg" address
	  mov	  AX,[BP].oh_reg   ; load object's register address
	  mov	  CX,offset tmp_reg ; load offset of temporary register
	  pushm	  <BX,AX,CX>	   ; push arguments to call
	  call	  %cons		   ; cons(tmp_reg, object, hash-counter)
	  mov	  BX,offset nil_reg ; load address of "nil register"
	  mov	  CX,offset tmp_reg ; load address of temporary register
	  pushm	  <BX,CX,CX>	   ; push arguments to cons
	  call	  %cons		   ; cons(tmp_reg, (cons obj hash), nil)
	  mov	  SP,BP		   ; drop arguments from stack
	  mov	  DI,[BP].oh_key3  ; load hash bucket number * 3
	  mov	  BX,tmp_page	   ; load pointer to newest list cell
	  mov	  AX,tmp_disp
	  %LoadPage ES,BX
;;;	  mov	  ES,pagetabl+[BX]
	  mov	  SI,AX		   ; pointer is in ES:[SI]
	  xchg	  obj_ht+[DI],BL   ; header <- pointer to list cell
	  xchg	  word ptr obj_ht+[DI]+1,AX
	  mov	  ES:[SI].cdr_page,BL ; (set-cdr! list-cell chain-header)
	  mov	  ES:[SI].cdr,AX

;     create a bignum to hold the hash value
oh_ret:	  mov	  AX,WORDINCR*2+1  ; load the size of bignum result
	  push	  AX		   ;  and push it for use as argument
	  mov	  AX,BIGTYPE	   ; load type=bignum
	  push	  AX		   ;  and push it for use as argument
	  push	  [BP].oh_reg	   ; push address of destination register
	  mov	  AX,DS		   ; ES <- DS
	  mov	  ES,AX
	  call	  %allocbl	   ; allocate the bignum
	  mov	  SP,BP		   ; drop arguments off the TIPC's stack
	  mov	  BX,[BP].oh_reg   ; load destination register's address
	  mov	  SI,[BX].C_page   ; load bignum's page number
	  %LoadPage ES,SI           ; load bignum page's paragraph address
;;;	  mov	  ES,pagetabl+[SI] ; load bignum page's paragraph address
	  mov	  SI,[BX].C_disp   ; load bignum's displacement
	  mov	  AX,[BP].oh_key   ; load hash bucket number
	  mov	  ES:[SI].big_data,AX ;  and store it into LSW of bignum
	  mov	  AX,[BP].oh_ctr   ; load counter for this object
	  mov	  ES:[SI].big_2nd,AX ;  and store it into MSW of bignum
	  mov	  ES:[SI].big_sign,0 ; sign <- 0 (positive number)

;     return to caller
	  add	  SP,offset oh_BP  ; deallocate local storage
	  pop	  BP		   ; restore caller's BP register
	  pop	  SI		   ; restore the location pointer
	  pop	  ES		   ; restore caller's ES register
	  ret			   ; return to calling procedure
%objhash  endp

;************************************************************************
;*			     Object Unhash				*
;************************************************************************
unhs_arg  struc
un_reg	  dw	  ?		   ; argument register address
un_BP	  dw	  ?		   ; caller's BP
	  dw	  ?		   ; caller's SI
	  dw	  ?		   ; caller's ES
	  dd	  ?		   ; return address (far call)
	  dw	  ?		   ; return address (near call)
unhs_arg  ends

%objunhs  proc	  far
	  lods	  byte ptr ES:[SI] ; load the operand for object-unhash
	  push	  ES		   ; save the caller's ES register
	  push	  SI		   ; save the location pointer
	  push	  BP		   ; save the caller's BP register
	  sub	  SP,offset un_BP  ; allocate local storage
	  mov	  BP,SP		   ; establish local addressability

;     Begin the long process of validating the input
	  mov	  SI,AX
	  add	  SI,offset reg0
	  mov	  [BP].un_reg,SI
	  mov	  BX,[SI].C_page
	  cmp	  byte ptr ptype+[BX],BIGTYPE*2
	  je	  un_maybe

;     This hash-key is invalid, or object not found-- return #!false
un_false: xor	  AX,AX		   ; create a nil pointer
	  mov	  SI,[BP].un_reg   ; load destination register address
	  mov	  byte ptr [SI].C_page,AL ; store nil pointer into
	  mov	  [SI].C_disp,AX   ;  destination register

;     Return to Scheme Interpreter
un_ret:	  add	  SP,offset un_BP  ; deallocate local storage
	  pop	  BP		   ; restore caller's BP register
	  pop	  SI		   ; restore the location pointer
	  pop	  ES		   ; restore caller's ES register
	  ret

;     Continue checking bignum value
un_maybe: mov	  SI,[SI].C_disp   ; load bignum's offet
	  %LoadPage ES,BX	   ;  and paragraph address
;;;	  mov	  ES,pagetabl+[BX] ;  and paragraph address
	  cmp	  ES:[SI].big_sign,0
	  jne	  un_false	   ; if negative, not one of ours
	  cmp	  ES:[SI].big_len,8
	  jne	  un_false	   ; if more than four bytes of data, not ours
	  mov	  DI,ES:[SI].big_data ; load least significant word (bucket no.)
	  cmp	  DI,OHT_SIZE
	  jae	  un_false	   ; hash bucket index too large?  if so, jump
	  mov	  DX,DI		   ; DX <- bucket number
	  mov	  AX,ES:[SI].big_2nd
	  shl	  DI,1		   ; DI <- bucket number * 2
	  cmp	  AX,obj_cntr+[DI] ; test against next available counter value
	  jae	  un_false	   ; hash index too large? if so, jump
;     Note:  Search index (key) is in AX
	  add	  DI,DX		   ; DI <- bucket number * 3
	  add	  DI,offset obj_ht
	  mov	  DX,DS		   ; ES <- DS 
	  mov	  ES,DX
;     Note:  Search list whose header is in ES:[DI]
	  call	  oh_search	   ; search "ES:[DI]" for "AX"
	  cmp	  BL,0		   ; was index found?
	  je	  un_false	   ; if not found, return #!false (jump)
;     Search successful-- object/hash-value pair pointed to by ES:[SI]
	  mov	  DI,[BP].un_reg   ; load destination register's address
	  mov	  AX,ES:[SI].car   ; copy car field of found pair into
	  mov	  [DI].C_disp,AX   ;  the destination register
	  mov	  AL,ES:[SI].car_page
	  mov	  byte ptr [DI].C_page,AL
	  jmp	  un_ret	   ; return to caller w/ object in dest reg
%objunhs  endp

;************************************************************************
;*		    Local Support for Object Unhash			*
;************************************************************************
oh_search proc	  near
;     Compute pointer to current entry and save it
	  mov	  BL,ES:[DI].car_page
	  cmp	  BL,0
	  je	  oh_sret
	  mov	  DI,ES:[DI].car
	  %LoadPage ES,BX
;;;	  mov	  ES,pagetabl+[BX]
	  mov	  DX,ES		   ; save ES in DX
;     Compute pointer to object/hash-key pair
	  mov	  BL,ES:[DI].car_page
	  mov	  SI,ES:[DI].car
	  %LoadPage ES,BX
;;;	  mov	  ES,pagetabl+[BX]
;     Test cdr field (hash key) of pair for match
	  cmp	  ES:[SI].cdr,AX
	  jne	  oh_smore
;     A match!-- Return pair address in ES:[SI]
oh_sret:  ret

oh_smore: mov	  ES,DX		   ; restore ES
	  add	  DI,PTRSIZE	   ; adjust pointer to cdr field of curr entry
	  jmp	  oh_search	   ; iterate
oh_search endp
		    
;************************************************************************
;*		   Object Hash Table Garbage Collection			*
;************************************************************************
gc_args	  struc
prev_ES	  dw	  ?		   ; ES for previous entry
prev_off  dw	  ?		   ; offset for previous entry
curr_PG	  dw	  ?		   ; ES for current entry
curr_off  dw	  ?		   ; offset for current entry
pair_PG	  dw	  ?		   ; ES for object/hash-key pair
pair_off  dw	  ?		   ; offset for object/hash-key pair
gc_BP	  dw	  ?		   ; caller's BP
	  dw	  ?		   ; caller's ES
	  dd	  ?		   ; return address (far call)
	  dw	  ?		   ; return address (near call)
gc_args	  ends

%gc_oht	  proc	  far
	  push	  ES		   ; save caller's ES register
	  push	  BP		   ; save caller's BP register
	  sub	  SP,offset gc_BP  ; allocate local storage
	  mov	  BP,SP		   ; establish addressibility for local storage

;     Initialize parameters
	  mov	  SI,offset obj_ht ; load address of object hash table
	  mov	  CX,OHT_SIZE	   ; load number of entries in obj hash table
gc_loop:  mov	  AX,DS		   ; ES <- DS
	  mov	  ES,AX
	  push	  SI		   ; load current object hash table offset
	  push	  CX		   ; save iteration counter
	  call	  gc_nxt	   ; follow this entries chain
	  pop	  CX		   ; restore iteration counter
	  pop	  SI		   ; restore obj hash table offset
	  add	  SI,PTRSIZE	   ; advance offset pointer
	  loop	  gc_loop	   ; continue 'til all buckets processed

;     Return to caller
gc_xit:	  add	  SP,offset gc_BP  ; release local storage
	  pop	  BP		   ; restore the caller's BP register
	  pop	  ES		   ; restore the caller's ES register
	  ret			   ; return
%gc_oht	  endp

;************************************************************************
;*	  Local Support for Object Hash Table Garbage Collection	*
;************************************************************************
gc_nxt	  proc	  near
	  xor	  BX,BX		   ; clear register BX
	  mov	  BL,ES:[SI].car_page ; load page number for next entry
	  cmp	  BL,0		   ; does entry exist?
	  jne	  ogc_010	   ; if null pointer, jump to exit
	  ret			   ; return to gc_oht
;     save pointer to previous cell
ogc_010:  mov	  [BP].prev_ES,ES
	  mov	  [BP].prev_off,SI
;     compute and save pointer to current cell
	  mov	  DI,ES:[SI].car
	  %LoadPage ES,BX
;;;	  mov	  ES,pagetabl+[BX]
	  mov	  [BP].curr_PG,BX
	  mov	  [BP].curr_off,DI
;     compute and save pointer to object/hash-key pair
	  mov	  BL,ES:[DI].car_page
	  mov	  SI,ES:[DI].car
	  test	  SI,08000h	   ; is current cell marked as referenced?
	  jnz	  ogc_skip	   ; if marked, GC during OBJECT-HASH (jump)
	  %LoadPage ES,BX
;;;	  mov	  ES,pagetabl+[BX]
	  mov	  [BP].pair_PG,BX
	  mov	  [BP].pair_off,SI
;     see what object pointer points to
	  mov	  BL,ES:[SI].car_page
	  cmp	  BL,DEDPAGES*PAGEINCR ; is object a "special" one?
	  jb	  ogc_mark	   ; if a non-gc'ed page, must keep entry
	  mov	  SI,ES:[SI].car   ; load object offset
	  %LoadPage ES,BX
;;;	  mov	  ES,pagetabl+[BX] ; load object's paragraph address
	  mov	  DI,ptype+[BX]	   ; load type code for object
	  jmp	  branchtab+[DI]   ; jump to appropriate routine

;     object is a list cell-- test to see if it's marked
ogc_list: test	  byte ptr ES:[SI].list_gc,GC_BIT
	  jnz	  ogc_mark
	  jmp	  short ogc_del

;     Variable length object
ogc_var:  test byte ptr ES:[SI].vec_gc,GC_BIT
	  jnz	  ogc_mark

;     Object not referenced-- delete object hash table entry for it
ogc_del:  %LoadPage ES,[BP].curr_PG  ; reload pointer to current entry
	  mov	  SI,[BP].curr_off
	  mov	  AX,ES:[SI].cdr   ; load cdr field of current entry
	  mov	  BL,ES:[SI].cdr_page
	  mov	  ES,[BP].prev_ES  ; reload pointer to previous entry
	  mov	  SI,[BP].prev_off
	  mov	  ES:[SI].car,AX   ; store cdr field of current entry into
	  mov	  ES:[SI].car_page,BL ; previous entry
	  jmp	  gc_nxt	   ; process next entry

;     Object is marked as referenced-- mark obj hash table cells as referenced
ogc_mark: %LoadPage ES,[BP].pair_PG  ; load pointer to object/hash-key pair
	  mov	  SI,[BP].pair_off
	  or	  byte ptr ES:[SI].list_gc,GC_BIT ; mark pair entry referenced
ogc_skip: %LoadPage ES,[BP].curr_PG  ; load pointer to current entry
	  mov	  SI,[BP].curr_off
	  or	  byte ptr ES:[SI].list_gc,GC_BIT ; mark curr entry referenced
	  add	  SI,PTRSIZE	   ; advance pointer to cdr field of curr entry
	  jmp	  gc_nxt	   ; process next entry
gc_nxt	  endp

PROGX	  ends

prog	  segment byte public 'PROG'
	  assume  CS:PGROUP
;************************************************************************
;*		    Linkage to Object Hash Routine			*
;************************************************************************
	  public  obj_hash
obj_hash  proc	  near
	  call	  %objhash
	  extrn	  next:near
	  jmp	  next		   ; return to the Scheme interpreter
obj_hash  endp

	  public  obj_unhs
obj_unhs  proc	  near
	  call	  %objunhs
	  jmp	  next		   ; return to the Scheme interpreter
obj_unhs  endp

	  public  gc_oht
gc_oht	  proc	  near
	  call	  %gc_oht
	  ret
gc_oht    endp

prog	  ends
	  end
