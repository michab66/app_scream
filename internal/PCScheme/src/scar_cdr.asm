;                                                       =====> SCAR_CDR.ASM
;***************************************
;*   TIPC Scheme '84 Runtime Support   *
;*Interpreter -- Car and Cdr operations*
;*                                     *
;*  (C) Copyright 1984,1985,1986 by    *
;*   Texas Instruments Incorporated.   *
;*        All rights reserved.         *
;*                                     *
;* Date Written:  11 September 1984    *
;* Last Modification:  26 February 1986*
;***************************************
          include scheme.equ

; Modification History:
;   26 Feb 86 - modified the "CONS" support to attempt a "short circuit"
;    (JCJ)      allocation of a list cell, instead of calling the
;               "alloc_list_cell" support unconditionally.

          include sinterp.mac
          include sinterp.arg

take_car  macro
          cmp     byte ptr ptype+[BX],LISTTYPE*2
          jne     bad_car
          LoadPage ES,BX
;;;       mov     ES,pagetabl+[BX]
          mov     BL,ES:[SI].car_page
          mov     SI,ES:[SI].car
          endm

take_cdr  macro
          cmp     byte ptr ptype+[BX],LISTTYPE*2
          jne     bad_cdr
          LoadPage ES,BX
;;;       mov     ES,pagetabl+[BX]
          mov     BL,ES:[SI].cdr_page
          mov     SI,ES:[SI].cdr
          endm

;     load arguments for cxr
load_arg  macro
          lods    word ptr ES:[SI] ; fetch source/destination register numbers
          save    <SI>             ; save the location pointer
          mov     BL,AH            ; copy the source register number
          mov     SI,reg0_dis+[BX] ; load contents of the source register
          mov     BL,byte ptr reg0_pag+[BX]
          endm

car_cdr2  macro   arg1,arg2
          mov     CX,offset PGROUP:arg1&_last
          mov     DI,offset PGROUP:arg2&_CX
          jmp     load_ops
          endm

car_cdr3  macro   arg1,arg2,arg3
          mov     DX,offset PGROUP:arg1&_last
          mov     CX,offset PGROUP:arg2&_DX
          mov     DI,offset PGROUP:arg3&_CX
          jmp     load_ops
          endm

DGROUP    group   data
data      segment word public 'DATA'
          assume  DS:DGROUP
m_car     db      "CAR",0
m_cdr     db      "CDR",0
m_caar    db      "CAAR",0
m_cadr    db      "CADR",0
m_cdar    db      "CDAR",0
m_cddr    db      "CDDR",0
m_caaar   db      "CAAAR",0
m_caadr   db      "CAADR",0
m_cadar   db      "CADAR",0
m_caddr   db      "CADDR",0
m_cdaar   db      "CDAAR",0
m_cdadr   db      "CDADR",0
m_cddar   db      "CDDAR",0
m_cdddr   db      "CDDDR",0
m_cadddr  db      "CADDDR",0
m_%car    db      "%CAR",0
m_%cdr    db      "%CDR",0

m_table   dw      m_car,m_cdr,m_caar,m_cadr,m_cdar,m_cddr,m_caaar,m_caadr
          dw      m_cadar,m_caddr,m_cdaar,m_cdadr,m_cddar,m_cdddr,m_cadddr

m_setcar  db      "SET-CAR!",0
m_setcdr  db      "SET-CDR!",0
m_apendb  db      "APPEND!",0
m_ltail   db	  "LIST_TAIL",0
m_one     dw      1                ; a constant "one" (1)
m_two     dw      2                ; a constant "two" (2)
m_three   dw      3                ; a constant "three" (3)
data      ends

PGROUP    group   prog
prog      segment byte public 'PROG'
          assume  CS:PGROUP

car_cdr   proc    near

;     Entry points defined in "sinterp.asm"
          extrn   next:near        ; Top of interpreter
          extrn   next_PC:near     ; Reload ES,SI at top of interpreter
          extrn   next_SP:near     ; Reload SP,ES,SI at top of interpreter
          extrn   src_err:near     ; "source operand error" message display
          extrn   sch_err:near     ; "source operand error" message display
          extrn   printf_c:near    ; Error message print routine

;************************************************************************
;* %car                                                 %CAR    DEST    *
;*                                                                      *
;* Purpose:  To obtain the first element of a list.  This support is    *
;*              similar to the usual "car" operation except that %car   *
;*              returns #!unassigned if one tries to take the car of    *
;*              nil.                                                    *
;************************************************************************
          public  ld_car1
ld_car1:  lods    byte ptr ES:[SI] ; load operand
          save    <SI>             ; save the location pointer
          mov     BX,AX            ; copy operand register number to BX
          mov     SI,reg0_dis+[BX] ; load the source operand
          mov     BL,byte ptr reg0_pag+[BX]
          cmp     byte ptr ptype+[BX],LISTTYPE*2
          jne     bad_car1         ; if not a list cell, error (jump)
          cmp     BL,0             ; is source operand nil?
          jne     car_last         ; if not nil, jump
cxr_undf: mov     BX,AX            ; reload dest register number
          mov     byte ptr reg0_pag+[BX],UN_PAGE*2 ; set destination reg
          mov     reg0_dis+[BX],UN_DISP ;  to #!unassigned
          jmp     next_PC
bad_car1: mov     AX,offset m_%car
          jmp     bad_one

;************************************************************************
;* %cdr                                                 %CDR    DEST    *
;*                                                                      *
;* Purpose:  To obtain the rest of a list.  This support is similar     *
;*              to the usual "cdr" operation except that %cdr returns   *
;*              #!unassigned if one tries to take the cdr of nil.       *
;************************************************************************
          public  ld_cdr1
ld_cdr1:  lods    byte ptr ES:[SI] ; load operand
          save    <SI>             ; save the location pointer
          mov     BX,AX            ; copy operand register number to BX
          mov     SI,reg0_dis+[BX] ; load the source operand
          mov     BL,byte ptr reg0_pag+[BX]
          cmp     BL,0             ; is source operand nil?
          je      cxr_undf         ; if nil, return #!unassigned (jump)
          cmp     byte ptr ptype+[BX],LISTTYPE*2
          je      cdr_last         ; if a list cell, continue processing (jump)
          jmp     bad_cdr1         ; if not a list cell, error (jump)
bad_cdr1: mov     AX,offset m_%cdr
          jmp     bad_one

;************************************************************************
;*                                                      AL   AH         *
;* Take "car" of a list cell            LD_CAR          dest,src        *
;************************************************************************
          public  ld_car
ld_car:   load_arg
car_last: cmp     byte ptr ptype+[BX],LISTTYPE*2
          jne     bad_car          ; if not a list cell, error (jump)
          LoadPage ES,BX
;;;       mov     ES,pagetabl+[BX] ; load para addr of page containing cell
          mov     BL,AL            ; copy destination register number
          mov     AL,ES:[SI].car_page ; copy contents of car field into
          mov     byte ptr reg0_pag+[BX],AL ;  the destination register
          mov     AX,ES:[SI].car
          mov     reg0_dis+[BX],AX
          jmp     next_PC          ; return to the interpreter

car_CX:   take_car
          jmp     CX

car_DX:   take_car
          jmp     DX

;************************************************************************
;*                                                      AL   AH         *
;* Take "cdr" of a list cell            LD_CDR          dest,src        *
;************************************************************************
          public  ld_cdr
ld_cdr:   load_arg
cdr_last: cmp     byte ptr ptype+[BX],LISTTYPE*2
          jne     bad_cdr          ; if not a list cell, error (jump)
          LoadPage ES,BX
;;;       mov     ES,pagetabl+[BX] ; load para addr of page containing cell
          mov     BL,AL            ; copy destination register number
          mov     AL,ES:[SI].cdr_page ; copy contents of cdr field into
          mov     byte ptr reg0_pag+[BX],AL ;  the destination register
          mov     AX,ES:[SI].cdr
          mov     reg0_dis+[BX],AX
          jmp     next_PC          ; return to the interpreter


;     ***Error-- attempt to take "car" of non- list cell***
bad_car:
;     ***Error-- attempt to take "cdr" of non- list cell***
bad_cdr:  les     SI,dword ptr [BP].save_SI ; load next instruction's address
          xor     BX,BX            ; load opcode of failing instruction
          mov     BL,ES:[SI]-3
          shl     BX,1
          mov     AX,m_table+[BX]-128
bad_one:  les     SI,dword ptr [BP].save_SI ; load next instruction's address
          xor     BX,BX
          mov     BL,ES:[SI]-1     ; load register used as last operand
          add     BX,offset reg0
          pushm   <BX,m_one,AX>
          C_call  set_src_,,Load_ES
          jmp     sch_err          ; display error message

cdr_CX:   take_cdr
          jmp     CX

cdr_DX:   take_cdr
          jmp     DX

;************************************************************************
;*                                                      AL   AH         *
;* Take "cadddr" of a list cell         LD_CADDDR       dest,src        *
;************************************************************************
          public  ld_caddd
ld_caddd: load_arg
          take_cdr
          mov     DX,offset PGROUP:car_last
          mov     CX,offset PGROUP:cdr_DX
          jmp     cdr_CX

load_ops: load_arg
          jmp     DI

;************************************************************************
;*                                                      AL   AH         *
;* Take "caar" of a list cell           LD_CAAR         dest,src        *
;************************************************************************
          public  ld_caar
ld_caar:  car_cdr2 car,car

;************************************************************************
;*                                                      AL   AH         *
;* Take "cadr" of a list cell           LD_CADR         dest,src        *
;************************************************************************
          public  ld_cadr
ld_cadr:  car_cdr2 car,cdr

;************************************************************************
;*                                                      AL   AH         *
;* Take "cdar" of a list cell           LD_CDAR         dest,src        *
;************************************************************************
          public  ld_cdar
ld_cdar:  car_cdr2 cdr,car

;************************************************************************
;*                                                      AL   AH         *
;* Take "cddr" of a list cell           LD_CDDR         dest,src        *
;************************************************************************
          public  ld_cddr
ld_cddr:  car_cdr2 cdr,cdr

;************************************************************************
;*                                                      AL   AH         *
;* Take "caaar" of a list cell          LD_CAAAR        dest,src        *
;************************************************************************
          public  ld_caaar
ld_caaar: car_cdr3 car,car,car

;************************************************************************
;*                                                      AL   AH         *
;* Take "caadr" of a list cell          LD_CAADR        dest,src        *
;************************************************************************
          public  ld_caadr
ld_caadr: car_cdr3 car,car,cdr

;************************************************************************
;*                                                      AL   AH         *
;* Take "cadar" of a list cell          LD_CADAR        dest,src        *
;************************************************************************
          public  ld_cadar
ld_cadar: car_cdr3 car,cdr,car

;************************************************************************
;*                                                      AL   AH         *
;* Take "caddr" of a list cell          LD_CADDR        dest,src        *
;************************************************************************
          public  ld_caddr
ld_caddr: car_cdr3 car,cdr,cdr

;************************************************************************
;*                                                      AL   AH         *
;* Take "cdaar" of a list cell          LD_CDAAR        dest,src        *
;************************************************************************
          public  ld_cdaar
ld_cdaar: car_cdr3 cdr,car,car

;************************************************************************
;*                                                      AL   AH         *
;* Take "cdadr" of a list cell          LD_CDADR        dest,src        *
;************************************************************************
          public  ld_cdadr
ld_cdadr: car_cdr3 cdr,car,cdr

;************************************************************************
;*                                                      AL   AH         *
;* Take "cddar" of a list cell          LD_CDDAR        dest,src        *
;************************************************************************
          public  ld_cddar
ld_cddar: car_cdr3 cdr,cdr,car

;************************************************************************
;*                                                      AL   AH         *
;* Take "cdddr" of a list cell          LD_CDDDR        dest,src        *
;************************************************************************
          public  ld_cdddr
ld_cdddr: car_cdr3 cdr,cdr,cdr

;************************************************************************
;*                 Macro support for set-car!/set-cdr!                  *
;************************************************************************
set_cc    macro   field
          local   x
          lods    word ptr ES:[SI] ; load register numbers
          mov     DX,ES            ; save TIPC register ES
          mov     BL,AL
          mov     DI,reg0_pag+[BX] ; load dest register page number
          cmp     DI,0             ; are we trying to set car/cdr of nil?
          je      x                ; if (set-cxr nil v), error (jump)
          cmp     byte ptr ptype+[DI],LISTTYPE*2 ; Is destination a list cell?
          jne     x                ; If not, set_field! not defined
          LoadPage ES,DI
;;;       mov     ES,pagetabl+[DI] ; Load paragraph addr for dest page
          mov     DI,reg0_dis+[BX] ; Load destination displacement
          mov     BL,AH            ; Copy src register number
          mov     AL,byte ptr reg0_pag+[BX] ; redefine field's page number
          mov     ES:[DI].&field&_page,AL
          mov     AX,reg0_dis+[BX]  ; redefine field's displacement
          mov     ES:[DI].&field,AX
          mov     ES,DX             ; reload ES segment register
          jmp     next
x:        mov     BX,offset m_set&field ; load address of message text
IFIDN     <&field>,<car>
bad_stcr: mov     ES,DX
bad_st1:  xor     AX,AX
          mov     AL,ES:[SI]-1
          add     AX,offset reg0
          push    AX
          xor     AX,AX
          mov     AL,ES:[SI]-2
          add     AX,offset reg0
          pushm   <AX,m_two,BX>
          C_call  set_src_,<SI>,Load_ES
          restore <SI>
          jmp     sch_err
ELSE
          jmp     bad_stcr
ENDIF
          endm


;************************************************************************
;*                                                          AL   AH     *
;* Side effect car field  (set-car! dest src)   SET-CAR!    dest,src    *
;*                                                                      *
;* Purpose:  Interpreter support for the set-car! operation.            *
;************************************************************************
          public  set_car
set_car:  set_cc  car

;************************************************************************
;*                                                          AL   AH     *
;* Side effect cdr field  (set-cdr! dest src)   SET-CDR!    dest,src    *
;*                                                                      *
;* Purpose:  Interpreter support for the set-cdr! operation.            *
;************************************************************************
          public  set_cdr
set_cdr:  set_cc  cdr

          purge   set_cc

;************************************************************************
;*                                                      DL   DH  AL     *
;* Cons - Create and define new list cell       CONS    dest,car,cdr    *
;*                                                                      *
;* Purpose:  Interpreter support for the Scheme "cons" operation.       *
;************************************************************************
          public  s_cons
s_cons:   lods    word ptr ES:[SI] ; load destination/car register numbers
          mov     DX,AX            ;  and save in DX
          xor     AX,AX
          lods    byte ptr ES:[SI] ; load cdr register number
          save    <SI>             ; save the location pointer
;     Attempt a "short circuit" list cell allocation
          mov     DI,listpage
;;;       cmp     DI,END_LIST
;;;       je      cons_no
          shl     DI,1
          mov     SI,nextcell+[DI]
          cmp     SI,END_LIST
          je      cons_no
          LoadPage ES,DI
;;;       mov     ES,pagetabl+[DI] ; load list cell page's segment address
          mov     CX,ES:[SI].car
          mov     nextcell+[DI],CX
;     Move contents of CDR register to CDR field of new list cell
cons_ok:  mov     BX,AX            ; copy register number to BX
          mov     AL,byte ptr reg0_pag+[BX]
          mov     ES:[SI].cdr_page,AL
          mov     AX,reg0_dis+[BX]
          mov     ES:[SI].cdr,AX
;     Move contents of CAR register to CAR field of new list cell
          mov     BL,DH            ; copy CAR register number to BX
          mov     AL,byte ptr reg0_pag+[BX]
          mov     ES:[SI].car_page,AL
          mov     AX,reg0_dis+[BX]
          mov     ES:[SI].car,AX
;     Update destination register number with pointer to new list cell
          mov     BL,DL
          mov     reg0_pag+[BX],DI
          mov     reg0_dis+[BX],SI
          jmp     next_SP

;     "short circuit" list cell allocation failed-- go through channels
cons_no:  push    tmp_adr
          C_call  alloc_li,<AX,DX>,Load_ES
          add     SP,WORDINCR
          restore <AX,DX>
          mov     DI,tmp_page
          mov     SI,tmp_disp
          LoadPage ES,DI
;;;       mov     ES,pagetabl+[DI]
          jmp     cons_ok

;************************************************************************
;* List - Create and define new list cell w/ nil cdr    LIST    dest    *
;*                                                                      *
;* Purpose:  Interpreter support for the Scheme "list" operation.       *
;************************************************************************
          public  s_list
s_list:   lods    byte ptr ES:[SI] ; load destination register number
          mov     BX,offset tmp_reg ; load address of temporary register
          pushm   <AX,BX>          ; push dest reg number, temp_reg address
          C_call  alloc_li,<SI>,Load_ES ; allocate list cell
          add     SP,WORDINCR      ; dump argument from TIPC's stack
          pop     SI               ; restore destination register pointer
          mov     BX,tmp_page      ; load page number of new list cell
          mov     CX,BX
          LoadPage ES,BX
;;;       mov     ES,pagetabl+[BX] ; load list cell's page table address
          mov     DI,tmp_disp      ; load displacement of new list cell
;     copy car field into newly allocated list cell
          mov     AX,reg0_dis+[SI] ; load car's displacement, and
          mov     ES:[DI].car,AX   ;  store into new list cell
          mov     AL,byte ptr reg0_pag+[SI] ; load page number, and
          mov     ES:[DI].car_page,AL ;  store it, too
;     create nil cdr field into newly allocated list cell
          xor     AX,AX
          mov     ES:[DI].cdr,AX
          mov     ES:[DI].cdr_page,AL
;     copy pointer to new list cell into destination register
          mov     byte ptr reg0_pag+[SI],CL
          mov     reg0_dis+[SI],DI
          jmp     next_PC

;************************************************************************
;*                                                      AL   AH         *
;* (list a b)                                   LIST2   dest,src        *
;*                                                                      *
;* Purpose:  Interpreter support for the (list a b) operation.          *
;*                                                                      *
;* Description:  This operation:     (list a b)                         *
;*               is equivalent to:   (cons a (cons b nil))              *
;************************************************************************
          public  list2
list2:    lods    word ptr ES:[SI] ; fetch operands
          mov     BL,AL            ; save the destination register number
          push    BX
          mov     BL,AH            ; copy the source register number
          add     BX,offset reg0   ; compute source register address
          mov     AX,offset nil_reg ; load "nil_reg" address
          mov     CX,offset tmp_reg ; load "tmp_reg" address
          pushm   <AX,BX,CX>       ; push arguments to cons
          C_call  cons,<SI>,Load_ES ; call: cons(tmp_reg,src,nil_reg)
          pop     CX               ; restore tmp_reg address
          add     SP,WORDINCR*2    ; drop arguments from TIPC's stack
          pop     BX               ; restore destination register number
          add     BX,offset reg0   ; compute destination register address
          pushm   <CX,BX,BX>       ; push arguments to cons
          C_call  cons             ; call: cons(dest, dest, tmp_reg)
          jmp     next_SP          ; return to the interpreter

;************************************************************************
;* (append! list obj)                                append!  dest  src *
;*                                                                      *
;* Purpose:  Scheme interpreter support for the append! primitive       *
;************************************************************************
          public  appendb
appendb:  lods    word ptr ES:[SI] ; get args (AL=arg1, AH=arg2)
          save    <SI>             ; save the location pntr
          mov     BL,AL
          lea     DI,reg0+[BX]     ; DI=address of dest reg
          mov     BX,[DI].C_page   ; load list header from dest reg
          cmp     byte ptr ptype+[BX],LISTTYPE*2 ; is arg1 a list?
          jne     short not_list   ; if not, error (jump)
;
          cmp     BL,NIL_PAGE*2    ; is arg1 == nil?
          jne     short find_end   ; if not, continue (jump)
;
          mov     BL,AH            ; else get 2nd arg & return it in dest reg
          lea     SI,reg0+[BX]     ;     SI=address of src reg
          mov     BX,[SI].C_page   ;     Copy src reg to dest reg
          mov     [DI].C_page,BX
          mov     BX,[SI].C_disp
          mov     [DI].C_disp,BX
          jmp     next_PC          ; RETURN
;
find_end label near
          mov     CX,SB_CHECK      ; load shift-break iteration count
          mov     DI,[DI].C_disp
next_cell label near
          LoadPage ES,BX
;;;       mov     ES,pagetabl+[BX] ; load list cell page para address
          mov     BL,ES:[DI].cdr_page ; load list cell's cdr's page
          cmp     BL,NIL_PAGE*2    ; CDR == nil?
          je      short eolist     ; then end-of-list (jump)
          cmp     byte ptr ptype+[BX],LISTTYPE*2 ; still pointing to cons nodes?
          jne     short weird_lst
          mov     DI,ES:[DI].cdr   ; load list cell's cdr's displacement
          loop    next_cell
;     Every one in awhile, check for shift-break
          mov     CX,SB_CHECK      ; reload the shift-break iteration count
          cmp     s_break,0        ; has the shift-break key been depressed?
          je      next_cell        ; if no shift-break, jump
          push    m_three          ; push instruction length = 3
          C_call  restart          ; link to Scheme debugger
;     Note:  control does not return from "restart"
;
weird_lst label near               ; possible error checking here
                                   ; as list was non-nil terminated
eolist    label   near
          mov     BL,AH            ; else get 2nd arg & return it in dest reg
          lea     SI,reg0+[BX]     ; SI=address of src reg
          mov     BX,[SI].C_page   ; Copy src reg to dest reg
                                   ; check page # for src?
          mov     ES:[DI].cdr_page,BL
          mov     BX,[SI].C_disp
          mov     ES:[DI].cdr,bx
          jmp     next_PC          ; return to interpreter

not_list  label near
          mov     BX,offset m_apendb
          jmp     bad_st1

;************************************************************************
;* (list_tail list count)                       l_tail list(dest) count *
;*                                                                      *
;* Purpose:  Scheme interpreter support for the list_tail primitive     *
;************************************************************************

lt_args struc
COUNT	dw	?		; Long integer count of list element
	dw	?
REGSAVE dw	?
BP_SAVE	dw	?		; Saved base pointer
ES_SAVE dw	?		; Saved ES reg
lt_args ends

	public	l_tail
l_tail:	
	lods	word ptr ES:[SI]	; get register operands
	save	<SI>			; save instruction pointer

	push	ES			; save local registers
	push	BP
	sub	SP,offset BP_SAVE	; allocate local storage
	mov	BP,SP

	xor	BH,BH
	mov	BL,AL
	add	BX,offset reg0		; reg holding list ptr
	mov	[BP].REGSAVE,BX		; save for later

	xor	BH,BH
	mov	BL,AH
	add	BX,offset reg0		; get register containing count
	push	BX			;   and push for call
	lea	BX,[BP+COUNT]		; get location for return value
	push	BX			;   and push for call
	mov	DX,DS
	mov	ES,DX			; set ES for C routine
	C_call	int2long		; convert register to long
	mov	SP,BP
	or	ax,ax
	jnz	lt_err			;   jump on error
	mov	ax,[BP].COUNT+2		; get high word of long integer
	or	ax,ax			; if negative
	js	lt_rtn			;  return

	mov	SI,[BP].REGSAVE		; reg holding list ptr
	mov	BX,[SI].C_page		; BX <= page of list
        cmp     byte ptr ptype+[BX],LISTTYPE*2   ; is it a list ?
	jne	lt_err			         ;  no, jump

	mov	AX,BX			; AX <= page of list
	mov	BX,[SI].C_disp		; BX <= disp of list

lt_loop:
	mov	CX,[BP].COUNT+2		; get lsw of long int
	or	CX,[BP].COUNT
	jz	lt_rtn			; jump if long int = zero
	cmp	AX,NIL_PAGE		; end of list?
	je	lt_rtn			;   yes, return
	LoadPage ES,AX			; ES <= page address of list cell
	mov	AL,ES:[BX].cdr_page	; AX <= page # of cdr
	mov	BX,ES:[BX].cdr		; BX <= disp of cdr
	sub	word ptr [BP].COUNT,1   ; decrement count
	sbb	word ptr [BP].COUNT+2,0
	jmp	lt_loop			; and loop
lt_rtn:
	mov	byte ptr [SI].C_page,AL ; save page in reg
	mov	[SI].C_disp,BX		; save disp in reg
	add	SP,BP_SAVE
	pop	BP
	pop	ES
	jmp	next_SP

lt_err:
	add	SP,BP_SAVE
	pop	BP
	pop	ES   			; restore ES register
	restore <SI>			; and instruction pointer
        xor     AX,AX
        mov     AL,ES:[SI]-1
        add     AX,offset reg0		; get last operand
        push    AX			;   and push for call
        xor     AX,AX
        mov     AL,ES:[SI]-2
        add     AX,offset reg0		; get first operand
        push    AX			;   and push for call

        mov     BX,offset m_ltail       ; load address of message text
        pushm   <m_two,BX>		;   and push
        C_call  set_src_,<SI>,Load_ES
        restore <SI>
        jmp     sch_err


car_cdr   endp

prog      ends
          end
