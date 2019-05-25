;							=====> SVARS.ASM
;****************************************
;*   TIPC Scheme '84 Runtime Support   	*
;*  Interpreter -- Variable Operations 	*
;*				       	*
;*   (C) Copyright 1984, 1985, 1988	*
;*   Texas Instruments Incorporated.	*
;*	  All rights reserved.	       	*
;*				       	*
;* Date Written:  24 July 1984	       	*
;* Modification History:		*
;*  ?? 10/22/85 - ??			*
;*  rb  2/ 5/88 - MEMV, ASSV use EQV's  *
;*    definition of number equality     *
;*    (which is "=", *not* "equal").	*
;*    					*
;****************************************

	  include scheme.equ
	  include sinterp.mac

	  include sinterp.arg

DGROUP	  group	  data
data	  segment word public 'DATA'
	  assume  DS:DGROUP
m_fluid	  db	  "LD-FLUID",0
m_setfl	  db	  "SET-FLUID!",0
m_set_gl  db	  "SET!-GLOBAL",0
m_fl_p	  db	  "FLUID-BOUND?",0
m_ve_al	  db	  "MAKE-VECTOR",0
m_vec_s   db	  "VECTOR-SIZE",0
m_vecf	  db	  "VECTOR-FILL!",0
m_mkvt_a  dw	  m_ve_al	   ; address of "MAKE-VECTOR"
m_one	  dw	  1		   ; a constant "one" (1)
m_three	  dw	  3		   ; a constant "three" (3)
m_toobig  dw	  VECTOR_SIZE_LIMIT_ERROR ; numeric error code
data	  ends

PGROUP	  group	  prog
prog	  segment byte public 'PROG'
	  assume  CS:PGROUP

var_int	  proc	  near

;     Entry points defined in "sinterp.asm"
	  extrn	  next:near	   ; Top of interpreter
	  extrn	  next_PC:near	   ; Reload ES,SI at top of interpreter
	  extrn	  next_SP:near	   ; mov SP,BP before next_PC
	  extrn	  src_err:near	   ; "source operand error" message display
	  extrn	  sch_err:near	   ; Link to Scheme debugger

;************************************************************************
;* Macro support for global/fluid variable lookup			*
;************************************************************************
load	  macro   environ,err_msg,reg_p
	  local	  x,y
	  lods	  word ptr ES:[SI] ; load dest reg, constant number
	  save	  <SI>		   ; save current location pointer
	  mov	  BL,AL		   ; copy destination register number
	  mov	  DI,BX		   ;  into TIPC register DI
	  mov	  BL,AH		   ; isolate constant number
IFIDN	  <reg_p>,<REG>
	  mov	  SI,reg0_pag+[BX] ; load page number from symbol operand reg
	  mov	  AX,reg0_dis+[BX] ; likewise for the displacement
ELSE
	  mov	  AX,BX		   ; BX <- constant number * 3
	  shl	  AX,1
	  add	  BX,AX
	  add	  BX,CB_dis	   ; add offset for start of code block
	  xor	  AX,AX
	  mov	  AL,ES:[BX].cod_cpag ; load symbol's page number
	  mov	  SI,AX
	  mov	  AX,ES:[BX].cod_cdis ; load symbol's displacement
ENDIF
	  cmp	  byte ptr ptype+[SI],SYMTYPE*2 ; reg hold a symbol pointer?
	  jne	  y		   ; if not, jump to error handler
	  push	  DI		   ; save register number
	  mov	  DX,SI		   ; copy symbol's page number into DX
	  mov	  DI,environ&_pag	   ; load fluid environment pointer
	  mov	  SI,environ&_dis
;;;	  LoadPage ES,DI
;;;	  mov	  ES,pagetabl+[DI] ; load paragraph address for env. header
	  mov	  BX,DI		   ; BX <= page number
	  call	  lookup	   ; search the environment for symbol
	  cmp	  BX,0		   ; symbol found?
	  pop	  BX		   ; restore register number
	  je	  x		   ; if symbol not found, jump
	  mov	  AX,ES:[DI].cdr   ; load symbol's value pointer
	  mov	  reg0_dis+[BX],AX ;  and store into register
	  mov	  AL,ES:[DI].cdr_page
	  mov	  byte ptr reg0_pag+[BX],AL
	  jmp	  next_PC
;     symbol not found-- return '***unbound***
x:	  mov	  CX,offset environ&_reg ; load address of environment reg
	  corrpage DX		   ; adjust page number for call to C routine
	  add	  BX,offset reg0   ; compute address of destintatin register
	  pushm	  <BX,CX,AX,DX>	   ; push page, displacement, env, dest reg
	  C_call  sym_unde,,Load_ES ; call:  symbol_undefined(pg,ds,env,dest)
;***x:	  mov	  reg0_dis+[BX],UN_DISP
;***	  mov	  byte ptr reg0_pag+[BX],UN_PAGE*2
	  restore <SI>		   ; load next instruction's offset and
	  sub	  SI,3		   ;  back up PC to retry fluid load
	  jmp	  sch_err	   ; Link to Scheme debugger
;     error-- register doesn't contain a symbol
y:	  lea	  BX,err_msg
	  jmp	  src_err	   ; display error message
	  endm

;************************************************************************
;*							AL   AH		*
;* Fluid lookup					FLUID	dest,const	*
;*									*
;* Purpose:  Interpreter support for fluid variable lookup		*
;************************************************************************
	  public  ld_fluid
ld_fluid: load	  FNV,m_fluid,CONST

;************************************************************************
;*							AL   AH		*
;* Fluid lookup-register operand		FLUID-R	dest,sym	*
;*									*
;* Purpose:  Interpreter support for fluid variable lookup		*
;************************************************************************
	  public  ld_fl_r
ld_fl_r:  load	  FNV,m_fluid,REG

	  purge   load

;************************************************************************
;*							AL  AH		*
;* set-fluid!				ST-FLUID	src,const	*
;*									*
;* Purpose:  Interpreter support for fluid assignment.			*
;************************************************************************
	  public  st_fluid
st_fluid: lods	  word ptr ES:[SI] ; load source reg and constant number
	  save	  <SI>		   ; save current value of location pointer
	  push	  AX		   ; save symbol/value register numbers
	  mov	  BL,AH
	  mov	  AX,BX		   ; BX <- constant number * 3
	  shl	  AX,1
	  add	  BX,AX
	  add	  BX,CB_dis	   ; add in starting offset of code block
	  xor	  AX,AX
	  mov	  AL,ES:[BX].cod_cpag ; load pointer to search symbol
	  mov	  DI,AX
	  cmp	  byte ptr ptype+[DI],SYMTYPE*2 ; really a symbol?
	  jne	  setfl_er	   ; if not, jump
	  mov	  DX,DI		   ; copy symbol's page number
	  mov	  AX,ES:[BX].cod_cdis ; load symbol's displacement
	  mov	  DI,FNV_pag	   ; load pointer to fluid environment
	  mov	  SI,FNV_dis
;;;	  LoadPage ES,DI
;;;	  mov	  ES,pagetabl+[DI]
	  mov	  BX,DI		   ; Page number
	  call	  lookup	   ; search fluid environment for symbol
	  cmp	  BX,0		   ; symbol found in fluid environment?
	  je	  setfl_nf	   ; if not, error (jump)
	  pop	  AX		   ; restore operands
	  mov	  BL,AL		   ; copy source register number
	  mov	  AL,byte ptr reg0_pag+[BX] ; set cdr of fluid var entry
	  mov	  ES:[DI].cdr_page,AL ; to value in register
	  mov	  AX,reg0_dis+[BX]
	  mov	  ES:[DI].cdr,AX
	  jmp	  next_PC	   ; return to interpreter
;     error-- symbol register doesn't contain a symbol pointer
setfl_er: mov	  BX,offset m_setfl ; load error message text
	  jmp	  src_err	   ; jump to "source error" routine
;     error-- symbol not fluidly bound
setfl_nf: pop	  CX		   ; restore instruction's operands
	  xor	  CH,CH		   ; clear high order byte (constant number)
	  add	  CX,offset reg0   ; compute address of source register
	  corrpage DX		   ; convert page number to C's notation
	  pushm	  <CX,AX,DX>	   ; push arguments for error call
	  C_call  not_flui,,Load_ES ; call error routine
	  restore <SI>		   ; back up location pointer to retry
	  sub	  SI,3		   ;  the set-fluid! operation
	  jmp	  sch_err	   ; link to Scheme debugger

;     fluid-bound?			FLUID?	reg
	  public  fluid_p
fluid_p:  lods	  byte ptr ES:[SI] ; load the register number for test
	  save	  <SI>		   ; save the current location pointer
	  mov	  BX,AX		   ; copy register number of symbol
	  mov	  AX,reg0_dis+[BX]
	  mov	  DX,reg0_pag+[BX]
	  mov	  DI,DX
	  cmp	  byte ptr ptype+[DI],SYMTYPE*2 ; symbol pointer?
	  jne	  fl_p_er	   ; if not, error (jump)
	  mov	  DI,FNV_pag
	  mov	  SI,FNV_dis
;;;	  LoadPage ES,DI
;;;	  mov	  ES,pagetabl+[DI]
	  push	  BX
	  mov	  BX,DI		   ; Page number
	  call	  lookup
	  cmp	  BX,0
	  pop	  BX
	  je	  fl_p_nf
;     symbol is fluidly bound-- return 't
	  mov	  AL,T_PAGE*2
	  mov	  byte ptr reg0_pag+[BX],AL
	  mov	  AX,T_DISP
	  mov	  reg0_dis+[BX],AX
	  jmp	  next_PC
;     symbol not in fluid environment-- return 'nil
fl_p_nf:  xor	  AX,AX
	  mov	  byte ptr reg0_pag+[BX],AL
	  mov	  reg0_dis+[BX],AX
	  jmp	  next_PC
;     error-- operand of (fluid-bound? obj) is not a symbol
fl_p_er:  lea	  BX,m_fl_p
	  jmp	  src_err	   ; display error message

;************************************************************************
;*							AL    AH	*
;* Bind fluid variable				BIND-FL	const,src	*
;*									*
;* Purpose:  Interpreter support for binding (creating and defining)	*
;*		fluid variables						*
;*									*
;* Note:  At entry to this routine, ES is set to point to the beginning *
;*		of the page containing the current code block.		*
;************************************************************************
	  public  bind_fl
bind_fl:  lods	  word ptr ES:[SI] ; load src register, constant number
	  mov	  BL,AH		   ; copy the source register number
	  lea	  DI,reg0+[BX]	   ;  and compute its address
;     tmp_reg <- symbol
	  mov	  BL,AL		   ; BX <- constant number * 3
	  mov	  AX,BX
	  shl	  AX,1
	  add	  BX,AX
	  add	  BX,CB_dis	   ; add displacement of current code block
	  xor	  AX,AX
	  mov	  AL,ES:[BX].cod_cpag ; copy the symbol pointer into the
	  mov	  tmp_page,AX	      ;  temporary register
	  mov	  AX,ES:[BX].cod_cdis
	  mov	  tmp_disp,AX
;     cons(tmp_reg, tmp_reg, value)
	  mov	  AX,offset tmp_reg ; load address of temporary register
	  pushm	  <DI,AX,AX>	   ; push arguments to "cons"
	  C_call  cons,<SI>,Load_ES ; create (cons symbol value)
;     cons(FNV, tmp_reg, FNV)
	  mov	  AX,offset tmp_reg ; load address of temporary register
	  mov	  BX,offset FNV_reg ; load addr of fluid environment register
	  pushm	  <BX,AX,BX>	   ; push arguments to "cons"
	  C_call  cons		   ; create (cons (cons symbol value) FNV)
	  jmp	  next_SP	   ; return to interpreter

;************************************************************************
;* Unbind fluid variable				UNBIND-FL const *
;*									*
;* Purpose:  Interpreter support for unbinding (deleting) fluid		*
;*		variables						*
;*									*
;* Description:  The fluid environment is maintained as an a-list, so	*
;*		dropping fluids consists of cdr-ing down the list for	*
;*		the required number of elements.			*
;************************************************************************
	  public  unbind_f
unbind_f: lods	  byte ptr ES:[SI] ; load the count of fluids to drop
	  mov	  DX,ES		   ; save code block's paragraph address
	  mov	  CX,AX		   ; copy the drop count into CX
	  mov	  BL,byte ptr FNV_pag ; load the fluid environment pointer
	  mov	  DI,FNV_dis
unb_fl:	  LoadPage ES,BX
;;;    	  mov	  ES,pagetabl+[BX] ; load entry's paragraph address
	  mov	  BL,ES:[DI].cdr_page ; load cdr field of entry
	  mov	  DI,ES:[DI].cdr
	  loop	  unb_fl	   ; continue cdr'ing for desired count
	  mov	  byte ptr FNV_pag,BL ; re-define the fluid environment
	  mov	  FNV_dis,DI	   ;     register
	  mov	  ES,DX		   ; restore code block paragraph address
	  jmp	  next		   ; return to interpreter

;************************************************************************
;* Allocate vector				VEC-ALLOCATE	dest	*
;*									*
;* Purpose:  Interpreter support for the allocation of vector data	*
;*		objects.						*
;*									*
;* Note:  Vectors are set to zero after they are allocated to insure	*
;*		that all entries are valid Scheme pointers.  Zeroing a	*
;*		vector effectively sets all the entries to nil.		*
;*		If an array were not initialized, the garbage collector	*
;*		would interpret any leftover data as pointers, and	*
;*		might cause the Scheme Virtual Machine to go off the	*
;*		deep end.						*
;************************************************************************
	  public  vec_allo
vec_allo: lods	  byte ptr ES:[SI] ; load destination register number
	  save	  <SI>		   ; save the location pointer
	  mov	  BX,AX		   ;  and copy it to TIPC register BX
	  add	  BX,offset reg0
	  cmp	  byte ptr [BX].C_page,SPECFIX*2 ; is size a fixnum?
	  jne	  ve_al_er	   ; if not, error (jump)
	  mov	  AX,[BX].C_disp   ; load immediate value from register
	  shl	  AX,1		   ;  and sign extend it
	  sar	  AX,1
	  cmp	  AX,0		   ; value positive?
	  jl	  ve_al_er	   ; if not, error (jump)
	  cmp	  AX,10921	   ; check against maximum vector size
	  ja	  v_toobig	   ; if too many elements, error (jump)
	  mov	  CX,AX		   ; AX <- AX * 3 (multiply number of
	  shl	  AX,1		   ;  elements by size of pointer)
	  add	  AX,CX
	  mov	  CX,VECTTYPE	   ; load type of block to allocate
	  pushm	  <AX,CX,BX>	   ; push arguments
	  C_call  alloc_bl,,Load_ES ; call:  alloc_block(&reg, type, size)
	  pop	  BX		   ; recover address of reg holding vector ptr
	  mov	  AX,[BX].C_page   ; fetch page number from destination reg
	  corrpage AX		   ; correct for C callable routine
	  pushm	  <[BX].C_disp,AX> ; push page and displacement
	  C_call  zero_blk	   ; call:  zero_blk(page, disp)
	  jmp	  next_SP	   ; return to interpreter
;     ***Error-- invalid source operand for vec-alloc***
ve_al_er: mov	  SI,[BX].C_page   ; load operand's page number
	  cmp	  byte ptr ptype+[SI],BIGTYPE*2 ; is it a bignum?
	  je	  v_toobig	   ; if so, print "vector too big" message
	  lea	  BX,m_ve_al	   ; otherwise, print "source operand"
	  jmp	  src_err	   ;  error message
;     ***Error-- vector too large***
v_toobig: restore <SI>
	  sub	  SI,2
	  pushm	  <SI,m_mkvt_a>
	  C_call  disassem,,Load_ES
	  pushm	  <tmp_adr,m_toobig,m_one>
	  C_call  set_nume
	  jmp	  sch_err


;************************************************************************
;* Vector size					VECTOR-SIZE	dest	*
;*									*
;* Purpose:  Interpreter support for the vector-size function to return *
;*		the number of elements in a vector data object.		*
;*									*
;* Description:  The number of elements in a vector data object is	*
;*		determined by dividing the number of bytes (obtained	*
;*		from the block header of the vector data object) by the *
;*		number of bytes in a pointer (3), and subtracting the	*
;*		overhead of the block header (3 bytes).			*
;************************************************************************
	  public  vec_size
vec_size: lods	  byte ptr ES:[SI] ; load destination register number
	  mov	  BX,AX		   ;  and copy into TIPC register BX
	  save	  <SI>		   ; save the location pointer
	  mov	  SI,reg0_pag+[BX] ; load page number field of register
	  cmp	  ptype+[SI],VECTTYPE*2 ; is object a vector?
	  jne	  vec_s_er	   ; if not, error (jump)
	  mov	  DI,reg0_dis+[BX] ; load displacement of vector
	  LoadPage ES,SI
;;;	  mov	  ES,pagetabl+[SI] ; load vector's page paragraph address
	  mov	  AX,ES:[DI].vec_len ; load size of object (in bytes),
	  xor	  DX,DX		   ;  extend to double word,
	  mov	  CX,3		   ;  load divisor of three,
	  idiv	  CX		   ; divide no. bytes by pointer size
	  dec	  AX		   ; subtract off block overhead
	  mov	  reg0_dis+[BX],AX ; store number of elements
	  mov	  byte ptr reg0_pag+[BX],SPECFIX*2 ; set tag=fixnum
	  jmp	  next_PC	   ; return to interpreter
;     ***error-- operand doesn't point to a vector data object***
vec_s_er: lea	  BX,m_vec_s
	  jmp	  src_err	   ; display error message


;************************************************************************
;*								AL   AH	*
;* vector fill					vec-fill	vect,val*
;*									*
;* Purpose:  Scheme intepreter support for the vector-fill operation	*
;************************************************************************
	  public  vec_fill
vec_fill: lods	  word ptr ES:[SI] ; load operands
	  save	  <SI>		   ; save location pointer
	  xor	  BX,BX
	  mov	  BL,AL		   ; copy number of register containing vector
	  mov	  DI,reg0_dis+[BX] ; load vector pointer
	  mov	  BL,byte ptr reg0_pag+[BX] 
	  cmp	  byte ptr ptype+[BX],VECTTYPE*2 ; is it really a vector?
	  jne	  vecf_err	   ; if not, error (jump)
	  LoadPage ES,BX
;;;	  mov	  ES,pagetabl+[BX] ; load page address of vector's page
	  mov	  BL,AH		   ; copy pointer to fill value
	  mov	  AX,reg0_dis+[BX] ; load value to fill array
	  mov	  DL,byte ptr reg0_pag+[BX]
	  mov	  CX,ES:[DI].vec_len ; load vector length (in bytes) and
	  sub	  CX,BLK_OVHD	   ;  subtract off overhead for block header
	  jle	  vecf_fin	   ; if zero length vector, we're done
vecf_lp:  mov	  ES:[DI].vec_page,DL ; store value into current element
	  mov	  ES:[DI].vec_disp,AX ;  of vector
	  add	  DI,PTRSIZE	   ; increment pointer into vector
	  sub	  CX,PTRSIZE	   ; decrement array size
	  jg	  vecf_lp	   ; if more elements to define, loop (jump)
vecf_fin: jmp	  next_PC	   ; return to Scheme interpreter
vecf_err: lea	  BX,m_vecf
	  jmp     src_err

;************************************************************************
;*								AL   AH	*
;* (memq  obj,list)					MEMQ	dest,src*
;*									*
;* Purpose:  Scheme interpreter support for the memq primitive		*
;************************************************************************
;     Support for SHIFT-BREAK-- restart operation
memq_sb:  push	  m_three	   ; indicate instruction length = 3
	  C_call  restart	   ; link to Scheme debugger

	  public  memq
memq:	  lods	  word ptr ES:[SI] ; load operands
	  save	  <SI>		   ; save the current location pointer
	  mov	  BL,AL		   ; compute the destination register
memq_x:	  lea	  DI,reg0+[BX]	   ;  address in TIPC register DI
	  mov	  AL,byte ptr [DI].C_page ; copy search object pointer 
	  mov	  DX,[DI].C_disp   ;  into AL,DX (page, disp, respectively)
	  mov	  BL,AH		   ; copy pointer to search list
	  mov	  SI,reg0_dis+[BX] ; load contents of "list" register
	  mov	  BL,byte ptr reg0_pag+[BX]
	  jmp	  memq_go
memq_nxt: cmp	  byte ptr s_break,0 ; has shift-break been depressed?
	  jne	  memq_sb	   ; if interrupt, jump
	  mov	  BL,ES:[SI].cdr_page ; load cdr field and continue
	  mov	  SI,ES:[SI].cdr   ;  search
memq_go:  cmp	  BL,0		   ; nil pointer?
	  je	  memq_f	   ; if so, return nil (jump)
	  cmp	  byte ptr ptype+[BX],LISTTYPE*2 ; "list" object a list cell?
	  jne	  memq_f	   ; if not, return nil (jump)
	  LoadPage ES,BX
;;;	  mov	  ES,pagetabl+[BX] ; load paragraph address of list cell
	  cmp	  DX,ES:[SI].car   ; does displacement field of car match obj?
	  jne	  memq_nxt	   ; if not, test next element in list (jump)
	  cmp	  AL,ES:[SI].car_page ; does page field of car match obj?
	  jne	  memq_nxt	   ; if not, test next element in list (jump)
;     match found-- return pointer to current list cell
	  mov	  byte ptr [DI].C_page,BL ; set destination register to point
	  mov	  [DI].C_disp,SI   ;  to current list cell
	  jmp	  next_PC	   ; return to interpreter
;     no match-- return 'nil
memq_f:   xor	  AX,AX		   ; put null value into destination register
	  mov	  byte ptr [DI].C_page,AL
	  mov	  [DI].C_disp,AX
	  jmp	  next_PC	   ; return to interpreter


;************************************************************************
;*							AL   AH  	*
;* (memv  key,list)				MEMV   dest,src 	*
;*						       key, list	* 
;*									*
;* Purpose:  Scheme interpreter support for the memv primitive		*
;************************************************************************

memv_sb:  jmp	  memq_sb	   ; shift-break support-- link to debugger

	  public  memv
memv:	  lods	  word ptr ES:[SI] ; load operands
	  save	  <SI>		   ; save the current location pointer
	  mov	  BL,AL		   ; compute the destination register
	  mov	  DI,reg0_pag+[BX] ; load page number of search object
; The following 3 lines are sufficient for MEMV if EQV doesn't require
; an = test for numbers and only checks types instead.  All the remaining
; code for MEMV is to handle =.
;	  test	  attrib+[DI],FLONUMS+BIGNUMS+STRINGS
;	  jz	  memv_x	   ; unless one of above types, use "memq"
;	  jmp	  short memv_y     ; otherwise, use full "member" test
	  test	  attrib[DI],FIXNUMS+FLONUMS+BIGNUMS+STRINGS
	  jz	  memv_x	   ; unless one of above types, use "memq"
	  test	  attrib[DI],FIXNUMS+FLONUMS+BIGNUMS
	  jz	  memv_y	   ; for strings do "member" test
; key is a number
	  lea	  DI,reg0[BX]	   ; DI=address of VM reg containing key
	  mov	  BL,AH
	  lea	  SI,reg0[BX]	   ; SI=address of VM reg containing list
	  push	  [SI].C_page      ; tempsave "list" VM reg
	  push	  [SI].C_disp
	  jmp	  short memv_nxt
memv_x:   jmp	  memq_x	   ; these damn short relative jumps!!
memv_y:   jmp	  member_x
; this list element didn't match, go to the next element
memv_more: cmp	  s_break,0 	   ; shift-break (IBM: control-break) pressed?
	  jne	  memv_sb	   ; yes, do break
	  mov	  BX,[SI].C_disp   ; cdr our way down list
	  mov	  AL,ES:[BX].cdr_page
	  mov	  AH,0
	  mov	  [SI].C_page,AX
	  mov	  AX,ES:[BX].cdr
	  mov	  [SI].C_disp,AX
; loop over each element in the list
memv_nxt: mov	  BX,[SI].C_page
	  cmp	  BX,NIL_PAGE      ; at end of list?
	  je	  memv_f	   ; yes, jump
	  cmp	  byte ptr ptype[BX],LISTTYPE*2       ; looking at a cons?
	  jne	  memv_f	   ; no, jump
	  LoadPage ES,BX	   ; get cons into memory
	  mov	  BX,[SI].C_disp   ; ES:BX=address of cons cell
	  mov	  BL,ES:[BX].car_page
	  mov	  BH,0
	  test	  attrib[BX],FIXNUMS+FLONUMS+BIGNUMS  ; is list elt numeric?
	  jz	  memv_more	   ; no, jump
; key and list element are both numeric
	  mov	  tmp_reg.C_page,BX
	  mov	  BX,[SI].C_disp
	  mov	  BX,ES:[BX].car
	  mov	  tmp_reg.C_disp,BX
	  lea	  BX,tmp_reg
; begin comparison of key and list element
	  cmp	  byte ptr [DI].C_page,SPECFIX*2   ; is key a fixnum?
	  jne	  memv_float	   		   ; no, jump
	  cmp	  byte ptr [BX].C_page,SPECFIX*2   ; is list elt a fixnum?
	  jne	  memv_float	   		   ; no, jump
; both key and list element are fixnums
	  mov	  AX,[BX]	   ; AX=list elt
	  mov	  DX,[DI]	   ; DX=key
	  shl	  AX,1
	  shl	  DX,1
	  cmp	  AX,DX		   ; same number?
	  jne	  memv_more	   ; no, jump
; we have a match, copy list object-pointer to VM register containing key
memv_t:   mov	  AX,[SI].C_disp
	  mov	  [DI].C_disp,AX
	  mov	  AX,[SI].C_page
	  mov	  [DI].C_page,AX
	  jmp	  short memv_f1
; we have no match, copy '() to VM register containing key
memv_f:   xor	  AX,AX
	  mov	  [DI].C_page,AX
	  mov	  [DI].C_disp,AX
memv_f1:  pop	  [SI].C_disp	   ; restore original contents "list" VM reg
	  pop	  [SI].C_page
	  jmp	  next_PC	   ; return to interpreter
; key and list element are not both fixnums, do = operation
memv_float: mov	  AX,EQ_OP
	  pushm   <ES,DI,SI>       ; save our state around C call
	  pushm   <BX,DI,AX>	   ; list elt, key, operation
	  C_call  arith2,,Load_ES  ; do =
	  popm    <SI,SI,SI>	   ; get C args off stack
	  popm    <SI,DI,ES>	   ; restore our state
	  cmp	  AX,0		   ; AX negative means "error"
	  jge	  memv_flo2	   ; nope
	  jmp	  sch_err	   ; yes, go to error handler
memv_flo2: jg	  memv_t	   ; AX positive means "true"
	  jmp	  memv_more	   ; no match, go to next list element


;************************************************************************
;*							 AL   AH	*
;* (member  key,list)				MEMBER	dest,src	*
;*						        key, list	* 
;*									*
;* Purpose:  Scheme interpreter support for the member primitive	*
;************************************************************************
memb_sb:  jmp	  memq_sb	   ; shift-break support-- link to debugger
	  public  member
member:	  lods	  word ptr ES:[SI] ; load operands
	  save	  <SI>		   ; save the current location pointer
	  mov	  BL,AL
	  mov	  DI,reg0_pag+[BX] ; load search object's page number
	  test	  attrib+[DI],FIXNUMS+SYMBOLS+CONTINU+CLOSURE+PORTS+CODE+CHARS
	  jz	  member_x	   ; if not one of these, use "equal?" compare
	  jmp	  memq_x	   ; otherwise, use "memq" test
member_x: lea	  DI,reg0+[BX]	   ;  address in TIPC register DI
	  mov	  CL,byte ptr [DI].C_page ; load pointer to object in CL:DX
	  mov	  DX,[DI].C_disp
	  mov	  BL,CL
	  mov	  CH,byte ptr ptype+[BX] ; load type code of search object
	  mov	  BL,AH		   ; copy pointer to search list
	  mov	  SI,reg0_dis+[BX] ; load contents of "list" register
	  mov	  BL,byte ptr reg0_pag+[BX]
	  jmp	  memb_go
memb_mor: mov	  AX,BX
	  mov	  BL,ES:[SI].car_page
	  cmp	  CH,byte ptr ptype+[BX]
	  jne	  memb_nxt
	  pushm	  <AX,CX,DX,SI>    ; save registers across call
	  xor	  AX,AX
	  mov	  AL,ES:[SI].car_page
	  mov	  [BP].temp_reg.C_page,AX ; temp_reg <- (car list)
	  mov	  AX,ES:[SI].car
	  mov	  [BP].temp_reg.C_disp,AX
	  lea	  BX,[BP].temp_reg ; load address of temporary register
	  pushm   <BX,DI>	   ; push arguments
	  C_call  sequal_p,,Load_ES  ; call: sequal_p(&dest,&src)
	  pop	  DI		   ; retrieve destination register address
	  add	  SP,WORDINCR	   ; dump other arguments from stack
	  popm	  <SI,DX,CX,BX>    ; restore registers
	  LoadPage ES,BX	   ; restore page paragraph address
	  cmp	  AX,0		   ; were values equal?
	  jne	  memb_fnd	   ; if so, jump
memb_nxt: cmp	  s_break,0	   ; has shift-break key been depressed?
	  jne	  memb_sb	   ; if interrupt, jump
	  mov	  BL,ES:[SI].cdr_page ; load cdr field and continue
	  mov	  SI,ES:[SI].cdr   ;  search
memb_go:  cmp	  BL,0		   ; nil pointer?
	  je	  memb_f	   ; if so, return nil (jump)
	  cmp	  byte ptr ptype+[BX],LISTTYPE*2 ; "list" object a list cell?
	  jne	  memb_f	   ; if not, return nil (jump)
	  LoadPage ES,BX
;;;	  mov	  ES,pagetabl+[BX] ; load paragraph address of list cell
	  cmp	  DX,ES:[SI].car   ; does displacement field of car match obj?
	  jne	  memb_mor	   ; if not, test next element in list (jump)
	  cmp	  CL,ES:[SI].car_page ; does page field of car match obj?
	  jne	  memb_mor	   ; if not, test next element in list (jump)
;     "eq" match found-- return pointer to current list cell
memb_fnd: mov	  byte ptr [DI].C_page,BL ; set destination register to point
	  mov	  [DI].C_disp,SI   ;  to current list cell
	  jmp	  next_PC	   ; return to interpreter
;     no match-- return 'nil
memb_f:   xor	  AX,AX		   ; put null value into destination register
	  mov	  byte ptr [DI].C_page,AL
	  mov	  [DI].C_disp,AX
	  jmp	  next_PC	   ; return to interpreter

;************************************************************************
;*								AL  AH	*
;* (assq  obj,list)					ASSQ	obj,list*
;*									*
;* Purpose:  Scheme interpreter support for the assq primitive		*
;************************************************************************
	  public  assq
assq:	  lods	  word ptr ES:[SI] ; load operands
	  save	  <SI>		   ; save the location pointer
assq_go:  mov	  BL,AH		   ; copy the list register number
	  mov	  SI,reg0_pag+[BX]
	  cmp	  ptype+[SI],LISTTYPE*2 ; is second operand a list?
	  jne	  assq_err	   ; if not, error(?) (jump)
	  LoadPage ES,SI
	  mov	  DI,SI		   ; Save page number
;;;	  mov	  ES,pagetabl+[SI] ; load list page's paragraph address
	  mov	  SI,reg0_dis+[BX] ; load pointer to list operand
	  mov	  BL,AL		   ; load object register number
	  mov	  DX,reg0_pag+[BX] ; load pointer to search object
	  mov	  AX,reg0_dis+[BX]
	  push	  BX		   ; save destination register number
	  mov	  BX,DI		   ; Pass the page number
	  call	  lookup	   ; search list for eq? comparison of obj
	  pop	  SI		   ; restore destination register number
	  mov	  byte ptr reg0_pag+[SI],BL ; store result of search in
	  mov	  reg0_dis+[SI],DI ;  the destination register
	  jmp	  next_PC	   ; return to interpreter
;     ***second operand is not a list-- return nil***
assq_err: mov	  BL,AL		   ; copy destination register number
	  xor	  AX,AX
	  mov	  byte ptr reg0_pag+[BX],AL ; store nil into destination
	  mov	  reg0_dis+[BX],AX ;  register
	  jmp	  next_PC	   ; return to interpreter

;************************************************************************
;*							 AL  AH		*
;* (assv  key,alist)				ASSV	key,alist	*
;*									*
;* Purpose:  Scheme interpreter support for the assv primitive		*
;************************************************************************

assv_sb:  jmp	  memq_sb	   ; shift-break support-- link to debugger

	  public  assv
assv:	  lods	  word ptr ES:[SI] ; load operands
	  save	  <SI>		   ; save the location pointer
	  mov	  BL,AL		   ; get number of VM register containing key
	  mov	  DI,reg0_pag+[BX] ; load key's page number
; The following 3 lines are sufficient for ASSV if EQV doesn't require
; an = test for numbers and only checks types instead.  All the remaining
; code for ASSV is to handle =.
;	  test	  attrib+[SI],FLONUMS+BIGNUMS+STRINGS ; one of these?
;	  jz	  assq_go	   ; if not one of above, use assq (jump)
;	  jmp	  short assoc_go   ; if one of the above, use assoc
	  test	  attrib[DI],FIXNUMS+FLONUMS+BIGNUMS+STRINGS
	  jz	  assv_x	   ; unless one of above types, use "assq"
	  test	  attrib[DI],FIXNUMS+FLONUMS+BIGNUMS
	  jz	  assv_y	   ; for strings do "assoc" test
; key is a number
	  lea	  DI,reg0[BX]	   ; DI=address of VM reg containing key
	  mov	  BL,AH
	  lea	  SI,reg0[BX]	   ; SI=address of VM reg containing list
	  push	  [SI].C_page      ; tempsave "alist" VM reg
	  push	  [SI].C_disp
	  jmp	  short assv_nxt
assv_x:   jmp	  assq_go	   ; these damn short relative jumps!!
assv_y:   jmp	  assoc_go
; this list element didn't match, go to the next element
assv_more: cmp	  s_break,0 	   ; shift-break (IBM: control-break) pressed?
	  jne	  assv_sb	   ; yes, do break
	  mov	  BX,[SI].C_page
	  LoadPage ES,BX	   ; get toplevel cons back into memory
	  mov	  BX,[SI].C_disp   ; ES:BX=address of toplevel cons cell
	  mov	  AL,ES:[BX].cdr_page  ; cdr down the alist
	  mov	  AH,0
	  mov	  [SI].C_page,AX
	  mov	  AX,ES:[BX].cdr
	  mov	  [SI].C_disp,AX
; loop over each element in the list
assv_nxt: mov	  BX,[SI].C_page
	  cmp	  BX,NIL_PAGE      ; at end of list?
	  je	  assv_f	   ; yes, jump
	  cmp	  byte ptr ptype[BX],LISTTYPE*2       ; looking at a cons?
	  jne	  assv_f	   ; no, jump
	  LoadPage ES,BX	   ; get toplevel cons into memory
	  mov	  BX,[SI].C_disp   ; ES:BX=address of toplevel cons cell
	  push	  BX		   ; tempsave it
	  mov	  BL,ES:[BX].car_page
	  mov	  BH,0
	  cmp	  byte ptr ptype[BX],LISTTYPE*2  ; is car of toplevel cons also a cons?
	  je	  assv_down	   ; yes, jump
assv_pop: pop	  BX		   ; normalize stack
assv_more1: jmp	  assv_more	   ; look at next toplevel cons
assv_down: mov	  DX,BX
	  pop	  BX		   ; (ES:BX=address of toplevel cons again)
	  mov	  BX,ES:[BX].car   ; DX:BX=object ptr to 2nd level cons
	  LoadPage ES,DX	   ; ES:BX=address of 2nd level cons cell
	  push	  BX		   ; tempsave it
	  mov	  BL,ES:[BX].car_page
	  mov	  BH,0
	  test	  attrib[BX],FIXNUMS+FLONUMS+BIGNUMS ; is its car numeric?
	  jz	  assv_pop	   ; no, jump
	  mov	  tmp_reg.C_page,BX ; yes, move car ptr into tmp_reg
	  pop	  BX		   ; (ES:BX=address of 2nd level cons again)
	  mov	  BX,ES:[BX].car
	  mov	  tmp_reg.C_disp,BX
	  lea	  BX,tmp_reg	   ; BX=address of tmp_reg
; begin comparison of key and list element
	  cmp	  byte ptr [DI].C_page,SPECFIX*2   ; is key a fixnum?
	  jne	  assv_float	   		   ; no, jump
	  cmp	  byte ptr [BX].C_page,SPECFIX*2   ; is list elt a fixnum?
	  jne	  assv_float	   		   ; no, jump
; both key and list element are fixnums
	  mov	  AX,[BX]	   ; AX=list elt
	  mov	  DX,[DI]	   ; DX=key
	  shl	  AX,1
	  shl	  DX,1
	  cmp	  AX,DX		   ; same number?
	  jne	  assv_more1	   ; no, jump
	  jmp	  short assv_t
; we have no match, copy '() to VM register containing key
assv_f:   xor	  AX,AX
	  mov	  [DI].C_page,AX
	  mov	  [DI].C_disp,AX
assv_f1:  pop	  [SI].C_disp	   ; restore original contents "alist" VM reg
	  pop	  [SI].C_page
	  jmp	  next_PC	   ; return to interpreter
; we have a match, copy list object-pointer to VM register containing key
assv_t:   mov	  BX,[SI].C_page
	  LoadPage ES,BX
	  mov	  BX,[SI].C_disp   ; ES:BX=address of toplevel cons
	  mov	  AX,ES:[BX].car   ; move car of this cons to dest. register
	  mov	  [DI].C_disp,AX
	  mov	  AL,ES:[BX].car_page
	  mov	  AH,0
	  mov	  [DI].C_page,AX
	  jmp	  assv_f1	   ; return to interpreter
; key and list element are not both fixnums, do = operation
assv_float: mov	  AX,EQ_OP
	  pushm   <ES,DI,SI>       ; save our state around C call
	  pushm   <BX,DI,AX>	   ; list elt, key, operation
	  C_call  arith2,,Load_ES  ; do =
	  popm    <SI,SI,SI>	   ; get C args off stack
	  popm    <SI,DI,ES>	   ; restore our state
	  cmp	  AX,0		   ; AX negative means "error"
	  jge	  assv_flo2	   ; nope
	  jmp	  sch_err	   ; yes, go to error handler
assv_flo2: jg	  assv_t	   ; AX positive means "true"
	  jmp	  assv_more	   ; no match, go to next list element


;************************************************************************
;*								AL  AH	*
;* (assoc  obj,list)					ASSOC	obj,list*
;*									*
;* Purpose:  Scheme interpreter support for the assoc primitive		*
;*									*
;* Register Usage:  DX - address of destination register		*
;*		 ES:SI - pointer to current list cell			*
;************************************************************************
	  public  assoc
assoc:	  lods	  word ptr ES:[SI] ; load operands
	  save	  <SI>		   ; save the location pointer
	  mov	  BL,AL		   ; copy search object's register number
	  mov	  SI,reg0_pag+[BX] ; load search object's page number
	  test	  attrib+[SI],FIXNUMS+SYMBOLS+CONTINU+CLOSURE+PORTS+CODE+CHARS
	  jz	  assoc_go
	  jmp	  assq_go	   ; if one of the above, use assq (jump)
assoc_go: mov	  DX,BX		   ; copy obj's reg number into TIPC reg DX
	  add	  DX,offset reg0   ; compute address of search obj register
	  mov	  BL,AH		   ; copy list register number
	  mov	  SI,reg0_dis+[BX] ; load displacement pointer of "list"
	  mov	  BL,byte ptr reg0_pag+[BX] ; load page number of "list"
assoc_lp: cmp	  BL,0		   ; end of list? (nil pointer?)
	  je	  assoc_nf	   ; if end of list, jump
	  cmp	  byte ptr ptype+[BX],LISTTYPE*2 ; is list operand a list?
	  jne	  assoc_er	   ; if not, error(?) (jump)
	  LoadPage ES,BX
	  mov	  AX,BX		   ;****** SAVE PAGE *********
;;;	  mov	  ES,pagetabl+[BX] ; load list page's paragraph address
	  mov	  BL,ES:[SI].car_page ; load page number of car
	  cmp	  byte ptr ptype+[BX],LISTTYPE*2 ; does car point to list cell?
	  jne	  assoc_nl	   ; if not a list cell, jump
	  mov	  DI,ES:[SI].car   ; load displacement pointer of car field
	  pushm	  <AX,SI>	   ;****** REALLY SAVE PAGE****
	  LoadPage ES,BX
;;;	  mov	  ES,pagetabl+[BX]
	  xor	  AX,AX
	  mov	  AL,ES:[DI].car_page ; copy car field into tmp_reg
	  mov	  tmp_page,AX
	  mov	  AX,ES:[DI].car
	  mov	  tmp_disp,AX
	  mov	  AX,offset tmp_reg
	  pushm	  <DX,AX>	   ; push arguments to call
	  C_call  sequal_p,,Load_ES ; compare equality of the two pointers
	  add	  SP,WORDINCR	   ; dump tmp_reg address
	  pop	  DX		   ; restore obj/dest register address
	  popm	  <SI,BX>	   ; restore ES,SI registers
	  LoadPage ES,BX	   ;********** Restore Para Address *****
	  cmp	  AX,0		   ; were pointers equal?
	  jne	  assoc_t	   ; if equal, jump
assoc_nl: xor	  BX,BX		   ; clear high order byte of BX
	  mov	  BL,ES:[SI].cdr_page ; follow cdr field
	  mov	  SI,ES:[SI].cdr
	  cmp	  byte ptr s_break,0 ; has the shift-break key been depressed?
	  je	  assoc_lp	   ; if no shift-break, loop
	  jmp	  memq_sb	   ; if interrupt, jump to debugger support
;     pointers "equal"-- return pointer to car field of current list cell
assoc_t:  mov	  DI,DX		   ; copy destination register address to DI
	  mov	  AL,ES:[SI].car_page ; return cdr field of list cell
	  mov	  byte ptr [DI].C_page,AL
	  mov	  AX,ES:[SI].car
	  mov	  [DI].C_disp,AX
	  jmp	  next_PC	   ; return to interpreter
;     end of search, or error detected-- return nil
assoc_er:
assoc_nf: mov	  DI,DX		   ; copy destination register address to DI
	  mov	  byte ptr [DI].C_page,NIL_PAGE*2 ; store nil pointer into
	  mov	  [DI].C_disp,NIL_DISP ;  destination register
	  jmp	  next_PC	   ; return to interpreter

var_int	  endp

;************************************************************************
;*			Lookup Symbol is Assoc List			*
;*									*
;* Purpose:  To search a linked list for a given pointer		*
;*									*
;* Description:	 The list to be searched has the following format:	*
;*									*
;*		+--------+--------+		+--------+-------+	*
;*	    +-->|symbol->|value ->|	    +-->|symbol->|value->|	*
;*	    |	+--------+--------+	    |	+--------+-------+	*
;*	    |				    |				*
;*	+---+----+--------+		+---+----+--------+		*
;*	|   o	 |   o----+----...----->|   o	 |  (nil) |		*
;*	+--------+--------+		+--------+--------+		*
;*									*
;*     The symbol portion of the list entries are compared against the	*
;*     search symbol for an identical match.  When found, a pointer to	*
;*     the matched symbol's symbol-value entry is returned.  If the	*
;*     symbol is not found, a value of nil is returned.			*
;*									*
;* Registers upon entry:  AX - search symbol's displacement		*
;*                        BX - page number of list to search            *
;*			  DL - search symbol's page number		*
;*                        SI - displacement within page number 	 	*
;*			       of list to search			*
;*									*
;* Registers on exit:	  BL - page number of cell whose car is the	*
;*				search symbol, or zero if not found	*
;*			  DI - displacement of list cell found, or nil	*
;*			  ES:[DI] - points to cell found		*
;************************************************************************
	  public  lookup
lookup	  proc	  near
lookloop:
	  mov	  CX,BX	  	   ; Save Page number
	  LoadPage ES,BX	   ; Load Paragraph address of page
	  mov	  BL,ES:[SI].car_page ; load car of next list cell in the list
	  cmp	  byte ptr ptype+[BX],LISTTYPE*2 ; is car a list cell?
	  mov	  DI,ES:[SI].car
	  jne	  look_err	   ; if not a list cell, jump
	  LoadPage ES,BX
;;;	  mov	  ES,pagetabl+[BX] ; load paragraph address of its page
	  cmp	  AX,ES:[DI].car   ; does car's disp match search symbol's?
	  jne	  look_nf	   ; if not, keep searching (jump)
	  cmp	  DL,ES:[DI].car_page ; does car's page match search symbol's?
	  je	  look_fnd	   ; if so, we've got a match (jump)
;     no match-- continue through linked list
look_nf:  mov	  BX,CX		   ; restore page number
	  LoadPage ES,BX	   
	  mov	  BL,ES:[SI].cdr_page ; load the cdr field
	  cmp	  byte ptr ptype+[BX],LISTTYPE*2 ; is cdr another list cell?
	  jne	  look_err	   ; if not, error(?)
	  mov	  SI,ES:[SI].cdr
	  cmp	  BX,0		   ; is cdr nil?
	  jne	  lookloop	   ; if not, branch
	  xor	  DI,DI		   ; make BX:DI nil
look_fnd: ret			   ; return pointer to caller
;
look_err: xor	  BX,BX		   ; create a nil pointer to return
	  xor	  SI,SI
	  ret
lookup	  endp

;************************************************************************
;*		   C-callable Fluid Variable Lookup			*
;*									*
;* Purpose:  To retrieve the fluid binding for a variable.		*
;*									*
;* Calling Sequence:  stat = fluid_lookup(&reg)				*
;*			where &reg - address of the register containing	*
;*					the symbol to be looked up.	*
;*					On exit, "reg" contains the	*
;*					current binding for the symbol,	*
;*					if found.			*
;*			      stat - search status: TRUE=symbol found	*
;*					FALSE=symbol not found		*
;*									*
;* Note:  If the call to "lookup" doesn't find the desired symbol, it	*
;*		will return a nil pointer.  It is correct to always	*
;*		return the cdr of the pointer "lookup" returns, since	*
;*		the cdr of nil is itself nil-- a valid value.		*
;************************************************************************
fl_lk_ar  struc
	  dw	  ?		   ; caller's BP
	  dw	  ?		   ; caller's ES
	  dw	  ?		   ; return address
fl_lk_rg  dw	  ?		   ; register address
fl_lk_ar  ends

	  public  fluid_lo
fluid_lo  proc	  near
	  push	  ES		   ; save caller's ES
	  push	  BP		   ;  and BP
	  mov	  BP,SP
;     load pointer to search symbol in DL:AX
	  mov	  BX,[BP].fl_lk_rg ; load register address
	  mov	  AX,[BX].C_disp
	  mov	  DL,byte ptr [BX].C_page
;     load pointer to search list (fluid environment) in ES:[SI]
	  mov	  BX,FNV_pag
	  mov	  SI,FNV_dis
;;;	  LoadPage ES,BX
;;;	  mov	  ES,pagetabl+[BX]
;     search the fluid environment for the symbol
	  call	  lookup
;     store "cdr" of returned cell into register
	  mov	  SI,[BP].fl_lk_rg
	  mov	  AL,ES:[DI].cdr_page
	  mov	  byte ptr [SI].C_page,AL
	  mov	  AX,ES:[DI].cdr
	  mov	  [SI].C_disp,AX
;      set return code (BX=0 if symbol not found) and return
	  mov	  AX,BX
	  pop	  BP		   ; restore caller's BP
	  pop	  ES		   ;  and ES
	  ret			   ; return to caller
fluid_lo  endp

prog	  ends
	  end
