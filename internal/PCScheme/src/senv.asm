;							=====> SENV.ASM
;***************************************
;*   TIPC Scheme '84 Runtime Support   *
;*Interpreter -- Environment Operations*
;*				       *
;*    (C) Copyright 1985 by Texas      *
;*     Instruments Incorporated.       *
;*	  All rights reserved.	       *
;*				       *
;* Date Written:  5 March 1985	       *
;* Last Modification:   2 FEB 1987     *
;***************************************
;
; Modification history
;
; tc 2/10/87  fixed define so that it will define in
;	      to current environment if not already
;	      there.	



	  include scheme.equ
	  include sinterp.mac

	  include sinterp.arg
	  include stackf.equ

DGROUP	  group	  data
data	  segment word public 'DATA'
	  assume  DS:DGROUP
m_ld_en	  db	  "ld-env",0
m_st_en	  db	  "st-env",0
m_def_en  db	  "define-env",0
m_en_par  db	  "environment-parent",0
m_env_lu  db	  "env-lu",0
m_ld_gl	  db	  "ld-global",0
m_defb	  db	  "define!",0
m_st_gl	  db	  "st-global",0
m_setgnv  db	  "set-global-env!",0
;     Note:  the following three (3) definitions are order dependent
lcl_reg	  equ	  $	     	   ; local "register"
lcl_disp  dw	  0
lcl_page  dw	  0
;     End of order dependent definitions
data	  ends

PGROUP	  group	  prog
prog	  segment byte public 'PROG'
	  assume  CS:PGROUP

s_env	  proc	  near

;     Entry points defined in "sinterp.asm"
	  extrn	  next:near	   ; Top of interpreter
	  extrn	  next_PC:near	   ; Reload ES,SI at top of interpreter
	  extrn	  next_SP:near	   ; All of the above, with "mov SP,BP" first
	  extrn	  src_err:near	   ; "source operand error" message display
	  extrn	  printf_c:near	   ; Error message print routine
	  extrn	  not_yet:near	   ; Feature not yet implemented
	  extrn	  sch_err:near	   ; Link to Scheme level debugger

;     Entry point defined in "svars.asm"
	  extrn	  lookup:near

;************************************************************************
;* push environment			     PUSH-ENV   list-of-symbols *
;*									*
;* Purpose:  Scheme interpreter support to "push" a new rib onto the	*
;*		current heap allocated environment.			*
;************************************************************************
	  public  push_env
push_env: lods	  byte ptr ES:[SI] ; load code block constant pointer
;     allocate new environment object
	  mov	  BX,ENV_SIZE-BLK_OVHD ; load size of environment data object,
	  mov	  CX,ENVTYPE	   ;  environment type code, and
	  mov	  DX,offset tmp_reg ; temporary register address
	  pushm	  <BX,CX,DX>	   ; push arguments to 'allocate_block'
	  C_call  alloc_bl,<AX,SI>,Load_ES ; allocate new environment object

;     fetch pointer to list-of-symbols
	  restore <AX,ES>
	  mov	  BX,AX
	  shl	  AX,1
	  add	  BX,AX		   ; BX <- constant number * 3
	  add	  BX,CB_dis	   ; add code block displacement to BX
	  mov	  AX,ES:[BX].cod_cdis ; load constant from code block
	  mov	  DL,ES:[BX].cod_cpag

;     place previous env pointer in new one; update stack frame's env pointer
	  mov	  BX,tmp_page	   ; load pointer to new env object
	  mov	  DI,tmp_disp
	  LoadPage ES,BX
;;;	  mov	  ES,pagetabl+[BX]
	  mov	  SI,FP		   ; load current stack frame pointer
	  xchg	  BL,S_stack+[SI].sf_hpage ; exchange old/new env pointers
	  mov	  ES:[DI].env_ppag,BL
	  mov	  CX,DI
	  xchg	  CX,word ptr S_stack+[SI].sf_hdisp
	  mov	  ES:[DI].env_pdis,CX

;     put list-of-symbols pointer into new environment data object
	  mov	  ES:[DI].env_npag,DL ;  and store it
	  mov	  ES:[DI].env_ndis,AX

;     set tm2_reg to nil (initial empty list of values)
	  mov	  byte ptr tm2_page,NIL_PAGE*2 ; set tmp_reg to nil
	  mov	  tm2_disp,NIL_DISP

;     count number of symbols in the list-of-symbols
	  cmp	  DL,0		   ; is list of symbols nil?
	  je	  psh_end	   ; if empty list, jump
	  mov	  ES:[DI].env_vpag,NIL_PAGE*2 ; make value list pointer in env
	  mov	  ES:[DI].env_vdis,NIL_DISP ;  object nil to prevent GC problems
	  xor	  CX,CX		   ; zero the counter
	  xor	  BX,BX
	  mov	  BL,DL		   ; copy the list-of-symbols pointer
	  mov	  SI,AX		   ;  into BX:SI
psh_enxt: inc	  CX		   ; increment list length counter
	  LoadPage ES,BX	   ; follow the cdr field of the linked list
;;;	  mov	  ES,pagetabl+[BX] ; follow the cdr field of the linked list
	  mov	  BL,ES:[SI].cdr_page
	  mov	  SI,ES:[SI].cdr
	  cmp	  BL,0		   ; end of list?
	  jne	  psh_enxt

;     set up parameters for call to cons
	  mov	  DX,offset nil_reg
	  mov	  AX,offset tm2_reg
	  pushm	  <DX,AX,AX>
	  mov	  AX,DS		   ; load ES for call to Lattice C routine
	  mov	  ES,AX

;     create value list of nil pointers (linked through car field)
psh_cons: C_call  cons,<CX>	   ; cons a nil value cell
	  restore <CX>		   ; reload counter
	  loop	  psh_cons	   ; decrement count, loop if not zero
	  add	  SP,WORDINCR*3	   ; drop arguments off TIPC's stack

;     store pointer to list of values into environment data object
	  mov	  BX,tmp_page	   ; reload environment object pointer (it
	  LoadPage ES,BX	   ;  may have been moved during the consing
;;;	  mov	  ES,pagetabl+[BX] ;  may have been moved during the consing
	  mov	  DI,tmp_disp	   ;  of the nil values list)
psh_end:  mov	  AL,byte ptr tm2_page ; store pointer to list-of-values
	  mov	  ES:[DI].env_vpag,AL  ;  into env data object
	  mov	  AX,tm2_disp
	  mov	  ES:[DI].env_vdis,AX

	  jmp	  next_SP	   ; return to interpreter

;************************************************************************
;* hash-environment			     HASH-ENV                  	*
;*									*
;* Purpose:  Scheme interpreter support to return a hashed environment	*
;*		                                           		*
;************************************************************************
	  public  hash_env
hash_env: lods	  byte ptr ES:[SI] ; load destination register number
;     allocate new environment object
	  mov	  BX,(HT_SIZE*3)+BLK_OVHD  ; size of hashed env
	  mov	  CX,ENVTYPE	           ; environment type code
	  mov	  DX,offset tmp_reg        ; temporary register address
	  pushm	  <BX,CX,DX>	           ; push arguments to 'allocate_block'
	  C_call  alloc_bl,<AX,SI>,Load_ES ; allocate new environment object
	  mov	  SP,BP
	  push	  tmp_disp		   ; push new environment's displacement
	  mov	  BX,tmp_page		   ; get page offset of new env.
	  shr	  BX,1			   ; convert to number
	  push	  BX			   ; push new environment's page number
	  C_call  zero_blk   		   ; zero out the new environment
	  mov	  SP,BP
	  mov	  BX,tmp_page		   ; Now address the new environment
	  mov	  DI,tmp_disp
	  LoadPage ES,BX		   ; ES <= address of new environment
	  mov	  BX,FP		           ; get current stack frame pointer
	  mov	  AL,S_stack+[BX].sf_hpage ; get current env pointer from stack
	  mov	  ES:[DI].env_ppag,AL      ;   and store in new env object
	  mov	  AX,word ptr S_stack+[BX].sf_hdisp
	  mov	  ES:[DI].env_pdis,AX	   
	  restore <AX,SI,ES>		    ; restore saved regs
	  mov	  DI,AX			    ; DI <= destination register
	  mov	  BX,tmp_page		    ; get page number of new environment
	  mov	  byte ptr reg0_pag+[DI],BL ;   and place in destination reg
	  mov	  BX,tmp_disp		    ; get disp of new environment
	  mov     reg0_dis+[DI],BX	    ;   and place in destination
	  jmp	  next

;************************************************************************
;* drop-environment			     DROP-ENV I(number to drop)	*
;*									*
;* Purpose:  Scheme interpreter support to drop the most recently	*
;*		allocated rib from the current environment.		*
;************************************************************************
	  public  drop_env
drop_env: lods	  byte ptr ES:[SI] ; load drop count
	  save	  <SI>		   ; save the current location pointer
	  mov	  CX,AX		   ; copy drop count to CX
	  mov	  DI,FP		   ; load the current stack frame pointer
	  xor	  BX,BX
	  mov	  BL,S_stack+[DI].sf_hpage ; load environment pointer from
	  mov	  SI,word ptr S_stack+[DI].sf_hdisp ; the current stack frame
drop_lp:  LoadPage ES,BX
;;;       mov	  ES,pagetabl+[BX]
	  mov	  BL,ES:[SI].env_ppag ; copy parent's pointer from environment
	  mov	  SI,ES:[SI].env_pdis
	  loop	  drop_lp
	  mov	  S_stack+[DI].sf_hpage,BL ;  rib into the stack frame
	  mov	  word ptr S_stack+[DI].sf_hdisp,SI
	  jmp	  next_PC	   ; return to interpreter

;************************************************************************
;*		Macro Support for load/store-environment		*
;************************************************************************
ld_st	  macro	  direction,text
	  local	  x,y
	  lods	  word ptr ES:[SI] ; load operands
	  xor	  BH,BH
	  mov	  BL,AL		   ; copy destination register number
	  mov	  DI,BX		   ;  into TIPC register DI and
	  add	  DI,offset reg0   ;  compute its address
	  save	  <SI,DI>	   ; save location pointer, dest reg address
	  mov	  BL,AH		   ; copy constant number into
	  mov	  DI,BX		   ;  TIPC register DI
	  shl	  BX,1
	  add	  DI,BX		   ; DI <- constant number * 3
	  add	  DI,CB_dis	   ; compute address of code block constant
	  xor	  BH,BH
	  mov	  BL,ES:[DI].cod_cpag ; load symbol's page number
	  cmp	  byte ptr ptype+[BX],SYMTYPE*2 ; it is a symbol, isn't it?
	  jne	  x		   ; if not a symbol, error (jump)
;     call "srch_all" to search the current environment
	  mov	  CX,BX		   ; copy symbol pointer into CX:DX
	  mov	  DX,ES:[DI].cod_cdis
	  mov	  SI,FP		   ; load current stack frame pointer
	  mov	  BL,S_stack+[SI].sf_hpage ; load current env pointer into
	  mov	  SI,word ptr S_stack+[SI].sf_hdisp ;  BX:SI
	  call	  srch_all	   ; search environment for symbol
	  restore <DI>		   ; reload destination register address
	  cmp	  BL,0		   ; was symbol found in environment?
	  je	  y		   ; if not found, error (jump)
	  LoadPage ES,BX
;;;	  mov	  ES,pagetabl+[BX]
IFIDN	  <direction>,<load>
;     return value from cdr field of value cell returned by "srch_all"
	  mov	  AL,ES:[SI].cdr_page
	  mov	  byte ptr [DI].C_page,AL ; store value in destination register
	  mov	  AX,ES:[SI].cdr
	  mov	  [DI].C_disp,AX
ELSE
IFIDN	  <direction>,<store>
;     store value into cdr field of returned value cell
	  mov	  AL,byte ptr [DI].C_page ; store value into cdr field
	  mov	  ES:[SI].cdr_page,AL ;		of cell
	  mov	  AX,[DI].C_disp
	  mov	  ES:[SI].cdr,AX
ELSE
	  ***error***	Invalid 'direction'
ENDIF
ENDIF
;     return to the Scheme interpreter
	  jmp	  next_PC
;     ***error-- operand is not a symbol***
x:	  lea	  BX,text	   ; load text for instruction's name
	  jmp	  src_err	   ; display "source operand error" message
;     ***error-- symbol not found in environment***
y:	  corrpage CX
IFIDN	  <direction>,<load>
	  xor	  AX,AX		   ; signal current environment being used
	  pushm	  <DI,AX,DX,CX>	   ; push arguments for call
	  C_call  sym_unde,,Load_ES ; call: sym_undefined(pg,ds,env,dest);
ELSE
	  pushm	  <DX,CX>	   ; push arguments for call
	  C_call  not_lexi,,Load_ES ; call: not_lexically_bound(pg,ds);
ENDIF
	  restore <SI>		   ; load next instruction's offset and
	  sub	  SI,3		   ;  back it up to retry the ld/st
	  jmp	  sch_err	   ; link to Scheme debugger
	  endm

;************************************************************************
;* Load From Environment		   LD-ENV     R(dest),C(symbol)	*
;*									*
;* Purpose:  Scheme interpreter support to load from the current	*
;*		environment.						*
;************************************************************************
	  public  ld_env
;     load and process operands
ld_env:	  ld_st	  load,m_ld_en

;************************************************************************
;* Store Into Environment		   ST-ENV    R(value),C(symbol)	*
;*									*
;* Purpose:  Scheme interpreter support to store into the current	*
;*		environment.						*
;************************************************************************
	  public  st_env
;     load and process operands
st_env:	  ld_st	  store,m_st_en

	  purge	  ld_st

;************************************************************************
;*						    AL	    AL    AH	*
;* Define in Environment		   DEFINE   R(d=s1),R(s2),R(s3) *
;*					       s1=sym,s2=val,s3=env/nil *
;*									*
;* Purpose: Scheme interpreter support to define a symbol in a given	*
;*	    environment. This routine supports the MIT Scheme construct *
;*	    (set! (access sym env) value). In essence, the current env	*
;* 	    is searched for sym. If found, then its binding is modified *
;*	    to value. Otherwise, a new binding is added to the current  *
;*	    environment.						*
;************************************************************************
;     ***error-- invalid operand for define***
def_en_x: mov	  BX,offset m_def_en ; load "def-env" text
	  jmp	  src_err	   ; display "invalid source operand" message

	  public  def_env
def_env:  lods	  byte ptr ES:[SI] ; load symbol operand
	  mov	  DI,AX		   ; copy symbol register number to
	  add	  DI,offset reg0   ;  DI and compute the register's address
	  lods	  word ptr ES:[SI] ; load value/environment operands
	  save	  <SI,DI,AX>	   ; save loc ptr, dest reg addr, val/env opnds
;     validate and load the symbol operand
	  mov	  BX,[DI].C_page   ; fetch the symbol's page number
	  cmp	  byte ptr ptype+[BX],SYMTYPE*2 ; is first operand a symbol?
	  jne	  def_en_x	   ; if not a symbol, error (jump)
	  mov	  CX,BX		   ; place symbol pointer into CX:DX
	  mov	  DX,[DI].C_disp
;     validate and load environment operand
	  mov	  BL,AH		   ; copy env register number to BX
	  mov	  SI,reg0_dis+[BX] ; load environment pointer into BX:SI
	  mov	  BL,byte ptr reg0_pag+[BX]
	  cmp	  byte ptr ptype+[BX],ENVTYPE*2 ; is it an envirnoment object?
	  je	  def_e_ok	   ; if an environment, jump
	  cmp	  BL,0		   ; is it a nil pointer?
	  jne	  def_en_x	   ; if not nil, error (invalid operand; jump)
	  mov	  SI,FP		   ; load pointer to current stack frame
	  mov	  BL,S_stack+[SI].sf_hpage ; default environment to current
	  mov	  SI,word ptr S_stack+[SI].sf_hdisp ;  environment
;     search environment for the symbol
def_e_ok: pushm   <BX,SI>	   ; save environment pointer on stack
	  call	  srch_all	   ; search all rib's
	  restore <AX>		   ; restore 2nd and 3rd operands
	  cmp	  BL,0		   ; was symbol found?
	  je	  def_bind	   ; if not found, jump
	  LoadPage ES,BX	   ; load value cell page's paragraph address
;;;	  mov     ES,pagetabl+[BX] ; load value cell page's paragraph address
	  mov	  BL,AL		   ; copy value register number to BX
	  mov	  AL,byte ptr reg0_pag+[BX] ; set cdr of value cell to the
	  mov	  ES:[SI].cdr_page,AL ;  contents of the value register
	  mov	  AX,reg0_dis+[BX]
	  mov	  ES:[SI].cdr,AX
	  jmp	  next_SP	   ; return to interpreter

;     Symbol not found in environment -- bind it in given rib
def_bind: restore <DI>	 	   ; restore symbol register address
	  pop	  [BP].temp_reg.C_disp ; restore env pointer in local temp_reg
	  pop	  [BP].temp_reg.C_page
	  mov	  BL,AL		   ; compute value register address
	  add	  BX,offset reg0
	  lea	  SI,[BP].temp_reg ; load tmp_reg address
	  pushm	  <SI,BX,DI>	   ; push args to bind_it
	  call	  bind_it	   ; bind symbol in environment
	  jmp	  next_SP	   ; return to interpreter

;************************************************************************
;* Set Global Environment		       SET-GLOB-ENV!   R(value) *
;*									*
;* Purpose:  Scheme interpreter support to initialize the Global	*
;*		Environment Register (GNV_reg).				*
;************************************************************************
	  public  set_gnv
set_gnv:  lods	  byte ptr ES:[SI] ; load operand
	  mov	  DI,AX		   ; copy source register number to DI and
	  add	  DI,offset reg0   ;  compute source/destination reg address
	  mov	  AX,[DI].C_disp ; load pointer to new global environment
	  mov	  BX,[DI].C_page
	  cmp	  byte ptr ptype+[BX],ENVTYPE*2 ;it's an environment, isn't it?
	  jne	  set_g_er	   ; if operand not env, error (jump)
	  xchg	  byte ptr GNV_pag,BL ; copy env pointer to GNV_reg
	  xchg	  GNV_dis,AX
	  mov	  byte ptr [DI].C_page,BL ; store previous value of GNV_reg
	  mov	  [DI].C_disp,AX   ;  into the destination register
	  jmp	  next		   ; return to interpreter
;     ***error-- operand is not an environment object***
set_g_er: save	  <SI>		   ; save the location pointer
	  mov	  BX,offset m_setgnv ; load text for "set-global-env!"
	  jmp	  src_err	   ; display "source operand error" message

;************************************************************************
;*							     AL   AH	*
;* Load from Global Environment		        LD-GLOBAL    R(d),C(s1)	*
;*							      s1=symbol	*
;*									*
;* Purpose:  Scheme interpreter support to retrieve values for symbols	*
;*		defined in the current global environment.		*
;*									*
;* Note:  This instruction is an optimization of the LD-ENV operation.	*
;*		Here, the environment operand defaults to the current	*
;*		global environment, which is pointer to by GNV_reg.	*
;************************************************************************
	  public  ld_globl
ld_globl: lods	  word ptr ES:[SI] ; load operands
	  mov	  BL,AL		   ; copy the destintation register
	  mov	  DI,BX		   ;  into TIPC register DI and compute
	  add	  DI,offset reg0   ;  the destination register's address
	  save	  <SI,DI>	   ; save said, and the location pointer
;     validate the symbol operand and load symbol pointer
	  mov	  BL,AH		   ; copy the constant number
	  mov	  SI,BX		   ; SI <- constant number * 3
	  shl	  SI,1
	  add	  SI,BX
	  add	  SI,CB_dis	   ; add in displacement of current code block
	  mov	  BL,ES:[SI].cod_cpag ; load symbol's page number
	  mov	  DX,ES:[SI].cod_cdis ; load symbol pointer into CX:DX
ld_gl_x:  cmp	  byte ptr ptype+[BX],SYMTYPE*2 ; it is a symbol, isn't it?
	  jne	  ld_g_err	   ; if not a symbol, error (jump)
	  mov	  CX,BX
;     load pointer to the global environment
	  mov	  BL,byte ptr GNV_pag
	  mov	  SI,GNV_dis
;     search the global environment for the symbol-- test to see if found
	  pushm	  <CX,DX>	   ; save symbol pointer
	  call	  srch_all	   ; search global environment
	  restore <DI>		   ; reload destination register address
	  cmp	  BL,0		   ; was symbol found?
	  je	  ld_g_nf	   ; if not found, error (jump)
;     copy cdr field of value cell returned into the destination register
	  LoadPage ES,BX
;;;	  mov	  ES,pagetabl+[BX]
	  mov	  AL,ES:[SI].cdr_page ; copy cdr field of value cell
	  mov	  byte ptr [DI].C_page,AL ;  into destination register
	  mov	  AX,ES:[SI].cdr
	  mov	  [DI].C_disp,AX
	  jmp	  next_SP	   ; return to interpreter
;     ***error-- symbol operand wasn't a symbol pointer***
ld_g_err: mov	  BX,offset m_ld_gl ; load text for "ld-global"
	  jmp	  src_err	   ; display "invalid source operand" message
;     ***error-- global symbol not found***
ld_g_nf:  popm	  <DX,CX>	   ; restore symbol pointer
	  corrpage CX		   ; correct page number for call to C
	  mov	  AX,offset GNV_reg ; load address of global env register
	  pushm	  <DI,AX,DX,CX>	   ; push arguments for call
	  C_call  sym_unde,,Load_ES ; call: sym_undefined(pg,ds,env,dest)
	  restore <SI>		   ; load next intstruction's offset and
	  sub	  SI,3		   ;  back up location pointer to retry load
	  jmp	  sch_err	   ; link to Scheme debugger

;************************************************************************
;*							     AL   AH	*
;* Load from Global Environment	(reg operand)   LD-GLOBAL-R  R(d),R(s1)	*
;*							      s1=symbol	*
;*									*
;* Purpose:  Scheme interpreter support to retrieve values for symbols	*
;*		defined in the current global environment.		*
;*									*
;* Note:  This instruction is an optimization of the LD-ENV operation.	*
;*		Here, the environment operand defaults to the current	*
;*		global environment, which is pointer to by GNV_reg.	*
;************************************************************************
	  public  ld_globr
ld_globr: lods	  word ptr ES:[SI] ; load operands
	  mov	  BL,AL		   ; copy the destintation register
	  mov	  DI,BX		   ;  into TIPC register DI and compute
	  add	  DI,offset reg0   ;  the destination register's address
	  save	  <SI,DI>	   ; save said, and the location pointer
;     load symbol pointer
	  mov	  BL,AH		   ; copy the symbol's register number
	  mov	  DX,reg0_dis+[BX] ; load symbol's displacement
	  mov	  BL,byte ptr reg0_pag+[BX] ; load symbol's page number
	  jmp	  ld_gl_x	   ; continue process as ld-global

;************************************************************************
;*							  AL	  AH	*
;* Define in Global Environment			DEFINE!   R(d=s1),C(s2)	*
;*						     s1=value,s2=symbol *
;*									*
;* Purpose:  Scheme interpreter support to assign a variable in the	*
;*		current "global" environment.				*
;*									*
;* Note:  This instruction is an optimization of the DEFINE-ENV		*
;*		operation.  Here, the environment operand defaults to	*
;*		the current global environment, which is pointed to by	*
;*		GNV_reg.						*
;************************************************************************
	  public  define
define:	  lods	  word ptr ES:[SI] ; load operands
	  mov	  BL,AH		   ; copy constant number to BX
	  xor	  AH,AH
	  mov	  DI,AX		   ; copy value/destination register number
	  add	  DI,offset reg0   ;  to DI and compute the register's address
	  save	  <SI,DI>	   ; save location pointer, dest reg address
;     validate symbol operands and load it into CX:DX
	  mov	  SI,BX		   ; copy constant number into SI
	  shl	  SI,1
	  add	  SI,BX		   ; SI <- constant number * 3
	  add	  SI,CB_dis	   ; add starting offset of current code block
	  mov	  BL,ES:[SI].cod_cpag ; load symbol's page number
	  cmp	  byte ptr ptype+[BX],SYMTYPE*2 ; it is a symbol, isn't it?
	  jne	  defb_err	   ; if not a symbol, error (jump)
	  mov	  CX,BX		   ; put symbol pointer into CX:DX
	  mov	  DX,ES:[SI].cod_cdis
	  pushm	  <CX,DX>	   ; save pointer to symbol
;     load global environment pointer into BX:SI
	  mov	  BL,byte ptr GNV_pag
	  mov	  SI,GNV_dis
;     search the global environment for the symbol-- test to see if found
	  call	  srch_env
	  cmp	  BL,0
	  je	  defb_new
;     symbol was found-- set cdr of field returned to the value specified
	  restore <DI>
	  LoadPage ES,BX
;;;	  mov	  ES,pagetabl+[BX]
	  mov	  AL,byte ptr [DI].C_page
	  mov	  ES:[SI].cdr_page,AL
	  mov	  AX,[DI].C_disp
	  mov	  ES:[SI].cdr,AX
	  jmp	  next_SP	   ; return to interpreter
;     symbol wasn't found-- create new binding in current global environment
defb_new: mov	  AX,SP		   ; get address of symbol

;     In case you're wondering what just went on with the above instruction,
;     the page and displacement of the symbol to be bound are residing in the
;     correct order on the top of the stack.  The "mov AX,SP" captures the
;     address of said pointer so that it may be used as an argument to
;     sym_bind, below.

	  mov	  BX,offset GNV_reg ; load GNV_reg address (contains env ptr)
	  pushm	  <BX,[BP].save_DI,AX> ; push sym,val,env register pointers
	  call	  bind_it 	   ; create binding in global environment
	  jmp	  next_SP	   ; return to interpreter
;     ***error-- symbol operand wasn't a symbol***
defb_err: mov	  BX,offset m_defb
	  jmp	  src_err

;************************************************************************
;*							  AL	  AH	*
;* Define in Global Environment			ST-GLOBAL R(d=s1),C(s2)	*
;*						     s1=value,s2=symbol *
;*									*
;* Purpose:  Scheme interpreter support to assign a variable in the	*
;*		current "global" environment.				*
;*									*
;* Note:  This instruction is an optimization of the ST-ENV		*
;*		operation.  Here, the environment operand defaults to	*
;*		the current global environment, which is pointed to by	*
;*		GNV_reg.						*
;************************************************************************
	  public  st_globl
st_globl: lods	  word ptr ES:[SI] ; load operands
	  mov	  BL,AH		   ; copy constant number to BX
	  xor	  AH,AH
	  mov	  DI,AX		   ; copy value/destination register number
	  add	  DI,offset reg0   ;  to DI and compute the register's address
	  save	  <SI,DI>	   ; save location pointer, dest reg address
;     validate symbol operands and load it into CX:DX
	  mov	  SI,BX		   ; copy constant number into SI
	  shl	  SI,1
	  add	  SI,BX		   ; SI <- constant number * 3
	  add	  SI,CB_dis	   ; add starting offset of current code block
	  mov	  BL,ES:[SI].cod_cpag ; load symbol's page number
	  cmp	  byte ptr ptype+[BX],SYMTYPE*2 ; it is a symbol, isn't it?
	  jne	  st_gl_er	   ; if not a symbol, error (jump)
	  mov	  CX,BX		   ; put symbol pointer into CX:DX
	  mov	  DX,ES:[SI].cod_cdis
	  pushm	  <CX,DX>	   ; save pointer to symbol
;     load global environment pointer into BX:SI
	  mov	  BL,byte ptr GNV_pag
	  mov	  SI,GNV_dis
;     search the global environment for the symbol-- test to see if found
	  call	  srch_all
	  restore <DI>
	  cmp	  BL,0
	  je	  st_gl_nf
;     symbol was found-- set cdr of field returned to the value specified
	  LoadPage ES,BX
;;;	  mov	  ES,pagetabl+[BX]
	  mov	  AL,byte ptr [DI].C_page
	  mov	  ES:[SI].cdr_page,AL
	  mov	  AX,[DI].C_disp
	  mov	  ES:[SI].cdr,AX
	  jmp	  next_SP	   ; return to interpreter
;     symbol wasn't found-- inquire from user as to what to do
st_gl_nf: popm	  <DX,CX>	   ; restore pointer to symbol
	  corrpage CX		   ; adjust page number for C call
	  pushm	  <DI,DX,CX>	   ; push page, disp, value reg address
	  C_call  not_glob,,load_ES ; resolve error situation
	  restore <SI>		   ; load next instruction's offset and back
	  sub	  SI,3		   ;  location pointer up to retry the store
	  jmp	  sch_err	   ; link to Scheme debugger
;     ***error-- invalid operand to st-global***
st_gl_er: mov	  BX,offset m_st_gl
	  jmp	  src_err

;************************************************************************
;* Environment Predicate				ENV?	object  *
;*									*
;* Purpose:  Scheme interpreter support to test for an environment	*
;*		data object.						*
;************************************************************************
	  public  env_p
env_p:	  lods	  byte ptr ES:[SI] ; load the operand
	  mov	  DI,AX		   ;  and copy into TIPC register DI
	  mov	  BX,reg0_pag+[DI] ; load the operand's page number
	  cmp	  byte ptr ptype+[BX],ENVTYPE*2 ; is operand an environment?
	  je	  env_t		   ; if an environment object, jump
;     object not an env-- return a value of nil in the destination register
	  mov	  byte ptr reg0_pag+[DI],NIL_PAGE*2
	  mov	  reg0_dis+[DI],NIL_DISP*2
	  jmp	  next		   ; return to interpreter
;     object is an env-- return a value of 't in the destination register
env_t:	  mov	  byte ptr reg0_pag+[DI],T_PAGE*2
	  mov	  reg0_dis+[DI],T_DISP*2
	  jmp	  next		   ; return to interpreter

;************************************************************************
;* Make Environment 					MK-ENV	   dest *
;*									*
;* Purpose:  Scheme interpreter support to return a pointer to the	*
;*		current environment.					*
;************************************************************************
	  public  mk_env
mk_env:   lods	  byte ptr ES:[SI] ; load destination register number
	  mov	  DI,AX		   ;  and put it in TIPC register DI
	  mov	  BX,FP		   ; load the current stack frame pointer
	  mov	  AL,S_stack+[BX].sf_hpage ; load current env pointer from stack
	  mov	  byte ptr reg0_pag+[DI],AL;  and put in destination register
	  mov	  AX,word ptr S_stack+[BX].sf_hdisp
	  mov	  reg0_dis+[DI],AX
	  jmp	  next		   ; return to interpreter

;************************************************************************
;* Environment Parent					ENV-PARENT  env *
;*									*
;* Purpose:  Scheme interpreter return the "parent" of a given		*
;*		environment.						*
;************************************************************************
	  public  env_par
env_par:  lods	  byte ptr ES:[SI] ; load the environment operand
	  save	  <SI>		   ; save the current location pointer
	  mov	  DI,AX		   ; copy operand register number to DI
	  mov	  BX,reg0_pag+[DI] ; load operand's page number
	  cmp	  byte ptr ptype+[BX],ENVTYPE*2 ; is operand an environment?
	  jne	  env_p_er	   ; if not an environment, error (jump)
	  mov	  SI,reg0_dis+[DI] ; load pointer to environment object
	  LoadPage ES,BX
;;;	  mov	  ES,pagetabl+[BX]
	  mov	  AL,ES:[SI].env_ppag ; load parent pointer from env object
	  mov	  byte ptr reg0_pag+[DI],AL ;  and put in destination register
	  mov	  AX,ES:[SI].env_pdis
	  mov	  reg0_dis+[DI],AX
	  jmp	  next_PC	   ; return to interpreter
;     ***error-- invalid operand***
env_p_er: lea	  BX,m_en_par	   ; load text addr for "environment-parent"
	  jmp	  src_err	   ; display "invalid source operand" message

;************************************************************************
;* Lookup In Environment			ENV-LU	  R(d=s1),R(s2)	*
;*						       s1=symbol,s2=env	*
;************************************************************************
	  public  env_lu
env_lu:	  lods	  word ptr ES:[SI] ; load operands
;     fetch and validate first operand (symbol pointer)
	  xor	  BH,BH
	  mov	  BL,AL
	  mov	  DI,BX
	  add	  DI,offset reg0
	  save	  <SI,DI>	   ; save location pointer; dest reg address
	  mov	  CX,[DI].C_page   ; copy symbol pointer into CX:DX
	  mov	  DX,[DI].C_disp
	  mov	  BX,CX		   ; test to make sure that first operand
	  cmp	  byte ptr ptype+[BX],SYMTYPE*2 ;  is a symbol
	  jne	  env_lu_x	   ; if not a symbol, error (jump)
;     fetch and validate second operand (environment pointer)
	  mov	  BL,AH		   ; copy env register number
	  mov	  SI,reg0_dis+[BX] ; copy environment pointer into BX:SI
	  mov	  BL,byte ptr reg0_pag+[BX]
	  cmp	  byte ptr ptype+[BX],ENVTYPE*2 ; it is an env, isn't it?
	  jne	  env_lu_x	   ; if operand not environment, error (jump)
;     search the environment for the symbol
	  call	  srch_all	   ; search all ribs
;     store result of search into destination register
	  restore <DI>		   ; reload the destination register address
	  mov	  byte ptr [DI].C_page,BL
	  mov	  [DI].C_disp,SI
	  jmp	  next_PC	   ; return to interpreter
;     ***error-- invalid operand***
env_lu_x: mov	  BX,offset m_env_lu
	  jmp	  src_err

s_env	  endp

;************************************************************************
;*		Local Support - Search Environment (all of it)		*
;*									*
;* Input Parameters:  CX:DX - search symbol				*
;*		      BX:SI - environment chain pointer			*
;*									*
;* Output Parameters: BX:SI - value cell for symbol			*
;************************************************************************
srch_all  proc	  near
	  pushm	  <BX,SI,CX,DX>	   ; save pointer to current rib
	  call	  srch_env	   ; search rib for desired symbol
	  cmp	  BX,0		   ; was symbol found?
	  jne	  srch_ok	   ; if symbol found, jump
	  popm	  <DX,CX,SI,BX>	   ; restore pointer to current rib
	  LoadPage ES,BX
;;;	  mov	  ES,pagetabl+[BX] ; load pointer to parent rib
	  mov	  BL,ES:[SI].env_ppag
	  mov	  SI,ES:[SI].env_pdis
	  cmp	  BX,0		   ; does parent rib exist?
	  jne	  srch_all	   ; if no parent, symbol not found (jump)
	  jmp	  short srch_nok
srch_ok:  add	  SP,WORDINCR*4	   ; dump env pointer off stack
srch_nok: ret			   ; return search result to caller
srch_all  endp

;************************************************************************
;*		Local Support - Search Environment (one rib)		*
;*									*
;* Input Parameters:  CX:DX - search symbol				*
;*		      BX:SI - environment chain pointer			*
;*									*
;* Output Parameters: BX:SI - value cell for symbol			*
;************************************************************************
srch_env  proc	  near
	  LoadPage ES,BX           ; load paragraph address of env chain 
;;;	  mov	  ES,pagetabl+[BX] ; load paragraph address of env chain
	  cmp	  ES:[SI].env_len,ENV_SIZE ; hash table or "rib"?
	  jne	  srch_ht	   ; if hash table, jump
	  pushm	  <BX,SI>	   ; save pointer to environment
;;;;	  pushm	  <ES,SI>	   ; save pointer to environment
	  mov	  AX,1		   ; initialize counter
	  xor	  BX,BX
	  mov	  BL,ES:[SI].env_npag ; load pointer to list of symbols
	  mov	  SI,ES:[SI].env_ndis
srch_mor: cmp	  BL,0		   ; more symbols in this rib?
	  je	  srch_nf	   ; if end of symbol list, jump
	  LoadPage ES,BX
;;;	  mov	  ES,pagetabl+[BX]
	  cmp	  DX,ES:[SI].car   ; is symbol disp eq to this entry?
	  jne	  srch_nxt	   ; if no match, jump
	  cmp	  CL,ES:[SI].car_page ; is page number eq?
	  je	  srch_fnd	   ; if symbol's page number eq, jump
srch_nxt: inc	  AX		   ; increment symbol count
	  mov	  BL,ES:[SI].cdr_page ; follow cdr field of linked list
	  mov	  SI,ES:[SI].cdr
	  jmp	  short srch_mor   ; loop
srch_fnd: mov	  CX,AX		   ; move counter symbol counter to CX
	  popm	  <SI,BX>	   ; recover pointer to environment chain
	  LoadPage ES,BX
;;;;	  popm	  <SI,ES>	   ; recover pointer to environment chain
	  mov	  BL,ES:[SI].env_vpag ; load pointer to value list
	  mov	  SI,ES:[SI].env_vdis
	  jmp	  short srch_f1
srch_lp:  LoadPage ES,BX	   ; follow chain through car field of linked
;;;       mov	  ES,pagetabl+[BX] ; follow chain through car field of linked
	  mov	  BL,ES:[SI].car_page ;  list
	  mov	  SI,ES:[SI].car
srch_f1:  loop	  srch_lp	   ; not value entry for symbol, loop (jump)
	  ret			   ; return to caller
;     symbol not found-- return nil
srch_nf:  add	  SP,WORDINCR*2    ; drop env pointer off stack
	  ret			   ; return to caller
;
;		     Hash Table Rib Format
;
srch_ht:  pushm	  <BX,SI>	   ; save arguments to srch_env
	  mov	  lcl_page,CX	   ; store symbol pointer in tmp_reg
	  mov	  lcl_disp,DX
	  mov	  AX,offset lcl_reg ; load address of lcl_reg and push
	  push	  AX		   ;  it as an argument to sym_hash
	  call	  sym_hash	   ; get the hash value for the symbol
	  add	  SP,WORDINCR	   ; drop the argument off the stack
	  cmp	  AX,HT_SIZE	   ; valid hash value returned?
	  jae	  srch_htx	   ; if not valid, error (jump)
;     fetch symbol chain from indicated hash table bucket
	  popm	  <SI,BX>	   ; restore pointer to environment object
	  add	  SI,AX		   ; env-ptr += hash-value * 3
	  shl	  AX,1
	  add	  SI,AX
          LoadPage ES,BX           ; load environment page's paragraph address
;;;	  mov	  ES,pagetabl+[BX] ; load environment page's paragraph address
	  mov	  BL,ES:[SI].env_npag ; load pointer to hash chain
	  cmp	  BL,0		   ; is chain empty?
	  je	  srch_nfx	   ; if chain is empty, symbol not found (jump)
	  mov	  SI,ES:[SI].env_ndis
          LoadPage ES,BX
;;;	  mov	  ES,pagetabl+[BX]
	  mov	  DX,lcl_page	   ; restore symbol pointer into DX:AX
	  mov	  AX,lcl_disp
	  call	  lookup	   ; search for symbol in linked list
	  mov	  SI,DI		   ; put pointer returned in BX:SI
	  ret			   ; return to caller
;     ***error-- symbol operand wasn't a symbol***
srch_htx: add	  SP,WORDINCR*2	   ; drop saved arguments off stack
	  xor	  BX,BX		   ; return a nil pointer
srch_nfx: xor	  SI,SI
	  ret
srch_env  endp

;************************************************************************
;*			Symbol Binding Routine				*
;*									*
;* Purpose:  Lattice C callable routine to return the bind a value to	*
;*		a symbol in a given environment.			*
;*									*
;* Calling Sequence:  sym_bind(symbol, value, environment)		*
;*			where symbol - register containing the symbol	*
;*					pointer				*
;*			       value - register containing the value to *
;*					be assigned			*
;*			 environment - register containing a pointer to *
;*					the environment in which the	*
;*					binding is to take place	*
;************************************************************************
bind_arg  struc
	  dw	  ?		   ; caller's BP
	  dw	  ?		   ; caller's ES
	  dw	  ?		   ; return address
bnd_sym	  dw	  ?		   ; address of symbol register
bnd_val	  dw	  ?		   ; address of value register
bnd_env	  dw	  ?		   ; address of environment register
bind_arg  ends

	  public  sym_bind
bind_it   proc	  near
	  push	  ES		   ; save the caller's ES register
	  push	  BP		   ; save the caller's BP register
	  mov	  BP,SP		   ; establish addressability for local data
	  jmp	  sb_new	   ; bind symbol in current environment

sym_bind: push	  ES		   ; save the caller's ES register
	  push	  BP		   ; save the caller's BP register
	  mov	  BP,SP		   ; establish addressability for local data

;     see if symbol is already present in the environment
	  mov	  BX,[BP].bnd_sym  ; load address of symbol register
	  mov	  CX,[BX].C_page   ; load symbol pointer into CX:DX
	  mov	  DX,[BX].C_disp
	  mov	  BX,[BP].bnd_env  ; load address of environment register
	  mov	  SI,[BX].C_disp   ; load environment pointer into BX:SI
	  mov	  BX,[BX].C_page
	  call	  srch_all	   ; search the environment for the symbol
	  cmp	  BL,0		   ; was the symbol found in the environment?
	  je	  sb_new	   ; if symbol not found, jump
;     store the value into the cdr field of the returned value cell
          LoadPage ES,BX           ; load value cell's paragraph address
;;;	  mov	  ES,pagetabl+[BX] ; load value cell's paragraph address
	  mov	  BX,[BP].bnd_val  ; load address of value register
	  mov	  AL,byte ptr [BX].C_page ; copy value from value register
	  mov	  ES:[SI].cdr_page,AL ;  into the cdr field of the value cell
	  mov	  AX,[BX].C_disp
	  mov	  ES:[SI].cdr,AX
	  jmp	  sb_ret	   ; return to caller

;     fetch pointer to environment-- decide format of said
sb_new:	  mov	  SI,[BP].bnd_env
	  mov	  BX,[SI].C_page
	  mov	  SI,[SI].C_disp
          LoadPage ES,BX
;;;	  mov	  ES,pagetabl+[BX]
	  cmp	  ES:[SI].env_len,ENV_SIZE
	  jne	  sb_ht
;
;		bind symbol to "rib" format environment
;
;     cons(env[name], symbol, env[name])
	  mov	  AL,ES:[SI].env_npag ; copy name list chain from environment
	  mov	  byte ptr tmp_page,AL ; object to tmp_reg
	  mov	  AX,ES:[SI].env_ndis
	  mov	  tmp_disp,AX
	  mov	  AX,offset tmp_reg
	  pushm	  <AX,[BP].bnd_sym,AX> ; push arguments to "cons"
	  call	  cons		   ; cons symbol to front of name list
	  mov	  BX,[BP].bnd_env  ; reload pointer to environment object
	  mov	  SI,[BX].C_disp   ;  (it may have been relocated during the
	  mov	  BX,[BX].C_page   ;  consing operation)
          LoadPage ES,BX
;;;	  mov	  ES,pagetabl+[BX] ;
	  mov	  AL,byte ptr tmp_page ; update name list pointer in the
	  mov	  ES:[SI].env_npag,AL ;  environment object
	  mov	  AX,tmp_disp
	  mov	  ES:[SI].env_ndis,AX
;     cons(env[value], env[value], value)
	  mov	  AL,ES:[SI].env_vpag ; copy value list chain from environment
	  mov	  byte ptr tmp_page,AL ; object to tmp_reg
	  mov	  AX,ES:[SI].env_vdis
	  mov	  tmp_disp,AX
	  mov	  AX,offset tmp_reg
	  pushm	  <[BP].bnd_val,AX,AX> ; push arguments to "cons"
	  call	  cons		   ; cons value to front of value list
	  mov	  BX,[BP].bnd_env  ; reload pointer to environment object
	  mov	  SI,[BX].C_disp   ;  (it may have been relocated during the
	  mov	  BX,[BX].C_page   ;  consing operation)
          LoadPage ES,BX
;;;	  mov	  ES,pagetabl+[BX] ;
	  mov	  AL,byte ptr tmp_page ; update value list pointer in the
	  mov	  ES:[SI].env_vpag,AL ;  environment object
	  mov	  AX,tmp_disp
	  mov	  ES:[SI].env_vdis,AX
	  jmp	  sb_ret	   ; return to caller
;
;		bind symbol to "hash table" format environment
;
sb_ht:
;     cons(tmp_reg, symbol, value)
	  mov	  AX,offset tmp_reg ; load address of tmp_reg
	  mov	  BX,offset nil_reg ; load address of nil_reg
;     Note:  we're pushing the arguments for both calls to "cons" in the
;	     following statement
	  pushm	  <BX,AX,AX,[BP].bnd_val,[BP].bnd_sym,AX> ; push args to cons
	  call	  cons
	  add	  SP,3*WORDINCR	   ; drop the top three arguments from the stack
;     cons(tmp_reg, tmp_reg, nil_reg)
	  call	  cons
;     obtain hash value for the symbol
	  push	  [BP].bnd_sym
	  call	  sym_hash
	  mov	  BX,AX		   ; multiply hash value by 3
	  shl	  AX,1
	  add	  BX,AX
	  mov	  SI,[BP].bnd_env  ; load pointer to environment object
	  add	  BX,[SI].C_disp   ;  (which may have been moved during
	  mov	  SI,[SI].C_page   ;  the consing operations)
          LoadPage ES,SI
;;;	  mov	  ES,pagetabl+[SI]
	  mov	  AX,tmp_page	   ; load pointer to second list cell
	  mov	  SI,AX
	  xchg	  AL,ES:[BX].env_npag ; swap list header in environment hash
	  mov	  DX,tmp_disp	   ;     table with the pointer to the second
	  mov	  DI,DX		   ;	 list cell
	  xchg	  DX,ES:[BX].env_ndis
          LoadPage ES,SI           ; load pointer to second list cell
;;;	  mov	  ES,pagetabl+[SI] ; load pointer to second list cell
	  mov	  ES:[DI].cdr_page,AL ; update entry in environment hash table
	  mov	  ES:[DI].cdr,DX
	  
;     return to calling procedure
sb_ret:	  mov	  SP,BP		   ; clean up the TIPC's stack
	  pop	  BP		   ; restore caller's BP
	  pop	  ES		   ; restore caller's ES, too
	  ret			   ; return to caller
bind_it   endp

;************************************************************************
;*			Symbol Lookup Routine				*
;*									*
;* Purpose:  Lattice C callable routine to return the value bound to	*
;*		a symbol in a given environment.			*
;*									*
;* Calling Sequence:  sym_bind(symbol, environment)			*
;*			where symbol - register containing the symbol	*
;*					pointer				*
;*			 environment - register containing a pointer to *
;*					the environment to be searched	*
;************************************************************************
look_arg  struc
	  dw	  ?		   ; caller's BP
	  dw	  ?		   ; caller's ES
	  dw	  ?		   ; return address
look_sym  dw	  ?		   ; address of symbol register
look_env  dw	  ?		   ; address of environment register
look_arg  ends

	  public  sym_look
sym_look  proc	  near
	  push	  ES		   ; save the caller's ES register
	  push	  BP		   ; save the caller's BP register
	  mov	  BP,SP		   ; establish addressability for local data

;     see if symbol is already present in the environment
	  mov	  BX,[BP].look_sym ; load address of symbol register
	  mov	  CX,[BX].C_page   ; load symbol pointer into CX:DX
	  mov	  DX,[BX].C_disp
	  mov	  BX,[BP].look_env ; load address of environment register
	  mov	  SI,[BX].C_disp   ; load environment pointer into BX:SI
	  mov	  BX,[BX].C_page
	  call	  srch_all	   ; search the environment for the symbol
	  xor	  AX,AX		   ; set result to false, in case search failed
	  cmp	  BL,0		   ; was the symbol found in the environment?
	  je	  look_ret	   ; if symbol not found, jump
;     return the value in the cdr field in the argument register
          LoadPage ES,BX           ; load value cell's paragraph address
;;;	  mov	  ES,pagetabl+[BX] ; load value cell's paragraph address
	  mov	  BX,[BP].look_sym ; load address of register
	  mov	  AL,ES:[SI].cdr_page ; copy current binding into the
	  mov	  byte ptr [BX].C_page,AL ; argument register
	  mov	  AX,ES:[SI].cdr
	  mov	  [BX].C_disp,AX
	  mov	  AX,1		   ; set result to "TRUE"
;     return to calling procedure
look_ret: pop	  BP		   ; restore caller's BP
	  pop	  ES		   ; restore caller's ES, too
	  ret			   ; return to caller
sym_look  endp

;************************************************************************
;*			Symbol Hashing Routine				*
;*									*
;* Purpose:  Lattice C callable routine to return the hash value for	*
;*		a given symbol.						*
;*									*
;* Calling Sequence:  hash = sym_hash(reg)				*
;*			reg  - register containing symbol pointer	*
;*			hash - the hash value (if page/disp don't point *
;*				to a symbol, -1 is returned)		*
;*									*
;* Methods Used:  The hash value is computed by summing the characters	*
;*		of the symbol and returning the remainder on division	*
;*		by the length of the hash table (HT_SIZE).		*
;*									*
;* Note:  This routine must return the same hash value as the routine	*
;*		"hash" in SUPPORT.C.  If the hashing algorithm is	*
;*		changed, it must be changed in both routines.		*
;************************************************************************
sh_args	  struc
	  dw	  ?		   ; caller's BP
	  dw	  ?		   ; return address
sh_reg	  dw	  ?		   ; symbol pointer register address
sh_args	  ends

	  public  sym_hash
sym_hash  proc	  near
	  push	  BP		   ; save caller's BP
	  mov	  BP,SP
;     Fetch pointer to symbol-- make sure object is a symbol
	  mov	  DI,[BP].sh_reg   ; load register address
	  mov	  BX,[DI].C_page   ; load symbol's page number
	  cmp	  byte ptr ptype+[BX],SYMTYPE*2 ; is object a symbol?
	  jne	  sh_error	   ; if not a symbol, error (jump)
	  push	  ES		   ; save caller's ES
          LoadPage ES,BX           ; load symbol page's paragraph address
;;;	  mov	  ES,pagetabl+[BX] ; load symbol page's paragraph address
	  mov	  SI,[DI].C_disp   ; load symbol's displacement
;     Fetch hash value from symbol object
	  xor	  AH,AH		   ; clear high order byte of AX
	  mov	  AL,ES:[SI].sym_hkey ; fetch hash key
;     Return value in TIPC register AX
	  pop	  ES		   ; restore caller's ES
sh_ret:   pop	  BP		   ; restore caller's BP
	  ret			   ; return
;     ***error-- input argument wasn't a symbol pointer***
sh_error: mov	  AX,-1		   ; return a hash value of -1
	  jmp	  short sh_ret	   ; return invalid hash value
sym_hash  endp

prog	  ends
	  end
