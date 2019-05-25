;							=====> SINTERP.ASM
;******************************************************************************
;*   TIPC Scheme '84 Runtime Support                                          *
;*	      Interpreter						      *
;*									      *
;*  (C) Copyright 1984,1985,1986,1987 by Texas						*
;*     Instruments Incorporated.					      *
;*	  All rights reserved.						      *
;*									      *
;* Date Written:  2 May 1984						      *
;* Last Modification:							      *
;*   11 Feb 86 - Replaced support for even? and odd? to reduce code size and  *
;*		 to update error messages.				      *
;*	       - Improved error handling for divide,quotient, and remainder.  *
;*	       - Fixed divide by zero error in Remainder function	      *
;*   21 Oct 86 - added an additional argument to %graphics - dbs	      *
;*    7 Nov 86 - %graphics accepts negative arguments (for clipping) - rb     *
;*    7 Jan 87 - added random I/O - dbs 				      *
;*   10 Feb 87 - added new opcode (186) for read-line - tc		      *
;*    8 Mar 87 - XLI - rb						      *
;*   16 Mar 87 - Added dos-err entry point for detection of Dos I/O errors.   *
;*   17 Feb 88 - Mods so sinterp will work in protected mode - tc	      *
;* 		  * Macros in SMMU.MAC allow stores into code segment	      *
;*		  * Graphics for pro mode moved to PROIO.ASM		      *
;*		  * %ESC function modified to look for sw-int and call 	      *
;*		    SOFTINT function in PRO2REAL.ASM			      *
;*		  * Timer interrupts no longer taken over for pro mode.       *
;*		    Engines work based on # vm instructions executed for pro  *
;*		    mode. Interpreter loop for engines included (eng_next1)   *
;*		    and settimer, rsttimer included here (conditionally of    *
;*		    course).						      *
;*									      *
;*									      *
;******************************************************************************

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; The following files are defined in smmu.equ but are split out here so
; that this module will assemble....

	  include schemed.equ
	  include schemed.ref
	  include schemed.mac
	  purge markedp,pushptr,popptr
	  include smmu.mac
	  purge %LoadPage,%LoadPage0,%LoadPage1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	  include sinterp.mac
	  include sinterp.arg
	  include pcmake.equ
	  include stackf.equ	   ; define stack frame format

XGROUP	  group   progx
progx	  segment word public 'progx'

IFDEF PROMEM
	  extrn	  softint:far	   ; interface for sw_int (see PRO2REAL.ASM)
ELSE
	  extrn	  graphit:far	   ; interface to graphics primitives
ENDIF
	  extrn   str_apnd:far	   ; substring append support
	  extrn   str_disp:far	   ; %substring-display support


progx	  ends

DGROUP	  group   data
data	  segment word public 'DATA'
	  assume  DS:DGROUP

	  extrn   page0:word

;     Primary opcode lookup table
op_table  dw	  copy		   ; 000- load	    dest,src
	  dw	  ld_const	   ; 001- ld-const  dest,constant-number (byte)
	  dw	  ld_imm	   ; 002- ld-imm    dest,immed-value (byte)
	  dw	  ld_nil	   ; 003- ld-nil    dest
	  dw	  PGROUP:ld_local  ; 004- ld-local  dest,entry-number (byte)
	  dw	  PGROUP:ld_lex    ; 005- ld-lex    dest,entry-no,delta-level
	  dw	  PGROUP:ld_env    ; 006- ld-env    R(dest),C(sym)
	  dw	  PGROUP:ld_globl  ; 007- ld-global dest,constant-number (byte)

	  dw	  PGROUP:ld_fluid  ; 008- ld-fluid  dest,constant-number (byte)
	  dw	  ld_off_s	   ; 009- ld-vec-s  vect,offset (byte)
	  dw	  ld_off_l	   ; 010- ld-vec-l  vect,offset (word)
	  dw	  ld_off_r	   ; 011- ld-vec-r  vect,offset (reg)
	  dw	  PGROUP:st_local  ; 012- st-local  src,entry-number (byte)
	  dw	  PGROUP:st_lex    ; 013- st-lex    src,entry-no,delta-level
	  dw	  PGROUP:st_env    ; 014- st-env    R(val),C(sym)
	  dw	  PGROUP:st_globl  ; 015- st-global src,constant-number (byte)

	  dw	  PGROUP:st_fluid  ; 016- st-fluid  src,constant-number (byte)
	  dw	  st_off_s	   ; 017- st-vec-s  vect,offset (byte),src
	  dw	  st_off_l	   ; 018- st-vec-l  vect,offset (word),src
	  dw	  st_off_r	   ; 019- st-vec-r  vect,offset (reg),src
	  dw	  PGROUP:set_car   ; 020- set-car!  dest,src
	  dw	  PGROUP:set_cdr   ; 021- set-cdr!  dest,src
	  dw	  recompil	   ; 022- (unused) formerly set-ref!
	  dw	  recompil	   ; 023- (unused) formerly swap-ref!

	  dw	  PGROUP:spop	   ; 024- pop	    dest
	  dw	  PGROUP:spush	   ; 025- push	    src
	  dw	  PGROUP:sdrop	   ; 026- drop	    count (unsigned byte)
	  dw	  PGROUP:ld_globr  ; 027- ld-global-r dest,sym
	  dw	  recompil	   ; 028- (unused- formerly push-heap)
	  dw	  PGROUP:bind_fl   ; 029- bind-fl   const,src
	  dw	  PGROUP:unbind_f  ; 030- unbind_fl count (byte)
	  dw	  PGROUP:define    ; 031- define!   src,const

	  dw	  jmp_shrt	   ; 032- jmp_s     label (byte)
	  dw	  jmp_long	   ; 033- jmp_l     label (word)
	  dw	  j_nil_s	   ; 034- jnil_s    reg,label (byte)
	  dw	  j_nil_l	   ; 035- jnil_l    reg,label (word)
	  dw	  j_nnil_s	   ; 036- jnnil_s   reg,label (byte)
	  dw	  j_nnil_l	   ; 037- jnnil_l   reg,label (word)
	  dw	  j_atm_s	   ; 038- jatom_s   reg,label (byte)
	  dw	  j_atm_l	   ; 039- jatom_l   reg,label (word)

	  dw	  j_natm_s	   ; 040- jnatom_s  reg,label (byte)
	  dw	  j_natm_l	   ; 041- jnatom_l  reg,label (word)
	  dw	  j_eq_s	   ; 042- jeq_s     reg,label (byte)
	  dw	  j_eq_l	   ; 043- jeq_l     reg,label (word)
	  dw	  j_neq_s	   ; 044- jneq_s    reg,label (byte)
	  dw	  j_neq_l	   ; 045- jneq_l    reg,label (word)
	  dw	  recompil	   ; 046- (unused) formerly deref
	  dw	  recompil	   ; 047- (unused) formerly ref

	  dw	  PGROUP:call_lcl  ; 048- call	    lbl,delta-level,delta-heap
	  dw	  PGROUP:call_ltr  ; 049- call-tr   lbl,delta-level,delta-heap
	  dw	  PGROUP:call_cc   ; 050- call/cc   lbl,delta-level,delta-heap
	  dw	  PGROUP:cl_cctr   ; 051- call/cc-tr lbl delta-level,delta-heap
	  dw	  PGROUP:call_clo  ; 052- call-cl   reg,number-args
	  dw	  PGROUP:call_ctr  ; 053- call-cl-tr reg,number-args
	  dw	  PGROUP:clcc_c    ; 054- call/cc-cl reg
	  dw	  PGROUP:clcc_ctr  ; 055- call/cc-cl-tr reg

	  dw	  PGROUP:apply	   ; 056- apply-cl  reg,arg
	  dw	  PGROUP:apply_tr  ; 057- apply-cl-tr reg,arg
	  dw	  PGROUP:execute   ; 058- execute   reg
	  dw	  PGROUP:s_exit    ; 059- exit
	  dw	  PGROUP:cr_close  ; 060- close     dest,label,number-args
	  dw	  PGROUP:drop_env  ; 061- drop-env  count
	  dw	  PGROUP:hash_env  ; 062- make-hashed-environment
	  dw	  PGROUP:ld_fl_r   ; 063- ld-fluid-r dest,sym

	  dw	  PGROUP:ld_car    ; 064- car	    dest,src
	  dw	  PGROUP:ld_cdr    ; 065- cdr	    dest,src
	  dw	  PGROUP:ld_caar   ; 066- caar	    dest,src
	  dw	  PGROUP:ld_cadr   ; 067- cadr	    dest,src
	  dw	  PGROUP:ld_cdar   ; 068- cdar	    dest,src
	  dw	  PGROUP:ld_cddr   ; 069- cddr	    dest,src
	  dw	  PGROUP:ld_caaar  ; 070- caaar     dest,src
	  dw	  PGROUP:ld_caadr  ; 071- caadr     dest,src

	  dw	  PGROUP:ld_cadar  ; 072- cadar     dest,src
	  dw	  PGROUP:ld_caddr  ; 073- caddr     dest,src
	  dw	  PGROUP:ld_cdaar  ; 074- cdaar     dest,src
	  dw	  PGROUP:ld_cdadr  ; 075- cdadr     dest,src
	  dw	  PGROUP:ld_cddar  ; 076- cddar     dest,src
	  dw	  PGROUP:ld_cdddr  ; 077- cdddr     dest,src
	  dw	  PGROUP:ld_caddd  ; 078- cadddr    dest,src
	  dw	  PGROUP:s_cons    ; 079- cons	    dest,car,cdr

	  dw	  add		   ; 080- add	    dest,src
	  dw	  addi		   ; 081- add-imm   dest,imm (signed byte)
	  dw	  sub		   ; 082- sub	    dest,src
	  dw	  mul		   ; 083- mul	    dest,src
	  dw	  muli		   ; 084- mul-imm   dest,imm (signed byte)
	  dw	  div		   ; 085- div	    dest,src
	  dw	  divi		   ; 086- div-imm   dest,imm (signed byte)
	  dw	  quo		   ; 087- quotient  dest,src **integers only**

	  dw	  modulo	   ; 088- remainder dest,src
	  dw	  PGROUP:ld_car1   ; 089- %car	    src=dest
	  dw	  PGROUP:ld_cdr1   ; 090- %cdr	    src=dest
	  dw	  random	   ; 091- %random   dest
	  dw	  lt_p		   ; 092- <	    dest,src
	  dw	  le_p		   ; 093- <=	    dest,src
	  dw	  eq_n		   ; 094- =	    dest,src
	  dw	  gt_p		   ; 095- >	    dest,src

	  dw	  ge_p		   ; 096- >=	    dest,src
	  dw	  ne_p		   ; 097- <>	    dest,src
	  dw	  maximum	   ; 098- max	    dest,src
	  dw	  minimum	   ; 099- min	    dest,src
	  dw	  eq_p		   ; 100- eq?	    dest,src
	  dw	  eqv_p 	   ; 101- eqv?	    dest,src
	  dw	  equal_p	   ; 102- equal?    dest,src
	  dw	  PGROUP:memq	   ; 103- memq	    dest,src

	  dw	  PGROUP:memv	   ; 104- memv	    dest,src
	  dw	  PGROUP:member    ; 105- member    dest,src
	  dw	  reverseb	   ; 106- reverse!  list
	  dw	  not_yet	   ; 107- reverse   list
	  dw	  PGROUP:assq	   ; 108- assq	    obj,list
	  dw	  PGROUP:assv	   ; 109- assv	    obj,list
	  dw	  PGROUP:assoc	   ; 110- assoc     obj,list
	  dw	  PGROUP:s_list    ; 111- list	    obj

	  dw	  PGROUP:appendb   ; 112- append!   list,obj
	  dw	  append	   ; 113- append    list,obj
	  dw	  not_yet	   ; 114- delq!     obj,list
	  dw	  not_yet	   ; 115- delete!   obj,list
	  dw	  getprop	   ; 116- get-prop  name,prop
	  dw	  putprop	   ; 117- put-prop  name,val,prop
	  dw	  proplist	   ; 118- proplist  name
	  dw	  remprop	   ; 119- remprop   name,prop

	  dw	  PGROUP:list2	   ; 120- list2     dest=src1,src2
	  dw	  not_yet	   ; 121- list-ref  dest=src1,src2
	  dw	  PGROUP:l_tail	   ; 122- list-tail dest,count
	  dw	  not_op	   ; 123- (unused)
	  dw	  not_op	   ; 124- (unused)
	  dw	  b_xor 	   ; 125- bitwise-xor dest=src1,src2
	  dw	  b_and 	   ; 126- bitwise-and dest=src1,src2
	  dw	  b_or		   ; 127- bitwise-or  dest=src1,src2


;     Note:  the second half of the opcodes are "second class" opcodes,
;	in that TIPC register BH will not be zero at the entry to the
;	support routine.  For the following instructions, BH will
;	contain the value one (1).

	  dw	  atom_p	   ; 128- atom?     dest
	  dw	  closur_p	   ; 129- closure?  dest
	  dw	  code_p	   ; 130- code?     dest
	  dw	  contin_p	   ; 131- continuation? dest
	  dw	  even_p	   ; 132- even?     dest
	  dw	  float_p	   ; 133- float?    dest
	  dw	  PGROUP:fluid_p   ; 134- fluid-bound? dest
	  dw	  integr_p	   ; 135- integer?  dest

	  dw	  null_p	   ; 136- null?     dest
	  dw	  number_p	   ; 137- number?   dest
	  dw	  odd_p 	   ; 138- odd?	    dest
	  dw	  pair_p	   ; 139- pair?     dest
	  dw	  port_p	   ; 140- port?     dest
	  dw	  proc_p	   ; 141- proc?     dest
	  dw	  recompil	   ; 142- (unused) formerly ref?
	  dw	  string_p	   ; 143- string?   dest

	  dw	  symbol_p	   ; 144- symbol?   dest
	  dw	  vector_p	   ; 145- vector?   dest
	  dw	  eq_z_p	   ; 146- zero?     dest
	  dw	  lt_z_p	   ; 147- negative? dest
	  dw	  gt_z_p	   ; 148- positive? dest
	  dw	  sabs		   ; 149- abs	    dest
	  dw	  float 	   ; 150- float     dest
	  dw	  minus 	   ; 151- minus     dest

	  dw	  sfloor	   ; 152- floor     dest
	  dw	  sceiling	   ; 153- ceiling   dest
	  dw	  struncat	   ; 154- truncate  dest
	  dw	  sround	   ; 155- round     dest
	  dw	  char_p	   ; 156- char?     dest
	  dw	  PGROUP:env_p	   ; 157- env?	    dest
	  dw	  not_op	   ; 158- (unused)
	  dw	  not_op	   ; 159- (unused)

	  dw	  asc_char	   ; 160- asc->char reg
	  dw	  char_asc	   ; 161- char->asc reg
	  dw	  recompil	   ; 162- (unused) formerly gensym
	  dw	  not_op	   ; 163- (unused)
	  dw	  not_op	   ; 164- (unused)
	  dw	  slength	   ; 165- length  list
	  dw	  lst_pair	   ; 166- last-pair list
	  dw	  substr	   ; 167- substr  str,pos,len

	  dw	  PGROUP:vec_allo  ; 168- alloc-vec dest
	  dw	  PGROUP:vec_size  ; 169- vect-length dest
	  dw	  PGROUP:vec_fill  ; 170- vect-fill vect,val
	  dw	  not_yet	   ; 171- make-pack-vect len,bits/elem,signed?
	  dw	  s_disply	   ; 172- %substr-display str,start,end,row,wind
	  dw	  not_op	   ; 173- (unused)
	  dw	  set_tim	   ; 174- %start-timer	src=ticks
	  dw	  rst_tim	   ; 175- %stop-timer	dest=ticks remaining

	  dw	  popen 	   ; 176- open-port	filename,mode
	  dw	  pclose	   ; 177- close-port	port
	  dw	  PGROUP:spprin1   ; 178- prin1 	obj,port
	  dw	  PGROUP:spprinc   ; 179- princ 	obj,port
	  dw	  PGROUP:spprint   ; 180- print 	obj,port
	  dw	  PGROUP:spnewlin  ; 181- newline	port
	  dw	  recompil	   ; 182- (unused) formerly read
	  dw	  recompil	   ; 183- (unused) formerly file-exists?

	  dw	  PGROUP:prt_len   ; 184- print-length	obj
	  dw	  recompil	   ; 185- (unused) formerly current-column
	  dw	  PGROUP:srd_line  ; 186- read-line	dest=src (src={port})
	  dw	  PGROUP:srd_atom  ; 187- read-atom	dest=src (src={port})
	  dw	  PGROUP:read_cha  ; 188- read-char	dest=src
	  dw	  PGROUP:trns_chg  ; 189- %transcript	src
	  dw	  PGROUP:rd_ch_rd  ; 190- read-char-ready? dest=src
	  dw	  sfasl 	   ; 191- fasl	  string

	  dw	  PGROUP:ch_eq_p   ; 192- char= 	char1,char2
	  dw	  PGROUP:ch_eq_ci  ; 193- char-equal?	char1,char2
	  dw	  PGROUP:ch_lt_p   ; 194- char< 	char1,char2
	  dw	  PGROUP:ch_lt_ci  ; 195- char-less?	char1,char2
	  dw	  PGROUP:ch_up	   ; 196- char-upcase	char
	  dw	  PGROUP:ch_down   ; 197- char-downcase char
	  dw	  str_lng	   ; 198- string-length string
	  dw	  PGROUP:st_ref    ; 199- string-ref	string,index

	  dw	  PGROUP:st_set    ; 200- string-set!	string,index,char
	  dw	  PGROUP:make_str  ; 201- make-string	length,char
	  dw	  PGROUP:str_fill  ; 202- string-fill!	string,char
	  dw	  str2sym	   ; 203- string->symbol string
	  dw	  str2usym	   ; 204- string->uninterned-symbol string
	  dw	  sym2str	   ; 205- symbol->string symbol
	  dw	  srch_nx	   ; 206- srch-next	str,start,end,charset
	  dw	  srch_pr	   ; 207- srch-prev	str,start,end,charset

	  dw	  PGROUP:make_win  ; 208- make-window	label
	  dw	  set_w_at	   ; 209- set-wind-attr wind,attr,value
	  dw	  PGROUP:get_wind  ; 210- get-wind-attr wind,attr
	  dw	  clr_wind	   ; 211- clear-window	wind
	  dw	  PGROUP:save_win  ; 212- save-window	wind
	  dw	  PGROUP:rest_win  ; 213- restore-wind	wind
	  dw	  s_append	   ; 214- %str-append	R(d=s1),R(s2),...,R(s7)
	  dw	  PGROUP:sgraph	   ; 215- %graphics	R(s1),R(s2),...,R(s7)

	  dw	  sreify	   ; 216- %reify	R(s1=d),R(s2) ;obj,indx
	  dw	  PGROUP:mk_env    ; 217- mk-env	R(d)
	  dw	  PGROUP:env_par   ; 218- env-par	R(d=s1) ; s1=env
	  dw	  PGROUP:env_lu    ; 219- env-lu	R(d=s1),R(s2) ; sym,env
	  dw	  PGROUP:def_env   ; 220- def-env	R(d=s1),R(s2),R(s3) sve
	  dw	  PGROUP:push_env  ; 221- push-env	C(s1) ; s1=list of syms
	  dw	  PGROUP:drop_env  ; 222- drop-env
	  dw	  PGROUP:ld_env    ; 223- ld-env	R(d),C(s1) ; s1=symbol

	  dw	  PGROUP:st_env    ; 224- st-env	R(d=s1),C(s2) ; val,sym
	  dw	  PGROUP:set_gnv   ; 225- set-glob-env! R(s1) ; s1=env
	  dw	  sreifyb	   ; 226- %reify!	R(s1),R(s2),R(s3);o,i,v
	  dw	  obj_hash	   ; 227- object-hash	R(d=s1)
	  dw	  obj_unhs	   ; 228- object-unhash R(d=s1)
	  dw	  reify_s	   ; 229- reify-stack	R(d=s1)
	  dw	  reify_sb	   ; 220- reify-stack!	R(s1),R(s2)
	  dw	  sfpos 	   ; 231- set-file-position!

	  dw	  s_esc1	   ; 232- %esc1 	R(d=s1)
	  dw	  s_esc2	   ; 233- %esc2 	R(d=s1),R(s2)
	  dw	  s_esc3	   ; 234- %esc3 	R(d=s1),R(s2),R(s3)
	  dw	  s_esc4	   ; 235- %esc4 	R(d=s1),R(s2),...,R(s4)
	  dw	  s_esc5	   ; 236- %esc5 	R(d=s1),R(s2),...,R(s5)
	  dw	  s_esc6	   ; 237- %esc6 	R(d=s1),R(s2),...,R(s6)
	  dw	  s_esc7	   ; 238- %esc7 	R(d=s1),R(s2),...,R(s7)
	  dw	  xesc		   ; 239- %xesc 	R(d=s1),R(len),
				   ;			R(arg1),...,R(arg16);
				   ;			all R(argn) are optional

	  dw	  not_op	   ; 240- (unused)
	  dw	  not_op	   ; 241- (unused)
	  dw	  not_op	   ; 242- (unused)
	  dw	  not_op	   ; 243- (unused)
	  dw	  not_op	   ; 244- (unused)
	  dw	  not_op	   ; 245- (unused)
	  dw	  not_op	   ; 246- (unused)
	  dw	  sgc2		   ; 247- gc-with-compaction

	  dw	  exit_op	   ; 248- halt=(exit) [return to MS-DOS]
	  dw	  gc		   ; 249- %garbage-collect
	  dw	  ptyme 	   ; 250- %internal-time  dest
	  dw	  reset 	   ; 251- reset
	  dw	  s_reset	   ; 252- scheme-reset
	  dw	  clr_regs	   ; 253- %clear-registers
	  dw	  not_op	   ; 254- (reserved for escape)
	  dw	  debug_op	   ; 255- %begin-debug

reset_BP  dw	  0		   ; initial value of BP for reset purposes

zero_reg  dw	  0,SPECFIX*2	   ; a "register" containing a fixnum 0
zero_adr  dw	  zero_reg	   ; the address of "zero_reg" (for pushing)
m_one	  dw	  1		   ; a constant "one" (1)
m_zerodv  dw	  ZERO_DIVIDE_ERROR ; error code for division by zero

m_not_op  db	  "[VM INTERNAL ERROR] Undefined opcode",LF,0
m_cod_er  db	  "[VM INTERNAL ERROR] %x:%04x isn't a code base",LF,0
m_not_yt  db	  "[VM INTERNAL ERROR] Feature not implemented",LF,0
m_recomp  db	  "[VM ERROR encountered!] Object module incompatible with "
	  db	  "this Version",LF,"Recompile from Source",LF,0

;;;m_il_st   db   "[VM ERROR encountered!] VECTOR-SET! operand is write "
;;;	  db	  "protected",LF,0
;;;m_deref   db   "DEREF",0
m_ld_r	  db	  "LD_R",0
m_st_r	  db	  "ST_R",0
;;;m_setref  db   "SET_REF!",0
;;;m_swaprf  db   "SWAP_REF!",0
m_revb	  db	  "REVERSE!",0
m_even	  db	  "EVEN?",0
m_odd	  db	  "ODD?",0
m_v_ld	  db	  "VECTOR-REF",0
m_v_st	  db	  "VECTOR-SET!",0
m_DIV	  db	  "/",0
m_MODULO  db	  "REMAINDER",0
m_QUOTNT  db	  "QUOTIENT",0
m_VOE	  dw	  VECTOR_OFFSET_ERROR ; error number for "offset out of range"
masc_ch   db	  "INTEGER->CHAR",0
mch_asc   db	  "CHAR->INTEGER",0
m_bckwrd  db	  "[VM INTERNAL ERROR] sinterp: instruction preceding %x:%04x "
	  db	  "set direction flag",LF,0
m_reg0	  db	  "[VM INTERNAL ERROR] sinterp: instruction preceding %x:%04x "
	  db	  "clobbered register",LF,0
;;;m_bad_st  db   "[VM INTERNAL ERROR] sinterp: instruction preceding %x:%04x "
;;;	  db	  "screwed up the stack",LF,0
IFNDEF PROMEM
m_graph   db	  "%GRAPHICS",0
ENDIF
m_esc	  db	  "%ESCN",0

; XLI errors (numbered from 1, not 0)
xli_err   dw	  0				 ;this spot unused
	  dw	  0,xli_err2			 ;other 0's no longer used spots
	  dw	  0,xli_err4,xli_err5,xli_err6
	  dw	  xli_err7,xli_err8,xli_err9,xli_err10,xli_err11,xli_err12
	  dw	  xli_err13,xli_err14,xli_err15,xli_err16
; XLI fatal errors print via print_and_exit
;xli_err1  db	   '[VM FATAL ERROR] Unable to determine length of %XESC VM instruction',LF,0
; XLI normal errors print via sch_err as secondary line to [VM ERROR ...] message
xli_err2  db	  '[XLI] First argument to XCALL is not string or symbol',0
;xli_err3  db	   '[XLI] Improper number of arguments given to XCALL',0
xli_err4  db	  '[XLI] An argument to XCALL is invalid',0
xli_err5  db	  '[XLI] The return value of XCALL is invalid',0
xli_err6  db	  '[XLI] The function requested by XCALL is not available',0
xli_err7  db	  '[XLI] Number too large to fit in 32 bits',0
xli_err8  db	  '[XLI] Sync error',0
xli_err9  db	  '[XLI] Error in releasing memory of external program',0
xli_err10 db	  '[XLI] No memory is available for external program',0
xli_err11 db	  '[XLI] Error in loading external program',0
xli_err12 db	  '[XLI] No more external programs can be loaded',0
xli_err13 db	  '[XLI] File to load not found',0
xli_err14 db	  '[XLI] Number too large to fit in 16 bits',0
xli_err15 db      '[XLI] Version mismatch',0
xli_err16 db      '[XLI] Error reported by external program',0

IFDEF PROMEM
;
; Following definitions are for protected mode engines. They will be
; used in eng_next1, settimer, and rsttimer defined later in this
; module.
;
tickstat  db	-1	;status of engine (0=timeout,1=running,-1=none)
lo_time	  dw	?	;timer ticks (per vm instuction executed)
hi_time   dw	?
ENDIF

data	  ends


PGROUP	  group   prog
prog	  segment byte public 'PROG'
	  assume  CS:PGROUP

;     Interpreter support routines defined in "scar_cdr.asm"
	  extrn   ld_car:near
	  extrn   ld_cdr:near
	  extrn   ld_car1:near
	  extrn   ld_cdr1:near
	  extrn   ld_caar:near
	  extrn   ld_cadr:near
	  extrn   ld_cdar:near
	  extrn   ld_cddr:near
	  extrn   ld_caaar:near
	  extrn   ld_caadr:near
	  extrn   ld_cadar:near
	  extrn   ld_caddr:near
	  extrn   ld_cdaar:near
	  extrn   ld_cdadr:near
	  extrn   ld_cddar:near
	  extrn   ld_cdddr:near
	  extrn   ld_caddd:near
	  extrn   set_car:near
	  extrn   set_cdr:near
	  extrn   s_cons:near
	  extrn   s_list:near	   ; (list obj)
	  extrn   list2:near	   ; (list a b)
	  extrn   appendb:near	   ; (append! a b)
	  extrn   l_tail:near	   ; (list-tail list count)

;     Interpreter support routines defined in "sstack.asm"
	  extrn   set_pos:far	   ; set-file-position!

;     Interpreter support routines defined in "sstack.asm"
	  extrn   spush:near	   ; push register contents onto Scheme stack
	  extrn   spop:near	   ; pop into register from Scheme stack
	  extrn   sdrop:near	   ; drop elements from top of Scheme stack
	  extrn   ld_local:near    ; load from local stack frame
	  extrn   st_local:near    ; store into local stack frame
	  extrn   ld_lex:near	   ; load from higher level stack frame
	  extrn   st_lex:near	   ; store into higher level stack frame
	  extrn   call_lcl:near    ; local call
	  extrn   call_ltr:near    ; local call, tail recursive
	  extrn   call_clo:near    ; call closure object
	  extrn   call_ctr:near    ; call closure object, tail recursive
	  extrn   call_cc:near	   ; local call/cc
	  extrn   cl_cctr:near	   ; local call/cc, tail recursive
	  extrn   clcc_c:near	   ; call/cc, closure object
	  extrn   clcc_ctr:near    ; call/cc, closure object, tail recursive
	  extrn   apply:near	   ; apply closure object
	  extrn   apply_tr:near    ; apply closure object, tail recursive
	  extrn   execute:near	   ; execute code block
	  extrn   s_exit:near	   ; exit procedure
	  extrn   cr_close:near    ; create closure

	  extrn   force_ca:near    ; entry point to force call (to debugger)

;     Interpreter support routines defined in "svars.asm"
	  extrn   ld_fluid:near    ; load value of fluid variable
	  extrn   ld_fl_r:near	   ; load value of fluid variable - reg source
	  extrn   st_fluid:near    ; store value into fluid variable
	  extrn   bind_fl:near	   ; bind fluid variable
	  extrn   unbind_f:near    ; unbind fluid variables
	  extrn   fluid_p:near	   ; fluid-bound? predicate
	  extrn   vec_allo:near    ; allocate vector
	  extrn   vec_size:near    ; vector-size
	  extrn   vec_fill:near    ; vector-fill
	  extrn   memq:near	   ; memq
	  extrn   memv:near	   ; memv
	  extrn   member:near	   ; member
	  extrn   assq:near	   ; assq
	  extrn   assv:near	   ; assv
	  extrn   assoc:near	   ; assoc

;     Interpreter support routines defined in "sstring.asm"
	  extrn   ch_eq_p:near	   ; (char= char1 char2)
	  extrn   ch_eq_ci:near    ; (char-equal? char1 char2)
	  extrn   ch_lt_p:near	   ; (char< char1 char2)
	  extrn   ch_lt_ci:near    ; (char-less? char1 char2)
	  extrn   ch_up:near	   ; (char-upcase char)
	  extrn   ch_down:near	   ; (char-downcase char)
	  extrn   make_str:near    ; (make-string len char)
	  extrn   str_fill:near    ; (string-fill! string char)
	  extrn   st_ref:near	   ; (string-ref string index)
	  extrn   st_set:near	   ; (string-set! string index char)

;     Interpreter support routines defined in "senv.asm"
	  extrn   env_p:near	   ; (environment? obj)
	  extrn   mk_env:near	   ; (make-environment)
	  extrn   env_par:near	   ; (environment-parent env)
	  extrn   env_lu:near	   ; (environment-lookup sym env)
	  extrn   def_env:near	   ; (define symbol value env)
	  extrn   push_env:near    ; (push-environment list)
	  extrn   drop_env:near    ; (drop-environment)
	  extrn   hash_env:near    ; (make-hashed-environment)
	  extrn   ld_env:near	   ; (load-env symbol)
	  extrn   st_env:near	   ; (store-env value symbol)
	  extrn   set_gnv:near	   ; (set-global-env! env)
	  extrn   ld_globl:near    ; load value of global variable
	  extrn   ld_globr:near    ; load value of global variable - reg source
	  extrn   st_globl:near    ; store value into global variable
	  extrn   define:near	   ; define! value for global variable

;     Interpreter support routines defined in "sobjhash.asm"
	  extrn   obj_hash:near    ; (object-hash obj)
	  extrn   obj_unhs:near    ; (object-unhash obj)

;     Interpreter support routines defined in "cwindow.asm"
	  extrn   make_win:near    ; (make-window label)
	  extrn   get_wind:near    ; (get-window-attribute port attribute)
	  extrn   save_win:near    ; (window-save-contents port)
	  extrn   rest_win:near    ; (window-restore-contents port contents)
	  extrn   trns_chg:near    ; (transcript-on "filename")
	  extrn   rd_ch_rd:near    ; (read-char-ready? port)
	  extrn   read_cha:near    ; (read-char port)

;     Interpreter support routines defined in "cread.asm"
	  extrn   srd_line:near    ; (read-line port)
	  extrn   srd_atom:near    ; (read-atom port)

;     Interpreter support routines defined in "cprint.asm"
	  extrn   spprin1:near
	  extrn   spprinc:near
	  extrn   spprint:near
	  extrn   spnewlin:near
	  extrn   prt_len:near

;     XLI
	  extrn   xli_xesc:near    ; XLI xesc handler
;	  extrn   print_an:near    ; fatal errors

IFDEF PROMEM
;     GRAPHICS - protected mode scheme graphics handler in PROIO.ASM
	  extrn	  sgraph:near	   ;Handle %graphics primitives
ENDIF

;     Entry point to force debug mode prior to next VM instruction
	  public  force_de
force_de: mov	  AX,word ptr CS:trc_forc
	  STORE_WORD_IN_CS PROG,next1,AX 	; Protected Mode Macro
	  ret

IFNDEF PROMEM
;     Entry point to force a timeout prior to next VM instruction. This
;     will be called from the tick routine in STIMER.ASM. Protected
;     mode scheme doesn't use this, as it counts vm instructions as
;     engine ticks.
;
	  public  force_ti
force_ti: mov	  AX,word ptr CS:tim_forc
	  XCHG_WORD_IN_CS PROG,next1,AX		;Protected Mode Macro
	  STORE_WORD_IN_CS PROG,reset_tim,AX	;Protected Mode Macro
	  ret
ENDIF

;     Entry point to process shift-break prior to next VM instruction
	  public  shft_brk
dbg_addr  dw	  VM_debug	   ; address of the variable VM_debug
	  dw	  DGROUP	   ; DGROUP segment address
sbrk_adr  dw	  s_break	   ; address of the variable s_break
reset_sb  dw	  0
shft_brk: push	  ES		            ; save the current ES
	  les	  SI,dword ptr CS:dbg_addr  ; load the long address for VM_debug
	  mov	  DI,CS:sbrk_adr            ; load address for s_break
	  inc	  word ptr ES:[DI]          ;  and increment shift-break flag
	  cmp	  word ptr ES:[SI],0        ; are we in VM_debug mode?
	  pop	  ES		            ; restore ES
	  jne	  force_de	            ; if we're in VM_debug mode, jump
	  mov	  AX,word ptr CS:shft_nxt   ;  else, force a trap to the debugger
	  cmp	  AX,word ptr CS:next1      ; Shift-Brk already depressed?
	  je	  shft_brt	            ; if a duplicate request, skip it
	  XCHG_WORD_IN_CS PROG,next1,AX	    ;  else enter scheme debugger on
	  STORE_WORD_IN_CS PROG,reset_sb,AX ;       next vm instruction
shft_brt: ret			   	    ; continue processing

	  public  run,interp
run	  proc	  near
	  mov	  AX,word ptr CS:next_go1   ; modify interpreter loop to disable
	  STORE_WORD_IN_CS PROG,next1,AX    ; instruction level trace capability
interp:   push	  BP
	  sub	  SP,offset sint_BP
	  mov	  BP,SP
	  mov	  reset_BP,BP	   ; save initial value of BP for reset

;     Set up initial interpreter parameters
	  mov	  [BP].C_ES,ES
	  mov	  SI,[BP].cod_ent  ; load address of entry point offset
	  mov	  SI,[SI]	   ;  and load PC
	  mov	  BX,CB_pag	   ; load code base page number
	  cmp	  ptype+[BX],CODETYPE*2 ; if page doesn't contain code,
	  jne	  code_err	   ;  we've got an error
	  LoadCode ES,BX	   ; load code page paragraph addr
;	  mov	  ES,pagetabl+[BX] ; load code page paragraph addr
	  mov	  [BP].save_ES,ES  ;  and save it off
	  jmp	  next		   ; jump to start of interpreter
;     ***error-- invalid code base-- not a code page***
code_err:			   ; push the ptr's disp, page no, and
	  mov	  AX,offset DGROUP:m_cod_er ; address of message
	  pushm   <CB_dis,BX,AX>
	  C_call  printf	   ; print error message
	  jmp	  debug 	   ; begin debug mode

trc_oops: cld			   ; clear direction * checking code
	  lea	  BX,m_bckwrd	   ;		      *  (see below)
	  jmp	  short trc_err    ;		     *

trc_reg0: lea	  BX,m_reg0
trc_err:  mov	  AX,CB_pag	   ; R0 not nil-- print error message
	  corrpage AX
	  pushm   <SI,AX,BX>
	  C_call  printf,<SI>,Load_ES
	  restore <SI>
	  jmp	  debug

;**bad_stk:
;**	  lea	  BX,m_bad_st	   ; inconsistent runtime stack error
;**	  jmp	  short trc_err

next_tr1:
	  dec	  [BP].no_insts    ; decrement count of instructions to run
	  jge	  next_go	   ; if not zero, continue decoding
	  jmp	  exit		   ; out of instructions-- return to debugger
next_go1  equ	  $
next_go:  xor	  AX,AX 	   ; Clear high order byte of AX

	  mov	  BX,SI 	   ;*	These instructions check to make
	  xor	  SI,SI 	   ;  * sure that the direction flag is set
	  lodsb 		   ;  * in the forward direction.  If not,
	  cmp	  SI,1		   ;  * the "lods" in the interpreter will
	  mov	  SI,BX 	   ;  * decrement the location pointer
	  jne	  trc_oops	   ;*	instead of incrementing it.

	  cmp	  reg0_pag,NIL_PAGE*2 ;*
	  jne	  trc_reg0	      ;  * These instructions check to
	  cmp	  reg0_dis,NIL_DISP   ;  * make sure R0 contains nil (by
	  jne	  trc_reg0	      ;*   convention)

	  cmp	  page0,0	   ;*
	  jne	  trc_reg0	   ; *
	  cmp	  page0+2,0	   ; * Verify that the location for
	  jne	  trc_reg0	   ; * the null pointer (page 0, offset 0)
	  cmp	  page0+4,0	   ; * is still (cons '() '())
	  jne	  trc_reg0	   ;*

;     Validate the contents of each of the Scheme registers
	  mov	  CX,NUM_REGS+4    ; load number of regsiter into CX (counter)
;     Note: also checks GNV_reg, FNV_reg, CB_reg, and tmp_reg
	  mov	  DI,offset reg0   ; address of register 0
	  mov	  DX,nextpage	   ; load number of pages allocated
more_reg: mov	  AX,[DI].C_page   ; load page number field of next register
	  cmp	  AX,SPECFIX*2	   ; does register contain a fixnum?
	  je	  off_ok	   ; if so, skip offset check (jump)
	  cmp	  AX,SPECCHAR*2    ; does register contain a character?
	  je	  off_ok	   ; if so, skip offset check (jump)
	  mov	  BX,AX 	   ; copy page number (times 2) into BX
	  ror	  AX,1		   ; divide by 2, LSB to sign position
	  cmp	  AX,DX 	   ; is page number too large?
	  jae	  trc_reg0	   ; if too large or odd, error (jump)
	  mov	  AX,[DI].C_disp   ; load displacement field from register
	  cmp	  AX,psize+[BX]    ; is offset too big?
	  jae	  trc_reg0	   ; if offset too big, error (jump)
off_ok:   add	  DI,size C_ptr    ; increment register offset
	  loop	  more_reg	   ; continue testing all registers

;**   Test consistency of Scheme's runtime stack
;**	  mov	  BX,FP 	   ; load current stack frame pointer
;**	  cmp	  BX,0
;**	  je	  stk_ok
;**more_stk:
;**	  mov	  AL,S_stack+[BX]  ; load return address code base page number
;**	  mov	  DI,AX
;**	  cmp	  byte ptr ptype+[DI],CODETYPE*2 ; is this a code block?
;**	  jne	  bad_stk	   ; if not, bad dynamic link
;**	  cmp	  byte ptr S_stack+[BX]+6,SPECFIX*2 ; is dynamic link a fixnum?
;**	  jne	  bad_stk	   ; if not, bad dynamic link
;**	  mov	  BX,word ptr S_stack+[BX]+7 ; load pointer to caller's FP
;**	  sub	  BX,BASE	   ; inside current stack buffer?
;**	  ja	  more_stk	   ; if so, continue testing (jump)
;**stk_ok:

	  xor	  AX,AX 	   ; clear TIPC register AX
	  lods	  byte ptr ES:[SI] ; Fetch next instruction's opcode
	  mov	  BX,AX
	  shl	  BX,1		   ; Multiply opcode by two for use as index
	  jmp	  op_table+[BX]


trc_go	  equ	  $
	  jmp	  short $+(next_trc-next1) ; jump to overwrite "next" for debug
next_trc: jmp	  next_tr1

tim_forc  equ	  $
	  jmp	  short $+(next_tim-next1) ; jump to force debug mode
next_tim: jmp	  timeout	   ; Force execution into debug mode

trc_forc  equ	  $
	  jmp	  short $+(next_dbg-next1) ; jump to force debug mode
next_dbg: jmp	  debug 	   ; Force execution into debug mode

shft_nxt  equ	  $
	  jmp	  short $+(next_sb-next1) ; jump to force Scheme debug mode
next_sb : jmp	  sc_debug	   ; Force execution into Scheme debug mode

IFDEF PROMEM
;
; The following code is for use by engines under protected mode scheme.
; We had a problem collecting timer interrupts from AI Architects OSx86,
; so I just implemented a different interpreter loop which decrements
; a timer tick upon each vm instruction.
;
; Note: this code must be within 128 bytes of next1 (below) so that a
;       short jump can be performed.

eng_tick  equ $
	  jmp	  short $+(eng_next1-next1) ; jump to engine loop

eng_next1:
	  sub	  lo_time,1		   ;decrement engine tick
	  sbb	  hi_time,0		   ;if not zero
	  jnz	  eng_next2		   ; continue
	  cmp	  lo_time,0
	  jnz	  eng_next2
	  mov	  tickstat,0		   ;zero counter, record timeout
	  jmp	  timeout		   ;force timeout condition
eng_next2:
	  xor	  ax,ax 	           ;clear high order byte of ax
	  lods	  byte ptr es:[si]         ;fetch next instruction's opcode
	  mov	  bx,ax			    
	  shl	  bx,1		           ;make into index
	  jmp	  op_table+[bx]            ;go execute the vm instruction code
ENDIF

;
; Following is the main vm interpreter loop. Note that the location at
; next1 can (and will be) code modified to jump into the debugger, a
; trace loop, and a loop for handling engines in protected mode.
;
	  public  next_SP,next_PC,next
next_SP:  mov	  SP,BP 	   ; Restore SP after call
next_PC:  les	  SI,dword ptr [BP].save_SI  ; Reload interpreter's PC & ES
next:
next1	  equ	  $
	  xor	  AX,AX 	   ; Clear high order byte of AX
	  lods	  byte ptr ES:[SI] ; Fetch next instruction's opcode
	  mov	  BX,AX
	  shl	  BX,1		   ; Multiply opcode by two for use as index
	  jmp	  op_table+[BX]    ; go execute the vm instruction code


;     Jump if nil, short	JNILS	reg,offset
j_nil_s:  lods	  word ptr ES:[SI] ; load operand, offset
	  mov	  BL,AL 	   ; copy register number
	  cmp	  byte ptr reg0_pag+[BX],0 ; test for null pointer
	  jne	  next		   ; Jump if not nil
	  mov	  AL,AH
	  cbw			   ; Sign extend short displacement
	  add	  SI,AX 	   ; Add jump offset to current PC
	  jmp	  next		   ; Return to interpreter

;     Jump if not nil, short	JNNILS	reg,offset
j_nnil_s: lods	  word ptr ES:[SI] ; load operand, offset
	  mov	  BL,AL 	   ; copy register number
	  cmp	  byte ptr reg0_pag+[BX],0 ; test for null pointer
	  je	  next		   ; Jump if nil
	  mov	  AL,AH
	  cbw			   ; Sign extend short displacement
	  add	  SI,AX 	   ; Add jump offset to current PC
	  jmp	  next		   ; Return to interpreter

;     Jump if atom,short	JATOMS	reg,offset
j_atm_s:  lods	  word ptr ES:[SI] ; Load register, offset
	  mov	  BL,AL 	   ; copy register number to test
	  test	  attrib+[BX],ATOM ; test for atom attribute
	  jz	  next		   ; if not atom, return to interpreter
	  mov	  AL,AH 	   ; position branch offset and
	  cbw			   ;  sign extend to 16 bits
	  add	  SI,AX 	   ; add jump offset to current PC
	  jmp	  next		   ; return to interpreter

;     Jump if not atom,short	JNATOMS reg,offset
j_natm_s: lods	  word ptr ES:[SI] ; Load register, offset
	  mov	  BL,AL 	   ; copy register number to test
	  test	  attrib+[BX],ATOM ; test for atom attribute
	  jnz	  next		   ; if atom, return to interpreter
	  mov	  AL,AH 	   ; position branch offset and
	  cbw			   ;  sign extend to 16 bits
	  add	  SI,AX 	   ; add jump offset to current PC
	  jmp	  next		   ; return to interpreter

;     Jump if eq?, short	JEQS	src1,src2,offset
j_eq_s:   lods	  word ptr ES:[SI] ; load registers to compare
	  mov	  BL,AH
	  mov	  DI,BX
	  add	  DI,offset reg0   ; compute address of src2
	  mov	  BL,AL 	   ; copy src1 register number
	  lods	  byte ptr ES:[SI] ; load branch displacement,
	  cbw			   ;  sign extend,
	  mov	  CX,AX 	   ;  and save it
	  mov	  AX,reg0_dis+[BX]
	  cmp	  AX,[DI].C_disp   ; are displacements eq?
	  jne	  next
j_eq_s1:  mov	  AL,byte ptr reg0_pag+[BX]
	  cmp	  AL,byte ptr [DI].C_page ; are page numbers eq?
	  jne	  j_eq_nxt
	  add	  SI,CX 	   ; add offset to current PC
j_eq_nxt: jmp	  next

;     Jump if not eq?, short	JNEQS	src1,src2,offset
j_neq_s:  lods	  word ptr ES:[SI] ; load registers to compare
	  mov	  BL,AH
	  mov	  DI,BX
	  add	  DI,offset reg0   ; compute address of src2
	  mov	  BL,AL 	   ; copy src1 register number
	  lods	  byte ptr ES:[SI] ; load branch displacement,
	  cbw			   ;  sign extend,
	  mov	  CX,AX 	   ;  and save it
	  mov	  AX,reg0_dis+[BX]
	  cmp	  AX,[DI].C_disp   ; are displacements eq?
	  jne	  j_neq_s2
j_neq_s1: mov	  AL,byte ptr reg0_pag+[BX]
	  cmp	  AL,byte ptr [DI].C_page ; are page numbers eq?
	  je	  j_neq_s3
j_neq_s2: add	  SI,CX 	   ; add offset to current PC
j_neq_s3: jmp	  next

;     Jump if eq?, long 	JEQL	src1,src2,offset
j_eq_l:   lods	  word ptr ES:[SI] ; load registers to compare
	  mov	  BL,AH
	  mov	  DI,BX
	  add	  DI,offset reg0   ; compute address of src2
	  mov	  BL,AL 	   ; copy src1 register number
	  lods	  word ptr ES:[SI] ; load branch displacement
	  mov	  CX,AX 	   ;  and save same
	  mov	  AX,reg0_dis+[BX]
	  cmp	  AX,[DI].C_disp   ; are displacements eq?
	  je	  j_eq_s1	   ; if eq?, continue testing
	  jmp	  next		   ; otherwise, back to interpreter

;     Jump if not eq?, long	JNEQL	src1,src2,offset
j_neq_l:  lods	  word ptr ES:[SI] ; load registers to compare
	  mov	  BL,AH
	  mov	  DI,BX
	  add	  DI,offset reg0   ; compute address of src2
	  mov	  BL,AL 	   ; copy src1 register number
	  lods	  word ptr ES:[SI] ; load branch displacement
	  mov	  CX,AX 	   ;  and save same
	  mov	  AX,reg0_dis+[BX]
	  cmp	  AX,[DI].C_disp   ; are displacements eq?
	  je	  j_neq_s1	   ; if equal, continue test
	  add	  SI,CX 	   ; add offset to current location pointer
	  jmp	  next		   ; back to the interpreter

;     Jump if nil, long 	JNILL	reg,offset
j_nil_l:  lods	  byte ptr ES:[SI] ; Load the register to test
	  mov	  BL,AL 	   ; copy register number
	  lods	  word ptr ES:[SI] ; load branch offset
	  cmp	  byte ptr reg0_pag+[BX],0 ; Test for null pointer
	  jne	  j_nil_l1	   ; Jump if not nil
	  add	  SI,AX 	   ; Add jump offset to current PC
j_nil_l1: jmp	  next		   ; Return to interpreter

;     Jump if not nil, long	JNNILL	reg,offset
j_nnil_l: lods	  byte ptr ES:[SI] ; Load the register to test
	  mov	  BL,AL 	   ; copy register number
	  lods	  word ptr ES:[SI] ; load branch offset
	  cmp	  byte ptr reg0_pag+[BX],0 ; Test for null pointer
	  je	  j_nnil_1	   ; if nil, return to interpreter
	  add	  SI,AX 	   ; Add jump offset to current PC
j_nnil_1: jmp	  next		   ; Return to interpreter

;     Jump if atom,long 	JATOMS	reg,offset
j_atm_l:  lods	  byte ptr ES:[SI] ; Load register to test
	  mov	  BL,AL 	   ; copy register number to test
	  lods	  word ptr ES:[SI] ; load branch offset
	  test	  attrib+[BX],ATOM ; test for atom attribute
	  jz	  j_atm_l1	   ; if not atom, return to interpreter
	  add	  SI,AX 	   ; add jump offset to current PC
j_atm_l1: jmp	  next		   ; return to interpreter

;     Jump if not atom,long	JNATOMS reg,offset
j_natm_l: lods	  byte ptr ES:[SI] ; Load register to test
	  mov	  BL,AL 	   ; copy register number to test
	  lods	  word ptr ES:[SI] ; load branch offset
	  test	  attrib+[BX],ATOM ; test for atom attribute
	  jnz	  j_natm_1	   ; if atom, return to interpreter
	  add	  SI,AX 	   ; add jump offset to current PC
j_natm_1: jmp	  next		   ; return to interpreter

;     Jump unconditionally, short
jmp_shrt: lods	  byte ptr ES:[SI]
	  cbw			   ; sign extend the byte offset
	  add	  SI,AX
	  jmp	  next

;     Jump unconditionally, long
jmp_long: lods	  word ptr ES:[SI]
	  add	  SI,AX
	  jmp	  next

;     Move register to register:  COPY	   dest,src
copy:	  lods	  word ptr ES:[SI] ; load regs, increment PC
	  mov	  BL,AH 	   ; copy source register number into
	  mov	  DI,BX 	   ;   DI (clear high byte)
	  mov	  BL,AL 	   ; copy destination register number
	  mov	  AX,reg0_dis+[DI]
	  mov	  reg0_dis+[BX],AX
	  mov	  AL,byte ptr reg0_pag+[DI]
	  mov	  byte ptr reg0_pag+[BX],AL
	  jmp	  next

;************************************************************************
;*							 AL   AH	*
;* Load constant from constant's area           LD-CONST dest,const     *
;*									*
;* Purpose:  Interpreter support for loading a compile time constant	*
;*		into a register of the Scheme virtual machine.		*
;************************************************************************
ld_const: lods	  word ptr ES:[SI] ; load dest reg and constant number
	  mov	  BL,AL 	   ; copy destination register number
	  mov	  DI,BX 	   ;  into TIPC register DI
	  mov	  BL,AH 	   ; isolate constant number
	  mov	  AX,BX 	   ; BX <- constant number * 3
	  shl	  AX,1
	  add	  BX,AX
	  add	  BX,CB_dis	   ; add displacement to start of code block
	  mov	  AL,ES:[BX].cod_cpag
	  mov	  byte ptr reg0_pag+[DI],AL
	  mov	  AX,ES:[BX].cod_cdis
	  mov	  reg0_dis+[DI],AX
	  jmp	  next

;************************************************************************
;*							 AL   AH	*
;* Load immediate value 			LD-IMM	 dest,imm	*
;*									*
;* Purpose:  Interpreter support for loading an immediate value 	*
;*		into a register of the Scheme virtual machine.		*
;************************************************************************
ld_imm:   lods	  word ptr ES:[SI] ; load dest reg, immediate value
	  mov	  BL,AL 	   ; copy the destination register number
	  mov	  AL,AH 	   ; isolate and sign extend the
	  cbw			   ;  immediate value
	  sal	  AX,1		   ; clear high order byte of immediate
	  shr	  AX,1		   ;  value, and
	  mov	  reg0_dis+[BX],AX ; store it
	  mov	  byte ptr reg0_pag+[BX],SPECFIX*2 ; set reg tag=fixnum
	  jmp	  next

;************************************************************************
;* Load nil						ld-nil	dest	*
;*									*
;* Purpose:  Scheme interpreter support to load the value "nil" into    *
;*		a VM register						*
;************************************************************************
ld_nil:   lods	  byte ptr ES:[SI] ; load destination register number
	  mov	  BX,AX
	  xor	  AX,AX
	  mov	  byte ptr reg0_pag+[BX],AL ; store value of 'nil into
	  mov	  reg0_dis+[BX],AX ;  destination register
	  jmp	  next		   ; back to the interpreter


;************************************************************************
;*			Macro Support for Vector Load			*
;************************************************************************
vec_load  macro   ld_type
	  local   y,z
IFIDN	  <ld_type>,<LONG>
	  mov	  DX,4		   ; record length of this instruction
	  lods	  byte ptr ES:[SI] ; load vector pointer/destination reg
	  mov	  DI,AX 	   ; copy pointer to TIPC register DI
	  lods	  word ptr ES:[SI] ; load fullword offset
	  jmp	  short ld_v_go1   ; continue processing
ELSE
	  lods	  word ptr ES:[SI] ; load vect pointer, offset operands
	  mov	  BL,AL 	   ; copy vector pointer/destination reg
	  mov	  DI,BX 	   ;  number into TIPC register DI
IFIDN	  <ld_type>,<SHORT>
	  mov	  AL,AH 	   ; convert immediate byte offset to
	  cbw			   ;  a fullword value
	  jmp	  short ld_v_go    ; continue processing
ELSE
IFIDN	  <ld_type>,<REG>
	  mov	  BL,AH 	   ; copy number of index register
	  cmp	  byte ptr reg0_pag+[BX],SPECFIX*2 ; index a fixnum?
	  jne	  z		   ; if not, error (jump)
	  mov	  AX,reg0_dis+[BX] ; load immediate value from index register
	  shl	  AX,1		   ; sign extend 15 bit immediate
	  sar	  AX,1
ld_v_go:  mov	  DX,3		   ; record length of this instruction
ld_v_go1: save	  <SI>		   ; save current location pointer
	  mov	  CX,AX 	   ; multiply the index value by 3
	  shl	  AX,1		   ;  (3 bytes/element)
	  add	  AX,CX
	  jl	  y
	  mov	  BL,byte ptr reg0_pag+[DI] ; load page number for vector ptr
	  cmp	  byte ptr ptype+[BX],VECTTYPE*2 ; does it point to a vector?
	  jne	  z		   ; if not, error (jump)
	  LoadPage ES,BX	   ; load paragraph address for vector's page
;	  mov	  ES,pagetabl+[BX] ; load paragraph address for vector's page
	  mov	  SI,reg0_dis+[DI] ; load vector offset
	  add	  AX,offset vec_data ; add offset of 1st vector element
	  cmp	  AX,ES:[SI].vec_len ; is reference within bounds?
	  jge	  y		   ; if not, error (jump)
	  add	  SI,AX 	   ; add index to vector offset
	  mov	  AL,ES:[SI].car_page ; copy vector element to destination
	  mov	  byte ptr reg0_pag+[DI],AL ; register
	  mov	  AX,ES:[SI].car
	  mov	  reg0_dis+[DI],AX
	  jmp	  next_PC	   ; return to the interpreter
;     ***error-- offset out of bounds***
y:	  mov	  AX,offset m_v_ld
vbad_off: restore <SI>		   ; restore the location pointer
	  sub	  SI,DX 	   ;  and back it up to start of instruction
	  pushm   <SI,AX>	   ; push LP and "VECTOR-REF/SET!" text as args
	  C_call  disassem,,Load_ES ; disassemble instruction for *irritant*
	  pushm   <tmp_adr,m_VOE,m_one> ; push numeric error parameters
	  C_call  set_nume
	  restore <SI>		   ; reload next instruction's address
	  jmp	  sch_err	   ; link to Scheme debugger
;     ***error-- invalid operand to vector-load instruction***
z:	  lea	  BX,m_v_ld
	  jmp	  src_err	   ; display error message
ELSE
	  ***error***  bad macro operand
ENDIF
ENDIF
ENDIF
	  endm

;************************************************************************
;*							    AL	 AH	*
;* Vector Load with short offset		LD-VEC-S    vect,offset *
;*									*
;* Purpose:  Scheme interpreter support for vector load instructions	*
;*		with short offset fields				*
;************************************************************************
ld_off_s: vec_load SHORT

;************************************************************************
;*							    AL	 AX	*
;* Vector Load with long offset 		LD-VEC-L    vect,offset *
;*									*
;* Purpose:  Scheme interpreter support for vector load instructions	*
;*		with long offset fields 				*
;************************************************************************
ld_off_l: vec_load LONG

;************************************************************************
;*							    AL	 AH	*
;* Vector Load with register offset		LD-VEC-R    vect,offset *
;*									*
;* Purpose:  Scheme interpreter support for vector load instructions	*
;*		with register offset fields				*
;************************************************************************
ld_off_r: vec_load REG

	  purge   vec_load

;************************************************************************
;*			Macro Support for Vector Store			*
;************************************************************************
vec_st	  macro   st_type
	  local   x,y,z
IFIDN	  <st_type>,<LONG>
	  mov	  [BP].save_DX,5   ; record length of this instruction
	  lods	  byte ptr ES:[SI] ; load vector pointer register
	  mov	  DI,AX 	   ; copy pointer to TIPC register DI
	  lods	  word ptr ES:[SI] ; load fullword offset
	  jmp	  short st_v_go1   ; continue processing
ELSE
	  lods	  word ptr ES:[SI] ; load vector pointer, offset operand
	  mov	  BL,AL 	   ; copy vector pointer register
	  mov	  DI,BX 	   ;  number into TIPC register DI
IFIDN	  <st_type>,<SHORT>
	  mov	  AL,AH 	   ; convert immediate byte offset to
	  cbw			   ;  a fullword value
	  jmp	  short st_v_go    ; continue processing
ELSE
IFIDN	  <st_type>,<REG>
	  mov	  BL,AH 	   ; copy number of index register
	  cmp	  byte ptr reg0_pag+[BX],SPECFIX*2 ; index a fixnum?
	  jne	  z		   ; if not, error (jump)
	  mov	  AX,reg0_dis+[BX] ; load immediate value from index register
	  shl	  AX,1		   ; sign extend 15 bit immediate
	  sar	  AX,1
st_v_go:  mov	  [BP].save_DX,4
st_v_go1: mov	  CX,AX 	   ; save index value in TIPC register CX
	  lods	  byte ptr ES:[SI] ; load source register number
	  save	  <SI>		   ; save current location pointer
	  xor	  DX,DX 	   ; save the source register number in
	  mov	  DL,AL 	   ;  TIPC register DX
	  mov	  AX,CX 	   ; multiply the index value by 3
	  shl	  AX,1		   ;  (3 bytes/element)
	  add	  AX,CX
	  jl	  y
	  mov	  BL,byte ptr reg0_pag+[DI] ; load page number for vector ptr
	  cmp	  byte ptr ptype+[BX],VECTTYPE*2 ; does it point to a vector?
	  jne	  z		   ; if not, error (jump)
;;;	  test	  attrib+[BX],READONLY ; is vector's page write protected?
;;;	  jnz	  x		   ; if write protected, error (jump)
	  LoadPage ES,BX	   ; load paragraph address for vector's page
;	  mov	  ES,pagetabl+[BX] ; load paragraph address for vector's page
	  mov	  SI,reg0_dis+[DI] ; load vector offset
	  add	  AX,offset vec_data ; add in offset of 1st vector element
	  cmp	  AX,ES:[SI].vec_len ; is reference within bounds?
	  jge	  y		   ; if not, error (jump)
	  add	  SI,AX 	   ; add index to vector offset
	  mov	  DI,DX 	   ; copy source regsiter number into DI
	  mov	  AL,byte ptr reg0_pag+[DI] ; copy contents of source register
	  mov	  ES:[SI].car_page,AL ;  into the element of the vector
	  mov	  AX,reg0_dis+[DI]
	  mov	  ES:[SI].car,AX
	  jmp	  next_PC	   ; return to the interpreter
;;;;	 ***error-- write protection violation***
;;;x:	  error   <m_il_st>
;     ***error-- offset out of bounds***
y:	  restore <DX>
	  mov	  AX,offset m_v_st
	  jmp	  vbad_off
;     ***error-- invalid operand to vector-load instruction***
z:	  lea	  BX,m_v_st
	  jmp	  src_err	   ; display error message
ELSE
	  ***error***  bad macro operand
ENDIF
ENDIF
ENDIF
	  endm

;************************************************************************
;*						    AL	 AH	AL	*
;* Vector Store with short offset	ST-VEC-S    vect,offset,src	*
;*									*
;* Purpose:  Scheme interpreter support for vector store instructions	*
;*		with short offset fields				*
;************************************************************************
st_off_s: vec_st  SHORT

;************************************************************************
;*						    AL	 AX	AL	*
;* Vector Store with long offset	ST-VEC-L    vect,offset,src	*
;*									*
;* Purpose:  Scheme interpreter support for vector store instructions	*
;*		with long offset fields 				*
;************************************************************************
st_off_l: vec_st  LONG

;************************************************************************
;*						    AL	 AH	AL	*
;* Vector Store with register offset	ST-VEC-R    vect,offset,src	*
;*									*
;* Purpose:  Scheme interpreter support for vector store instructions	*
;*		with register offset fields				*
;************************************************************************
st_off_r: vec_st  REG

	  purge   vec_st


;;;;	 Load from reference cell		DEREF	dest
;;;deref:	  lods	  byte ptr ES:[SI] ; fetch operand, increment location pointer
;;;	  mov	  DX,ES 	   ; save TIPC register ES
;;;	  mov	  BX,AX 	   ; move destination register field and
;;;	  add	  BX,offset reg0   ;  and compute destination reg address
;;;	  mov	  DI,[BX].C_page   ; load source reg page number
;;;	  cmp	  byte ptr ptype+[DI],REFTYPE*2 ; does page contain ref cells?
;;;	  jne	  not_ref	   ; if not, jump (must be reference type)
;;;	  mov	  ES,pagetabl+[DI] ; load page's paragraph address
;;;	  mov	  DI,[BX].C_disp   ; load source displacement into page
;;;	  mov	  AX,ES:[DI].car   ; load disp at source location
;;;	  mov	  [BX].C_disp,AX   ; store into destination register
;;;	  mov	  AL,ES:[DI].car_page ; load page number at source location
;;;	  mov	  byte ptr [BX].C_page,AL ; store into destination register
;;;	  mov	  ES,DX 	   ; restore TIPC register ES (code block para)
;;;	  jmp	  next		   ; branch back to interpreter
;;;;	 error-- object of ref not a reference cell
;;;not_ref:  save	  <SI>		   ; save current location pointer
;;;	  lea	  BX,m_deref
;;;	  jmp	  src_err	   ; display error message

;;;;	 Create a reference cell	  (ref obj)
;;;ref:   lods	  byte ptr ES:[SI] ; load register number
;;;	  lea	  BX,[BP].temp_reg ; load address of temp register and
;;;	  push	  BX		   ;  push as argument to "alloc_ref_cell"
;;;	  C_call  alloc_re,<AX,SI>,Load_ES ; allocate ref cell
;;;	  mov	  SP,BP
;;;	  mov	  BX,[BP].temp_reg.C_page ; Load page number of ref cell
;;;	  mov	  ES,pagetabl+[BX] ; load paragraph address of ref cell page
;;;	  mov	  DI,[BP].temp_reg.C_disp ; load the cell's displacement
;;;	  mov	  SI,[BP].save_AX  ; restore reg number from old AX into SI
;;;;	 copy pointer to object into newly allocated ref cell-- update dest reg
;;;	  mov	  AX,DI 	   ; copy displacement
;;;	  xchg	  AX,reg0_dis+[SI] ; load displacement, and
;;;	  mov	  ES:[DI].car,AX   ;  store into new ref cell
;;;	  mov	  AX,BX 	   ; copy page number
;;;	  xchg	  AX,reg0_pag+[SI] ; load page number, and
;;;	  mov	  ES:[DI].car_page,AL ;  store it, too
;;;	  jmp	  next_PC

;************************************************************************
;*							       AL  AH	*
;* Set Reference (set_ref! ref val)		       SETREF  ref,val	*
;************************************************************************
;;;set_ref:  lods	  word ptr ES:[SI] ; load src/dest register numbers
;;;	  save	  <SI>		   ; save the location pointer
;;;	  mov	  BL,AL 	   ; copy dest register number
;;;	  mov	  DI,BX
;;;	  mov	  SI,reg0_dis+[DI] ; copy displacement of ref cell
;;;	  mov	  BL,byte ptr reg0_pag+[DI] ; copy ref cell's page number
;;;	  cmp	  byte ptr ptype+[BX],REFTYPE*2 ; it is a ref cell, isn't it?
;;;	  jne	  not_strf	   ; if not, error
;;;	  mov	  ES,pagetabl+[BX] ; load paragraph of ref cell's page
;;;	  mov	  BL,AH 	   ; copy source register number
;;;	  mov	  AX,reg0_dis+[BX] ; load contents of source register and
;;;	  mov	  ES:[SI].car,AX   ;  and copy into ref cell
;;;	  mov	  reg0_dis+[DI],AX ;  and into destination register
;;;	  mov	  AL,byte ptr reg0_pag+[BX]
;;;	  mov	  ES:[SI].car_page,AL
;;;	  mov	  byte ptr reg0_pag+[DI],AL
;;;	  jmp	  next_PC	   ; return to interpreter
;;;;	 Error-- destination of set_ref! or swap_ref! not a ref cell
;;;not_strf: error   <m_setref,m_dest,m_error> ; display error message

;************************************************************************
;*							      AL   AH	*
;* Swap Reference (swap_ref! ref val)		     SWAPREF  dest,val	*
;************************************************************************
;;;swap_ref: lods	  word ptr ES:[SI] ; load src/dest register numbers
;;;	  save	  <SI>		   ; save the current location pointer
;;;	  mov	  BL,AL 	   ; copy dest register number
;;;	  mov	  DI,BX
;;;	  mov	  SI,reg0_dis+[DI] ; copy displacement of ref cell
;;;	  mov	  BL,byte ptr reg0_pag+[DI] ; copy ref cell's page number
;;;	  cmp	  byte ptr ptype+[BX],REFTYPE*2 ; it is a ref cell, isn't it?
;;;	  jne	  not_swrf	   ; if not, error
;;;	  mov	  ES,pagetabl+[BX] ; load paragraph of ref cell's page
;;;	  mov	  BL,AH 	   ; copy source register number
;;;	  mov	  AX,reg0_dis+[BX] ; load contents of source register and
;;;	  xchg	  ES:[SI].car,AX   ;  and exchange with contents of ref
;;;	  mov	  reg0_dis+[DI],AX ;  cell
;;;	  mov	  AL,byte ptr reg0_pag+[BX]
;;;	  xchg	  ES:[SI].car_page,AL
;;;	  mov	  byte ptr reg0_pag+[DI],AL
;;;	  jmp	  next_PC	   ; return to interpreter
;;;not_swrf: error   <m_swaprf,m_dest,m_error> ; display error message

;     Negation (minus obj)	MINUS dest
minus:	  lods	  byte ptr ES:[SI] ; load register field
	  mov	  DI,AX 	   ;  and copy into DI
	  add	  DI,offset reg0   ; load address of register
	  cmp	  [DI].C_page,SPECFIX*2 ; is this a fixnum?
	  jne	  minus_nf	   ; if not, go out of line
	  mov	  AX,[DI].C_disp   ; load immediate value
	  shl	  AX,1		   ; align for sign extension
minusmrg: neg	  AX		   ; negate the immediate value
	  jo	  minus_ov	   ; overflow?	if so, make bignum
	  shr	  AX,1		   ; re-align immediate value
	  mov	  [DI].C_disp,AX   ; store result into register
	  jmp	  next		   ; return to interpreter
;     Not a fixnum-- call arithmetic support
minus_nf: mov	  DX,MINUS_OP	   ; indicate negation sub-opcode

;     Process unary operation out of line
arith_1:  pushm   <DI,DX>	   ; push reg addr, sub-opcode
	  C_call  arith1,<SI>,Load_ES ; call unary arithmetic support
	  cmp	  AX,0		   ; was error encountered?
	  jne	  arith_1x	   ; if error, jump
	  jmp	  next_SP	   ; process next instruction
arith_1x: jmp	  sch_err	   ; link to Scheme debugger

minus_ov: mov	  AX,16384	   ;Create result
	  sub	  DI,offset reg0   ; Convert register addr back to reg number

;     Fixnum overflow-- convert to bignum
enlrg1:   cwd			   ; Convert to long integer
enlrg2:   add	  DI,offset reg0   ; compute address of destination register
	  pushm   <DX,AX,DI>	   ; push long int, reg addr
	  C_call  enlarge,<SI>,Load_ES ; create bignum
	  jmp	  next_SP	   ; process next instruction

;     Support for absolute value  (abs n)
sabs:	  lods	  byte ptr ES:[SI] ; load destination register number
	  mov	  DI,AX
	  add	  DI,offset reg0   ; load register address
	  cmp	  [DI].C_page,SPECFIX*2 ; Fixnum (immediate)?
	  jne	  abs_nf	   ; if not, go out-of-line
	  mov	  AX,[DI].C_disp   ; load immediate value
	  shl	  AX,1		   ; shift to position sign bit
	  cmp	  AX,0		   ; how's it relate to zero?
	  jl	  minusmrg	   ; if negative, negate
	  jmp	  next		   ; else do nothing
abs_nf:   mov	  DX,ABS_OP	   ; load absolute value subopcode
	  jmp	  arith_1	   ; process out of line

;************************************************************************
;*	    Macro support for out-of-line calls to Lattice C		*
;************************************************************************
OTL_R_	  =	  1
OTL_RT_   =	  1
OTL_R	  macro   rtn,error_p
	  local   x
IFNDEF	  rtn
	  extrn   rtn:near
ENDIF
	  mov	  DI,offset PGROUP:rtn ; load address of routine
IFIDN	  <error_p>,<TEST_RESULT>
IF	  OTL_RT_
OTL_RT_   =	  0
otlr1t:   lods	  byte ptr ES:[SI] ; load register operand
	  save	  <SI>		   ; save the location pointer
	  add	  AX,offset reg0   ; compute address of register
	  push	  AX		   ;  and push as single argument
	  mov	  AX,DS 	   ; set ES to point to the
	  mov	  ES,AX 	   ;  current data segment
	  call	  DI		   ; call desired routine
	  cmp	  AX,0		   ; was error detected?
	  jl	  x		   ; if error, jump
	  jmp	  next_SP	   ; return to interpreter
x:	  jmp	  sch_err	   ; link to Scheme debugger
ELSE
	  jmp	  otlr1t	   ; call desired routine
ENDIF
ELSE
IF	  OTL_R_
OTL_R_	  =	  0
otlr1:	  lods	  byte ptr ES:[SI] ; load register operand
	  save	  <SI>		   ; save the location pointer
	  add	  AX,offset reg0   ; compute address of register
	  push	  AX		   ;  and push as single argument
	  mov	  AX,DS 	   ; set ES to point to the
	  mov	  ES,AX 	   ;  current data segment
	  mov	  AX,offset PGROUP:next_SP ; push "next_SP" as the
	  push	  AX		   ;  return address
	  jmp	  DI		   ; tail recursive call to routine
ELSE
	  jmp	  otlr1 	   ; call desired routine
ENDIF
ENDIF
	  endm

OTL_R2_   =	  1
OTL_R2T_  =	  1
OTL_R2	  macro   rtn,error_p
	  local   x
IFNDEF	  rtn
	  extrn   rtn:near
ENDIF
	  mov	  DI,offset PGROUP:rtn ; load address of routine
IFIDN	  <error_p>,<TEST_RESULT>
IF	  OTL_R2T_
OTL_R2T_  =	  0
otlr2t:   lods	  word ptr ES:[SI] ; load register operand
	  save	  <SI>		   ; save the location pointer
	  xor	  BX,BX
	  mov	  BL,AH
	  add	  BX,offset reg0   ; compute address of register
	  xor	  AH,AH
	  add	  AX,offset reg0
	  pushm   <BX,AX>	   ; push register addresses as arguments
	  mov	  AX,DS
	  mov	  ES,AX
	  call	  DI		   ; call desired routine
	  cmp	  AX,0		   ; was error detected?
	  jl	  x		   ; if error, jump
	  jmp	  next_SP	   ; return to interpreter
x:	  jmp	  sch_err	   ; link to Scheme debugger
ELSE
	  jmp	  otlr2t
ENDIF
ELSE
IF	  OTL_R2_
OTL_R2_   =	  0
otlr2:	  lods	  word ptr ES:[SI] ; load register operand
	  save	  <SI>		   ; save the location pointer
	  xor	  BX,BX
	  mov	  BL,AH
	  add	  BX,offset reg0   ; compute address of register
	  xor	  AH,AH
	  add	  AX,offset reg0
	  pushm   <BX,AX>	   ; push register addresses as arguments
	  mov	  AX,DS 	   ; set ES to point to the current data
	  mov	  ES,AX 	   ;  segment
	  mov	  AX,offset PGROUP:next_SP ; push address of "next_SP" as
	  push	  AX		   ;  the return address
	  jmp	  DI		   ; tail recursive call to desired routine
ELSE
	  jmp	  otlr2
ENDIF
ENDIF
	  endm

OTL_R3_   =	  1
OTL_R3T_  =	  1
OTL_R3	  macro   rtn,error_p
	  local   x
IFNDEF	  rtn
	  extrn   rtn:near
ENDIF
	  mov	  DI,offset PGROUP:rtn ; load address of routine
IFIDN	  <error_p>,<TEST_RESULT>
IF	  OTL_R3T_
OTL_R3T_  =	  0
otlr3t:   lods	  byte ptr ES:[SI] ; load 1st operand
	  add	  AX,offset reg0   ;  and compute register address
	  mov	  CX,AX
	  lods	  word ptr ES:[SI] ; load 2nd and 3rd operands
	  save	  <SI>		   ; save the location pointer
	  xor	  BX,BX
	  mov	  BL,AH
	  add	  BX,offset reg0   ; compute address of register
	  xor	  AH,AH
	  add	  AX,offset reg0
	  pushm   <BX,AX,CX>	   ; push register addresses as arguments
	  mov	  AX,DS
	  mov	  ES,AX
	  call	  DI		   ; call desired routine
	  cmp	  AX,0		   ; was error detected?
	  jl	  x		   ; if error, jump
	  jmp	  next_SP	   ; return to interpreter
x:	  jmp	  sch_err	   ; link to Scheme debugger
ELSE
	  jmp	  otlr3t	   ; call desired routine
ENDIF
ELSE
IF	  OTL_R3_
OTL_R3_  =	  0
otlr3:	  lods	  byte ptr ES:[SI] ; load 1st operand
	  add	  AX,offset reg0   ;  and compute register address
	  mov	  CX,AX
	  lods	  word ptr ES:[SI] ; load 2nd and 3rd operands
	  save	  <SI>		   ; save the location pointer
	  xor	  BX,BX
	  mov	  BL,AH
	  add	  BX,offset reg0   ; compute address of register
	  xor	  AH,AH
	  add	  AX,offset reg0
	  pushm   <BX,AX,CX>	   ; push register addresses as arguments
	  mov	  AX,DS
	  mov	  ES,AX
	  mov	  AX,offset PGROUP:next_SP ; push address of "next_SP" as
	  push	  AX		   ;  the return address
	  jmp	  DI		   ; tail recursive call to desired routine
ELSE
	  jmp	  otlr3 	   ; call desired routine
ENDIF
ENDIF
	  endm

OTL_R4_   =	  1
OTL_R4T_  =	  1
OTL_R4	  macro   rtn,error_p
	  local   x
IFNDEF	  rtn
	  extrn   rtn:near
ENDIF
	  mov	  DI,offset PGROUP:rtn ; load address of routine
IFIDN	  <error_p>,<TEST_RESULT>
IF	  OTL_R4T_
OTL_R4T_  =	  0
otlr4t:   lods	  word ptr ES:[SI] ; load 1st  and 2nd operands
	  xor	  CX,CX
	  xor	  DX,DX
	  mov	  DL,AL 	   ; copy 1st operand register number
	  add	  DX,offset reg0
	  mov	  CL,AH 	   ; copy 2nd operand register number
	  add	  CX,offset reg0
	  lods	  word ptr ES:[SI] ; load 3rd and 4th operands
	  save	  <SI>		   ; save the location pointer
	  xor	  BX,BX
	  mov	  BL,AH 	   ; copy 4th operand register number
	  add	  BX,offset reg0   ; compute address of register
	  xor	  AH,AH
	  add	  AX,offset reg0
	  pushm   <BX,AX,CX,DX>    ; push register addresses as arguments
	  mov	  AX,DS
	  mov	  ES,AX
	  call	  DI		   ; call desired routine
	  cmp	  AX,0		   ; was error detected?
	  jl	  x		   ; if error, jump
	  jmp	  next_SP	   ; return to interpreter
x:	  jmp	  sch_err	   ; link to Scheme debugger
ELSE
	  jmp	  otlr4t	   ; call desired routine
ENDIF
ELSE
IF	  OTL_R4_
OTL_R4_  =	  0
otlr4t:   lods	  word ptr ES:[SI] ; load 1st  and 2nd operands
	  xor	  CX,CX
	  xor	  DX,DX
	  mov	  DL,AL 	   ; copy 1st operand register number
	  add	  DX,offset reg0
	  mov	  CL,AH 	   ; copy 2nd operand register number
	  add	  CX,offset reg0
	  lods	  word ptr ES:[SI] ; load 3rd and 4th operands
	  save	  <SI>		   ; save the location pointer
	  xor	  BX,BX
	  mov	  BL,AH 	   ; copy 4th operand register number
	  add	  BX,offset reg0   ; compute address of register
	  xor	  AH,AH
	  add	  AX,offset reg0
	  pushm   <BX,AX,CX,DX>    ; push register addresses as arguments
	  mov	  AX,DS
	  mov	  ES,AX
	  mov	  AX,offset PGROUP:next_SP ; push address of "next_SP" as
	  push	  AX		   ;  the return address
	  jmp	  DI		   ; tail recursive call to desired routine
ELSE
	  jmp	  otlr4 	   ; call desired routine
ENDIF
ENDIF
	  endm

;************************************************************************
; Convert number to fixnum (toward nearest integer)	ROUND  reg	*
;************************************************************************
sround:   OTL_R   round,TEST_RESULT

;************************************************************************
; Convert number to fixnum (toward - infinity)		FLOOR  reg	*
;************************************************************************
sfloor:   OTL_R   floor,TEST_RESULT

;************************************************************************
; Convert number to fixnum (toward + infinity)		CEILING  reg	*
;************************************************************************
sceiling: OTL_R   ceiling,TEST_RESULT

;************************************************************************
; Convert number to fixnum (toward zero)		TRUNCATE  reg	*
;************************************************************************
struncat: OTL_R   truncate,TEST_RESULT

;************************************************************************
; Convert number to fixnum				FLOAT reg	*
;************************************************************************
float:	  OTL_R   sfloat,TEST_RESULT

;************************************************************************
;* Support for string->symbol			(string->symbol  dest)	*
;************************************************************************
str2sym:  OTL_R   str_2_sy,TEST_RESULT

;************************************************************************
;* string->uninterned-symbol	      (string->uninterned-symbol dest)	*
;************************************************************************
str2usym: OTL_R   str_2_us,TEST_RESULT

;************************************************************************
;* Support for symbol->string			(symbol->string  dest)	*
;************************************************************************
sym2str:  OTL_R   sym_2_st,TEST_RESULT

;************************************************************************
;* Support for fast load			       (fasl filename)	*
;************************************************************************
sfasl:	  OTL_R   fasl,TEST_RESULT

;;;;************************************************************************
;;;;* Support for unique symbol generation		(gensym sym)	*
;;;;************************************************************************
;;;gensym:   OTL_R   sgensym

;************************************************************************
;* Support for prop-list			      (prop-list name)	*
;************************************************************************
proplist: OTL_R   prop_lis,TEST_RESULT

;************************************************************************
;* Support for random					 (random seed)	*
;************************************************************************
random:   OTL_R   srandom

;;;;************************************************************************
;;;;* Support for current-column			 (current-column dest)	*
;;;;************************************************************************
;;;curr_clm: OTL_R   current_

;;;;************************************************************************
;;;;* Support for line-length			    (line-length dest)	*
;;;;************************************************************************
;;;line_lng: OTL_R   line_len

;;;;************************************************************************
;;;;* Support for set-line-length!		(set-line-length! len)	*
;;;;************************************************************************
;;;set_lng:  OTL_R   set_line

;;;;************************************************************************
;;;;* Support for file-exists?			(file-exists? string)	*
;;;;************************************************************************
;;;file_ex:  OTL_R   file_exi

;************************************************************************
;* Support for %internal-time			(%internal-time  dest)	*
;************************************************************************
ptyme:	  OTL_R   ptime

;************************************************************************
;* Support for make-window			    (make-window dest)	*
;************************************************************************
;;;mk_wind:  OTL_R   make_win,TEST_RESULT

;************************************************************************
;* Support for clear-window			   (clear-window dest)	*
;************************************************************************
clr_wind: OTL_R   clear_wi,TEST_RESULT

;************************************************************************
;* Support for read-char			      (read-char dest)	*
;************************************************************************
;;;readch:   OTL_R   read_cha,TEST_RESULT

;************************************************************************
;* Support for close-port			      (close-port port) *
;************************************************************************
pclose:   OTL_R   spclose,TEST_RESULT

;************************************************************************
;* Support for newline					 (newline port) *
;************************************************************************
;;;pnewlin:  OTL_R   spnewlin,TEST_RESULT

;************************************************************************
;* Support for read					    (read port) *
;************************************************************************
;;;pread:	  OTL_R   spread,TEST_RESULT

;************************************************************************
;* Support for print-length			     (print-length obj) *
;************************************************************************
;;;prt_len_: OTL_R   prt_len

;************************************************************************
;* Support for %transcript			 (%transcript port/nil) *
;************************************************************************
;;;transcrip: OTL_R   trns_chg

;************************************************************************
;* Support for read-char-ready? 		(read-char-ready? port) *
;************************************************************************
;;;read_cr:  OTL_R   rd_ch_rd,TEST_RESULT

;************************************************************************
;* Support for save-window		    (save-window-contents port) *
;************************************************************************
;;;sav_wind: OTL_R   save_win,TEST_RESULT

;************************************************************************
;* Support for read-atom				(read-atom port)*
;************************************************************************
;;;read_at:  OTL_R   srd_atom,TEST_RESULT

;************************************************************************
;* Support for %start-timer			 (%start-timer #-ticks) *
;************************************************************************
set_tim:  OTL_R   cset_tim,TEST_RESULT

;************************************************************************
;* Support for %stop-timer				  (%stop-timer) *
;************************************************************************
rst_tim:  OTL_R   crst_tim,TEST_RESULT

;************************************************************************
;* Support for STRING-LENGTH			(STRING-LENGTH	STRING) *
;************************************************************************
str_lng:  OTL_R   st_len,TEST_RESULT

;************************************************************************
;* Support for REIFY-STACK			    (REIFY-STACK index) *
;************************************************************************
reify_s:  OTL_R   reif_stk,TEST_RESULT

;************************************************************************
;* Support for princ				     (princ obj {port}) *
;************************************************************************
;;;pprinc:   OTL_R2  spprinc,TEST_RESULT

;************************************************************************
;* Support for get-prop 			  (get-prop name prop)	*
;************************************************************************
getprop:  OTL_R2  get_prop

;************************************************************************
;* Support for rem-prop 			  (rem-prop name prop)	*
;************************************************************************
remprop:  OTL_R2  rem_prop

;************************************************************************
;* Support for get-window-attribute   (get-window-attribute wind attr)	*
;************************************************************************
;;;get_w_at: OTL_R2  get_wind,TEST_RESULT

;************************************************************************
;* Support for open-port			       (open port mode) *
;************************************************************************
popen:	  OTL_R2  spopen,TEST_RESULT

;************************************************************************
;* Support for prin1				     (prin1 obj {port}) *
;************************************************************************
;;;pprin1:   OTL_R2  spprin1,TEST_RESULT

;************************************************************************
;* Support for print				     (print obj {port}) *
;************************************************************************
;;;pprint:   OTL_R2  spprint,TEST_RESULT

;************************************************************************
;* Support for restore-window	    (restore-window-contents port data) *
;************************************************************************
;;;res_wind: OTL_R2  rest_win,TEST_RESULT

;************************************************************************
;* Support for REIFY-STACK!		      (REIFY-STACK index value) *
;************************************************************************
reify_sb: OTL_R2  reif_stb,TEST_RESULT

;************************************************************************
;* Support for APPEND				      (APPEND list obj) *
;************************************************************************
append:   OTL_R2  sappend,TEST_RESULT

;************************************************************************
;* Support for put-prop 		     (put-prop name value prop) *
;************************************************************************
putprop:  OTL_R3  put_prop,TEST_RESULT

;************************************************************************
;* Substring (substring string position length)   SUBSTR    str,pos,len *
;************************************************************************
substr:   OTL_R3  ssubstr,TEST_RESULT

;************************************************************************
;* Support for set-window-attr	  (get-window-attribute wind attr val)	*
;************************************************************************
set_w_at: OTL_R3  set_wind,TEST_RESULT

;************************************************************************
;* Support for subset-find-next-char-in-set (... str start end charset) *
;************************************************************************
srch_nx:  OTL_R4  srch_nxt,TEST_RESULT

;************************************************************************
;* Support for subset-find-prev-char-in-set (... str start end charset) *
;************************************************************************
srch_pr:  OTL_R4  srch_prv,TEST_RESULT

;************************************************************************
;* Interface to set file position   (set-file-position! port chunk# bytes)
;************************************************************************

sfpos:	  OTL_R3  set_pos,TEST_RESULT

	  purge   OTL_R,OTL_R2,OTL_R3,OTL_R4

;************************************************************************
;*							 AL  AH    AL	*
;* Support for "reification"                    (%reify  obj index)     *
;*						(%reify! obj index val) *
;************************************************************************
sreifyb:  mov	  CX,1		   ; set flag for "store" operation
	  jmp	  short sreif_10   ; skip next instruction
sreify:   xor	  CX,CX 	   ; set flag for "load" operation
sreif_10: lods	  word ptr ES:[SI] ; load obj,index operand register numbers
	  xor	  BX,BX
	  mov	  BL,AL 	   ; copy obj's register number and
	  lea	  DI,reg0+[BX]	   ;  compute obj register's address
	  mov	  BL,AH 	   ; copy index's register number and
	  add	  BX,offset reg0   ;  compute index register's address
	  cmp	  CX,0		   ; is this a load or a store?
	  je	  sreif_20	   ; if a load, jump
	  xor	  AX,AX
	  lods	  byte ptr ES:[SI] ; load value register number and
	  add	  AX,offset reg0   ;  compute value register's address
	  push	  AX		   ; push value reg as argument
sreif_20: pushm   <BX,DI,CX>	   ; push index reg, obj reg, direction
	  C_call  reify,<SI>,Load_ES ; call:  reify(dir,obj,index{,val});
	  cmp	  AX,0		   ; test result of reification request
	  jne	  sreif_30	   ; if error, jump
	  jmp	  next_SP	   ; return to interpreter
;     ***error-- error status returned from reify call***
sreif_30: restore <SI>		   ; reload the location pointer
	  jmp	  sch_err	   ; link to Scheme debugger

;************************************************************************
;* Macro definition - Interpreter support for binary operations 	*
;*									*
;* Purpose:  To generate interpreter support for operations of the	*
;*		form:							*
;*			   OP	   dest,src				*
;*		where:							*
;*		   destination reg <- destination reg OP source reg	*
;************************************************************************
bin_op	  macro   operation
	  local   label1,label2,label3,label4
	  lods	  word ptr ES:[SI] ; load destination/source register numbers
	  mov	  BL,AL 	   ; copy destination reg number to
	  mov	  DI,BX 	   ;  register DI
	  mov	  AL,byte ptr reg0_pag+[DI] ; test to see in destination's
	  cmp	  AL,SPECFIX*2	   ;  page contains fixnums
	  jne	  label1	   ; if not, process out of line (jump)
	  mov	  BL,AH 	   ; copy source register number
	  cmp	  AL,byte ptr reg0_pag+[BX] ; is second operand also a fixnum?
	  jne	  label1	   ; if not, process out of line (jump)
	  mov	  BX,reg0_dis+[BX] ; load source (second) operand
	  mov	  AX,reg0_dis+[DI] ; load destination (first) operand
	  shl	  AX,1		   ; adjust sign bits of both
	  shl	  BX,1		   ;  operands
IFIDN	  <operation>,<ADD>
	  add	  AX,BX 	   ; add the two operands
	  jo	  add_ov	   ; overflow? if so, convert to bignum (jump)
ELSE
IFIDN	  <operation>,<SUB>
	  sub	  AX,BX 	   ; subtract the two operands
	  jo	  sub_ov	   ; overflow? if so, convert to bignum (jump)
ELSE
IFIDN	  <operation>,<MUL>
	  sar	  AX,1		   ; divide first operand by 2
	  imul	  BX		   ; multiply the two operands
	  jo	  mul_ov	   ; overflow? if so, convert to bignum (jump)
ELSE
IFIDN	  <operation>,<DIV>
	  cmp	  BX,0		   ; is the divisor zero?
	  je	  zero_div	   ; if so, error
	  cwd			   ; convert dividend to a doubleword
	  idiv	  BX		   ; divide the two operands
	  cmp	  DX,0		   ; is remainder zero?
	  jne	  div_frac	   ; if so, return flonum result (jump)
	  shl	  AX,1		   ; clear high order bit of result
ELSE
IFIDN	  <operation>,<QUOT>
	  cmp	  BX,0		   ; is the divisor zero?
	  je	  zero_dvq	   ; if so, error
	  cwd			   ; convert dividend to a doubleword
	  idiv	  BX		   ; divide the two operands
	  shl	  AX,1		   ; clear high order bit of result
ELSE
IFIDN	  <operation>,<MOD>
	  cmp	  BX,0		   ; is the divisor zero?
	  je	  zero_dvm	   ; if so, error (jump)
	  cwd			   ; convert dividend to a doubleword
	  idiv	  BX		   ; divide the two operands (gives remainder)
	  mov	  AX,DX 	   ; copy remainder to AX
ELSE
IFIDN	  <operation>,<MAX>
	  cmp	  AX,BX 	   ; compare the two operands
	  jge	  max_done	   ; if destination operand biggest, jump
	  mov	  AX,BX 	   ; copy the source operand to AX
ELSE
IFIDN	  <operation>,<MIN>
	  cmp	  AX,BX 	   ; compare the two operands
	  jle	  max_done	   ; if destination operand smallest, jump
	  mov	  AX,BX 	   ; copy the source operand to AX
ELSE
IFIDN	  <operation>,<XOR>
	  xor	  AX,BX 	   ; xor the two operands
ELSE
IFIDN	  <operation>,<AND>
	  and	  AX,BX 	   ; and the two operands
ELSE
IFIDN	  <operation>,<OR>
	  or	  AX,BX 	   ; ior the two operands
ELSE
	   ***error***		   ; undefined instruction
ENDIF
ENDIF
ENDIF
ENDIF
ENDIF
ENDIF
ENDIF
ENDIF
ENDIF
ENDIF
ENDIF
	  shr	  AX,1		   ; convert result to 15 bit value
	  mov	  reg0_dis+[DI],AX ; store result into destination register
	  jmp	  next		   ; return to the interpreter
IFIDN	  <operation>,<ADD>
label1:   mov	  DX,ADD_OP	   ; load operation type
;     General arithmetic support for non-interget binary arithmetic operations
;	Registers at this point:  AH - source register number
;				  BH - (zero)
;				  DX - arithmetic sub-opcode (operation type)
;				  DI - destination register number
bin_ool:  mov	  BL,AH 	   ; copy source register number
	  add	  BX,offset reg0   ; compute source register's address
	  add	  DI,offset reg0   ; compute destination register's address
	  pushm   <BX,DI,DX>	   ; push arguments on TIPC's stack
	  C_call  arith2,<SI>,load_ES ; process the non-integer operation
	  cmp	  AX,0		   ; error encountered?
	  jne	  label4	   ; if error detected, jump
	  jmp	  next_SP	   ; return to the interpreter
label4:   jmp	  sch_err	   ; link to Scheme debugger
ELSE
IFIDN	  <operation>,<MAX>
label1:   mov	  DX,GE_OP	   ; load operation type
max_ool:  mov	  BL,AH 	   ; copy source register number
	  add	  BX,offset reg0   ; compute source register's address
	  add	  DI,offset reg0   ; compute destination register's address
	  pushm   <BX,DI,DX>	   ; push arguments on TIPC's stack
	  C_call  arith2,<BX,DI,SI>,load_ES ; process the non-integer operation
	  cmp	  AX,0		   ; what was the result of the comparison?
	  jl	  label3	   ; if error detected, jump
	  jne	  label2	   ; jump, if correct value already in dest reg
	  restore <BX,DI>	   ; restore register addresses
	  mov	  AX,[BX].C_disp   ; copy source operand into the destination
	  mov	  [DI].C_disp,AX   ;  register
	  mov	  AL,byte ptr [BX].C_page
	  mov	  byte ptr [DI].C_page,AL
label2:   jmp	  next_SP	   ; return to the interpreter
label3:   jmp	  sch_err	   ; link to Scheme debugger
ELSE
IFIDN	  <operation>,<MIN>
label1:   mov	  DX,LE_OP	   ; load operation type
	  jmp	  max_ool	   ; process non-integer comparison out of line
ELSE
label1:   mov	  DX,operation&_OP	   ; load operation type
	  jmp	  bin_ool	   ; process non-integer operation out of line
ENDIF
ENDIF
ENDIF
	  endm


;************************************************************************
; Addition (+ obj1 obj2)				ADD   dest,src	*
;************************************************************************
add:	  bin_op  ADD
sub_ov:   cmc			   ; complement the carry bit for subtract
add_ov:   rcr	  AX,1		   ; Shift in sign bit
	  jmp	  enlrg1	   ; convert to bignum

;************************************************************************
;* Subtraction (- obj1 obj2)				SUB   dest,src	*
;************************************************************************
sub:	  bin_op  SUB

;************************************************************************
;* Multiplication (* obj1 obj2) 			MUL   dest,src	*
;************************************************************************
mul:	  bin_op  MUL
mul_ov:   sar	  DX,1		   ;Divide product by 2
	  rcr	  AX,1
	  jmp	  enlrg2	   ;Convert to bignum

;************************************************************************
;* Division (/ obj1 obj2)				DIV   dest,src	*
;************************************************************************
div:	  bin_op  DIV
;     ***Error-- Division by Zero***
zero_div: mov	  BX,offset m_DIV  ; load text for "\"
zd_010:   sub	  SI,3		   ; back up location pointer to start of inst.
	  pushm   <SI,BX>	   ; push inst addr, function name
	  C_call  disassem,<SI>,Load_ES ; "disassemble" the instruction
	  pushm   <tmp_adr,m_zerodv,m_one> ; push irritant,div code,no restart
	  C_call  set_nume	   ; set_numeric_error(1,ZERO_DIV,tmp_reg)
	  restore <SI>		   ; load restart address (not used)
	  jmp	  sch_err	   ; link to Scheme debugger
;     ***Fractional Result from Division-- Convert to Flonum***
div_frac: add	  DI,offset reg0   ; compute destination register address
	  push	  DI		   ;  and push as argument to "sfloat"
	  C_call  sfloat,<SI>,load_ES ; convert destination op to flonum
	  les	  SI,dword ptr [BP].save_SI ; restore location pointer
	  sub	  SI,2		   ; back up the location pointer
	  xor	  BX,BX 	   ; clear TIPC register BX
	  jmp	  div		   ; re-execute div in floating point

;************************************************************************
;* Integer Division (quotient obj1 obj2)	    QUOTIENT   dest,src *
;************************************************************************
quo:	  bin_op  QUOT
zero_dvq: mov	  BX,offset m_QUOTNT ; load address of "QUOTIENT" text
	  jmp	  zd_010	   ; indicate divide by zero

;************************************************************************
;* Modulo (mod obj1 obj2)				MOD   dest,src	*
;************************************************************************
modulo:   bin_op  MOD
zero_dvm: mov	  BX,offset m_MODULO ; load address of "REMAINDER" text
	  jmp	  zd_010	   ; indicate divide by zero

;************************************************************************
;* Maximum value (max obj1 obj2)			MAX   dest,src	*
;************************************************************************
maximum:  bin_op  MAX
max_done: jmp	  next		   ; return to interpreter

;************************************************************************
;* Minimum value (min obj1 obj2)			MIN   dest,src	*
;************************************************************************
minimum:  bin_op  MIN

;************************************************************************
;* (bitwise-xor obj1 obj2)				XOR   dest,src	*
;************************************************************************
b_xor:	  bin_op  XOR

;************************************************************************
;* (bitwise-and obj1 obj2)				AND   dest,src	*
;************************************************************************
b_and:	  bin_op  AND

;************************************************************************
;* (bitwise-or obj1 obj2)				OR    dest,src	*
;************************************************************************
b_or:	  bin_op  OR

	  purge   bin_op

;************************************************************************
;* Macro definition - Interpreter support for immediate operations	*
;*									*
;* Purpose:  To generate interpreter support for operations of the	*
;*		form:							*
;*			   OP	   dest,immediate			*
;*		where:							*
;*		   destination reg <- destination reg OP immediate	*
;************************************************************************
immed_op  macro   operation
	  local   label1,label2,label3,label4
	  lods	  word ptr ES:[SI] ; load destination reg/immediate value
	  mov	  BL,AL 	   ; copy destination reg number to
	  mov	  DI,BX 	   ;  register DI
	  mov	  AL,AH 	   ; sign extend immediate operand
	  cbw
	  cmp	  byte ptr reg0_pag+[DI],SPECFIX*2 ; dest operand a fixnum?
	  jne	  label1	   ; if not, process out of line (jump)
	  mov	  BX,AX 	   ; move immediate operand to BX
	  mov	  AX,reg0_dis+[DI] ; load destination (first) operand
	  shl	  AX,1		   ; adjust sign bits of both
	  shl	  BX,1		   ;  operands
IFIDN	  <operation>,<ADD>
	  add	  AX,BX 	   ; add the two operands
	  jo	  addi_ov	   ; overflow? if so, convert to bignum (jump)
ELSE
IFIDN	  <operation>,<SUB>
	  sub	  AX,BX 	   ; subtract the two operands
	  jo	  addi_ov	   ; overflow? if so, convert to bignum (jump)
ELSE
IFIDN	  <operation>,<MUL>
	  sar	  AX,1		   ; divide first operand by 2
	  imul	  BX		   ; multiply the two operands
	  jo	  muli_ov	   ; overflow? if so, convert to bignum (jump)
ELSE
IFIDN	  <operation>,<DIV>
	  cmp	  BX,0		   ; is the divisor zero?
	  je	  zero_dvi	   ; if so, error
	  cwd			   ; convert dividend to a doubleword
	  idiv	  BX		   ; divide the two operands
	  cmp	  DX,0		   ; is remainder zero?
	  jne	  divi_frc	   ; if not, need flonum result (jump)
	  shl	  AX,1		   ; clear high order bit of result
ELSE
IFIDN	  <operation>,<MOD>
	  cmp	  BX,0		   ; is the divisor zero?
	  je	  label2	   ; if so, assume result is the dividend
	  cwd			   ; convert dividend to a doubleword
	  idiv	  BX		   ; divide the two operands (gives remainder)
	  mov	  AX,DX 	   ; copy remainder to AX
label2:
ELSE
IFIDN	  <operation>,<MAX>
	  cmp	  AX,BX 	   ; compare the two operands
	  jge	  mxi_done	   ; if destination operand biggest, jump
	  mov	  AX,BX 	   ; copy the source operand to AX
ELSE
IFIDN	  <operation>,<MIN>
	  cmp	  AX,BX 	   ; compare the two operands
	  jle	  mxi_done	   ; if destination operand smallest, jump
	  mov	  AX,BX 	   ; copy the source operand to AX
ELSE
	   ***error***		   ; undefined instruction
ENDIF
ENDIF
ENDIF
ENDIF
ENDIF
ENDIF
ENDIF
	  shr	  AX,1		   ; convert result to 15 bit value
	  mov	  reg0_dis+[DI],AX ; store result into destination register
	  jmp	  next		   ; return to the interpreter
IFIDN	  <operation>,<ADD>
label1:   mov	  DX,ADD_OP	   ; load operation type
;     General arithmetic support for non-integer immediate operations
;	Registers at this point:  AX - immediate value
;				  DX - arithmetic sub-opcode (operation type)
;				  DI - destination register number
bini_ool: add	  DI,offset reg0   ; compute address of destination register
	  lea	  BX,[BP].temp_reg ; load address of temporary register
	  and	  AX,07fffH	   ; mask off sign bit of immediate value
	  mov	  [BX].C_disp,AX   ;  and create a fixnum value in a
	  mov	  [BX].C_page,SPECFIX*2 ;  temporary register
	  pushm   <BX,DI,DX>	   ; push arguments on TIPC's stack
	  C_call  arith2,<SI>,load_ES ; process the non-integer operation
	  cmp	  AX,0		   ; was error detected?
	  jne	  label3	   ; if error encountered, jump
	  jmp	  next_SP	   ; return to the interpreter
label3:   jmp	  sch_err	   ; link to Scheme debugger
ELSE
IFIDN	  <operation>,<MAX>
label1:   mov	  DX,GE_OP	   ; load operation type
maxi_ool: add	  DI,offset reg0   ; compute destination register's address
	  lea	  [BX].temp_reg    ; load address of temporary register
	  and	  AX,07fffH	   ; mask off sign bit of immediate value
	  mov	  [BX].C_disp,AX   ;  and create a fixnum value in a
	  mov	  [BX].C_page,SPECFIX*2 ;  temporary register
	  pushm   <BX,DI,DX>	   ; push arguments on TIPC's stack
	  C_call  arith2,<BX,DI,SI>,load_ES ; process the non-integer operation
	  cmp	  AX,0		   ; what was the result of the comparison?
	  jl	  label4	   ; if error detected, jump
	  jne	  label2	   ; jump, if correct value already in dest reg
	  restore <BX,DI>	   ; restore register addresses
	  mov	  AX,[BX].C_disp   ; copy source operand into the destination
	  mov	  [DI].C_disp,AX   ;  register
label2:   jmp	  next_SP	   ; return to the interpreter
label4:   jmp	  sch_err	   ; link to the Scheme debugger
ELSE
IFIDN	  <operation>,<MIN>
label1:   mov	  DX,LE_OP	   ; load operation type
	  jmp	  maxi_ool	   ; process non-integer comparison out of line
ELSE
label1:   mov	  DX,operation&_OP	   ; load operation type
	  jmp	  bini_ool	   ; process non-integer operation out of line
ENDIF
ENDIF
ENDIF
	  endm


;************************************************************************
;* Add immediate					ADDI	reg,val *
;************************************************************************
addi:	  immed_op ADD
addi_ov:  jmp	  add_ov


;************************************************************************
;* Multiply Immediate					MULI	reg,val *
;************************************************************************
muli:	  immed_op MUL
muli_ov:  jmp	  mul_ov	   ; convert to bignum


;************************************************************************
;* Divide Immediate					DIVI	reg,val *
;************************************************************************
divi:	  immed_op DIV
zero_dvi: jmp	  zero_div	   ; process divide by zero
divi_frc: add	  DI,offset reg0   ; compute destination register address
	  push	  DI		   ;  and push as argument to "sfloat"
	  C_call  sfloat,<SI>,load_ES ; convert destination op to flonum
	  les	  SI,dword ptr [BP].save_SI ; restore location pointer
	  sub	  SI,2		   ; back up the location pointer
	  xor	  BX,BX 	   ; clear TIPC register BX
	  jmp	  divi		   ; re-execute div immed in floating point

	  purge   immed_op

;************************************************************************
;* Test for (null? obj) 				NULL?	reg	*
;************************************************************************
null_p:   lods	  byte ptr ES:[SI] ; load number of register to test
	  mov	  BX,AX 	   ;  and copy it into BX
	  cmp	  byte ptr reg0_pag+[BX],0 ; is page number 0?
	  je	  null_t	   ; if register nil, jump
	  xor	  AX,AX 	   ; set register to nil (test false)
	  mov	  byte ptr reg0_pag+[BX],AL
	  mov	  reg0_dis+[BX],AX
	  jmp	  next
null_t:   mov	  AL,T_PAGE*2	   ; set register to 't
	  mov	  byte ptr reg0_pag+[BX],AL
	  mov	  AX,T_DISP
	  mov	  reg0_dis+[BX],AX
	  jmp	  next

;************************************************************************
;*							       AL   AH	*
;* Test for eq? (pointers identical)			EQ?    dest,src *
;************************************************************************
eq_p:	  lods	  word ptr ES:[SI] ; load source/dest operands
	  mov	  BL,AL 	   ; copy destination register number
	  mov	  DI,BX 	   ;  into TIPC register DI
	  mov	  BL,AH 	   ; copy source register number
	  mov	  AX,reg0_dis+[BX] ; load page number of source operand
	  cmp	  AX,reg0_dis+[DI] ; are the displacements identical?
	  jne	  eq_p_no	   ; if not, jump
	  mov	  AL,byte ptr reg0_pag+[BX] ; load src operand's page number
	  cmp	  AL,byte ptr reg0_pag+[DI] ; are page numbers identical?
	  jne	  eq_p_no	   ; if not, jump
	  mov	  byte ptr reg0_pag+[DI],T_PAGE*2 ; they're "eq"-- set
	  mov	  reg0_dis+[DI],T_DISP ;  result to 't (true)
	  jmp	  next		   ; return to the interpreter
;     pointers are not identical-- set result to nil
eq_p_no:  xor	  AX,AX
	  mov	  byte ptr reg0_pag+[DI],AL ; set page number and
	  mov	  reg0_dis+[DI],AX ;  displacement of result register to nil
	  jmp	  next		   ; return to the interpreter

;************************************************************************
;*							       AL   AH	*
;* Test for eqv? (pointers identical, or numbers equal) EQ?    dest,src *
;************************************************************************
eqv_p:	  lods	  word ptr ES:[SI] ; load source/dest operands
	  mov	  BL,AL 	   ; copy destination register number
	  mov	  DI,BX 	   ;  into TIPC register DI
	  mov	  BL,AH 	   ; copy source register number
	  mov	  AX,reg0_dis+[BX] ; load page number of source operand
	  cmp	  AX,reg0_dis+[DI] ; are the displacements identical?
	  jne	  eqv_p_no	   ; if not, jump
	  mov	  AL,byte ptr reg0_pag+[BX] ; load src operand's page number
	  cmp	  AL,byte ptr reg0_pag+[DI] ; are page numbers identical?
	  jne	  eqv_p_no	   ; if not, jump
	  mov	  byte ptr reg0_pag+[DI],T_PAGE*2 ; they're "eq"-- set
	  mov	  reg0_dis+[DI],T_DISP ;  result to 't (true)
	  jmp	  next		   ; return to the interpreter
;     pointers are not identical-- test for numbers
eqv_p_no: mov	  AH,BL 	   ; copy source register number and load
	  mov	  BL,byte ptr reg0_pag+[BX] ;  page number from source reg
	  test	  attrib+[BX],FIXNUMS+BIGNUMS+FLONUMS
	  jz	  eqv_p_s	   ; if not a number, jump
	  mov	  AX,DI 	   ; copy destination register number and load
	  mov	  BL,byte ptr reg0_pag+[DI] ;  page number from dest reg
	  test	  attrib+[BX],FIXNUMS+BIGNUMS+FLONUMS
	  jz	  eqv_p_s	   ; if not a number, jump
	  sub	  SI,2		   ;   else set ip back to operands
	  jmp	  eq_n		   ;	    and go test with "="
eqv_p_s:  test	  attrib+[BX],STRINGS
	  jz	  eqv_p_f	   ; if not a string, operands aren't eqv (jump)
	  add	  DI,offset reg0   ;   else compute address of destination reg
	  jmp	  short equal_p1   ;	    test using "equal?"
eqv_p_f:  xor	  AX,AX
	  mov	  byte ptr reg0_pag+[DI],AL ; set page number and
	  mov	  reg0_dis+[DI],AX ;  displacement of result register to nil
	  jmp	  next		   ; return to the interpreter


;************************************************************************
;*								AL   AH *
;* Test equality of s-expressions			equal?	dest,src*
;*									*
;* Purpose:  Scheme interpreter support for the testing of "equality"   *
;*		of two s-expressions.					*
;************************************************************************
equal_p:  lods	  word ptr ES:[SI] ; load operands to be compared
	  mov	  BL,AL 	   ; copy destination register number
	  lea	  DI,reg0+[BX]	   ;  and load its address
equal_p1: mov	  BL,AH 	   ; copy source register number
	  add	  BX,offset reg0   ;  and compute its address, too
	  pushm   <BX,DI>	   ; push arguments onto TIPC's stack
	  C_call  sequal_p,<SI>,Load_ES ; call: sequal(&dest,&src)
	  pop	  DI		   ; restore destination register's address
	  cmp	  AX,0		   ; are operands equal? (return code not zero)
	  je	  equal_f	   ; if not equal, jump
	  mov	  byte ptr [DI].C_page,T_PAGE*2 ; set result register
	  mov	  [DI].C_disp,T_DISP ;	to 't
	  jmp	  next_SP	   ; return to interpreter
equal_f:  mov	  byte ptr [DI].C_page,AL ; set result register to nil
	  mov	  [DI].C_disp,AX
	  jmp	  next_SP	   ; return to interpreter


;************************************************************************
;* Macro definition - Support for attribute tests			*
;************************************************************************
attr_mac  macro   condition
	  mov	  DX,condition	   ; load attribute mask for test
IFIDN	  <condition>,<LISTCELL>
attr_1:   lods	  byte ptr ES:[SI] ; fetch register to test
	  mov	  BX,AX 	   ; copy register number
	  mov	  DI,reg0_pag+[BX] ; load page number and
attr_2:   mov	  AX,attrib+[DI]   ;  and fetch page's attributes
	  and	  AX,DX 	   ; test against mask
	  jnz	  attr_3	   ; if non-zero, test is true (jump)
	  mov	  byte ptr reg0_pag+[BX],AL ; set result to nil (0)
	  mov	  reg0_dis+[BX],AX
	  jmp	  next		   ; return to interpreter
attr_3:   mov	  AL,T_PAGE*2	   ; set result to true
	  mov	  byte ptr reg0_pag+[BX],AL
	  mov	  AX,T_DISP
	  mov	  reg0_dis+[BX],AX
	  jmp	  next		   ; return to interpreter
ELSE
	  jmp	  attr_1	   ; continue attribute test
ENDIF
	  endm


;     Test for (atom? obj)
atom_p:   attr_mac ATOM

;     Test for (char? obj)
char_p:   attr_mac CHARS

;     Test for (closure? obj)
closur_p: attr_mac CLOSURE

;     Test for (code? obj)
code_p:   attr_mac CODE

;     Test for (continuation? obj)
contin_p: attr_mac CONTINU

;     Test for (float? obj)
float_p:  attr_mac FLONUMS

;     Test for (integer? obj)
integr_p: attr_mac FIXNUMS+BIGNUMS

;     Test for (number? obj)
number_p: attr_mac NUMBERS

;     Test for (pair? obj)
pair_p:   attr_mac LISTCELL

;     Test for (port? obj)
port_p:   mov	  DX,PORTS	   ; load "port" attribute bit mask
	  lods	  byte ptr ES:[SI] ; load instruction's operand
	  mov	  BX,AX 	   ;  and copy it into BX
	  mov	  DI,reg0_pag+[BX] ; load the page number of the operand
	  cmp	  DI,CON_PAGE	   ; is it same page as 'console?
	  jne	  attr_2	   ; if not, jump
	  mov	  AX,reg0_dis+[BX] ; load the displacement of the operand
	  cmp	  AX,CON_DISP	   ; is it 'console?
	  je	  attr_3	   ; if so, return #!true (jump)
	  jmp	  attr_2	   ; if not 'console, return #!false

;     Test for (proc? obj)
proc_p:   attr_mac CONTINU+CLOSURE

;     Test for (ref? obj)
ref_p:	  attr_mac REFS

;     Test for (string? obj)
string_p: attr_mac STRINGS

;     Test for (symbol? obj)
symbol_p: attr_mac SYMBOLS

;     Test for (vector? obj)
vector_p:  attr_mac VECTORS

	  purge   attr_mac

;************************************************************************
;*		      Common Support for EVEN?/ODD?			*
;*									*
;* Input Parameters:  ES:[SI] - pointer to even?/odd? instruction's     *
;*				operand.				*
;*		      DX ------ text address for "EVEN?" or "ODD?" to   *
;*				be used to create an error message if	*
;*				an error is detected.			*
;*									*
;* Output Parameters:  Zero Flag (condition code) - 0 => even number	*
;*						    1 => odd number	*
;*									*
;* Note:  If an invalid operand is detected, this routine exits to the	*
;*	  Scheme debugger.						*
;************************************************************************
eo_which: lods	  byte ptr ES:[SI] ; load operand to even?/odd? instruction
	  mov	  BX,AX 	   ; copy register number to BX
	  add	  BX,offset reg0   ;  and compute operand register's address
	  cmp	  byte ptr [BX].C_page,SPECFIX*2 ; is operand a fixnum?
	  jne	  eo_010	   ; if not a fixnum, jump
	  test	byte ptr [BX].C_disp,1 ; test LSB of fixnum value
	  ret			   ; return to even?/odd? support
;     Operand isn't a fixnum-- test for a bignum
eo_010:   mov	  DI,[BX].C_page   ; fetch operand's page number
	  cmp	  byte ptr ptype+[DI],BIGTYPE*2 ; is operand a bignum?
	  jne	  eo_020	   ; if not a bignum, error (jump)
	  mov	  CX,ES 	   ; save value in ES
	  LoadPage ES,DI	   ; load bignum;s paragraph address
;	  mov	  ES,pagetabl+[DI] ; load bignum's paragraph address
	  mov	  DI,[BX].C_disp   ; load bignum's displacement
	  test	  byte ptr ES:[DI].big_data,1 ; test LSB of bignum
	  mov	  ES,CX 	   ; restore ES register
	  ret			   ; return to even?/odd? support
;     ***Error-- operand isn't an integer***
eo_020:  pushm	  <BX,m_one,DX>    ; push operands to "set_src_error"
	 C_call   set_src_,<SI>,Load_ES ; call said
	 jmp	  sch_err	   ; link to Scheme debugger

;************************************************************************
;* is an integer even?					even?	dest	*
;*									*
;* Purpose:  Scheme interpreter support for the even? predicate.	*
;************************************************************************
even_p:   mov	  DX,offset m_even ; load text addr, in case of error
	  call	  eo_which	   ; is value even or odd?
	  jnz	  eo_false	   ; if LSB on, jump
eo_true:  mov	  byte ptr [BX].C_page,T_PAGE*2 ; result is #!true
	  mov	  [BX].C_disp,T_DISP
	  jmp	  next		   ; return to Scheme interpreter

;************************************************************************
;* is an integer odd?					odd?	dest	*
;*									*
;* Purpose:  Scheme interpreter support for the odd? predicate. 	*
;************************************************************************
odd_p:	  mov	  DX,offset m_odd  ; load text addr, in case of error
	  call	  eo_which	   ; is value even or odd?
	  jnz	  eo_true	   ; if LSB on, jump
eo_false: xor	  AX,AX 	   ; create a zero value for use as #!false
	  mov	  byte ptr [BX].C_page,AL ; result is #!false
	  mov	  [BX].C_disp,AX
	  jmp	  next		   ; return to Scheme interpreter

;************************************************************************
;* Macro definition - Support for arithmetic testing  (cond n1 n2)	*
;************************************************************************
JE_OPCOD  =	  01110100b
JNE_OPCOD =	  01110101b
JL_OPCOD  =	  01111100b
JGE_OPCOD =	  01111101b
JLE_OPCOD =	  01111110b
JG_OPCOD  =	  01111111b
cond_mac  macro   cond
	  local   x,y,y1,z,pred_T,labelx
IFIDN	  <cond>,<ne>
	  mov	  DX,NE_OP	   ; Load "!=" sub-opcode
	  STORE_BYTE_IN_CS PROG,cnd_jmp,JNE_OPCOD	;Protected Mode Macro
	  jmp	  short cnd_go
ELSE
IFIDN	  <cond>,<l>
	  mov	  DX,LT_OP	   ; Load "<" sub-opcode
	  STORE_BYTE_IN_CS PROG,cnd_jmp,JL_OPCOD	;Protected Mode Macro
	  jmp	  short cnd_go
ELSE
IFIDN	  <cond>,<g>
	  mov	  DX,GT_OP	   ; Load ">" sub-opcode
	  STORE_BYTE_IN_CS PROG,cnd_jmp,JG_OPCOD	;Protected Mode Macro
	  jmp	  short cnd_go
ELSE
IFIDN	  <cond>,<le>
	  mov	  DX,LE_OP	   ; Load "<=" sub-opcode
	  STORE_BYTE_IN_CS PROG,cnd_jmp,JLE_OPCOD	;Protected Mode Macro
	  jmp	  short cnd_go
ELSE
IFIDN	  <cond>,<ge>
	  mov	  DX,GE_OP	   ; Load ">=" sub-opcode
	  STORE_BYTE_IN_CS PROG,cnd_jmp,JGE_OPCOD	;Protected Mode Macro
	  jmp	  short cnd_go
ELSE
IFIDN	  <cond>,<e>
	  mov	  DX,EQ_OP	   ; Load "=" sub-opcode
	  STORE_BYTE_IN_CS PROG,cnd_jmp,JE_OPCOD	;Protected Mode Macro
cnd_go:   lods	  word ptr ES:[SI] ; load register numbers to compare
	  mov	  BL,AL 	   ; copy n1 register number
	  mov	  DI,BX 	   ;  into DI (clear high order byte)
	  cmp	  byte ptr reg0_pag+[DI],SPECFIX*2 ; is n1 a fixnum?
	  jne	  y		   ; if not, perform comparison out of line
	  mov	  BL,AH 	   ; copy n2 register number
	  cmp	  byte ptr reg0_pag+[BX],SPECFIX*2; is n2 a fixnum?
	  jne	  y1		   ; jump if not
	  mov	  AX,reg0_dis+[BX] ; load n2's immediate value
	  mov	  DX,reg0_dis+[DI] ; load n1's immediate value
	  shl	  DX,1		   ; adjust immediate values to sign
	  shl	  AX,1		   ; extend
	  cmp	  DX,AX 	   ; compare the two operands
cnd_jmp   equ	  $
	  j&cond	  z	   ; jump if comparison is satisfied
	  xor	  AX,AX 	   ; store '() in destination register
	  mov	  byte ptr reg0_pag+[DI],AL
	  mov	  reg0_dis+[DI],AX
	  jmp	  next		   ; return to interpreter
z:	  mov	  AL,T_PAGE*2	   ; store 't in destination register
	  mov	  byte ptr reg0_pag+[DI],AL
	  mov	  AX,T_DISP
	  mov	  reg0_dis+[DI],AX
	  jmp	  next		   ; return to interpreter
;     Operand(s) not fixnums-- perform comparison in C routine
y:	  mov	  BL,AH
y1:	  add	  BX,offset reg0   ; Load address of source register
	  add	  DI,offset reg0   ; Load address of destination register
	  pushm   <BX,DI,DX>	   ; Push src, dest, op arguments
	  C_call  arith2,<DI,SI>,load_ES ; Call the arithmetic processor
	  restore <DI>
	  cmp	  AX,0		   ; test result returned from arith2
	  jl	  labelx	   ; jump if error condition detected
	  jne	  pred_T	   ; jump if comparison is "true"
	  mov	  byte ptr [DI].C_page,AL ; store '() into destination register
	  mov	  [DI].C_disp,AX
	  jmp	  next_SP	   ; return to interpreter
pred_T:   mov	  byte ptr [DI].C_page,T_PAGE*2 ; set result register to 't
	  mov	  [DI].C_disp,T_DISP
	  jmp	  next_SP	   ; return to interpreter
labelx:   jmp	  sch_err	   ; link to Scheme debugger
ELSE
	  ***ERROR*** condition not recognized
ENDIF
ENDIF
ENDIF
ENDIF
ENDIF
ENDIF
	  endm

;     Test for numeric inequality (!= n1 n2)
ne_p:	  cond_mac ne

;     Test for numeric less than (< n1 n2)
lt_p:	  cond_mac l

;     Test for numeric greater than (> n1 n2)
gt_p:	  cond_mac g

;     Test for numeric less than or equal (<= n1 n2)
le_p:	  cond_mac le

;     Test for numeric greater than or equal (>= n1 n2)
ge_p:	  cond_mac ge

;     Test for numeric equality  (= n1 n2)
eq_n:	  cond_mac e

	  purge   cond_mac

;************************************************************************
;* Macro definition - Support for arithmetic testing (cond:0 n) 	*
;************************************************************************
cnd1_mac  macro   cond
	  local   x,y,z,cnd1_T,w
IFIDN	  <cond>,<l>
	  mov	  DX,NEG_OP	   ; load negative? comparison subopcode
	  STORE_BYTE_IN_CS PROG,cnd1_jmp,JL_OPCOD	;Protected Mode Macro
	  jmp	  cnd1_go	   ; process comparison with zero
ELSE
IFIDN	  <cond>,<g>
	  mov	  DX,POS_OP	   ; load positive? comparison subopcode
	  STORE_BYTE_IN_CS PROG,cnd1_jmp,JG_OPCOD	;Protected Mode Macro
	  jmp	  cnd1_go          ; process comparison with zero
ELSE
IFIDN	  <cond>,<e>
	  mov	  DX,ZERO_OP	   ; load zero? comparison subopcode
	  STORE_BYTE_IN_CS PROG,cnd1_jmp,JE_OPCOD	;Protected Mode Macro
cnd1_go:  lods	  byte ptr ES:[SI] ; load number of register to test
	  mov	  BX,AX
	  cmp	  byte ptr reg0_pag+[BX],SPECFIX*2 ; fixnum (immediate)?
	  jne	  y		   ; if not, go out of line
	  mov	  AX,reg0_dis+[BX] ; load immediate value
	  shl	  AX,1		   ; position sign bit-- set compare code
	  cmp	  AX,0
cnd1_jmp  equ	  $
	  j&cond	  z		   ; jump if condition satisfied
	  xor	  AX,AX 	   ; return '() in destination regsiter
	  mov	  byte ptr reg0_pag+[BX],AL
	  mov	  reg0_dis+[BX],AX
	  jmp	  next		   ; return to interpreter
z:	  mov	  AL,T_PAGE*2	   ; return 't in destintation register
	  mov	  byte ptr reg0_pag+[BX],AL
	  mov	  AX,T_DISP
	  mov	  reg0_dis+[BX],AX
	  jmp	  next		   ; return to interpreter
;     operand is not a fixnum-- call C routine to perform test
y:	  add	  BX,offset reg0   ; load address of destination reg
	  pushm   <BX,DX>	   ; push <reg, opcode> arguments
	  C_call  arith1,<BX,SI>,Load_ES ; link to arithmetic support
	  restore <BX>
	  cmp	  AX,0		   ; test result returned from "arith1"
	  jl	  w		   ; was error encountered?
	  jne	  cnd1_T	   ; if error, jump
	  mov	  byte ptr [BX].C_page,AL ; set result to "nil"
	  mov	  [BX].C_disp,AX
	  jmp	  next_SP	   ; return to interpreter
cnd1_T:   mov	  byte ptr [BX].C_page,T_PAGE*2 ; set result to "t"
	  mov	  [BX].C_disp,T_DISP
	  jmp	  next_SP	   ; resume interpretation
w:	  jmp	  sch_err	   ; link to Scheme debugger
ELSE
	  ***ERROR*** invalid comparison type
ENDIF
ENDIF
ENDIF
	  endm


;     Test for equality to zero (zero? n)
eq_z_p:   cnd1_mac e

;     Test for less than zero (negative? n)
lt_z_p:   cnd1_mac l

;     Test for greater than zero (positive? n)
gt_z_p:   cnd1_mac g

	  purge   cnd1_mac

;************************************************************************
;* (ascii->char n)				       ascii->char dest *
;*									*
;* Purpose:  Scheme interpreter support for the ascii->char function.	*
;************************************************************************
asc_char: lods	  byte ptr ES:[SI] ; load operand
	  mov	  DI,AX 	   ; copy dest register number into DI
	  cmp	  byte ptr reg0_pag+[DI],SPECFIX*2 ; is operand a finxum?
	  jne	  asc_cher	   ; if not, error (jump)
	  and	  reg0_dis+[DI],00ffH ; "and" off low order eight bits
	  mov	  byte ptr reg0_pag+[DI],SPECCHAR*2 ; convert to character
	  jmp	  next		   ; return to interpreter
asc_cher: save	  <SI>		   ; save the incremented location pointer
	  lea	  BX,masc_ch	   ; load address of "ascii->char" text
	  jmp	  src_err	   ; display invalid operand message

;************************************************************************
;* (char->ascii n)				       char->ascii dest *
;*									*
;* Purpose:  Scheme interpreter support for the char->ascii function.	*
;************************************************************************
char_asc: lods	  byte ptr ES:[SI] ; load operand
	  mov	  DI,AX 	   ; copy dest register number into DI
	  cmp	  byte ptr reg0_pag+[DI],SPECCHAR*2 ; is operand a char?
	  jne	  ch_ascer	   ; if not, error (jump)
	  mov	  byte ptr reg0_pag+[DI],SPECFIX*2 ; convert to a fixnum
	  jmp	  next		   ; return to interpreter
ch_ascer: save	  <SI>		   ; save the incremented location pointer
	  lea	  BX,mch_asc	   ; load address of "char->ascii" text
	  jmp	  src_err	   ; display invalid operand message

;************************************************************************
;*     Support for list length	(length list)				*
;************************************************************************
slength:  lods	  byte ptr ES:[SI] ; load register containing list header
	  mov	  BX,AX
	  save	  <SI>		   ; save the program counter
	  lea	  DI,reg0+[BX]	   ; load the address of the dest reg
	  mov	  BX,[DI].C_page   ; load list header from src/dest reg
	  mov	  SI,[DI].C_disp
	  xor	  AX,AX 	   ; zero the counter
	  mov	  CX,SB_CHECK	   ; load shift-break iteration count
slenloop: cmp	  BL,NIL_PAGE*2    ; pointer to nil?
	  je	  slendone
	  cmp	  byte ptr ptype+[BX],LISTTYPE*2 ; list pointer?
	  jne	  slendone
	  inc	  AX		   ; increment list cell count
	  LoadPage ES,BX	   ; load list cell page para address
;	  mov	  ES,pagetabl+[BX] ; load list cell page para address
	  mov	  BL,ES:[SI].cdr_page ; load cdr of list cell
	  mov	  SI,ES:[SI].cdr
	  loop	  slenloop	   ; cdr down list
;     Every so many iterations, check the shift-break key
	  mov	  CX,SB_CHECK	   ; reload the shift-break iteration count
	  cmp	  s_break,0	   ; has the shift-break key been depressed?
	  je	  slenloop	   ; if no interrupt, continue (jump)
slen_sb:  mov	  AX,2		   ; load instruction length = 2
	  jmp	  restart1	   ; link to Scheme debugger
slendone: mov	  byte ptr [DI].C_page,SPECFIX*2 ; return result as a
	  mov	  [DI].C_disp,AX   ;  fixnum (immediate)
	  jmp	  next_PC	   ; return to interpreter

;************************************************************************
;*     Support for Last-pair	(last-pair list)			*
;************************************************************************
lst_pair: lods	  byte ptr ES:[SI] ; load src/destination register
	  save	  <SI>		   ; save the interpreter's program counter
	  mov	  DI,AX
	  mov	  BX,reg0_pag+[DI] ; load register's page number field
	  cmp	  BL,NIL_PAGE*2    ; null pointer?
	  je	  lst_exit	   ; if so, do nothing
	  cmp	  byte ptr ptype+[BX],LISTTYPE*2 ; does reg point to list cell?
	  jne	  lst_exit	   ; if not, return it as is
	  mov	  SI,reg0_dis+[DI] ; load register's displacement
	  xor	  DX,DX
	  mov	  CX,SB_CHECK	   ; load the shift-break iteration count
lst_loop: LoadPage ES,BX	   ; load page's paragraph address
;	  mov	  ES,pagetabl+[BX] ; load page's paragraph address
	  mov	  DL,ES:[SI].cdr_page ; load page number of cdr pointer
	  cmp	  DL,NIL_PAGE*2    ; cdr nil?
	  je	  lst_done	   ; if so, return current pointer
	  mov	  DI,DX 	   ; copy cdr's page number
	  cmp	  byte ptr ptype+[DI],LISTTYPE*2 ; cdr points to list cell?
	  jne	  lst_done	   ; if not, we're at the end of our list
	  mov	  BL,DL 	   ; follow linked list
	  mov	  SI,ES:[SI].cdr
	  loop	  lst_loop
;     Every so many iterations, check the shift-break key
	  mov	  CX,SB_CHECK	   ; reload the shift-break iteration count
	  cmp	  s_break,0	   ; has the shift-break key been depressed?
	  je	  lst_loop	   ; if no interrupt, continue (jump)
	  jmp	  slen_sb	   ; link to Scheme debugger
lst_done: mov	  DI,AX 	   ; re-load destination register number
	  mov	  byte ptr reg0_pag+[DI],BL ; store page number
	  mov	  reg0_dis+[DI],SI ; store displacement
lst_exit: jmp	  next_PC	   ; return to interpreter

;************************************************************************
;* (reverse! list)					reverse!  dest	*
;*									*
;* Purpose:  Scheme interpreter support for the reverse! primitive	*
;*									*
;* Notes:  The following registers are used by this routine:		*
;*		BL - page number of the current list cell		*
;*		DI - displacement of the current list cell		*
;*		ES - paragraph address of the current list cell 	*
;*			Note: ES:[DI] address the current list cell	*
;*		DL - page number of the previous list cell		*
;*		AX - displacement of the previous list cell		*
;*		SI - destination register number			*
;************************************************************************
	  public  reverseb
reverseb: lods	  byte ptr ES:[SI] ; load operand containing list pointer
	  save	  <SI>		   ; preserve the location pointer
	  mov	  BL,AL 	   ; copy number of operand register
	  mov	  SI,BX 	   ;  and put a copy into TIPC register SI
	  mov	  BL,byte ptr reg0_pag+[SI] ; load contents of operand register
	  mov	  DI,reg0_dis+[SI] ;  for initial "current cell" pointer
	  xor	  AX,AX 	   ; define previous list cell to be 'nil
	  xor	  DX,DX
rev_lp:   cmp	  BL,0		   ; end of list (current cell nil)?
	  je	  rev_done	   ; if so, jump
	  cmp	  byte ptr ptype+[BX],LISTTYPE*2 ; is current cell a list cell?
	  jne	  rev_huh	   ; if not, error (jump)
	  LoadPage ES,BX	   ; load current cell's page address
;	  mov	  ES,pagetabl+[BX] ; load current cell's page address
	  xchg	  ES:[DI].cdr_page,DL ; swap cdr field with previous cell
	  xchg	  ES:[DI].cdr,AX   ;  pointer
	  xchg	  BX,DX 	   ; current cell <-> (cdr current cell)
	  xchg	  DI,AX
	  jmp	  rev_lp	   ; continue down list
;     list reversal complete-- update destintation register
rev_done: mov	  byte ptr reg0_pag+[SI],DL ; make destination register point
	  mov	  reg0_dis+[SI],AX ;  to new head of (reversed) list
	  jmp	  next_PC	   ; return to the interpreter
;     ***error-- not a valid linked list***
rev_huh:  mov	  byte ptr reg0_pag+[SI],DL ; make destination register point
	  mov	  reg0_dis+[SI],AX ;  to new head of (reversed) list
	  lea	  BX,m_revb
	  jmp	  src_err	   ; display error message


IFNDEF PROMEM

; Real mode scheme has graphics linked in with the vm (see graphics.exe),
; and can therefore call it directly.  Protected mode scheme must xfer
; to a real mode graphics routine - see PROIO.asm.

;************************************************************************
;* Interface to Graphic Primitives	     (%graphics  arg1 ... arg7) *
;************************************************************************
sgraph:   mov	  CX,7		   ; load counter-- seven arguments
	  xor	  DX,DX 	   ; set error flag = FALSE
	  lods	  byte ptr ES:[SI] ; load first argument
	  save	  <AX>		   ;  and save as destination register
	  jmp	  short sgraph0
sgraph1:  lods	  byte ptr ES:[SI] ; load next argument
sgraph0:  xor	  AH,AH 	   ; be sure high byte is zero
	  mov	  BX,AX 	   ; copy register number to BX
	  cmp	  byte ptr reg0_pag+[BX],SPECFIX*2 ; is arg a fixnum?
	  je	  sgraph2	   ; if arg *is* a fixnum, o.k. (jump)
	  inc	  DX		   ; indicate an invalid argument
sgraph2:  mov	  AX,reg0_dis+[BX] ; expand 15-bit signed int to 16-bit signed int
	  shl	  AX,1
	  sar	  AX,1
	  push	  AX		   ; push 16-bit signed integer
;sgraph2:  push    reg0_dis+[BX]    ; push immediate value of argument
	  loop	  sgraph1	   ; continue 'til all arguments processed
	  cmp	  DX,0		   ; any argument errors?
	  jne	  sgraph3	   ; if errors encountered, jump
	  save	  <SI>		   ; save the location pointer
;;;   the following two lines are comment out
;;;   allow graphics mode for unknown PC machine
;;;	  cmp	  PC_MAKE,UNKNOWN  ; running on a TIPC?
;;;	  je	  not_pc	   ; if not a TI or IBM brand PC, jump
	  call	  graphit	   ; perform the graphics operation
	  shl	  AX,1		   ; clear high order bit of result
	  shr	  AX,1		   ;  (convert to immediate value)
	  mov	  BX,[BP].save_AX  ; reload destintation register number
	  mov	  reg0_dis+[BX],AX ; store returned result into destination reg
not_pc:   jmp	  next_SP	   ; return to interpreter
sgraph3:  mov	  BX,offset m_graph ; load addr of "%graphics" text
	  jmp	  src_err	   ; link to Scheme debugger
ENDIF

;************************************************************************
;* Interface to XLI external escape					*
;* (%xesc length nargs "name" arg1 ... arg16) where argx is optional    *
;*     length = # args *to %xesc instruction*				*
;************************************************************************

xesc:	  lods	  byte ptr es:[si] ;get xesc length (variable-length inst.);
				   ;afterwards, bytecode@ (ES:SI) points
				   ;at name string
	  xor	  ah,ah
	  mov	  dx,si 	   ;tempsave bytecode@ to name string
	  add	  si,ax 	   ;get ptr to next opcode
	  dec	  si
	  save	  <es,si>
	  mov	  si,dx 	   ;restore bytecode@ to name string
	  call	  xli_xesc	   ;do xesc
	  restore <es,si>	   ;ES:SI is next opcode @
	  cmp	  ax,0		   ;any errors?
	  jne	  xesc_10	   ;yes, jump
	  jmp	  next		   ;return to interpreter
; normal errors - ax=error number, bx=irritant
xesc_10:  
	  xchg	  bx,ax		   ;now ax=irritant,bx=error message			
	  shl	  bx,1		   ;make message# into index
	  mov	  bx,xli_err[bx]   ;bx => error message
	  mov	  cx,1		   ;cx =  1, error non-restartable
	  pushm   <ax,bx,cx>
	  mov	  ax,ds
	  mov	  es,ax 	   ;Lattice C needs DS=ES
	  C_call  set_erro	   ;set up error
	  jmp	  sch_err	   ;jump into debugger



;************************************************************************
;* ******************************************************************** *
;* *								      * *
;* *			    Error routines			      * *
;* *								      * *
;* ******************************************************************** *
;************************************************************************

IFDEF PROMEM
;
; Engine timer support for protected mode
;
set_args  struc
	  dw	  ?
	  dw	  ?
hi	  dw	  ?
lo	  dw	  ?
set_args  ends

	  public  settimer
; initialize the timer, set up the engine tick vm loop, and begin
settimer  proc    near
	  xor	  ax,ax			   ;clear ax
	  cmp	  tickstat,-1		   ;check for normal run mode
	  jne	  no_set		   ;abort if timeout or engine running
	  push	  bp
	  mov	  bp,sp
	  mov	  ax,[bp].hi		   ;initialize the timer
	  mov	  hi_time,ax
	  mov	  ax,[bp].lo
	  mov	  lo_time,ax
	  mov	  ax,word ptr cs:eng_tick     ;ax = handle to engine loop
	  XCHG_WORD_IN_CS PROG,next1,ax	      ;int loop now jumps to engine loop
	  STORE_WORD_IN_CS PROG,reset_tim,ax  ;save original value in reset_tim
	  mov	  al,1			   
	  mov	  tickstat,al		   ;denote engine now running
	  pop	  bp
no_set:   ret
settimer  endp

	  public  rsttimer
; reset vm loop to original vm loop and return whats left in timer
rsttimer  proc    near
	  cmp	  tickstat,1		   ;only if timeout or engine running
	  ja 	  no_reset		   ;otherwise, forget it
	  mov	  ax,cs:reset_tim          ;get inst saved at top of vm loop
	  STORE_WORD_IN_CS PROG,next1,AX   ;reset forced branch at top (next1)
	  mov	  tickstat,-1		   ;no engine running
no_reset: 
	  mov	  ax,hi_time		   ;return time left
	  mov	  bx,lo_time
	  ret				   ;return
rsttimer  endp

ENDIF

;************************************************************************
;*			     Timer Ran Down				*
;************************************************************************
;     Note:  the "reset_tim" variable must be in the code segment 'cause
;		there's no telling where the DS register points when a
;		timer interrupt occurs.
reset_tim dw	  0		   	 ; save area for resetting a timer int
	  public  timeout
timeout:  
IFNDEF PROMEM
	  mov	  AX,CS:reset_tim        ; for real mode scheme, reset forced 
	  STORE_WORD_IN_CS PROG,next1,AX ; branch at top of vm loop
ENDIF
	  C_call  rsttimer	         ; turn off the timer support
	  mov	  BX,TIMEOUT_CONDITION   ; load "timeout" error code
time_1:   xor	  AX,AX 	         ; set code for "restartable" operation
time_2:   mov	  CX,offset nil_reg      ; set *irritant* to 'nil
time_3:   pushm   <CX,BX,AX>	         ; push arguments to call
	  C_call  set_nume,<SI>,Load_ES  ; call: set_numeric_error(1,13,nil_reg)
	  restore <SI>		         ; load next instruction's offset
	  jmp	  sch_err	         ; link to Scheme debugger

;************************************************************************
;*			Shift-Break Interrupt				*
;************************************************************************
sc_debug: mov	  AX,CS:reset_sb           ; reset forced branch at top
	  STORE_WORD_IN_CS PROG,next1,AX   ; of vm loop
	  mov	  s_break,0	           ; reset shift-break flag
	  mov	  BX,SHIFT_BREAK_CONDITION ; load "shift-break" error code
	  jmp	  time_1	           ; complete link to Scheme debugger

;************************************************************************
;*			DOS fatal I/O error process			*
;************************************************************************
	  public  dos_err
dos_err:  pop	  AX		   ; dump return address
	  pop	  AX		   ; restart/non-restart flag
	  pop	  BX		   ; error code
	  pop	  CX		   ; *irritant*
	  mov	  BP,reset_BP	   ; clean up stack
	  jmp	  time_3	   ; go invoke Scheme debugger

;************************************************************************
;*			Error-- Undefined Opcode			*
;************************************************************************
not_op:   dec	  SI		   ; back up location pointer
	  save	  <SI>		   ;  and save it
	  mov	  BX,offset m_not_op ; load address of error message
	  mov	  byte ptr tmp_page,SPECFIX*2 ; convert opcode to a fixnum
	  mov	  tmp_disp,AX	   ;  representation for use as "irritant"
	  mov	  AX,offset tmp_reg
	  jmp	  recom_1	   ; jump to common processing point

;************************************************************************
;*			Error-- Invalid Source Operand			*
;************************************************************************
	  public  src_err
;     Note:  at this point, BX contains the address for text of failing inst.
src_err:  xor	  AX,AX 	    ; AX <- 0
	  pushm   <AX,BX>	    ; push string address, 0
	  C_call  set_src_,,Load_ES ; call:  set_src_err(text, 0);
	  jmp	  sch_err	    ; link to Scheme debugger


;************************************************************************
;*   Error-- Object Module Not Compatible With Current Revision Level	*
;************************************************************************
recompil: mov	  AX,offset nil_reg
	  mov	  BX,offset m_recomp
recom_1:  mov	  CX,1
	  pushm   <AX,BX,CX>
	  C_call  set_erro	    ; set the error parameters
	  restore <SI>		    ; reload the current location pointer
	  jmp	  sch_err	    ; link to Scheme debugger

;************************************************************************
;*			Error-- Feature Not Yet Implemented		*
;************************************************************************
	  public  not_yet
not_yet:  mov	  BX,offset m_not_yt ; load address of "not yet implemented"
	  push	  BX		   ;  and push as argument to printf
	  dec	  SI		   ; back up location pointer
	  save	  <SI>		   ;  and save it

;     ***general call to printf for message reporting***
	  public  printf_c
printf_c: C_call  printf,,Load_ES ; call printf
	  mov	  SP,BP 	   ; dump arguments off stack
	  restore <SI>		   ; reload location pointer into SI
	  jmp	  debug 	   ; begin debug mode

;************************************************************************
;*		    Force Restart of Current Operation			*
;************************************************************************
	  public  restart
restart:  pop	  AX		   ; discard the return address
	  pop	  AX		   ; fetch instruction length
	  mov	  BP,reset_BP	   ; clean up the TIPC's stack
restart1: sub	  [BP].save_SI,AX  ; back up the instruction pointer
	  jmp	  next_SP	   ; return to the Scheme interpreter

;************************************************************************
;*			Link to the Scheme Debugger			*
;************************************************************************
	  public  sch_err
sch_err:  push	  SI		   ; save address of instruction to retry
	  call	  force_ca	   ; force a new stack frame to be built
	  mov	  SP,BP 	   ; drop argument from TIPC's stack
	  mov	  BX,SPECCODE*2    ; load code base pointer for debug init
	  mov	  byte ptr CB_pag,BL
	  mov	  CB_dis,0
	  LoadCode ES,BX	   ; load code base's paragraph address
;	  mov	  ES,pagetabl+[BX] ; load code base's paragraph address
	  save	  <ES>		   ;  and save it off
	  mov	  SI,ERR_ent	   ; load error entry point offset
	  jmp	  next		   ; begin link to debugger

;************************************************************************
;* Scheme-Reset/Reset							*
;*									*
;* Purpose:  To re-initialize the VM's environment to correct for       *
;*		some error condition					*
;************************************************************************
	  public  force_re
force_re: mov	  BP,reset_BP	   ; reset TIPC stack to its initial state
;     Note:  control falls through to the scheme-reset code below

s_reset:  C_call  scheme_r,,Load_ES ; Adjust fluid environment
;     Note:  control falls through the reset code below

reset:	  C_call  reset_fa,,Load_ES ; reset %fasl input data structures
	  xor	  AX,AX 	   ; create a value of zero/nil
;     set the "previous stack segment" register to nil
	  mov	  PREV_pag,AX
	  mov	  PREV_dis,AX
;     set the current code base to the loader's code page
	  mov	  CB_dis,AX
	  mov	  CB_pag,SPECCODE*2
;     reset the current stack base to zero and initialize FP
	  mov	  BASE,AX
	  mov	  FP,AX
	  mov	  TOS,SF_OVHD-PTRSIZE
;     set the location pointer and code paragraph address
	  mov	  BX,SPECCODE*2
	  LoadCode ES,BX
;	  mov	  ES,pagetabl+[BX]
	  save	  <ES>
	  mov	  SI,RST_ent	   ; load the new location pointer
;     Note:  Control falls through the %clear-registers support below

;************************************************************************
;* Clear VM registers					clear-regs	*
;************************************************************************
clr_regs: save	  <SI>		   ; save the current location pointer
	  mov	  BX,UN_DISP	   ; load pointer for "unbound" symbol
	  mov	  DX,UN_PAGE*2
	  mov	  CX,NUM_REGS-2    ; load iteration count
	  mov	  DI,offset reg0+(SIZE C_ptr)*2 ; load address of VM register 2
	  mov	  AX,DS 	   ; set TIPC register ES to point to the
	  mov	  ES,AX 	   ;  current data section
clr_loop: mov	  AX,BX 	   ; copy '**unbound** displacement pointer
	  stosw 		   ;  and store it into next register
	  mov	  AX,DX 	   ; do likewise for the page number component
	  stosw 		   ; for the '**unbound** symbol
	  loop	  clr_loop	   ; iterate through the VM's registers
	  xor	  AX,AX
	  mov	  DI,offset reg0   ; store #!false into R0 and R1
	  mov	  CX,4
rep	  stosw
	  mov	  tmp_disp,AX	   ; clear the VM's temporary register, too
	  mov	  tmp_page,AX
	  mov	  tm2_disp,AX	   ; clear the VM's temporary register, too
	  mov	  tm2_page,AX
	  jmp	  next_PC	   ; return to the interpreter

;************************************************************************
;* Escape to user defined assembly language or C Function	%escn	*
;************************************************************************
s_esc1:   mov	  CX,1		   ; load argument count
	  jmp	  short s_escn	   ; branch to general argument load routine
s_esc2:   mov	  CX,2		   ; load argument count
	  jmp	  short s_escn	   ; branch to general argument load routine
s_esc3:   mov	  CX,3		   ; load argument count
	  jmp	  short s_escn	   ; branch to general argument load routine
s_esc4:   mov	  CX,4		   ; load argument count
	  jmp	  short s_escn	   ; branch to general argument load routine
s_esc5:   mov	  CX,5		   ; load argument count
	  jmp	  short s_escn	   ; branch to general argument load routine
s_esc6:   mov	  CX,6		   ; load argument count
	  jmp	  short s_escn	   ; branch to general argument load routine
	  public  s_esc7
s_esc7:   mov	  CX,7		   ; load argument count
	  jmp	  short s_escn	   ; branch to general argument load routine

s_escn:   mov	  DX,CX 	   ; copy count of arguments
s_es_10:  xor	  AX,AX
	  lods	  byte ptr ES:[SI] ; load next argument register number
	  add	  AX,offset reg0
	  push	  AX
	  loop	  s_es_10	   ; continue 'til all arguments processed
	  push	  DX		   ; push number of arguments

IFDEF PROMEM
;
; This is pretty kludgy, but there wasn't time to redo all the %esc
; functions into vm codes, and we need software interrupt to work
; as before. Anyway, sw-int is the only %escape function with seven
; arguments, so lets short circuit here and call the protected mode
; software interrupt code.
;
	  cmp	  dx,7		;are there seven arguments?
	  jne	  s_es_12	;  no, can't be sw_int
	  save	  <SI>		;save off instruction pointer
	  call	  softint	;and call the pro mode sw_int routine
	  cmp	  ax,0		;any errors?
	  je      s_es_20	;  no, jump
; errors - ax=error number, bx=msg address, cx=irritant
	  pushm   <cx,bx,ax>	;push args to error routine
	  mov	  ax,ds
	  mov	  es,ax 	;Lattice C needs DS=ES
	  C_call  set_erro	;call error routine
	  restore <SI>		;restore instruction pointer
	  jmp	  sch_err	;and jump to scheme debugger
ENDIF

s_es_12:
	  C_call  asm_link,<SI>,Load_ES
s_es_13:
	  cmp	  AX,0
	  je	  s_es_20
	  restore <SI>		   ; restore address of next instruction
	  mov	  BX,offset m_esc
	  jmp	  src_err	   ; report some sort of operand error
s_es_20:  jmp	  next_SP	   ; no error reported; return to interpreter

;************************************************************************
;* (%str-append str1 start1 end1 {nil,char,str2} str3 start3 end3)	*
;************************************************************************
s_append: jmp	  str_apnd	   ; far jump to substring-append support

;************************************************************************
;* (%substring-display str start end row-displacement window)		*
;************************************************************************
	  public  s_disply	   ; ***temporary***
s_disply: jmp	  str_disp	   ; far jump to substring-display support

;************************************************************************
;* Invoke garbage collection					gc	*
;************************************************************************
gc:	  mov	  byte ptr tmp_page,0 ; clear tmp_reg prior to GC
	  mov	  tmp_disp,0
	  mov	  byte ptr tm2_page,0 ; clear tm2_reg prior to GC
	  mov	  tm2_disp,0
	  C_call  garbage,<SI>,Load_ES ; call garbage collection driver
	  jmp	  next_SP

;************************************************************************
;* Invoke garbage collection with compaction			gc2	*
;************************************************************************
sgc2:	  mov	  byte ptr tmp_page,0 ; clear tmp_reg prior to GC
	  mov	  tmp_disp,0
	  mov	  byte ptr tm2_page,0 ; clear tm2_reg prior to GC
	  mov	  tm2_disp,0
	  C_call  garbage,<SI>,Load_ES ; call garbage collection driver
	  C_call  gcsquish
	  jmp	  next_SP

;************************************************************************
;* Begin Debug						   %begin-debug *
;************************************************************************
	  public  debug_op
debug_op: mov	  VM_debug,1	         ; enable VM debugger for (%begin-debug)
debug:	  
	  mov	  AX,word ptr CS:trc_go  ; modify interpreter to enable instr.
	  STORE_WORD_IN_CS PROG,next1,AX ; Protected Mode Macro
	  mov	  s_break,0	         ; reset shift-break flag
	  mov	  AX,2		         ; set return value = 2 (begin debug)
	  jmp	  short exit_010

exit_op:  dec	  SI		         ; back up PC to won't fall past end
	  mov	  AX,1		         ; set return value = 1 (halt)
	  jmp	  short exit_010

exit:	  xor	  AX,AX 	         ; set return value = 0 (suspend)
exit_010: mov	  SP,BP
	  mov	  BX,[BP].cod_ent
	  mov	  [BX],SI
	  add	  SP,offset sint_BP
	  pop	  BP
	  mov	  DX,DGROUP
	  mov	  ES,DX
	  ret
run	  endp

prog	  ends
	  end
