;							=====> XLI.ASM
; PC Scheme External Language Interface (XLI)
; (c) 1987,1988 by Texas Instruments Incorporated  --  all rights reserved
; Author:  Bob Beal
; History:
;   rb  3/20/87 - original
;   rb  2/ 2/88 - check XLI ID; 
;		  added external-program error return
;


	  page	  84,120
	  name	  PCSXLI
	  title   PC Scheme External Language Interface

	  include scheme.equ
	  include xli.equ
	  include xli.ref
	  include xli.mac

	  subttl  Group and Constant definitions
	  page
pgroup	  group   prog
xgroup	  group   progx
dgroup	  group   data

	  subttl  Data segment definitions
	  page

data	  segment para public 'DATA'
	  assume  ds:dgroup

	  public  load_table,work_area,active_exe	;??? for debugging

; Various tables
load_table  dw	  N_EXE dup (0)    ;PSP addresses (segment)
fb_table    dd	  N_EXE dup (0)    ;file block addresses (offset,segment)
pb_table    dd	  N_EXE dup (0)    ;parm block addresses (offset,segment)
state_table state N_EXE dup (<>)   ;child's regs at point it called us
status_table label word 	   ;records .EXE state (MSBy) and index (LSBy)
x	  =	  0
	  rept	  N_EXE
	  dw	  x
x	  =	  x+1
	  endm

; Parameter block for EXEC function request
zero	  equ	  $		   ;a constant zero
exec_pblock dw	  0		   ;env@ (use Scheme's)
	    dw	  zero,seg zero    ;cmd line@ (don't care)
	    dd	  -1		   ;FCB@'s (don't care)
	    dd	  -1

; Working storage (during a given call to the external routine)
	  align   16,data
work_area label   word		   ;for dealing with PCS data values
exe_name  label   byte		   ;a filename from XLI's control file
	  db	  PAD_SIZE*N_ARGS dup (0)  ;during xesc, non-strings go here
; other information required during an xesc call
work_info xesc_struc <> 		;general info
swap_table swap_struc N_ARGS dup (<>)	;records swap state for each XCALL arg
exe_name1 dw	  offset exe_name  ;pointer into exe_name to filename.extension
				   ;(i.e. points past directory prefix)
bid_name  dw	  0		   ;pointer into exe_name; used for bidding child
; the child currently active or being loaded
active_exe dw	  0		   ;(same format as status table)
; system .EXE information
sysflag   db	  0		   ;0=user .EXE; 1=system .EXE
				   ;look for system .EXE's only in pcs-sysdir
pcs_exe   db	  'newtrig.exe',0  ;PCS system .EXE files
pcs_exe_len equ   $-pcs_exe

; State (context) information
; child's registers upon calling PCS
save_ax   dw	  0		   ;actually, we ignore ax..di entries
save_bx   dw	  0
save_cx   dw	  0
save_dx   dw	  0
save_si   dw	  0
save_di   dw	  0
save_ds   dw	  0
save_es   dw	  0
save_ss   dw	  0
save_sp   dw	  0
save_bp   dw	  0
; our registers upon calling child
pcs_state state   <>		   ;our state at point of calling child
; the SP just after the indirect call through "gate";
; error recovery is done with:
;	  mov	  SP,gate_SP
;	  ret
; which returns to the instruction following the indirect call
gate_SP   dw	  0


data	  ends


	  subttl  Code segment:  load_all
	  page

; external variables
	  extrn   ctl_file:word,pcs_sysd:word
	  extrn   regs:word
; external routines
	  extrn   alloc_fl:near,int2long:near,long2int:near,alloc_bl:near
	  extrn   getbase:near

progx	  segment para public 'PROGX'
	  assume  cs:xgroup,ds:dgroup,es:dgroup,ss:dgroup

	  public  %xli_gate
	  public  load_all,load_exe,bid_child,c2p_handler,c2p_terminate
	  public  xesc1,unload_all,find_open_spot,table_search
	  public  do_floarg,do_fixarg,do_bigarg,do_strarg
	  public  do_floval,do_intval,do_TFval,do_strval
	  public  unload_exe,unload_all,update_swap_table


; This routine is called when PCS first comes up.  External programs are
; loaded first (by this file) and then the Scheme heap is allocated (initmem).
; Any errors encountered are ignored.  If we run of memory, initmem
; should see it too and notify smain to abort.

load_all  proc
; First copy into exe_name buffer the pcs-sysdir name.
	  cld
	  push	  ds				 ;make ES=DS
	  pop	  es
	  mov	  di,pcs_sysd
	  mov	  cx,64 			 ;max length of pathname
	  mov	  al,0
    repne scasb 				 ;look for eos character (=0)
	  jcxz	  la_5				 ;jump if none
	  dec	  di
la_5:	  mov	  cx,di
	  sub	  cx,pcs_sysd			 ;get length of pcs-sysdir
						 ;without eos character
	  mov	  si,pcs_sysd
	  mov	  di,offset exe_name
      rep movsb 				 ;copy pcs-sysdir name (- eos)
						 ;into exe_name buffer
	  mov	  al,'\'                         ;append \ onto pcs-sysdir name
	  dec	  di
	  cmp	  [di],al			 ;is '\' last char of pcs-sysdir?
	  je	  la_7				 ;yes; write it over itself
	  inc	  di				 ;no; move past end, then write
la_7:	  stosb
	  mov	  exe_name1,di			 ;exe_name1 points to next
						 ;open position in exe_name
; load system .EXE files
	  mov	  bx,ctl_file
	  cmp	  byte ptr [bx],'-'              ;suppress loading system .EXE's?
	  jne	  sysload			 ;no
	  inc	  ctl_file			 ;yes, move past marker
	  jmp	  short userload		 ;and skip loading system .EXE's
sysload:  mov	  si,offset pcs_exe		 ;get first system .EXE name
	  mov	  di,exe_name1
	  mov	  cx,pcs_exe_len
      rep movsb 				 ;copy into buffer after
						 ;pcs-sysdir name
	  mov	  sysflag,1			 ;set sysflag
	  call	  load_exe			 ;load the file
; open XLI control file
userload: mov	  sysflag,0
	  dos_fr  FR_OPEN,,,ctl_file,ds
	  mov	  bx,ax 			 ;put handle in BX
	  jnc	  next_file			 ;jump if no open errors
	  jmp	  close1			 ;can't open file, exit
; read in next filename off the control file and append it to
; the pcs-sysdir name
next_file: mov	  dx,exe_name1			 ;init buffer @
next_char: dos_fr FR_READ,bx,1,dx,ds		 ;read 1 character
	  jnc	  la_10 			 ;jump if no error, else
						 ;suddenly can't read control
						 ;file, close it and exit
close:	  dos_fr  FR_CLOSE,bx
close1:   ret

la_10:	  cmp	  ax,0				 ;at eof?
	  jz	  close 			 ;yes, jump
; we've read a character
	  mov	  si,dx
	  cmp	  byte ptr [si],' '              ;blank?
	  je	  next_char			 ;yes, skip it
	  cmp	  byte ptr [si],0Dh		 ;carriage return?
	  je	  got_file			 ;yes, jump
	  cmp	  byte ptr [si],' '              ;control char?
	  jl	  next_char			 ;yes, skip it
	  inc	  dx				 ;point to next buffer position
	  jmp	  next_char
; we've read a complete filename, go load it
got_file: mov	  byte ptr [si],0		 ;form ASCIIZ string
	  call	  load_exe			 ;bid it
	  jnc	  next_file			 ;jump if no errors
	  cmp	  ax,0				 ;any open slots?
	  je	  close 			 ;no, jump
	  cmp	  ax,2				 ;file found?
	  je	  next_file			 ;no, jump
	  cmp	  ax,8				 ;ran out of memory?
	  jne	  next_file			 ;no, jump; ignorable error
	  jmp	  close 			 ;yes

load_all  endp

; Given exe_name, this routine loads the child into any available open slot.
; On exit:
;   success: carry clear
;   failure: carry set
;	     AX = 0  : no open slots
;	     AX <> 0 : EXEC failure code
load_exe  proc
	  push	  ax
	  push	  bx
; if we succeed, state=EXE_NONE
	  call	  find_open_spot		 ;this sets active_exe
	  mov	  ax,0
	  jc	  le_exit			 ;no open slots
; set state=EXE_TSR for time between EXEC and TSR
	  load_index itself
	  mov	  bh,EXE_TSR
	  mov	  active_exe,bx
	  cmp	  sysflag,1			 ;loading system .EXE?
	  je	  le_5				 ;yes, look only in pcs-sysdir
	  mov	  ax,exe_name1			 ;try current directory first
	  mov	  bid_name,ax
	  call	  bid_child
	  jnc	  le_10 			 ;bid succeeded, jump
le_5:	  mov	  ax,offset exe_name		 ;try again with pcs-sysdir prefix
	  mov	  bid_name,ax
	  call	  bid_child
	  jc	  le_exit			 ;bid failed, jump
; child is ready, set state=EXE_NORM
le_10:	  load_index itself
	  mov	  bh,EXE_NORM
	  mov	  ax,bx
	  load_index status_table
	  mov	  status_table[bx],ax
	  clc
le_exit:  pop	  bx
	  pop	  ax
	  ret
load_exe  endp

;le_err:   cmp	   ax,0
;	  je	  le_exit		;it's not up, just exit
;	  cmp	  ax,8
;	  jne	  fail1
;	  mov	  ax,XLI_ERR_NO_MEMORY
;	  jmp	  xli_err_exit
;fail1:    cmp	   ax,2
;	  jne	  fail2
;	  mov	  ax,XLI_ERR_NO_SUCH_FILE
;	  jmp	  xli_err_exit
;fail2:    mov	   ax,XLI_ERR_BAD_EXEC
;	  jmp	  xli_err_exit
;le_err1:  cmp	   xli_up,0		 ;can't do usual error handling if
;					;system's not up yet
;	  je	  le_exit		;it's not up, just exit
;	  mov	  ax,XLI_ERR_NO_AVAILABLE_SLOTS
;	  jmp	  xli_err_exit

; Given a filename in "exe_name", initialize it under XLI.
; The EXEC status is returned.
; Assume AX..DI,ES are destroyed; DS,SS,SP,BP are preserved.
bid_child proc
	  push	  ds		   ;save parent's state
	  push	  bp
	  save_parent
	  mov	  cs:stk_seg,ss
	  mov	  cs:stk_offset,sp
	  dos_fr  FR_EXEC,<offset exec_pblock>,,bid_name,ds,ds

; The following are external entry points accessible by the child.
biddbg:   jmp	  tsr_done	   ; --- THE BIG 4 ---  (not for child's use)
	  jmp	  c2p_handler	   ; --- THE BIG 4 ---  for XCALL's
	  jmp	  c2p_terminate    ; --- THE BIG 4 ---  for child termination

tsr_done: cli
	  mov	  ss,cs:stk_seg
	  mov	  sp,cs:stk_offset
	  sti
	  pop	  bp
	  pop	  ds
	  ret

stk_seg    dw	  0		   ;bootstrap parent's state after EXEC
stk_offset dw	  0		   ;from here

bid_child endp

	  subttl  Code segment:  Child->parent handler
	  page

; On entry to this routine PCS is executing in the child's environment.
; The relevant stack entries at this point are:
;	  SS:SP (top)  ->  IP	   ;child's far return address
;			   CS
;			   length  ;child's length; for TSR
;			   PSP@    ;child's PSP@
;			   ////    ;(the rest of the stack)

c2p_handler label near
	  resume_parent
	  load_index itself
	  cmp	  bh,EXE_NORM		;normal call from child?
	  jne	  c2_10 		;no, jump; could be TSR
	  jmp	  normal		;yes, jump--rejoin xesc
c2_err:   mov	  ax,XLI_ERR_SYNC_ERR
	  jmp	  xli_err_exit
c2_10:	  cmp	  bh,EXE_TSR		;first call from child? (before TSR)
	  jne	  c2_err		;no, jump
	  load_index state_table
	  lea	  bx,state_table[bx]
	  mov	  es,[bx].st_ss
	  mov	  bp,[bx].st_sp 	;ES:BP is child's SS:SP
	  mov	  ax,es:[bp].cs_psp	;get child's PSP off its stack
	  load_index load_table
	  mov	  load_table[bx],ax	;save it
	  push	  ds			;-----> DS set to child's PSP
	  mov	  ds,ax
	  mov	  ax,ds:fb_ptr		;get file block @
	  mov	  cx,ds:fb_ptr+2
	  mov	  dx,ds:env_ptr 	;get env block @ (seg addr)
	  pop	  ds			;<-----
	  load_index fb_table
	  mov	  word ptr fb_table[bx],ax	 ;save it
	  mov	  word ptr fb_table+2[bx],cx
	  push	  es			;tempsave child's SS:SP on stack
	  push	  bp
	  mov	  bp,ax
	  mov	  es,cx 		;ES:BP is file block @
	  mov	  ax,es:[bp].fb_pb
	  mov	  cx,es:[bp].fb_pb+2	;get parm block @
	  load_index pb_table
	  mov	  word ptr pb_table[bx],ax	 ;save it
	  mov	  word ptr pb_table+2[bx],cx
	  test	  word ptr es:[bp].fb_flags,FB_KEEPENV
						 ;keep child's env block?
	  jnz	  c2_20 			 ;yes, jump
	  dos_fr  FR_RELMEM,,,,,dx		 ;no, release it for child
c2_20:	  pop	  bp
	  pop	  es
	  mov	  dx,es:[bp].cs_len	;get child's length off its stack
; we're ready to TSR the child
	  dos_fr  FR_TSR,,,dx
; we don't drop through -----------------------------------------


	  subttl  Code segment:  Child termination
	  page

; After the child has performed its wrapup, it calls this routine
; to deallocate its memory and make its spot in the load table available.
c2p_terminate label near
	  mov	  ax,dgroup		;we needn't save child's context now
	  mov	  ds,ax
	  restore_parent
	  load_index load_table 	;release the child
	  dos_fr  FR_RELMEM,,,,,load_table[bx]
	  jc	  ct_err
	  load_index itself		;mark its spot as available
	  xor	  bh,bh
	  mov	  ax,bx
	  load_index status_table
	  mov	  status_table[bx],ax
	  jmp	  normal1		;rejoin unload_exe
ct_err:   mov	  ax,XLI_ERR_RELMEM
	  jmp	  xli_err_exit

	  subttl  Code segment:  xesc
	  page

; This is the handler for the "%xesc" opcode.
;
; On entry:
;	  AX = length of xesc call (= inst length - 1)
;	  ES:SI = pointer to bytecode containing the (reg# x 4) of
;		  the %xesc name string
; On exit:
; normal: the VM reg that contained the name string on entry
;	    will contain the page:offset of the return value;
;	    there may be side effects in strings that were arguments to %xesc
;	  BX = 0 (no errors)
; error:  BX = error#

xesc	  proc	  near
xesc1:	  cld
	  sub	  ax,2				 ;adjust to #args *to %xesc*
						 ;(len, name are not args)
	  mov	  work_info.xs_nargs,ax 	 ;save actual #args
; Get from register# (actually, reg x 4) to lookup string.
	  lods	  byte ptr es:[si]
	  mov	  bl,al
	  xor	  bh,bh 			 ;BX is reg# x 4 of name string
	  mov	  work_info.xs_pc,si		 ;save bytecode@
	  mov	  work_info.xs_pc+2,es
	  mov	  work_info.xs_rvreg,bx 	 ;save reg# x 4;
						 ;return value goes here
	  lea	  bx,regs[bx]			 ;reg# x 4 -> VM reg @
	  mov	  si,[bx].C_page
	  cmp	  ptype[si],STRTYPE*2		 ;is it a string?
	  je	  xesc_5			 ;yes, jump
	  cmp	  ptype[si],SYMTYPE*2		 ;is it a symbol?
	  je	  xesc_3			 ;yes, jump
	  mov	  ax,XLI_ERR_NAME_BAD_TYPE	 ;error: name not string, symbol
	  jmp	  xesc_err_exit
xesc_3:   %LoadPage es,si			 ;page# in SI -> para# in ES
	  mov	  bp,[bx].C_disp		 ;ES:BP is symbol object @
	  mov	  ax,es:[bp].sym_len		 ;get symbol object length
	  sub	  ax,sym_ovhd			 ;subtract symbol's overhead
	  add	  bp,sym_ovhd			 ;skip past overhead
	  jmp	  short xesc_9
xesc_5:   %LoadPage es,si			 ;page# in SI -> para# in ES
	  mov	  bp,[bx].C_disp		 ;ES:BP is string object @
	  mov	  ax,es:[bp].str_len		 ;get string object length
	  cmp	  ax,0				 ;is it positive?
	  jge	  xesc_8			 ;yes, jump; normal string
	  add	  ax,str_ovhd*2 		 ;no, assume short string
						 ;rather than really long string
						 ;and make positive
xesc_8:   sub	  ax,str_ovhd			 ;subtract string's overhead
	  add	  bp,str_ovhd			 ;skip past overhead
xesc_9:   mov	  work_area.srch_slen,ax	 ;save length of string data
	  mov	  work_area.srch_sptr,bp	 ;save address of string data
	  mov	  work_area.srch_sptr+2,es
; Look for a match.
	  call	  table_search			 ;is there a match?
						 ;(sets active_exe if so)
	  jnc	  xesc_10			 ;yes, jump
	  mov	  bx,work_info.xs_rvreg		 ;get the name
	  lea	  bx,regs[bx]
	  mov	  ax,XLI_ERR_NO_SUCH_NAME	 ;error: no such name loaded
	  jmp	  xesc_ext_err_exit		 ;use alternate error point
	  					 ;so name gets printed with
						 ;error message
xesc_10:  mov	  dx,ax 			 ;tempsave selector
; There was a match.
; Collect the info we'll need to guide us thru xesc call.
	  load_index fb_table
	  mov	  bp,word ptr fb_table[bx]
	  mov	  es,word ptr fb_table+2[bx]	 ;ES:BP is file block @
	  mov	  ax,es:[bp].fb_id		 ;XLI ID
	  cmp	  ax,XLI_ID
	  je	  xesc_15
	  mov	  ax,XLI_ERR_BAD_VERSION
	  jmp	  xesc_err_exit
xesc_15:  mov	  ax,es:[bp].fb_flags		 ;flags
	  mov	  work_info.xs_flags,ax
	  load_index pb_table
	  mov	  bp,word ptr pb_table[bx]
	  mov	  es,word ptr pb_table+2[bx]	 ;ES:BP is parm block @
	  mov	  work_info.xs_pb_segment,es	 ;parm block's segment address
	  lea	  ax,es:[bp].pb_rv
	  mov	  work_info.xs_rvptr,ax
	  mov	  work_info.xs_rvptr+2,es	 ;return value's address
	  mov	  es:[bp].pb_rv,0		 ;zero out return value
	  mov	  es:[bp].pb_rv+2,0
	  mov	  es:[bp].pb_rv+4,0
	  mov	  es:[bp].pb_rv+6,0
	  mov	  es:[bp].pb_rvtype,SWI_TF	 ;set ret value's type to T/F
	  mov	  es:[bp].pb_ss,0		 ;zero out special service
	  add	  ax,8
	  mov	  work_info.xs_args,ax		 ;first arg's address
	  mov	  work_info.xs_args+2,es
	  mov	  work_info.xs_local,offset work_area ;work area address
	  mov	  work_info.xs_local+2,seg work_area
; Begin initializing child's parameter block.
	  mov	  es:[bp].pb_select,dx		 ;store selector into parm block
	  mov	  work_info.xs_select,dx
; Move the xesc arguments to their places for the xesc call.
;	  mov	  cx,work_info.xs_nargs 	 ;CX is #args
;xesc_20:  cmp	   cx,0 			  ;any left?
;	  jcxz	  xesc_50			 ;no, jump
	  mov	  cx,0				 ;CX is current arg#
xesc_20:  cmp	  cx,work_info.xs_nargs 	 ;any left?
	  je	  xesc_50			 ;no, jump
	  push	  cx				 ;tempsave current arg#
	  mov	  si,work_info.xs_pc
	  mov	  es,work_info.xs_pc+2		 ;ES:SI is bytecode@
	  lods	  byte ptr es:[si]
	  mov	  work_info.xs_pc,si		 ;save next bytecode @
	  mov	  bl,al
	  xor	  bh,bh 			 ;BX is reg# x 4
; Put the reg# and current arg @ into swap table
	  mov	  ax,bx
	  xchg	  bx,cx 			 ;BX is current arg#
	  shl	  bx,1
	  shl	  bx,1				 ;make index into swap table
	  mov	  word ptr swap_table[bx].sw_reg,ax	  ;save VM reg# x 4
	  mov	  ax,work_info.xs_args
	  mov	  word ptr swap_table[bx].sw_offset,ax	  ;save arg@
	  mov	  bx,cx 			 ;restore reg# x 4
; Dispatch on argument type
	  lea	  bx,regs[bx]			 ;BX is VM reg @
	  mov	  di,[bx].C_page		 ;get its page#
	  mov	  di,ptype[di]			 ;and its type
;	  push	  cx
	  call	  cs:word ptr do_arg[di]	 ;handle one type of object
	  add	  work_info.xs_local,PAD_SIZE	 ;incr XLI-local ptr
						 ;(maintain alignment)
	  pop	  cx				 ;restore current arg#
;	  dec	  cx
	  inc	  cx
	  jmp	  xesc_20

; Everything's ready.  Call the child.
xesc_50:  call	  update_swap_table
	  call_child 1

; We're back with a return value--unless it's a special service call.
normal:   cld
	  load_index pb_table
	  mov	  bp,word ptr pb_table[bx]
	  mov	  es,word ptr pb_table+2[bx]	 ;ES:BP is parm block @
	  cmp	  es:[bp].pb_ss,0		 ;any special services?
	  je	  xesc_60			 ;no, jump
	  call	  ssr				 ;do special service and
	  jmp	  xesc_50			 ;return immediately back
						 ;across the interface
; Now we're really back with the return value
xesc_60:  mov	  di,es:[bp].pb_rvtype
	  mov	  work_area.xs_rvtype,di	 ;return value's type
	  cmp	  di,RV_ERR			 ;external-pgm error return?
	  jne     xesc_65			 ;no, jump
	  mov	  bp,work_info.xs_rvptr
	  mov	  es,work_info.xs_rvptr+2	 ;ES:BP points to return value
	  					 ;(external-pgm error message)
	  call	  cs:word ptr do_val[SWI_STR*2]  ;build string
	  mov	  bx,work_info.xs_rvreg
	  lea	  bx,regs[bx]			 ;BX=addr of reg with error string
	  mov	  ax,XLI_ERR_EXTERNAL_ERROR      ;AX=XLI error code
	  jmp	  short xesc_ext_err_exit
						 
xesc_65:  cmp	  di,N_RV			 ;return value out of range?
	  jb	  xesc_70			 ;no, jump
	  mov	  ax,XLI_ERR_VALUE_BAD_TYPE
	  jmp	  xesc_err_exit
xesc_70:  shl	  di,1
	  mov	  bp,work_info.xs_rvptr
	  mov	  es,work_info.xs_rvptr+2	 ;ES:BP point to return value
	  call	  cs:word ptr do_val[di]	 ;handle one type of return value
	  lea	  bx,nil_reg			 ;CX says "nil irritant"
	  mov	  ax,0				 ;BX=0 says no errors
	  ret

; This file's error exit processing.  Reset the stack so that we return
; immediately to the gate.  BX should be set with an error code before
; jumping here.  
xli_err_exit:
xesc_err_exit:					 ;AX contains error#
	  lea	  bx,nil_reg			 ;BX is "nil irritant"
; Another exit label.  This allows both AX (XLI error code) and BX
; (the address of the VM register with the "irritant") to be set beforehand.
xli_ext_err_exit:
xesc_ext_err_exit:
	  mov	  sp,gate_sp			 ;return to gate
	  ret


;; Jump tables
; indexed by argument type (standard PCS type tag)
do_arg	  dw	  do_lstarg			 ;0=list (#f only)
	  dw	  do_fixarg			 ;1=fixnum
	  dw	  do_floarg			 ;2=flonum
	  dw	  do_bigarg			 ;3=bignum
	  dw	  do_symarg			 ;4=symbol (#t only)
	  dw	  do_strarg			 ;5=string
	  dw	  do_errarg			 ;6 the rest we don't care about
	  dw	  do_errarg			 ;7
	  dw	  do_errarg			 ;8
	  dw	  do_errarg			 ;9
	  dw	  do_errarg			 ;10
	  dw	  do_errarg			 ;11
	  dw	  do_errarg			 ;12
	  dw	  do_errarg			 ;13
	  dw	  do_errarg			 ;14
	  dw	  do_errarg			 ;15

; indexed by value type (SW-INT return types)
do_val	  dw	  do_intval			 ;0=integer
	  dw	  do_TFval			 ;1=true/false
	  dw	  do_strval			 ;2=string
	  dw	  do_floval			 ;3=flonum

xesc	  endp

	  subttl  Code segment:  Copy arguments into place for child
	  page

; On entry to all the argument handler routines:
;	  BX = pointer to VM reg with page:offset of Scheme object

do_floarg proc	  near
	  mov	  si,[bx].C_page		 ;get object's page#
	  %LoadPage es,si			 ;swap it in
	  mov	  si,[bx].C_disp		 ;ES:SI is Scheme object @
	  test	  work_info.xs_flags,FB_NEAR	 ;near flag on?
	  jz	  do_flo10			 ;no, jump
; Set up destination address
	  ; near
	  mov	  cx,work_info.xs_args		 ;dest is in child
	  mov	  dx,work_info.xs_args+2
	  mov	  work_info.xs_dest,cx
	  mov	  work_info.xs_dest+2,dx
	  jmp	  short do_flo20
	  ; far
do_flo10: mov	  cx,work_info.xs_local 	 ;dest is in XLI-local area
	  mov	  dx,work_info.xs_local+2
	  mov	  work_info.xs_dest,cx
	  mov	  work_info.xs_dest+2,dx
; Copy the flonum data
do_flo20: inc	  si				 ;incr past tag
	  mov	  di,work_info.xs_dest
	  push	  ds				 ;tempsave DS around copy
	  push	  es
	  mov	  es,work_info.xs_dest+2	 ;ES:DI points to dest
	  pop	  ds				 ;DS:SI is Scheme object @
	  mov	  cx,8
      rep movsb
	  pop	  ds				 ;restore our DS
	  test	  work_info.xs_flags,FB_NEAR	 ;near flag on?
	  jz	  do_flo30			 ;no, jump
; Copy pointer to data
	  ; near (no copy needed--data is in child's space)
	  mov	  cx,8				 ;incr arg@ past copied data
	  jmp	  short do_flo32
	  ; far  (pointer in child points to data in XLI space)
do_flo30: sub	  di,8				 ;back up dest @
	  mov	  cx,di
	  mov	  dx,es
	  mov	  bp,work_info.xs_args
	  mov	  es,work_info.xs_args+2	 ;ES:BP points to arg position
	  mov	  es:[bp],cx
	  mov	  es:[bp]+2,dx			 ;copy pointer there
; Increment arg pointer by an appropriate amount.
	  mov	  cx,4				 ;incr arg@ past copied ptr
do_flo32: test	  work_info.xs_flags,FB_PAD	 ;pad flag on?
	  jz	  do_flo35			 ;no, skip
	  mov	  cx,PAD_SIZE
do_flo35: add	  work_info.xs_args,cx
do_flo40: ret
do_floarg endp

do_bigarg proc	  near
	  mov	  si,[bx].C_page		 ;get object's page#
	  %LoadPage es,si			 ;swap it in
	  mov	  si,[bx].C_disp		 ;ES:SI is Scheme object @
; Stage the conversion to longint in XLI space.
	  mov	  bp,sp
	  push	  bx				 ;push VM reg@
	  push	  work_info.xs_local		 ;push local buffer@
	  mov	  work_info.C_fn,offset pgroup:int2long
	  call	  far ptr far_C
	  mov	  sp,bp
	  cmp	  ax,0				 ;did bignum convert OK?
	  je	  do_big5			 ;yes, jump
	  mov	  ax,XLI_ERR_BIG_TO_32_BITS	 ;error: bignum too big
						 ;to become longint
	  jmp	  xesc_err_exit
do_big5:  test	  work_info.xs_flags,FB_NEAR	 ;near flag on?
	  jz	  do_big20			 ;no, jump
	  ; near (copy longint to child's space)
	  mov	  bp,work_info.xs_local
	  mov	  es,work_info.xs_local+2	 ;ES:BP points to XLI-local longint
	  mov	  ax,es:[bp]			 ;move longint to regs
	  mov	  dx,es:[bp]+2
	  mov	  bp,work_info.xs_args
	  mov	  es,work_info.xs_args+2	 ;ES:BP points to dest
	  mov	  es:[bp],ax			 ;copy LSBy to child
	  mov	  cx,2
	  test	  work_info.xs_flags,FB_INT	 ;is 16-bit integer flag on?
	  jz	  do_big15			 ;no, jump
	  ; is the longint small enough for an int?
	  cmp	  dx,0				 ;DX should be either
						 ;all 0's or all 1's
	  je	  do_big32			 ;we can safely truncate
	  xor	  dx,0FFFFh			 ;complement DX
	  cmp	  dx,0				 ;try again
	  je	  do_big32			 ;we can safely truncate
	  mov	  ax,XLI_ERR_BIG_TO_16_BITS	 ;error: bignum too big
						 ;to become int
	  jmp	  xesc_err_exit
do_big15: mov	  es:[bp]+2,dx			 ;copy MSBy to child
	  mov	  cx,4
	  jmp	  short do_big32
	  ; far (pointer in child points to data in XLI-local space)
do_big20: mov	  ax,work_info.xs_local 	 ;move ptr to longint to regs
	  mov	  dx,work_info.xs_local+2
; Copy either the longint or a pointer to it.
	  mov	  bp,work_info.xs_args
	  mov	  es,work_info.xs_args+2	 ;ES:BP points to dest
	  mov	  es:[bp],ax			 ;copy to child
	  mov	  es:[bp]+2,dx
	  mov	  cx,4				 ;incr arg@ past longint
						 ;or pointer to longint
; Increment arg pointer by an appropriate amount.
do_big32: test	  work_info.xs_flags,FB_PAD	 ;pad flag on?
	  jz	  do_big35			 ;no, skip
	  mov	  cx,PAD_SIZE
do_big35: add	  work_info.xs_args,cx
do_big40: ret
do_bigarg endp

do_fixarg proc	  near
; Stage the conversion to int in XLI space
	  mov	  ax,[bx].C_disp		 ;get the fixnum data
	  shl	  ax,1				 ;deal with sign bit
	  sar	  ax,1				 ;AX is 16-bit signed int
; True and false are treated as the numbers 1 and 0, respectively.
; Boolean-argument processing merges into integer processing at this point.
do_log:   cwd					 ;DX:AX is 32-bit signed int
; Copy the int data to the proper place
	  mov	  bp,work_info.xs_args
	  mov	  es,work_info.xs_args+2	 ;ES:BP points to dest
	  test	  work_info.xs_flags,FB_NEAR	 ;near flag on?
	  jz	  do_fix20			 ;no, jump
	  ; near (copy int to child's space)
	  mov	  es:[bp],ax			 ;copy int to child
	  mov	  cx,2				 ;incr arg@ past int
	  test	  work_info.xs_flags,FB_INT	 ;is 16-bit integer flag on?
	  jnz	  do_fix30			 ;yes, jump
	  mov	  es:[bp]+2,dx			 ;no, copy high order 16 bits
	  mov	  cx,4				 ;incr arg@ past longint
	  jmp	  short do_fix30
	  ; far (pointer in child points to data in XLI-local space)
do_fix20: mov	  bx,work_info.xs_local
	  mov	  [bx],ax
	  mov	  [bx]+2,dx
	  mov	  ax,work_info.xs_local 	 ;move far ptr to int
						 ;or longint to child
	  mov	  cx,work_info.xs_local+2
	  mov	  es:[bp],ax
	  mov	  es:[bp]+2,cx
	  mov	  cx,4				 ;incr arg@ past ptr to int
; Increment arg pointer by an appropriate amount
do_fix30: test	  work_info.xs_flags,FB_PAD	 ;pad flag on?
	  jz	  do_fix35			 ;no, skip
	  mov	  cx,PAD_SIZE
do_fix35: add	  work_info.xs_args,cx
do_fix40: ret
do_fixarg endp

do_lstarg proc	  near				 ;looking for false only
	  cmp	  [bx].C_page,NIL_PAGE*2
	  jne	  do_xxerr
	  mov	  ax,0
	  jmp	  do_log
do_lstarg endp

do_xxerr: jmp	  do_errarg			 ;conditional jumps
						 ;are too short

do_symarg proc	  near				 ;looking for true only
	  cmp	  [bx].C_page,T_PAGE*2
	  jne	  do_xxerr
	  cmp	  [bx].C_disp,T_DISP
	  jne	  do_xxerr
	  mov	  ax,1
	  jmp	  do_log
do_symarg endp

do_strarg proc	  near
	  mov	  si,[bx].C_page
	  %LoadPage es,si			 ;load string into memory
	  mov	  si,[bx].C_disp		 ;ES:SI is Scheme object @
	  test	  work_info.xs_flags,FB_NEAR	 ;near flag on?
	  jz	  do_str10			 ;no, jump
	  ; near  (can't copy string because we don't have its address--
	  ;	  put nil address into parm block)
	  mov	  bp,work_info.xs_args
	  mov	  es,work_info.xs_args+2	 ;ES:BP is arg @
	  mov	  word ptr es:[bp],0
	  mov	  cx,2
	  jmp	  short do_str60
	  ; far   (we can copy the string's address, but need to check
	  ;	  on earlier strings' swap status)
do_str10:
	  mov	  ax,si
	  add	  ax,STR_OVHD
	  mov	  dx,es 			 ;DX:AX is string data @
	  mov	  bp,work_info.xs_args
	  mov	  es,work_info.xs_args+2	 ;ES:BP is arg @
	  mov	  es:[bp],ax
	  mov	  es:[bp]+2,dx
	  mov	  cx,4
; Increment arg pointer by an appropriate amount
do_str60: test	  work_info.xs_flags,FB_PAD	 ;padding on?
	  jz	  do_str65			 ;no, jump
	  mov	  cx,PAD_SIZE
do_str65: add	  work_info.xs_args,cx
	  ret
do_strarg endp

do_errarg proc	  near
	  mov	  ax,XLI_ERR_ARGN_BAD_TYPE
	  jmp	  xesc_err_exit
do_errarg endp

	  subttl  Code segment:  Copy return value back into Scheme
	  page

; On entry to all the value handler routines:
;	  ES:BP = pointer to return value

do_floval proc	  near
	  test	  work_info.xs_flags,FB_NEAR	 ;is near flag on?
	  jnz	  do_flv10			 ;yes, jump
	  ; far
	  mov	  ax,es:[bp]			 ;get ptr to number
	  mov	  dx,es:[bp]+2
	  mov	  bp,ax
	  mov	  es,dx 			 ;ES:BP points to number
	  ; near
do_flv10: mov	  dx,es:[bp]+6			 ;get double in registers
	  mov	  cx,es:[bp]+4
	  mov	  bx,es:[bp]+2
	  mov	  ax,es:[bp]
	  mov	  bp,sp 			 ;get BP set for C call
	  push	  dx				 ;push double
	  push	  cx
	  push	  bx
	  push	  ax
	  mov	  bx,work_info.xs_rvreg 	 ;push return value VM reg@
	  lea	  bx,regs[bx]
;	  lea	  bx,reg1			 ;temporary
	  push	  bx
	  mov	  work_info.C_fn,offset pgroup:alloc_fl
	  call	  far ptr far_C 		 ;C double -> PCS flonum
	  mov	  sp,bp 			 ;pop C args
	  ret
do_floval endp

do_TFval  proc	  near
	  mov	  cx,0
	  mov	  ax,es:[bp]			 ;get value
	  or	  ax,es:[bp]+2			 ;all bytes must = 0 to be nil
	  or	  ax,es:[bp]+4
	  or	  ax,es:[bp]+6
	  jz	  do_TF10			 ;yes (false object)
	  mov	  ax,T_DISP			 ;no  (true object)
	  mov	  cx,T_PAGE*2
do_TF10:
	  mov	  bx,work_info.xs_rvreg 	 ;push return value VM reg@
	  lea	  bx,regs[bx]
;	  lea	  bx,reg1			 ;temporary
	  mov	  [bx].C_disp,ax
	  mov	  [bx].C_page,cx
	  ret
do_TFval  endp

do_intval proc	  near
	  test	  work_info.xs_flags,FB_NEAR	 ;near flag on?
	  jnz	  do_int10			 ;yes, jump
	  ; far
	  mov	  ax,es:[bp]			 ;get ptr to number
	  mov	  dx,es:[bp]+2
	  mov	  bp,ax
	  mov	  es,dx 			 ;ES:BP points to number
	  ; near
do_int10: mov	  ax,es:[bp]			 ;get number
	  mov	  dx,es:[bp]+2
	  test	  work_info.xs_flags,FB_INT	 ;16-bit integer flag on?
	  jz	  do_int20			 ;no, jump
	  cwd					 ;yes, propagate sign
do_int20: mov	  bp,sp 			 ;get BP set for C call
	  push	  dx				 ;push longint
	  push	  ax
	  mov	  bx,work_info.xs_rvreg 	 ;push return value VM reg@
	  lea	  bx,regs[bx]
;	  lea	  bx,reg1			 ;temporary
	  push	  bx
	  mov	  work_info.C_fn,offset pgroup:long2int
	  call	  far ptr far_C 		 ;C longint -> PCS integer
						 ;(bignum or fixnum)
	  mov	  sp,bp 			 ;pop C args
	  ret
do_intval endp

do_strval proc	  near
; allocate the space for the return value string object
	  mov	  di,2				 ;DI=offset of length in
						 ;return value field (near)
	  test	  work_info.xs_flags,FB_NEAR	 ;is near flag on?
	  jnz	  do_stv10			 ;yes, jump
	  mov	  di,4				 ;different offset for far
do_stv10: mov	  cx,es:[bp][di]		 ;get string's length
	  mov	  bx,16380			 ;BX is max string length
	  cmp	  cx,bx 			 ;is CX short enough?
	  jbe	  do_stv15			 ;yes, jump
	  mov	  cx,bx 			 ;no, truncate at max
do_stv15: push	  cx				 ;tempsave length
	  mov	  bp,sp 			 ;get BP set for C call
	  push	  cx				 ;push length
	  mov	  ax,STRTYPE
	  push	  ax				 ;push type
	  mov	  bx,work_info.xs_rvreg
	  lea	  bx,regs[bx]
	  push	  bx				 ;push return value VM reg @
	  mov	  work_info.C_fn,offset pgroup:alloc_bl
	  call	  far ptr far_C 		 ;allocate string object;
						 ;"alloc_block" takes care
						 ;of overhead matters
	  mov	  sp,bp 			 ;pop C args
	  pop	  cx				 ;restore length
	  mov	  bx,work_info.xs_rvreg
	  lea	  bx,regs[bx]			 ;BX is return value VM reg @
	  mov	  di,[bx].C_disp
	  mov	  es,[bx].C_page
	  %LoadPage es,es			 ;ES:DI is dest object @
	  add	  di,3				 ;skip past string's overhead
; copy string data into string object
	  push	  es				 ;tempsave ES
	  mov	  si,work_info.xs_rvptr
	  mov	  es,work_info.xs_rvptr+2	 ;ES:SI points to return value
						 ;field in parameter block
	  mov	  ax,es:[si]
	  mov	  dx,work_info.xs_pb_segment	 ;DX:AX is src string @ (near)
	  test	  work_info.xs_flags,FB_NEAR	 ;is near flag on?
	  jnz	  do_stv50			 ;yes, jump
	  mov	  dx,es:[si]+2			 ;DX:AX is src string @ (far)
do_stv50: pop	  es				 ;restore ES
	  push	  ds				 ;tempsave our DS
	  mov	  si,ax 			 ;ES:DI is dest string @
	  mov	  ds,dx 			 ;DS:SI is src string @
      rep movsb 				 ;copy string
	  pop	  ds				 ;restore our DS
	  ret
do_strval endp

do_errval proc	  near
	  mov	  ax,XLI_ERR_VALUE_BAD_TYPE
	  jmp	  xesc_err_exit
do_errval endp

	  subttl  Code segment:  Special Services
	  page

; On entry, ES:BP is parm block pointer.

ssr	  proc	  near
	  mov	  bx,es:[bp].pb_ss		 ;get dispatch number
	  cmp	  bx,SS_SWAP
	  je	  ss1
	  jne	  ss_exit			 ;note we don't fall thru

ss_normal_exit:
	  load_index pb_table
	  mov	  bp,word ptr pb_table[bx]
	  mov	  es,word ptr pb_table[bx]+2
	  mov	  es:[bp].pb_ss,0		 ;clear ss field for normal exit
ss_exit:  ret

; the conditional jumps can't jump far enough, hence this table
ss1:	  jmp	  ssr_swap

; "Swap" special service

ssr_swap: mov	  bx,es:[bp].pb_ss_args 	 ;get arg#
	  mov	  cx,es:[bp].pb_ss_args+2	 ;get dest. length
	  shl	  bx,1
	  shl	  bx,1
	  push	  bx				 ;tempsave index into swap table
	  test	  work_info.xs_flags,FB_NEAR	 ;is near flag on?
	  jnz	  ss_10 			 ;yes, jump
	  ;
	  ; Far -------------------
	  ;
	  ; ss_args+0:	swap table index corr. to arg# (already in BX)
	  ;
	  ; ss returns:
	  ;   in ss_args+0:  true length of string
	  ;   in pb args:    far @ to string
	  ;
	  mov	  bx,word ptr swap_table[bx].sw_reg  ;BX is reg# x 4
	  lea	  bx,regs[bx]			 ;BX is reg@
	  mov	  si,[bx].C_page		 ;get object's page#
	  %LoadPage es,si			 ;load object into memory
	  mov	  si,[bx].C_disp		 ;ES:SI is string object @
	  inc	  si				 ;skip over tag
	  mov	  ax,es:[si]			 ;get string's length
	  inc	  si				 ;skip over length
	  inc	  si
	  cmp	  ax,0				 ;a short string?
	  jge	  ss_5				 ;no, jump
	  add	  ax,str_ovhd*2 		 ;yes
ss_5:	  sub	  ax,str_ovhd			 ;subtract off overhead
	  mov	  di,es 			 ;DI:SI is string data @
	  mov	  es,work_info.xs_pb_segment	 ;ES:BP is parameter block @
	  mov	  es:[bp].pb_ss_args,ax 	 ;put string length in ss_args
	  pop	  bx				 ;restore swap table index
	  mov	  bp,word ptr swap_table[bx].sw_offset ;ES:BP points to this arg's
						 ;location in parameter block
	  mov	  es:[bp],si			 ;put far @ in parm block
	  mov	  es:[bp]+2,di
	  jmp	  ss_normal_exit
	  ;
	  ; Near -------------------
	  ;
	  ; ss_args+0:	swap table index corr. to arg# (already in BX)
	  ;	   +2:	length (already in CX)
	  ;	   +4:	near ptr
	  ;
	  ; ss returns:
	  ;   in ss_args+0:  length used in copying
	  ;   in pb args:    near @ to string
	  ;
ss_10:	  mov	  ax,es:[bp].pb_ss_args+4	 ;get dest @
	  mov	  work_info.xs_dest,ax
	  mov	  ax,work_info.xs_pb_segment
	  mov	  work_info.xs_dest+2,ax
	  mov	  bx,word ptr swap_table[bx].sw_reg  ;get reg# x 4 corr. to arg
	  lea	  bx,regs[bx]			 ;BX is reg@
	  mov	  si,[bx].C_page
	  %LoadPage es,si			 ;load object into memory
	  mov	  si,[bx].C_disp		 ;ES:SI is string object @
	  inc	  si				 ;skip over tag
	  mov	  ax,es:[si]			 ;get string's length
	  inc	  si				 ;skip over length
	  inc	  si
	  cmp	  ax,0				 ;a short string?
	  jge	  ss_15 			 ;no, jump
	  add	  ax,str_ovhd*2 		 ;yes
ss_15:	  sub	  ax,str_ovhd			 ;subtract off overhead
	  cmp	  ax,cx 			 ;string len >= buffer len?
	  jae	  ss_20 			 ;yes, jump
	  mov	  cx,ax 			 ;CX is #chars to copy
ss_20:	  push	  ds				 ;tempsave our DS
	  push	  es
	  mov	  di,work_info.xs_dest
	  mov	  es,work_info.xs_dest+2	 ;ES:DI is dest @
	  pop	  ds				 ;DS:SI is src @
      rep movsb 				 ;copy string
	  pop	  ds				 ;restore our DS
	  load_index pb_table
	  mov	  bp,word ptr pb_table[bx]
	  mov	  es,word ptr pb_table[bx]+2	 ;ES:BP is parm block @
	  mov	  es:[bp].pb_ss_args,ax 	 ;put #chars copied in ss_args
	  pop	  bx				 ;restore swap table index
	  mov	  bp,word ptr swap_table[bx].sw_offset ;ES:BP points to this arg's
						 ;location in parameter block
	  mov	  ax,work_info.xs_dest
	  mov	  es:[bp],ax			 ;put near @ in parm block
	  jmp	  ss_normal_exit

ssr	  endp

	  subttl  Code segment:  update_swap_table
	  page

update_swap_table proc near
; for small models, PCS may indeed be swapping, but we don't care, as data
; is copied to the external program on its first reference and
; remains always available to the program since the pointer in the parm block
; points into the program's own space, not PCS's
	  test	  work_info.xs_flags,FB_NEAR	 ;is near flag on?
	  jnz	  ust_exit			 ;yes, exit
	  mov	  cx,0				 ;CX is argument counter
ust_10:   cmp	  cx,work_info.xs_nargs
	  jge	  ust_exit
	  mov	  bx,cx
	  shl	  bx,1
	  shl	  bx,1				 ;BX is swap table index
	  push	  bx				 ;tempsave index
	  mov	  bx,word ptr swap_table[bx].sw_reg  ;get reg# x 4 of argument
	  lea	  bx,regs[bx]			 ;BX is reg@
	  mov	  di,[bx].C_page		 ;get object's page#
	  mov	  di,ptype[di]			 ;then its type
	  cmp	  di,STRTYPE*2			 ;is it a string?
	  je	  ust_40			 ;yes, jump
	  pop	  bx				 ;no, discard index
ust_30:   inc	  cx				 ;incr to next argument
	  jmp	  ust_10
ust_40:   mov	  ax,[bx].C_page		 ;get page#
	  mov	  bp,sp 			 ;set up BP for calling C
	  push	  ax				 ;push page#
	  mov	  work_info.C_fn,offset pgroup:getbase	;this routine's not in
						 ;C but does use its calling
						 ;conventions
	  call	  far ptr far_C
	  mov	  sp,bp 			 ;pop C args
	  pop	  bx				 ;restore swap table index
	  ; If carry is clear, the argument is in memory already.
	  ; The address in the parm block should be OK since an object
	  ; coming into memory has its address updated at the time of the
	  ; swap.  Swapping does not cause a GC, so GC's shouldn't relocate
	  ; an address.  That leaves zeroing the addresses of objects
	  ; that were swapped out.
	  jnc	  ust_30			 ;object's in memory, jump
	  ; Carry set means object is swapped out.  Zero the argument's
	  ; pointer in the parm block.
	  mov	  bp,word ptr swap_table[bx].sw_offset
	  mov	  es,work_info.xs_pb_segment	 ;ES:BP points to this arg's
						 ;location in the parm block
	  mov	  word ptr es:[bp],0		 ;zero offset part of pointer
	  mov	  word ptr es:[bp]+2,0		 ;zero segment part of pointer
	  jmp	  ust_30
;
ust_exit: ret

update_swap_table endp


	  subttl  Code segment:  unload_exe
	  page

; Given active_exe, release it from memory and make its spot available again.
unload_exe proc   near
	  load_index state_table
	  mov	  es,word ptr state_table[bx].st_ss
	  mov	  bp,word ptr state_table[bx].st_sp  ;ES:BP is child's SS:SP
	  mov	  es:[bp].cs_psp,0		 ;set PSP@ to 0, our signal
						 ;to child to wrap things up
	  call_child 2				 ;call child one last time
normal1:  ret
unload_exe endp


	  subttl  Code segment:  unload_all
	  page

; This routine is called during PCS termination.  It notifies each
; child to do any wrapup, then the child will do its final call to us,
; where we release it.

unload_all proc   near
	  mov	  active_exe,0
ua_10:	  cmp	  active_exe,N_EXE	;looked at all entries?
	  je	  ua_exit		;yes, jump
	  load_index status_table
	  mov	  bx,status_table[bx]
	  cmp	  bh,EXE_NONE		;is slot empty?
	  jne	  ua_20 		;no, jump
ua_15:	  inc	  active_exe		;incr to next entry
	  jmp	  ua_10
ua_20:	  call	  unload_exe		;deallocate entry
	  jmp	  ua_15
ua_exit:  ret
unload_all endp

	  subttl  Code segment:  table_search
	  page

; We need to find a matching string.  From it we'll know
; which child has it and what value it maps to.
; On entrance:
;   work_area.srch_sptr is the seg:offset of the Scheme string (data proper)
;   work_area.slen is the string's length
; On exit:
;   if success:    AX = selector value
;		   active_exe = xxnnh, where n is the child
;		   carry clear
;   if fail:	   carry set
; AX..DI,ES,BP are destroyed.

table_search proc  near
	  cld					 ;to be safe
	  mov	  work_area.srch_exe,0
ts_10:	  cmp	  work_area.srch_exe,N_EXE	 ;looked at them all?
	  jne	  ts_15 			 ;no, jump
; No child had a match.  Return with carry set.
	  stc
	  jmp	  ts_exit
ts_15:	  mov	  bx,work_area.srch_exe
	  mov	  active_exe,bx
	  load_index status_table
	  mov	  ax,status_table[bx]
	  cmp	  ah,0				 ;is this an open spot?
	  jne	  ts_20 			 ;no, jump
ts_next:  inc	  work_area.srch_exe		 ;increment to next spot
	  jmp	  short ts_10
; We have a loaded file.  Figure out where its lookup table is.
ts_20:	  load_index fb_table
	  mov	  bp,word ptr fb_table[bx]
	  mov	  es,word ptr fb_table+2[bx]	 ;ES:BP is file block @
	  mov	  ax,es:[bp].fb_lut
	  mov	  dx,es:[bp].fb_lut+2
	  mov	  di,ax
	  mov	  es,dx 			 ;ES:DI is lookup table @
	  mov	  ah,0				 ;AH will be selector value
	  mov	  al,'/'                         ;AL is name delimiter
; Find the next name in the lookup table.
ts_30:	  cmp	  byte ptr es:[di],al		 ;looking at last delimiter?
	  je	  ts_next			 ;yes, jump
	  mov	  si,di 			 ;SI points at current name
	  mov	  cx,0FFh
    repne scasb 				 ;look for name delimiter
	  jcxz	  ts_next			 ;jump, should've found it by now
	  mov	  dx,di 			 ;DI, DX point at next name
	  mov	  cx,di
	  sub	  cx,si
	  dec	  cx				 ;CX is length of name in table
	  cmp	  work_area.srch_slen,cx	 ;are lengths equal?
	  jne	  ts_40 			 ;no, jump
; We matched lengths.  See if the strings themselves match.
	  mov	  di,si 			 ;get current name @ back in DI
	  push	  ds				 ;tempsave our DS
	  mov	  si,work_area.srch_sptr
	  mov	  ds,work_area.srch_sptr+2	 ;DS:SI is Scheme string @
     repe cmpsb
	  pop	  ds				 ;restore our DS
	  je	  ts_match			 ;jump if match
; The current table name didn't match.
ts_40:	  inc	  ah				 ;increment selector value
	  mov	  di,dx 			 ;restore next name @ to DI
	  jmp	  ts_30
; We matched.  Active_exe has child#, replace it with the corr. status value.
; Calculate the selector value (0-based) of the name and return it in AX.
; Clear carry.
ts_match: mov	  al,ah
	  xor	  ah,ah
	  load_index status_table
	  mov	  bx,status_table[bx]
	  mov	  active_exe,bx
	  clc
ts_exit:  ret
table_search endp

	  subttl  Code segment:  find_open_spot
	  page

; Find an open spot in the load_table.	Clear carry and set the LSBy of
; active_exe with the child# if we succeeded, else set carry.
find_open_spot proc near
	  push	  bx
	  push	  cx
	  mov	  cx,N_EXE
	  mov	  bx,0
fi_loop:  cmp	  byte ptr status_table[bx]+1,EXE_NONE
	  je	  fi_found
	  inc	  bx
	  inc	  bx
	  dec	  cx
	  cmp	  cx,0
	  jne	  fi_loop
	  stc			   ;set carry if no available entries
	  jmp	  short fi_exit
fi_found: mov	  bx,N_EXE
	  sub	  bx,cx
	  mov	  active_exe,bx
	  clc			   ;an open entry:  clear carry, set active_exe
fi_exit:  pop	  cx
	  pop	  bx
	  ret
find_open_spot endp

	  subttl  Far/near linkage to XLI
	  page

; Near linkage

; All other routines in this file are accessed through this one.
; On entry, BX contains an index into a jump table of routines to execute.
%xli_gate proc	  far
	  shl	  bx,1			;get index on word boundary
	  mov	  gate_sp,sp
	  sub	  gate_sp,2
; We adjusted the SP for the return address which the call in the next
; instruction will put on the stack.
; Error recovery by nested routines is done by restoring this SP value and
; then returning, which will bring them back to just after the call.
	  call	  cs:gate[bx]
	  ret

; jump table
gate	  dw	  load_all,xesc,unload_all

%xli_gate endp

	  subttl  Debugging XLI from PCS
	  page

; If PCS is run under DEBUG, executing (%xli-debug <0 or 1>) will
; execute the following code, which either installs an INT 3 (=1) or NOPs (=0).
; When INT 3 is executed, DEBUG is called.  This provides a way for
; writers of external routines to get a hook at execution time into
; their code for debugging.  Also, the value returned in AX is the PROGX offset
; of the jumps accessed from the external program.

; Registers destroyed:	AX

%xlidbg   proc	  far
	  or	  ax,ax
	  jz	  dbgoff
	  mov	  al,cs:dbgint		;install INT 3 instruction
	  mov	  xlidbg1,al
	  mov	  al,cs:dbgint+1
	  mov	  xlidbg1+1,al
	  jmp	  short dbgexit
dbgoff:   mov	  al,cs:dbgnop		;install 2 NOP instructions
	  mov	  xlidbg1,al
	  mov	  xlidbg1+1,al
dbgexit:  mov	  ax,offset biddbg	;return address of jump table
					;following EXEC of child
	  ret

; data for above routine
dbgint	  label   byte
	  int	  3			;INT 3 instruction
dbgnop	  label   byte
	  nop				;NOP instruction

%xlidbg   endp

progx	  ends


; Far linkage *to* XLI

prog	  segment byte public 'PROG'
	  assume  cs:pgroup

	  public  xli_init,xli_xesc,xli_term

; We preserve DS,ES,BP.  AX..DI are destroyed.
xli_init  proc	  near
	  mov	  bx,0
xli_10:   push	  bp			;we use ES:BP a lot
	  push	  es
	  call	  %xli_gate		;cross over into PROGX segment
	  pop	  es
	  pop	  bp
	  ret
xli_xesc: mov	  bx,1
	  jmp	  xli_10
xli_term: mov	  bx,2
	  jmp	  xli_10
xli_init  endp

; Far linkage *from* XLI
; (all the memory allocation routines are written in C).
; The caller of this should have set BP from SP before pushing the C args,
; then restore SP from BP afterwards to remove them from the stack.
; We don't preserve ES across the call.

far_C	  proc	  far
	  push	  ds			;C likes ES=DS
	  pop	  es
	  pop	  work_info.C_retadr	;get far @ off stack so C sees its args
	  pop	  work_info.C_retadr+2
	  call	  [work_info.C_fn]
	  push	  work_info.C_retadr+2
	  push	  work_info.C_retadr
	  ret				;C returns with return value in AX..DX
far_C	  endp

; Far linkage to XLI debug hook

; stack:
;	  saved BP
;	  return address (near call)
;	  arg	  (0=turn off, 1=turn on debug)

; AX,BX returns PROGX offset of the jump table following the EXEC of the child.
; This should be the same offset value as in the DOS terminate address vector
; in the child's PSP.

	  public  xlidbg

xlidbg	  proc	  near
	  push	  bp
	  mov	  bp,sp     ;after this instruction, stack matches comments
	  mov	  ax,[bp]+4
	  call	  %xlidbg
	  pop	  bp
	  ret
xlidbg	  endp

prog	  ends
	  end
