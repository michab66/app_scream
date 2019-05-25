;       					=====> REALSCHM.ASM
; PC Scheme Real Procedures for Protected Mode Scheme
; (c) 1987 by Texas Instruments Incorporated  --  all rights reserved
; Author:  Terry Caudill
; History:
;   tc  8/07/87  - to work in protected mode scheme  (real mode side)
;   tc 10/16/87	 - modified to use local stack as transaction buffer


	  page	  84,120
	  name	  PCSXLI
	  title   PC Scheme External Language Interface

	  .286c			;; Utilize the expanded 80286 instruction set
	  include xli.mac
	  include xli.ref
	  include xli.equ

	  subttl  Stack and Data segment definitions
	  page

stksize	   equ	20000	

stack	  segment para stack 'STACK'
s_base	  db	stksize dup (0)
stack	  ends

data	  segment para public 'DATA'
	  public  callers_ds,callers_dx
	  public  load_table,work_area,active_exe
;
; Registers which should be saved due to RPC call
;
trans_buf     equ $
callers_dx    dw  0
callers_ds    dw  0
return_ss     dw  0
return_sp     dw  0
return_bp     dw  0
result_buffer dw  0

;
; jump table for specified function requests. this table is position
; dependent - see rpc.equ and pro2real.asm
;
first_sys_func equ 	20		;max number of rpc functions		
next_avail_sys dw 	0		;next location in sys_func

rpc_func  dw	ret_buffer		;0 - return stack buffer address
	  dw    pctype			;1 - return pc type and graphics info
	  dw	load_exe		;2 - load xli file
	  dw    unload_all		;3 - unload all xli files
	  dw    xesc			;4 - perform xternal escape function
	  dw    ssr_return		;5 - Special Service return
	  dw	takeover_crt		;6 - takeover crt int handler (for exec)
	  dw	restore_crt		;7 - restore system crt int handler
	  dw	11 dup (unknown_func)   ;9 - 19

sys_func  dd	unknown_func,prog	;20
          dd	unknown_func,prog	;21
          dd	unknown_func,prog	;22
          dd	unknown_func,prog	;23
	  dd	unknown_func,prog	;24

;
; The following data structures support the XLI interface
;

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
	  db	  PAD_SIZE*N_ARGS dup (0)  ;during xesc, non-strings go here
; other information required during an xesc call
work_info xesc_struc <> 	      ;general info
swap_table swap_struc N_ARGS dup (<>) ;records swap state for each XCALL arg
bid_name  dw	  0		   ;pointer used for bidding child
; the child currently active or being loaded
active_exe dw	  0		   ;(same format as status table)

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


pc_make   dw	  1		   ;pc type 1 = tipc
crt_sav	  dw	  0,0		   ;location to save crt interrupt

data	  ends

prog	  segment para public 'PROG'
	  assume  cs:prog,ds:data,es:data,ss:stack

	  public  load_exe,bid_child,c2p_handler,c2p_terminate
	  public  xesc,unload_all,find_open_spot,table_search
	  public  do_floarg,do_fixarg,do_bigarg,do_strarg
	  public  do_floval,do_intval,do_TFval,do_strval
	  public  unload_exe,unload_all

; RPC_STARTUP
;  This routine will be started initially by the protected mode
;  application. Return the address of the message handler routine
;  in DS:DX.
rpc_init  proc	far
rpc_startup:
	  mov	ax,cs
	  mov	ds,ax
	  mov	dx,offset rpc_handler
	  ret
rpc_init  endp

; RPC_HANDLER
;  Main control routine for calls to real procedures from protected mode
;  scheme. When an RPC is issued, we will get control here.
;
;  Upon entry:
;     ds:dx => transaction buffer which contains a request. Typically,
;	       transaction_buffer[0] is an op code, which is used as
;	       an index into the RPC_FUNC table to determine the actual
;	       routine to call. Following locations in the transaction
;	       buffer can be used to pass other parameters and are
;	       dependent on the function called.	
;  Upon exit:
;    Transaction_buffer[0] should contain an error indication. 0 = no error
;
rpc_handler proc far
	  int	3			;for debugging purposes
	  pusha				;save callers state

; First of all, lets instantiate our own data segment and save off the
; address of the transaction buffer.
	  mov	ax,ds
	  mov	es,ax			;es => transaction buffer
	  mov	ax,data
	  mov	ds,ax			;ds => our local data
	  mov	callers_ds,es		;save off transaction buffer address
	  mov	callers_dx,dx
	  mov	di,dx
	  mov	bx,es:[di]		;bx = the request opcode
	  mov	word ptr es:[di],0	;default return value to zero (o.k.)
					;handlers must reset for errors.
	  cmp	bx,first_sys_func       ;normal rpc function request?
	  jb	rpc_h010		;yes, jump

; Opcodes >= first_sys_func reflect calls to system xli routines, and require
; arguments to be passed on the stack. Protected mode routines stuff the local
; stack segment (defined by STACK above) with the arguments before issueing
; the RPC. The code below must now instantiate the local stack and call a 
; handler in the sys_func table above.
	  mov	return_ss,ss		;save current stack segment
	  mov	return_sp,sp		;save current stack pointer
	  mov	return_bp,bp		;save current base pointer
	  mov	ax,stack		;get local stack
  	  mov	ss,ax			;  and instantiate
	  mov	sp,es:[di]+2		;transaction_buffer[2] = stack pointer
	  mov	bp,sp			;base pointer = stack pointer

	  sub	bx,first_sys_func	;calc index into sys_func table
	  shl	bx,1
	  shl	bx,1
	  call  dword ptr sys_func+[bx] ;call the routine

	  mov	ss,return_ss		;restore stack used upon entry 
	  mov	sp,return_sp
	  mov	bp,return_bp
	  les	di,dword ptr trans_buf	;restore access to transaction buffer
	  mov	es:[di],ax		;transaction_buffer[0] = return status
	  jmp	rpc_hret		;return to protected mode routine

; We have a normal rpc call. Our local stack segment may have been stuffed with 
; parmameters by the protected mode routine, so lets use it as our extra 
; segment (AIA provides a stack segment with the rpc and we can just use it as
; our stack). 
; 
rpc_h010:
	  mov	ax,stack
	  mov	es,ax
	  xor	di,di			;es:di => pro2real communication buffer
	  shl	bx,1			;convert func code to index
	  call	rpc_func+[bx]		;call function
rpc_hret:
	  popa				;restore callers regs
	  ret				;return to protected mode

rpc_handler endp

; UNKNOWN_FUNC
;  This routine is called when we get an undefined op-code. Return a
;  negative one to the protected mode routine as an error indicator.
;
unknown_func proc near
	  les	di,dword ptr trans_buf	  ;es:di => transaction buffer
	  mov	word ptr es:[di],-1	  ;return error condition
	  ret
unknown_func endp

; RET_BUFFER
;  Return address of local stack segment. This segment will be used by the
;  protected mode routines as a communication buffer between real and protected
;  mode. It will be used by other RPC function requests for passing args and
;  returning values. For system xli calls, it will be instantiated as the
;  stack (see above rpc_handler).
ret_buffer proc	near
	  les	di,dword ptr trans_buf	  ;es:di => transaction buffer
	  mov	es:[di]+2,stksize	  ;return length of communication buffer
	  mov	ax,offset s_base	  ;ax = stack base
	  mov	es:[di]+4,ax       	  ;return as communication buffer offset
	  mov	ax,stack		  ;get buffer segment
	  mov	result_buffer,ax	  ; and save for later
	  mov	es:[di]+6,ax	          ;return communication buffer segment
	  ret
ret_buffer endp
	   	

; PCTYPE
;    Determine type of PC we are running on and initialize screen.
;
; Upon Entry:
;    es:di => communication buffer
;
; Upon Exit:
;    Communication_buffer[0] = Machine Type
;     				  1 for TIPC or Business Pro in TI mode
;    				  FF for IBM-PC
;    				  FE for IBM-PC/XT
;    				  FD for IBM-PC/jr
;    				  FC for IBM-PC/AT or B-P in IBM mode
;    				  F8 for PS2 Model 80
;     				  0 for undeterminable
;    Communication_buffer[2] = Video Mode
;    Communication_buffer[4] = Character Height
;
pctype    proc	  near
	  push	  es		   ; save comm buffer
	  push	  ds		   ; save local data seg

	  mov	  ax,0FC00h	   ; move paragraph address of copyright
pc_002:   mov	  es,ax 	   ;  notice into ES
	  xor	  di,di 	   ;  Clear DI; 0 is lowest address in ROM @ES:
	  xor	  bx,bx 	   ;  Flag for "PC_MAKE"
	  mov	  cx,40h	   ;  This'll be as far as I go...
	  mov	  al,'T'           ;  look for beginning of "Texas Instruments"
	  cli			   ;  Stop interrupts - bug in old 8088's
again:
    repne scas	  byte ptr es:[di] ; SEARCH
	  or	  cx,cx 	   ; Reach my limit?
	  jz	  short pc_005	   ; quit if we've exhausted search
	  cmp	  byte ptr es:[di],'e'     ; make sure this is it
	  jne	  again 		   ; use defaults if not found
	  cmp	  byte ptr es:[di]+1,'x'   ; really make sure this is it
	  jne	  again

	  push	  ds
	  mov	  ds,bx 	   ; 0->DS for addressing low mem.

	  inc	  bx		   ; BX==1 => TIPC
	  mov	  ax,ds:word ptr [01A2h]   ; If TIPC then what kind?
	  pop	  ds		   ; get DS back

	  add	  al,ah 	   ; checkout vector 68 bytes 2 & 3
	  cmp	  al,0F0h	   ; if AL==F0 then TIPC=Business Pro
	  jne	  pc_010	   ; jump if not a B-P

	  in	  al,068h	   ; Read from port
	  push	  ax		   ; Save for later
	  and	  al,0FBh	   ; Enable CMOS
	  out	  068h,al	   ; Write back out
	  mov	  dx,8296h	   ; I/O address for B-P's mode byte
	  in	  al,dx 	   ; TI or IBM Mode on the B-P?
	  cmp	  al,0		   ; if not zero then B-P emulates a TIPC
	  pop	  ax		   ; Restore original port value
	  out	  068h,al	   ;   and write back out
	  jne	  pc_010	   ; jump if TIPC else IBM machine code is
				   ; where it should be.
	  jmp	  short pc_007
pc_005:   mov	  ax,es
	  cmp	  ah,0FEh	   ; test for segment offset FE00
	  jae	  pc_007	   ; two checks made? if so, jump
	  add	  ah,2		   ; go back and check segment offset
	  jmp	  pc_002	   ;  FE00
pc_007:   mov	  ax,0F000h

	  mov	  es,ax
	  mov	  al,byte ptr es:0FFFEh ; IBM's machine code is @F000:FFFE
	  cmp	  al,0f0h	   ; Is this suckah an IBM?
	  jb	  pc_010	   ; Jump if AL is below F0 (BX will be 0)
	  mov	  bl,al
pc_010:   
	  sti			   ; Turn interrups back on
	  cmp	  bx,1		   ; TIPC?
	  jne	  not_ti	   ;  no, jump

; We have a tipc, initialize the graphics
	  push	  0DF01h
	  pop	  es		   	    ; clear graphics planes
	  xor	  di,di
	  mov	  byte ptr es:[di],0AAh	    ; set red palette
	  mov	  byte ptr es:[di]+16,0CCh  ; set green palette
	  mov	  byte ptr es:[di]+32,0F0h  ; set blue palette

	  push	  0DF82h
	  pop	  es
	  mov	  byte ptr es:[di],040h     ; turn text on

	  mov	  ax,3			    ; ax = video mode
	            			    ; bx = pc type code
	  mov	  cx,8			    ; cx = character height
	  jmp	  pc_020			

; We have an ibm, (assumed) get current video mode
not_ti:
	  push	  bx		   ; save pc type code around bios calls
	  mov	  ax,0500h	   ; set active display page (for alpha modes)
	  int	  10h		   ; bios int
	  mov	  ah,15		   ; get current video mode
	  int	  10h		   ; bios int
	  xor	  ah,ah		   ; ax = video mode
	  pop	  bx   		   ; bx = pc type code
	  mov	  cx,8		   ; cx = character height
	  cmp	  ax,16		   ; if video mode = 16
	  jle	  pc_020	   ;   then
	  mov	  cx,14		   ;      reset character height
pc_020:
	  pop	  ds		   ; restore local data seg
	  pop	  es		   ; restore communication buffer
	  xor	  di,di
	  mov	  word ptr es:[di]+0,bx	 ; put PC_MAKE in transaction buffer
	  mov	  word ptr es:[di]+2,ax	 ; ditto video mode
	  mov	  word ptr es:[di]+4,cx  ; ditto char height
;
; and just for something different ... lets try some interrupts
;
TI_PBI	  equ	  05Dh		   ; TI  Program Break Interrupt
IBM_PBI   equ	  01Bh		   ; IBM Program Break Interrupt
GET_VEC	  equ	  035h
SET_VEC   equ	  025h 

	  mov	  pc_make,bx		;save pc type
	  mov	  al,TI_PBI		;default ti program break int
	  cmp	  bx,1			;are we tipc?
	  je	  vec_01		; yes, jump
	  mov	  al,IBM_PBI		; no,  get ibm pbi
vec_01:
	  mov	  ah,GET_VEC		;get vector
fix_010:
	  push	  ds			;tempsave data seg
	  mov	  dx,offset pbi_brk     ;dx=offset of handler
	  mov	  cx,cs
	  mov	  ds,cx 	        ;ds:dx => handler
	  int	  21h
	  pop	  ds			;restore data seg
	  ret

pbi_brk:
	  int	 3			;lets just break and
	  iret				;ignore for now

pctype    endp


ibm_crtint equ	010h
ti_crtint  equ	049h


;  Install new routine at the CRT DSR interrupt
;
takeover_crt proc near
	  int	  3
	  push	  es
	  push	  ds			; save segments

	  mov	  ah,035h		;ah = get int vector address
	  mov	  al,ibm_crtint		;al = ibm crt interrupt
	  cmp	  pc_make,1 		;is it an ibm?
	  jne	  take_010		; yes,  jump
	  mov	  al,ti_crtint		;al = ti crt interrupt
take_010:
	  push	  ax			;save around dos int
	  int	  21h			;get interrupt vector
	  mov	  crt_sav+2,es
	  mov	  crt_sav,bx		;save existing interrupt vector
	  pop	  ax			;restore int

	  mov	  ah,025h		;ah = set int vector, al = int number
	  mov	  dx,offset crtdsr
	  push	  cs
	  pop	  ds			;ds:dx => new interrupt handler
	  int	  21h			;set interrupt vector
	  pop	  ds
	  pop	  es
	  ret
takeover_crt endp

;
;  This routine restores the original routine for the CRT DSR interrupt
;
restore_crt proc  near
	  int	  3
	  push	  ds			;tempsave data segment

	  mov	  ah,025h		;ah = set int vector address
	  mov	  al,ibm_crtint		;al = ibm crt interrupt
	  cmp	  pc_make,1 		;is it a ibm?
	  jne	  restore_010		; yes,  jump
	  mov	  al,ti_crtint		;al = ti crt interrupt
restore_010:
	  mov	  dx,crt_sav
	  mov	  ds,crt_sav+2		;ds:dx => system interrupt handler
	  int	  21h			;set interrupt vector

	  pop	  ds			;restore data segment
	  ret
restore_crt endp

;
;  This is the do-nothing routine installed at the CRT DSR interrupt
;
crtproc   proc	  far
crtdsr:
	  sti
	  mov	 ax,0
	  iret
crtproc   endp


; LOAD_EXE
;    Load an XLI file as a child process, setting up all the necessary hooks
;    so that it can be called via an xesc, or system xli call.
;
; Upon Entry:
;    ES:DI => communication buffer. The structure ld_args (defined below)
;	      indicates the structure of the buffer.
; Upon Exit:				  
;   The first word in the transaction buffer will be set as follows:
;     The high order byte will contain a flags byte where	
;   	success = carry clear
;   	failure = carry set
;     The low order byte will contain the error
;	0    = no open slots
;	<> 0 = EXEC failure code

ld_args	  struc			;structure of transaction buffer for load exe
sysflag   dw	?		;1 = system flag, 0 = user defined
exe_index dw	?		;offset to exe name within pathname	  
pathname  db    ?		;pcs-sysdir pathname
ld_args	  ends

load_exe  proc
; if we succeed, state=EXE_NONE
	  call	  find_open_spot		 ;this sets active_exe
	  mov	  ax,0
	  jc	  le_exit			 ;no open slots
; set state=EXE_TSR for time between EXEC and TSR
	  load_index itself
	  mov	  bh,EXE_TSR
	  mov	  active_exe,bx
	  cmp	  es:[di].sysflag,1		 ;loading system .EXE?
	  je	  le_5				 ;yes, look only in pcs-sysdir
	  mov	  ax,es:[di].exe_index		 ;get address of filename only
	  mov	  bid_name,ax			 ;try current directory first
	  call	  bid_child
	  jnc	  le_10 			 ;bid succeeded, jump
le_5:	  
	  mov	  ax,pathname		 	 ;try looking in pcs-sysdir
	  add	  ax,di
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
le_exit:  
	  lahf					;load flags into ah
	  les	  di,dword ptr trans_buf        ;es:di => transaction buffer
	  mov	  es:[di],ax			;move result to rpc buffer
	  ret
load_exe  endp

;BID_CHILD
;    Given a filename in bid_name, initialize it under XLI.
;
; Upon Entry:
;    ES:bid_name => pathname of the file to bid
;
; Upon Exit:   
;    AX = EXEC status
;
; Assume AX..SI are destroyed; DS,ES,SS,SP,BP,DI are preserved.
bid_child proc
	  push	  di
	  push	  ds		   ;save parent's state
	  push	  es
	  push	  bp
	  save_parent
	  mov	  cs:stk_seg,ss
	  mov	  cs:stk_offset,sp

	  mov	  dx,bid_name	   
	  mov	  ax,es
	  mov	  ds,ax		   ;DS:DX = parm block
	  mov	  bx,data
	  mov	  es,bx
	  lea	  bx,exec_pblock   ;ES:BX = Asciiz pathname
	  mov	  ax,FR_EXEC
	  int	  21h

; The following are external entry points accessible by the child.
biddbg:   jmp	  tsr_done	   ; --- THE BIG 4 ---  (not for child's use)
	  jmp	  c2p_handler	   ; --- THE BIG 4 ---  for XCALL's
	  jmp	  c2p_terminate    ; --- THE BIG 4 ---  for child termination

tsr_done: cli
	  mov	  ss,cs:stk_seg
	  mov	  sp,cs:stk_offset
	  sti
	  pop	  bp
	  pop	  es
	  pop	  ds
	  pop	  di
	  ret

stk_seg    dw	  0		   ;bootstrap parent's state after EXEC
stk_offset dw	  0		   ;from here

bid_child endp

	  subttl  Code segment:  Child->Parent Handler
	  page

;C2P_HANDLER
;    This routine is invoked from the child program bid in BID_CHILD. Upon
;    entry we are executing in the child's environment. The relevant stack
;    stack entries at this point are:
;	  SS:SP (top)  ->  IP	   ;child's far return address
;			   CS
;			   length  ;child's length; for TSR
;			   PSP@    ;child's PSP@
;			   ////    ;(the rest of the stack)
;    The first time called, set up the linkage such that we can get back
;    to the routine via the xesc functionality.

c2p_handler label near
	  resume_parent
	  load_index itself
	  cmp	  bh,EXE_TSR		;first call (performing TSR)
	  je	  c2_10			; yes, jump
	  jmp	  normal		; no, normal call - rejoin xesc
c2_10:
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
	  mov	  es,cx 		         ;ES:BP is file block @
	  mov	  ax,es:[bp].fb_pb
	  mov	  cx,es:[bp].fb_pb+2	         ;get parm block @
	  load_index pb_table
	  mov	  word ptr pb_table[bx],ax	 ;save it
	  mov	  word ptr pb_table+2[bx],cx
;
	  test	  word ptr es:[bp].fb_flags,FB_SYSINT  ;system callable?
	  jz	  c2_40				       ; no, jump
	  mov	  ax,es:[bp].fb_sysint_addr
	  mov	  cx,es:[bp].fb_sysint_addr+2	     ;cx:ax is entry point
	  mov	  bx,next_avail_sys		     ;bx = next avail location
	  inc	  next_avail_sys		     ;bump next avail location
	  shl	  bx,1				     ;make index
	  shl	  bx,1
	  mov	  word ptr sys_func+[bx],ax	     ;save location in table
	  mov	  word ptr sys_func+[bx+2],cx

c2_40:
	  int	  3
	  test	  word ptr es:[bp].fb_flags,FB_KEEPENV
						 ;keep child's env block?
	  jnz	  c2_50 			 ;yes, jump
	  dos_fr  FR_RELMEM,,,,,dx		 ;no, release it for child
c2_50:	  pop	  bp
	  pop	  es
	  mov	  dx,es:[bp].cs_len	;get child's length off its stack
; we're ready to TSR the child
	  dos_fr  FR_TSR,,,dx
; we don't drop through -----------------------------------------


	  subttl  Code segment:  Child termination
	  page

;C2P_TERMINATE
; After the child has performed its wrapup, it calls this routine
; to deallocate its memory and make its spot in the load table available.
c2p_terminate label near
	  mov	  ax,data  		;we needn't save child's context now
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
ct_err:   mov	  bx,XLI_ERR_RELMEM
	  jmp	  xli_err_exit

	  subttl  Code segment:  xesc
	  page

;XESC
; This is the handler for the "%xesc" opcode.
;
; On entry:
;    ES:DI => Communication Buffer set up by protected mode routine.
;
;	+-----------------------------------------------+
;	| Routine name length	(1 word)		|
;	| Routine name		(above length)		|
;	|      .					|
;	|      .					|
;	| Number of Arguments   (1 word)		|
;	| Type of Arg1		(1 word)		|
;	| Arg1			(type dependent)	|		
;	|      .					|
;	|      .					|
;	|      .					|
;	| Type of Argn		(1 word)		|
;	| Argn			(type dependent)	|		
;	+-----------------------------------------------+
;
; On exit:
;    Communication buffer will contain return status, type, and value
;
;	+-----------------------------------------------+
;	| Return Status      	(1 word)		|
;	| Return Value Type	(1 word)      		|
;	| Return Value		(type dependent)	|
;	|      .					|
;	|      .					|
;	|      .					|
;	+-----------------------------------------------+
;
; 	  Return Status will either be 0 for normal return, or -1
;	  for a special service request.
;
;	  Return Value Type should be from 0 to N_RV (4) which are
;	  defined return types, or RV_ERR (10) which allows the
;	  external program to send back an error message.
;
; Note: Return status for xesc is actually returned in the transaction
;       buffer at TRANSACTION_BUFFER[0].
; Buffer definition for passing data back to protected mode
xesc_result struc
xesc_status dw ?
xesc_vtype  dw ?
xesc_value  dw ?
xesc_result ends


xesc	  proc	  near
	  mov	  return_sp,sp			 ;save stack in case errors

	  mov	  ax,ES:[di]			 ;get string length
	  mov	  work_area.srch_slen,ax	 ;save length of string data
	  add	  di,2
	  mov	  work_area.srch_sptr,di	 ;save address of string data
	  mov	  work_area.srch_sptr+2,es
	  add	  di,ax				 ;point past string
	  mov	  ax,word ptr es:[di]		 ;AX = number of args
	  mov	  work_info.xs_nargs,ax		 ;set up number args passed
	  add	  di,2

	  mov	  work_info.xs_pc,di		 ;and save in local area
	  mov	  work_info.xs_pc+2,es		 

; Look for a match.
	  call	  table_search			 ;is there a match?
						 ;(sets active_exe if so)
	  jnc	  xesc_10			 ;yes, jump
	  mov	  bx,XLI_ERR_NO_SUCH_NAME	 ;error: no such name loaded
	  jmp	  xesc_err_exit
xesc_10:  mov	  dx,ax 			 ;tempsave selector
; There was a match.
; Collect the info we'll need to guide us thru xesc call.
	  load_index fb_table
	  mov	  bp,word ptr fb_table[bx]
	  mov	  es,word ptr fb_table+2[bx]	 ;ES:BP is file block @

	  mov	  ax,es:[bp].fb_id		 ;get XLI ID
	  cmp	  ax,XLI_ID			 ;compare to our version
	  je	  xesc_15			 ;if equal, continue
	  mov	  bx,XLI_ERR_BAD_VERSION	 ; else note out of sync
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

	  mov	  cx,0
xesc_20:  cmp	  cx,work_info.xs_nargs 	 ;any left?
	  je	  xesc_50			 ;no, jump
	  push	  cx				 ;tempsave current arg#
	  mov	  bx,cx				 ;BX = current arg#
	  shl	  bx,1				 ; make index into swap table
	  shl	  bx,1
	  mov	  ax,work_info.xs_args			;get arg address
	  mov	  word ptr swap_table[bx].sw_offset,ax	;and save for later
	  mov	  si,work_info.xs_pc
	  mov	  es,work_info.xs_pc+2		 ;ES:SI points to arg type
	  mov 	  di,word ptr es:[si]
	  inc	  si
	  inc	  si				 ;ES:SI points to arg
; Dispatch on argument type
	  call	  cs:word ptr do_arg[di]	 ;handle one type of object
	  add	  work_info.xs_local,PAD_SIZE	 ;incr XLI-local ptr
						 ;(maintain alignment)
	  pop	  cx				 ;restore current arg#
	  inc	  cx
	  jmp	  xesc_20
xesc_50:
          call_child 1   			 ;Call the child.

; We're back with a return value--unless it's a special service call.
normal:   cld
	  load_index pb_table
	  mov	  bp,word ptr pb_table[bx]
	  mov	  es,word ptr pb_table+2[bx]	 ;ES:BP is parm block @
	  cmp	  es:[bp].pb_ss,0		 ;any special services?
	  je	  xesc_60			 ;no, jump

	  jmp 	  ssr				 ;special service. This will
	                       			 ;return from protected mode
						 ;to ssr_return before calling
						 ;the child again at xesc_50
; Now we're really back with the return value
xesc_60:  mov	  di,es:[bp].pb_rvtype
	  mov	  work_area.xs_rvtype,di	 ;return value's type

	  cmp	  di,RV_ERR			 ;external-pgm error return?
	  jne     xesc_65			 ;no, jump
	  mov	  si,work_info.xs_rvptr
	  mov	  es,work_info.xs_rvptr+2	 ;ES:SI points to return value
	  					 ;(external-pgm error message)
	  shl	  di,1				 ;return type to return
	  call	  do_strval			 ;build the string 
	  jmp	  xesc_75  			 ;and return
						 
xesc_65:  cmp	  di,N_RV			 ;return value out of range?
	  jb	  xesc_70			 ;no, jump
	  mov	  bx,XLI_ERR_VALUE_BAD_TYPE
	  jmp	  xesc_err_exit
xesc_70:  shl	  di,1
	  mov	  si,work_info.xs_rvptr
	  mov	  es,work_info.xs_rvptr+2	 ;ES:SI point to return value
	  call	  cs:word ptr do_val[di]	 ;handle one type of return value
xesc_75:
	  mov	  sp,return_sp			 ;clean up stack and return
	  ret

; This file's error exit processing.  Reset the stack so that we return
; correctly. BX should be set with an error code before jumping here.
xli_err_exit:
xesc_err_exit:
	  les	  di,dword ptr trans_buf         ;es:di => rpc buffer
	  mov	  es:[di],bx			 ;return status	  	
	  mov	  sp,return_sp			 ;clean up stack and return
	  ret


	  subttl  Code segment:  Special Services
	  page

; "Swap" special service
; On entry, ES:BP is parm block pointer.

ssr	  label   near

;	  mov	  bx,es:[bp].pb_ss		 ;get dispatch number
;	  cmp	  bx,SS_SWAP
;	  je	  ssr_swap
;	  jmp	  ss_exit

ssr_swap:
	  mov 	  ax,es:[bp].pb_ss_args 	 ;AX = arg#
	  test	  work_info.xs_flags,FB_NEAR	 ;near data?
	  jnz	  ssr_10  			 ; yes, jump
; far data
	  xor	  bx,bx				 ;BX = length (null)
	  mov	  cx,bx				 ;CX = offset (null)
	  mov	  dx,bx				 ;DX = segment(null)
	  jmp	  ss_15
; near data
ssr_10:
	  mov 	  bx,es:[bp].pb_ss_args+2	 ;BX = length
	  mov 	  cx,es:[bp].pb_ss_args+4	 ;CX = destination offset
	  mov	  work_info.xs_dest,cx		 ; save for return trip
	  mov     dx,work_info.xs_pb_segment	 ;DX = destination segment
ss_15:

	  mov	  di,stack
	  mov	  es,di
	  xor	  di,di				 ;ES:DI => result buffer

	  mov	  es:[di].xesc_status,-1	 ;SSR request
	  mov	  es:[di].xesc_status+2,ax	 ;arg #
	  mov	  es:[di].xesc_status+4,bx	 ;length
	  mov	  es:[di].xesc_status+6,cx	 ;offset address
	  mov	  es:[di].xesc_status+8,dx	 ;segment address
	  mov	  sp,return_sp			 ;clean up stack
	  ret					 ;and return to protected mode
					         ;routine to copy the string

ssr_return label near
	  mov	  return_sp,sp  
	  les	  di,dword ptr trans_buf	 ;load rpc buffer
	  mov	  ax,es:[di]+2			 ;get # args copied
	  load_index pb_table
	  mov	  bp,word ptr pb_table[bx]
	  mov	  es,word ptr pb_table[bx]+2
	  mov	  es:[bp].pb_ss,0		 ;Clear ss field for normal exit
	  mov 	  bx,es:[bp].pb_ss_args 	 ;Get arg#
	  mov	  es:[bp].pb_ss_args,ax		 ;Update # chars copied
	  shl	  bx,1
	  shl	  bx,1				 ;index into swap table
	  mov	  bp,word ptr swap_table[bx].sw_offset ;ES:BP =>arg's loc in parm block.

	  test	  work_info.xs_flags,FB_NEAR	 ;near data?
	  jnz	  ssr_r05			 ; yes, jump
; far data
	  push	  es				 ;tempsave
	  mov	  bx,stack
	  mov	  es,bx
	  xor	  di,di				 ;es:di => result buffer
	  mov	  ax,es:[di]+6			 ;ax = offset of string
	  mov	  bx,es:[di]+8			 ;bx = segment of string
	  pop	  es				 ;restore 
	  mov	  word ptr es:[bp],ax		 ;put far @ in parm block
	  mov	  word ptr es:[bp+2],bx
	  jmp	  xesc_50
; near data
ssr_r05:  
          mov	  ax,work_info.xs_dest
	  mov	  es:[bp],ax			 ;put near @ in parm block
	  jmp	  xesc_50


;; Jump tables
; indexed by argument type (standard PCS type tag)
do_arg	  dw	  do_fixarg			 ;0=list (#f only)
	  dw	  do_fixarg			 ;1=fixnum
	  dw	  do_floarg			 ;2=flonum
	  dw	  do_bigarg			 ;3=bignum
	  dw	  do_fixarg			 ;4=symbol (#t only)
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
do_flo20: 
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
	  add	  work_info.xs_pc,10		 ;update arg counter
do_flo40: ret
do_floarg endp

do_bigarg proc	  near
	  mov	  ax,es:[si]			 ;move longint to regs
	  mov	  dx,es:[si]+2
          test	  work_info.xs_flags,FB_NEAR	 ;near flag on?
	  jz	  do_big20			 ;no, jump
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
	  mov	  bx,XLI_ERR_BIG_TO_16_BITS	 ;error: bignum too big
						 ;to become int
	  jmp	  xesc_err_exit
do_big15: mov	  es:[bp]+2,dx			 ;copy MSBy to child
	  mov	  cx,4
	  jmp	  short do_big32
	  ; far (pointer in child points to data in XLI-local space)
do_big20: 
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
	  add	  work_info.xs_pc,6		 ;update arg counter
do_big40: ret
do_bigarg endp

do_fixarg proc	  near
	  mov	  ax,es:[si]			 ;move longint to regs
	  mov	  dx,es:[si]+2
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
	  add	  work_info.xs_pc,6		 ;update arg counter
do_fix40: ret
do_fixarg endp

do_xxerr: jmp	  do_errarg			 ;conditional jumps
						 ;are too short
do_strarg proc	  near
	  add	  work_info.xs_pc,4
	  mov	  bp,work_info.xs_args
	  mov	  es,work_info.xs_args+2	;ES:BP is arg @
	  mov	  word ptr es:[bp],0
	  xor	  cx,cx
	  test	  work_info.xs_flags,FB_NEAR	;Near data?
	  jnz	  do_str50
	  mov	  cx,2
	  mov	  word ptr es:[bp+2],0
do_str50:
	  add	  cx,2
	  test	  work_info.xs_flags,FB_PAD     ;padding on?
	  jz	  do_str65
	  mov	  cx,PAD_SIZE
do_str65: add	  work_info.xs_args,cx
	  ret
do_strarg endp

do_errarg proc	  near
	  mov	  bx,XLI_ERR_ARGN_BAD_TYPE
	  jmp	  xesc_err_exit
do_errarg endp


	  subttl  Code segment:  Copy return value back into Scheme
	  page

; On entry to all the value handler routines:
;	  ES:SI = pointer to return value
;	  DI	= return type
;

do_floval proc	  near
	  push	  ds				 ;save for this routine
	  mov	  cx,result_buffer
	  xor	  dx,dx        			 ;buffer for return values

	  test	  work_info.xs_flags,FB_NEAR	 ;is near flag on?
	  mov 	  ax,es				 
	  mov	  ds,ax				 ;ds now addresses result
	  jnz	  do_flv10			 ;yes, jump
	  ; far
	  mov	  ax,[si]			 ;get ptr to number
	  mov	  bx,[si]+2
	  mov	  si,ax
	  mov	  ds,bx 			 ;DS:SI points to number
	  ; near
do_flv10: 
	  mov	  ax,di				 ;save return type
	  mov	  es,cx
	  mov	  di,dx        			 ;ES:DI points to result buffer
	  mov	  es:[di].xesc_status,0		 ;  set return status
	  mov	  es:[di].xesc_vtype,ax		 ;  set return type
	  add	  di,xesc_value			 ;  now address value field
	  cld
	  mov	  cx,8
     rep  movsb					 ;  move float to buffer
	  pop	  ds
	  ret
do_floval endp

do_TFval  proc	  near
	  mov	  ax,es:[si]			 ;get value
	  or	  ax,es:[si]+2			 ;all bytes must = 0 to be nil
	  or	  ax,es:[si]+4
	  or	  ax,es:[si]+6
;
	  mov	  es,result_buffer
	  xor	  si,si				 ;ES:SI points to result buffer
	  mov	  es:[si].xesc_status,0		 ;  set return status
	  mov	  es:[si].xesc_vtype,di		 ;  set return type
	  mov	  es:[si].xesc_value,ax		 ;  set return value
	  ret
do_TFval  endp

do_intval proc	  near
	  test	  work_info.xs_flags,FB_NEAR	 ;near flag on?
	  jnz	  do_int10			 ;yes, jump
	  ; far
	  mov	  ax,es:[si]			 ;get ptr to number
	  mov	  dx,es:[si]+2
	  mov	  si,ax
	  mov	  es,dx 			 ;ES:BP points to number
	  ; near
do_int10: mov	  ax,es:[si]			 ;get number
	  mov	  dx,es:[si]+2
	  test	  work_info.xs_flags,FB_INT	 ;16-bit integer flag on?
	  jz	  do_int20			 ;no, jump
	  cwd					 ;yes, propagate sign
do_int20: 
	  mov	  es,result_buffer
	  xor	  si,si         		 ;ES:SI points to result buffer
	  mov	  es:[si].xesc_status,0		 ;  set return status
	  mov	  es:[si].xesc_vtype,di		 ;  set return type
	  mov	  es:[si].xesc_value,ax		 ;  set return value
	  mov	  es:[si].xesc_value+2,dx
	  ret
do_intval endp

do_strval proc	  near
	  mov	  ax,es:[si]
	  test	  work_info.xs_flags,FB_NEAR     ;is near flag on?
	  jz	  do_stv10			 ; no, jump
	  mov	  dx,work_info.xs_pb_segment	 ;DX:AX = string ptr
	  mov	  cx,es:[si]+2			 ;CX = string length
	  jmp	  short do_stv15
do_stv10: mov	  dx,es:[si]+2			 ;DX:AX = string ptr
	  mov	  cx,es:[si]+4			 ;get string length
do_stv15: mov	  bx,16380			 ;BX is max string length
	  cmp	  cx,bx				 ;is CX short enough
	  jbe	  do_stv20			 ;yes, jump
	  mov	  cx,bx				 ;no,  truncate at max
; DX:AX = string ptr, CX = string length, DI = return type
do_stv20:
	  mov	  es,result_buffer
	  xor	  si,si         		 ;ES:SI points to result buffer
	  mov	  es:[si].xesc_status,0		 ;  return status
	  mov	  es:[si].xesc_vtype,di		 ;  return type
	  mov	  es:[si].xesc_value,cx		 ;  length 
	  mov	  es:[si].xesc_value+2,ax	 ;  pass string pointer back
	  mov	  es:[si].xesc_value+4,dx
	  ret
do_strval endp

do_errval proc	  near
	  mov	  bx,XLI_ERR_VALUE_BAD_TYPE
	  jmp	  xesc_err_exit
do_errval endp


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

prog	  ends

	  end	rpc_startup
