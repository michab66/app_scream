;							=====> PRO2REAL.ASM
; PC Scheme Protected Mode -> Real Mode Interface
; (c) 1987 by Texas Instruments Incorporated  --  all rights reserved
;
; This Module contains code which interfaces to external programs via
; either the External Language Interface (XLI), Software Interrupt,
; or the Real Procedure Call (RPC). The RPC is specific to protected
; mode scheme only, and is used to implement XLI.

; Author:  Terry Caudill (from Bob Beal's original source)
; History:
;   rb  3/20/87 - original
;   tc  8/7/87  - to work in protected mode scheme
;   tc 10/13/87 - cleanup


	  page	  84,120
	  name	  EXTPROG
	  title   PC Scheme External Program Interface
	  .286c


	  subttl  Includes and Local Equates
	  page

	  include scheme.equ
	  include sinterp.arg
	  include xli.equ
	  include xli_pro.mac
	  include rpc.equ

;
; Dos function requests
;
DOS	   equ 021h			; Dos Function Request
DELETE_SEG equ 04900h			; Delete Segment
REAL_INTRP equ 0E3h			; Issue Real Interrupt  - from AIA
BLOCK_XFER equ 0EC00h			; Block	Transfer 	- from AIA
ALLOC_REAL equ 0E802h			; Create Real Data Seg  - from AIA
CREATE_WIN equ 0E803h			; Create Real Window    - from AIA


	  subttl  Group and Constant definitions
	  page
pgroup	  group   prog
xgroup	  group   progx
dgroup	  group   data


	  subttl  Data segment definitions
	  page

data	  segment para public 'DATA'
	  assume  ds:dgroup
	  public  rpc_handle
	  public  REAL_MODE_BUFFER,REAL_BUF_OFFSET,REAL_BUF_SELECTOR
	  public  REAL_BUF_PARA,REAL_BUF_TOP
	  public  C_fn
	  public  mem_entry,mem_table
; external variables
	  extrn   ctl_file:word,pcs_sysd:word
	  extrn   regs:word
	  extrn	  vid_mode:word,char_hgt:word

;
; The following data definitions are used in communication with real
; mode procedures and the real procedure call (RPC) mechanism provided
; in OS/286 by AI Architects.
;

rpc_real     db	"realschm.exe",0	; Name of RPC file to load
rpc_real_len equ $-rpc_real

rpc_handle   db	0			; Handle to real mode scheme routines
rpc_loaded   db 0			; Flag to note if rpc load was successful
rpc_saved_sp dw	?			; Saved stack pointer

REAL_MODE_BUFFER   equ $		; selector and offset of real mode
REAL_BUF_OFFSET    dw 0			; offset of real mode buffer
REAL_BUF_SELECTOR  dw 0			; segment selector of real mode buffer
REAL_BUF_PARA	   dw 0			; segment address of real mode buffer
REAL_BUF_TOP	   dw 0			; note buffer top

;
; The following are xli filenames which must be loaded and used by pcs
;
io_exe       db	"realio.exe" ,0           ;EXE file providing I/O support	
io_exe_len   equ $-io_exe

graph_exe    db	"graphics.exe" ,0         ;EXE file providing graphics support	
graph_exe_len equ $-graph_exe

trig_exe     db	"newtrig.exe" ,0          ;EXE file providing trig support	
trig_exe_len equ $-trig_exe

;
; The following table is used to load the system files required by pcs. The
; xli system files are order dependent. 
sys_files    equ $
;system xli files, order is dependent (see rpc.equ and realschm.asm)
	     dw	 io_exe,io_exe_len        ;io support       - xli system file
	     dw  graph_exe,graph_exe_len  ;graphics support - xli system file
;normal xli files
normal_files equ $
	     dw	 trig_exe,trig_exe_len	  ;trig file        - normal xli file
	     dw  0
;
; If the above files cannot be found, issue this message and abort scheme
;
FILERR	     db  0Dh,0Ah,"Fatal Error - unable to load file "
FILNAM	     db  20 dup (0)

;
; The following table contains gateways from the prog segment to the
; progx segment. The order is dependent on
; Table of RPC functions currently defined. Calling any of these functions
; requires synchronization with the real mode routine.
;
FAR_RPC	     equ	$
frpc_bid     equ	$-FAR_RPC
	     dw	init_rpc,progx		; bid real procedure
frpc_init    equ	$-FAR_RPC
     	     dw	xpcinit,progx		; get machine type
frpc_setcrt  equ	$-FAR_RPC
     	     dw	xsetcrt,progx		; set crt interrupt
frpc_resetcrt equ	$-FAR_RPC
     	     dw	xresetcrt,progx		; reset crt interrupt
frpc_ldall   equ	$-FAR_RPC
      	     dw	load_all,progx		; load xli files
frpc_unld    equ	$-FAR_RPC
     	     dw	unload_all,progx	; unload xli files
frpc_xesc    equ	$-FAR_RPC
     	     dw	xesc,progx		; perform xesc call

;
; The following hooks are used to call routines in the PROG segment
; from the PROGX segment. See the far_C routine in this module.
;
C_fn	     dw  ?	      		
C_retadr     dw  ?			; Used to call C routines from PROGX
	     dw  ?	

;
; Mem_table is used to hold selectors to real memory which must be allocated
; over the life of an xli call. At present, the memory is allocated so that
; xli routines may access far strings. See SSR within.
;
mem_entry    dw	 0		;entry into memory table
mem_table    dw	 N_ARGS dup (0)	;record memory allocated during xli call

;
; The following structures allow xesc and sw-int to share code
;
xesc_func    db	 ?			;0 = sw-int, 1 = xesc
error_return dw	 ?			;address of error handler

which_func   dw   swi_txt,xli_txt	;will be indexed by xesc_func above
swi_txt	     db   'SW-INT',0
xli_txt      db   'XCALL',0     

;
; Error return values for software interrupt
;
SWI_ERR_ARGN_BAD_TYPE	equ  1 		; Bad argument passed to sw-int
SWI_ERR_VALUE_BAD_TYPE	equ  2 		; Bad type passed to sw-int
SWI_ERR_BIG_TO_32_BITS	equ  3 		; Number to large for sw-int

swi_errs  dw	swi_arg0,swi_arg1,swi_arg2

;
; Software Interrupt error messages
;
swi_arg0  db	  'Invalid argument to SW-INT',0
swi_arg1  db	  'Invalid return value for SW-INT',0
swi_arg2  db	  'Argument to SW-INT too large to fit in 32 bits',0

;
; Protected Mode Fatal type errors
;
cr_win	  db	  'CREATE WINDOW',0
al_seg	  db	  'ALLOCATE SEGMENT',0
dl_seg	  db	  'DELETE SEGMENT',0	  
rl_int	  db	  'ISSUE REAL INTERRUPT',0

;
; Gate to abort code in sc.asm
;


data	  ends


	  subttl  Progx code segment definitions
	  page

; external routines
	  extrn   alloc_fl:near,int2long:near,long2int:near,alloc_bl:near
	  extrn   getbase:near
	  extrn	  chg_vmode:near
	  extrn   pro_erro:near

progx	  segment para public 'PROGX'
	  assume  cs:xgroup,ds:dgroup,es:dgroup,ss:dgroup

	  extrn	  xcabt:far

	  public  init_rpc,xpcinit,xsetcrt,xresetcrt,xesc,load_all,unload_all
	  public  ssr
	  public  do_floarg,do_fixarg,do_bigarg,do_strarg
	  public  do_floval,do_intval,do_TFval,do_strval
	  public  softint,swi_strarg,swi_strval



	  subttl  RPC interface routines
	  page

; INIT_RPC
;  Load the real mode portion of scheme and save the handle in rpc_handle.
;  Then call the rpc routine to return the real address of a buffer which
;  will be used on subsequent rpc requests. This buffer is mapped to a
;  protected mode selector and stored in REAL_BUF_SELECTOR.
;
;  The transaction buffer for an rpc must be pointed to by DS:DX. Note that
;  we build this buffer up on the local stack.
;
init_rpc  proc	far
	  push	bp
	  sub	sp,80			;allocate transaction buffer
	  mov	bp,sp			;should be large enough for filename
	  cld
	  mov	  di,pcs_sysd		;di => system directory pathname
	  mov	  cx,64 		;cx =  max length
	  mov	  al,0
    repne scasb 			;scan pathname for eos character (=0)
	  jcxz	  ini_10		;jump if none
	  dec	  di			;di => end of pathname
ini_10:	  
	  mov	  cx,di
	  sub	  cx,pcs_sysd		;cx =  length of system directory 
 	  mov	  di,sp			;di => stack (transaction buffer)
	  mov	  si,pcs_sysd		;si => pcs-sysdir
      rep movsb 			;copy system directory into buffer
	  mov	  al,'\'                ;follow directory name with \
	  stosb
          mov	  si,offset rpc_real	
	  mov	  cx,rpc_real_len
      rep movsb 			;follow directory w/real proc filename

;Initialize real procedure call
	  mov	dx,sp			;ds:dx => real procedure filename
	  mov	ah,RPC_INIT		;load and init real procedure
	  int	DOS			;extended Dos call for Protected mode
	  jnc	ini_20			;continue if no error encountered

	  mov	ax, offset rpc_real	;ax => file that couldn't load
	  mov	cx,rpc_real_len		;cx => length of filename
	  jmp	fatal_file_err		;jump to fatal error handler
	  
ini_20:			
	  mov	rpc_handle,al		;save handle to real procedure
	  inc	rpc_loaded		;note real procedure loaded

; Obtain communication buffer for subsequent RPC calls
	  mov	dx,bp			;ds:dx => transaction buffer
	  mov	word ptr [bp],RPCRETBUF	;return real buffer opcode
	  mov	cx,8			;pass 8 bytes
	  mov	bx,cx			;expect 8 bytes returned
	  mov	ah,RPC			;issue Real Procedure Call
	  int	DOS			;extended Dos call for Protected mode
					;ignore return status

	  mov	dx,[bp]+2		;get length of buffer
	  sub	dx,2			;calc top of stack
	  mov	REAL_BUF_TOP,dx		; and save
	  mov	si,sp
	  add	si,4			;ds:si => real address of buffer
	  mov	ax,[si]+2		;get paragraph address
	  mov	REAL_BUF_PARA,ax	; and save
;ds:si=> offset,seg of real buffer, dx=length
	  call  map_real_mem		;map real address to protected selector
	  mov	REAL_BUF_SELECTOR,ax	; and save

	  add	sp,80			;now clean up the stack
	  pop	bp			 
	  ret				;and return
init_rpc  endp

; XPCINIT
;  Determine the machine type and perform machine specific initialization.
;  Call the real mode routine to perform initialization functions via the
;  RPC mechanism.
;
; Input:  none
; Output: return status, pc machine type, and video mode are returned
;         in the communications buffer accessed by REAL_MODE_SELECTOR.
;
xpcinit   proc 	far
	  push	RPCTYPE			; Type code
	  mov	dx,sp			; ds:dx => arg buffer
	  mov	cx,2 			; cx = # arg bytes passed
	  mov	bx,cx			; bx = # result bytes expected
	  mov	al,rpc_handle		; Handle to real mode part
	  mov	ah,RPC			; Real Procedure Call
	  int	DOS			; Extended Dos call for Protected mode
; Check for errors here
	  pop	ax			; ignore return status
; Get the return values from the real mode buffer
	  MOVE_ARGS_FROM_BUF <PC_MAKE,VID_MODE,CHAR_HGT>,REAL_MODE_BUFFER

	  mov	ax,ds
	  mov	es,ax			; restore extra seg reg
	  ret				; and return
xpcinit   endp


; XSETCRT
;  Take over the real mode crt interrupt handler during a dos-call so that
;  display will not be written to.
;
; Input:  none
; Output: screen output will be inhibited
;
xsetcrt   proc	  far
	  push	RPCTAKCRT		; Take over crt interrupt handler
	  mov	dx,sp			; ds:dx => arg buffer
	  mov	cx,2 			; cx = # arg bytes passed
	  mov	bx,cx			; bx = # result bytes expected
	  mov	al,rpc_handle		; Handle to real mode part
	  mov	ah,RPC			; Real Procedure Call
	  int	DOS			; Extended Dos call for Protected mode
	  pop	ax			; ignore return status
	  ret				; and return
xsetcrt   endp


; XRESETCRT
;  Restore the original crt interrupt handler after a dos call so that the
;  display can once again be written to.
;
; Input:  none
; Output: screen output will be restored
;
xresetcrt proc	  far
	  push	RPCRSTCRT		; Restore crt interrupt handler
	  mov	dx,sp			; ds:dx => arg buffer
	  mov	cx,2 			; cx = # arg bytes passed
	  mov	bx,cx			; bx = # result bytes expected
	  mov	al,rpc_handle		; Handle to real mode part
	  mov	ah,RPC			; Real Procedure Call
	  int	DOS			; Extended Dos call for Protected mode
; Check for errors here
	  pop	ax			; ignore return status
	  ret				; and return
xresetcrt endp



	  subttl  RPC interface routines to XLI
	  page

; LOAD_ALL
;  A portion of the XLI routines is in real mode and is communicated with 
;  via the Real Procedure Call (RPC). Data must be passed to the real mode
;  routine via the real buffer REAL_MODE_BUFFER
;
;  Any errors encountered are currently ignored.

l_save	  struc
exe_name  dw	?			;index to start of exe name
handle	  dw	?			;file handle
l_len	  db	?			;marker for size of local area
l_save	  ends

load_all  proc  far
	  push	bp
	  sub	sp,l_len		;allocate local storage
	  mov	bp,sp

; calc length of pathname
	  cld
	  mov	di,pcs_sysd
	  mov	cx,64 			;max length of pathname
	  mov	al,0
    repne scasb 			;look for eos character (=0)
	  jcxz	la_10			;jump if none
	  dec	di
la_10:
	  mov	cx,di
	  sub	cx,pcs_sysd		;cx = length of pcs-sysdir
; copy pcs-sysdir into transaction buffer
	  push	cx			;tempsave length
	  RESET_REAL_BUFFER_OFFSET	;ensure start at buffer start
	  MOVE_ARGS_TO_BUF <1>,REAL_MODE_BUFFER,autoincr  ;system file first
	  add	di,2			;save space for exe index
	  pop	cx			;restore length
	  mov	si,pcs_sysd		;ds:si addresses pcs-sysdir
	  MOVE_TO_REAL_BUF autoincr	;move to real memory buffer

	  mov	al,'\'                	;append \ onto pcs-sysdir name
	  MOVE_BYTE_TO_BUF al,,autoincr
;save index to exe filename
	  mov	[bp].exe_name,di	;save offset after pcs-sysdir
	  mov	bx,di			;save offset after pcs-sysdir
	  mov	di,2
	  MOVE_ARGS_TO_BUF <bx>         ;save index to exe file

	  mov	di,bx			;position offset for .EXE name
;save control filename to transaction buffer
	  mov	bx,ctl_file	   	;get address of ctl file
	  cmp	byte ptr [bx],'-'	;user override normal xli files?
	  jne	sysload			; no,  jump
	  mov	word ptr normal_files,0 ; Yes, don't load normal xli files
	  inc	ctl_file		;      bump ptr to name
sysload:
; load all system files - di should not be modified in following loop
	  mov	si,offset sys_files
loadfile:
	  push	si			;save offset into file table
	  mov	cx,ds:[si+2]		;cx =  length
	  mov	si,ds:[si]		;si => filename
	  MOVE_TO_REAL_BUF          	;copy filename to buffer
	  push	RPCLDEXE		;RPC request code to load EXE
	  mov	dx,sp			;ds:dx => rpc request code
	  mov	cx,2			;cx = # arg bytes passed
	  mov	bx,cx			;bx = # arg bytes returned
	  mov	al,rpc_handle		;al = handle
	  mov	ah,RPC			;Issue Real Procedure Call
	  int	DOS			;Issue extended dos funcall
	  pop	ax     			;ah = flags, al= return status
	  pop	si			;restore index into file table
	  sahf				;load flags
	  jnc	load_10			;no carry, proceed
	  mov	cx,ds:[si+2]		;cx =  length
	  mov	ax,ds:[si]		;si => filename
	  jmp	fatal_file_err  	;go report error
load_10:
	  add	si,4			;address next entry
	  cmp	word ptr ds:[si],0	;any more entrys?
	  jne	loadfile		; yes, loop
userload:
	  xor	di,di			;address system flag
	  MOVE_ARGS_TO_BUF <0>		;indicate user defined xli
	  mov	di,[bp].exe_name	;di = index to exe name
; open XLI control file
	  mov	  dx,ctl_file		;dx = address of filename
	  mov	  ax,FR_OPEN		;dos function - open file
	  int	  DOS
	  mov	  [bp].handle,ax	;save handle
	  jnc	  next_file		;jump if no open errors
	  jmp	  close1		;can't open file, exit
; read in next filename off the control file and append it to
; the pcs-sysdir name. 
next_file: 
	  mov	  di,[bp].exe_name	;es:di => buffer after pathname
	  mov	  bx,[bp].handle	;bx = file handle
next_char: 
	  push	  0   			;allocate place on stack
	  mov	  dx,sp			;dx = address of buffer
	  mov	  cx,1			;read one character
	  mov	  ax,FR_READ		;dos function - read file
	  int	  DOS			;ignore errors
	  pop	  dx			;retrieve character
	  jnc	  la_20 		;jump if no error, else
					;suddenly can't read control
					;file, close it and exit
close:	  
	  mov	  bx,[bp].handle	;bx = file handle
	  mov	  ax,FR_CLOSE		;dos functions - close file
	  int	  DOS			;ignore errors
close1:   
	  add	sp,l_len		;adjust stack	 
	  pop	bp
	  ret				;return

la_20:	  cmp	  ax,0			;at eof?
	  jz	  close 		;yes, jump
; we've read a character
	  cmp	  dl,0Dh		 ;carriage return?
	  je	  got_file		 ;yes, jump
	  cmp	  dl,' '     		 ;blank or control char?
	  jle	  next_char		 ;yes, skip it
	  MOVE_BYTE_TO_BUF dl,,autoincr  ;move character to buffer
	  jmp	  next_char
; we've read a complete filename, go load it
got_file: 
	  MOVE_BYTE_TO_BUF 0		;form ASCZII string

	  push	RPCLDEXE		;RPC request code to load EXE
	  mov	dx,sp			;ds:dx => rpc buffer
	  mov	cx,2			;cx = # arg bytes passed
	  mov	bx,cx			;bx = # arg bytes returned
	  mov	al,rpc_handle		;al = handle
	  mov	ah,RPC			;Issue Real Procedure Call
	  int	DOS			;Issue extended dos funcall
	  pop	ax			;bump result arg from stack
	  sahf				;ah = flags
	  jnc	  next_file		;jump if no errors
	  xor	  ah,ah			;clear flags from result
	  cmp	  ax,0			;any open slots?
	  je	  close 		;no, jump
	  cmp	  ax,2			;file found?
	  je	  next_file		;no, jump
	  cmp	  ax,8			;ran out of memory?
	  jne	  next_file		;no, jump; ignorable error
	  jmp	  close 		;yes
load_all  endp


; UNLOAD_ALL
;  Call the real mode routine to unload all exe files.
; 
; Upon exit:
;    All previously bid xli programs will be released from real memory.
;
unload_all proc far
	  push  RPCUNLDALL		; RPC request code to unload all exe's
	  mov	dx,sp              	; ds:dx => arg buffer
	  mov	cx,2			; cx = # arg bytes passed
	  mov	bx,2			; bx = # result bytes expected
	  mov	al,rpc_handle		; Handle to real mode part
	  mov	ah,RPC			; Real Procedure Call
	  int	DOS			; Extended Dos call for Protected mode
	  pop	ax			; ignore errors
	  ret
unload_all endp

; FATAL_FILE_ERR
;   We are unable to load a system file in real mode, and cannot
;   continue with scheme. The routine XCABT (in sc.asm) will output
;   a message (via DOS function 9) to the console and abort. Our
;   io may not be available at the time of this error.
;
; On entry:
;   ax => filename we are trying to load
;   cx =  length of filename
;
	  public fatal_file_err
fat_err	  proc  near
fatal_file_err  label near
	  mov	bx,ss
	  mov	ds,bx
	  mov	es,bx				;ds,es,ss = data segment
	  mov	si,ax				;ds:si addresses filename
	  mov	di,offset FILNAM		;es:di addresses message
	  rep   movsb				;move filename into message
	  mov	byte ptr es:[di],"$"		;terminate byte
	  cmp	rpc_loaded,0		        ;have we gotten past rpc load?
	  je	fat_exit    		        ; no,  exit
	  call	unload_all 			; yes, ensure all xli's unloaded
fat_exit:
	  mov	dx,offset FILERR		;ds:dx => message
	  jmp	pgroup:xcabt			;exit to DOS
fat_err  endp

; FATAL_PRO_ERR
;   A protected mode operation has failed. Call pro_error in serror.c to
;   output an error message and attempt a scheme-reset.
;   a scheme reset.
;
; On entry:
;   ax =  error number
;   bx => function call name
;   cx => operation being performed (sw-int, xcall, etc.)
;
pro_err	  proc  near
fatal_pro_err  label near
	  push	  bp
	  mov	  bp,sp  			;set up stack for call
	  push	  ss
	  pop	  ds				;ensure ds = data segment
	  push    ax				;error number
	  push	  bx				;function call
	  push	  cx				;routine
	  mov	  C_fn, offset pgroup:pro_erro
	  call	  far ptr far_C		        ;control will not return here
pro_err   endp


; XESC				 
;   Handler for the "%xesc" opcode.
;
; On entry:
;   AX = length of xesc call (= inst length - 1)
;   ES:SI = pointer to bytecode arguments of the %xesc opcode
;
; On exit:
;   normal: the VM reg that contained the name string on entry will
;	    contain the page:offset of the return value; there may
;	    be side effects in strings that were arguments to %xesc
;	    BX = 0 (no errors)
;   error:  BX = error#
;
; Description:
;   A buffer is built for an RPC call to the real mode handler for
;   an external subroutine call (XCALL). The buffer is built in a  
;   buffer in the real mode routine as follows:
;
;   +----------------------------------------+
;   |  Routine name length         (1 word)  |
;   |  Routine name          (above length)  |
;   |					     |
;   |  Number of XCALL Arguments   (1 word)  |
;   |					     |
;   |  Type of Arg1		   (1 word)  |
;   |  Arg1		   (type dependent)  |
;   |      .				     |
;   |      .				     |
;   |      .				     |
;   |  Type of Argn	           (1 word)  |
;   |  Argn		   (type dependent)  |
;   +----------------------------------------+
;
;   After calling the real mode handler, the buffer will contain
;   result info and return values. See the structure "xesc_result"
;   for a description of the buffer upon return.
;

;
; This following data will be allocated locally within xesc
;
local_save struc			
; following is used to store return data from xli routines
xesc_status dw ?		; return status
xesc_vtype  dw ?		; type of value being returned
xesc_value  dw 4 dup (?)	; return value
; following is local data used in building xli call
saved_si    dw ?		; segment offset of vm bytecode
saved_es    dw ?		; segment address of vm bytecode
first_arg   dw ?		; first actual argument
arg_count   dw ?		; number of args (len,name are not args)
rvreg       dw ?		; vm register to hold return value
local_save  ends

arg_ptr	    equ	  saved_si	; alias for current argument pointer
ssr_status  equ   xesc_status	; ssr return status (will be -1)
ssr_argnum  equ   xesc_vtype	; argument requested (zero based) by ssr
ssr_len	    equ   xesc_value	; length requested
ssr_offset  equ   xesc_value+2	; real mode offset to store arg
ssr_seg     equ   xesc_value+4	; real mode segment to store arg
result_buf_len equ saved_si-xesc_status ; length of result buffer


xesc	  proc	  far			
       	  push	  bp 				;save callers bp
	  sub	  sp,rvreg+2			;reserve for local storage
	  mov	  rpc_saved_sp,sp		;save off stack pointer
	  mov	  bp,sp				; and update BP
							       
	  mov	  xesc_func,1
	  lea	  bx,xesc_err_exit		; Set up error handler for xesc
	  mov	  error_return,bx

	  mov	  [bp].saved_es,es		;save segaddr of arguments
	  inc	  si				;bump past name to first arg
	  mov	  [bp].saved_si,si		; and save
	  mov	  [bp].first_arg,si
	  dec	  si

	  sub	  ax,2				;calc # args (not incl. name)
	  mov	  [bp].arg_count,ax		; and save

	  RESET_REAL_BUFFER_OFFSET    		;ensure start at zero

;
; Move the string name to the real mode buffer
;
	  xor	  bh,bh
	  mov 	  bl,byte ptr es:[si]		;BX is reg# of name string
	  lea	  bx,regs[bx]			;VM reg @
	  mov	  [bp].rvreg,bx 		;  save as return register
	  mov	  si,[bx].C_page
	  cmp	  ptype[si],STRTYPE*2		;is it a string?
	  je	  xesc_15			;yes, jump
	  cmp	  ptype[si],SYMTYPE*2		;is it a symbol?
	  je	  xesc_10			;yes, jump
	  mov	  ax,XLI_ERR_NAME_BAD_TYPE	;error: name not string, symbol
	  jmp	  xesc_err_exit
;
; Warning : DS is not used for the local data segment in the following code
;
xesc_10:  
	  %LoadPage ds,si			;page# in SI -> para# in DS
	  mov	  si,ss:[bx].C_disp		;DS:SI is symbol object @
	  mov	  cx,[si].sym_len		;get symbol object length
	  sub	  cx,sym_ovhd			;subtract symbol's overhead
	  add	  si,sym_ovhd			;skip past overhead
	  jmp	  short xesc_25
xesc_15:  %LoadPage ds,si			;page# in SI -> para# in DS
	  mov	  si,ss:[bx].C_disp		;DS:DI is string object @
	  mov	  cx,[si].str_len		;get string object length
	  cmp	  cx,0				;is it positive?
	  jge	  xesc_20			;yes, jump; normal string
	  add	  cx,str_ovhd*2 		;no, assume short string
						;rather than really long string
						;and make positive
xesc_20:  sub	  cx,str_ovhd			;subtract string's overhead
	  add	  si,str_ovhd			;skip past overhead
xesc_25:  
	  push	  ds
	  push	  si				;temp save string ptr
	  push	  cx				;and length

	  mov	  ax,ss				;get local data seg
	  mov	  ds,ax

	  MOVE_ARGS_TO_BUF cx,REAL_MODE_BUFFER,autoincr  ;move length to buf

	  pop	  cx
	  pop	  si				
	  pop	  ds				;ds:si => string ptr
	  
	  MOVE_TO_REAL_BUF autoincr		;move string to buf

;
; Warning : DS is not used for the local data segment in the above code
;
	  mov	  ax,ss
	  mov	  ds,ax				;restore data segment
;
; Move argument count to real mode buffer
;	 
	  mov	  bx,[bp].arg_count
	  MOVE_ARGS_TO_BUF bx,,autoincr,save   	;move #args to buffer

;
; Move the xesc arguments to the real mode buffer.
;
	  cmp	  bx,0	    			;any arguments?
	  je	  xloop_done			; no, jump
xesc_loop:
	  les	  si,dword ptr [bp].arg_ptr     ;es:si => argument
	  inc	  [bp].saved_si			;bump for next time thru
	  xor	  bh,bh
	  mov     bl,byte ptr es:[si]		;pick up arg
	  lea	  bx,regs[bx]			;BX is VM reg @
	  mov	  si,[bx].C_page		;get its page#
	  mov	  si,ptype[si]		        ; and type
	  push	  si				;save around following
;move type info to buffer
	  MOVE_ARGS_TO_BUF si,REAL_MODE_BUFFER,autoincr
; Dispatch on argument type
	  pop	  si				;restore type #
	  call	  cs:word ptr do_arg[si]	;process argument (by type)
	  dec	  [bp].arg_count	        ;any more args left
	  jnz	  xesc_loop			; yes, loop
xloop_done:
	  RESET_REAL_BUFFER_OFFSET		;reset buffer ptr for later
;
; Now issue the RPC call, real routine knows where the buffer is
;
	  push	  0				 ;dummy word
	  push	  RPCXESC        		 ;RPC REQUEST CODE
xesc_57:
	  mov	  dx,sp				 ;DS:DX = transaction buffer
	  mov     cx,4
	  mov	  bx,cx				 ;DX = length of result
	  mov	  al,rpc_handle			 
	  mov	  ah,RPC			 ;Issue RPC
	  int	  DOS				 ;Extended Dos func
	  pop	  ax				 ;get return status
	  mov     sp,bp			 	 ;dump args off stack
	  or 	  ax,ax				 ;error during xesc call?
	  je	  normal			 ;  no, continue
	  cmp	  ax,XLI_ERR_NO_SUCH_NAME	 ;calling an unknown xli func?
	  jne	  xesc_null_err_exit		 ;  no, return error
	  mov	  bx,[bp].rvreg			 ;load bx with name requested
	  jmp	  xesc_err_exit		 	 ;and return with error
		
; We're back with a return value--unless it's a special service call.
; At this point, ES:DI should point to buffer.
normal:   cld

	  mov	  si,sp				;store data on stack (ds:si)
	  les	  di,dword ptr REAL_MODE_BUFFER	;address real buffer (es:di)
	  mov	  cx,result_buf_len		;cx = length
	  MOVE_FROM_REAL_BUF			;move return data to local stack

	  mov	  ax,[bp].xesc_status		;get return status
	  or	  ax,ax				;Check status
	  jl	  ssr				; <0 = SSR	
	          				;  0 = normal return
	  mov	  di,[bp].xesc_vtype		;get return value type
	  cmp	  di,N_RV*2			;out of range?
	  jb	  xesc_70			; no,  jump
	  cmp	  di,RV_ERR*2			;xli program error?
	  jne	  xesc_65			; no,  jump
	  mov	  si,bp				;
	  add	  si,xesc_value			;DS:SI => return value
	  mov	  bx,[bp].rvreg			;bx = return reg address
	  call	  do_strval			;go get the error message
	  mov	  ax,XLI_ERR_EXTERNAL_ERROR	;ax=error indication
	  mov	  bx,[bp].rvreg			;bx = return reg address
	  jmp	  xesc_err_exit			;bx=message
xesc_65:
	  mov	  ax,XLI_ERR_VALUE_BAD_TYPE     ;unkown return type
	  jmp	  xesc_null_err_exit		;return error
xesc_70:  
	  mov	  si,bp
	  add	  si,xesc_value			;DS:SI => return value
	  mov	  bx,[bp].rvreg			;bx = return reg address
	  call	  cs:word ptr do_val[di]	;process return value
	  mov	  ax,0				;AX=0 says no errors

xesc_null_err_exit:
	  lea	  bx,nil_reg			;"nil irritant" for some errors
; ax = error indicator (0 = no error), bx=irritant
xesc_err_exit label near
	  mov	  cx,mem_entry			;any entries in mem_table?
	  jcxz	  xesc_ex10			;no, jump
	  push	  ax				;tempsave error indicators
	  push	  bx
	  xor	  bx,bx
	  mov	  mem_entry,bx			;see if any real mode segments
xesc_ex05:
	  mov	  es,mem_table[bx]		;get entry in mem_table
	  mov	  ax,DELETE_SEG			;delete the real mode segment
	  int	  dos
	  jnc	  xesc_ex07
	  mov	  bx,offset dl_seg
	  mov	  cx,offset xli_txt
	  jmp	  fatal_pro_err			;control will not return here
xesc_ex07:
	  inc	  bx
	  inc 	  bx				;address next entry
	  loop	  xesc_ex05			;go release next one
	  pop	  bx				;restore error indicators
	  pop	  ax
; at this point, ax = error number, bx = irritant (if error)
xesc_ex10:
	  mov	  sp,rpc_saved_sp		;clean up stack
	  add	  sp,rvreg+2	  
	  pop	  bp				;restore callers bp
	  ret	    				;return
; SSR
;  A real procedure has issued a System Service Request (SSR). Currently,
;  this means to pass a string to the real procedure. The result buffer
;  indicates the argument from the %xesc call requested (0 based), the
;  length of the string, and the real mode segment/offset to place the
;  string. This routine copies the data into the real routine's address
;  space, and returns.
;

ssr	  label	  near
	  mov	  si,[bp].first_arg		 ;arg list pointer
	  add	  si,[bp].ssr_argnum		 ;now address arg desired
	  mov	  es,[bp].saved_es		 ;ES:SI addresses the arg
	  mov	  bl,byte ptr es:[si]		 ;get reg #
	  xor	  bh,bh
	  lea	  bx,regs[bx]			 ;BX is reg@

	  mov	  si,[bx].C_disp		 ;si = string object offset
	  mov	  bx,[bx].C_page		 ;bx = string object page #
	  %LoadPage es,bx			 ;es:si => string object
	  inc	  si				 ;skip over tag
	  cld
	  lods    word ptr es:[si]		 ;get string's length
	  cmp	  ax,0				 ;a short string?
	  jge	  ss_5				 ;no, jump
	  add	  ax,str_ovhd*2 		 ;yes
ss_5:	  sub	  ax,str_ovhd			 ;subtract off overhead
;
; es:si => string, ax = length
;
	  mov	  dx,[bp].ssr_len		 ;get length of dest string
	  or	  dx,dx				 ;if non-zero
	  jnz	  ss_10				 ;  then jump
;
; A length of zero indicates that the xli routine wants to address far
; strings. Allocate real memory and put the real segment address into
; the transaction buffer. PRO2REAL will move the string to real memory.
; The real memory selector is saved in mem_table, and released when we 
; exit this xesc call.
;
	  push	  ax			;save length
	  push	  si
	  push	  es			;save ptr to string
	  xor	  cx,cx			
	  mov	  dx,ax			;cx:dx = string length
	  mov	  ax,ALLOC_REAL		;Allocate real segment
	  int	  dos			;Allocate real segment
	  jnc	  ss_07
	  mov	  bx,offset al_seg
	  mov	  cx,offset xli_txt
	  jmp	  fatal_pro_err		;control will not return here
ss_07:
; ax=selector, bx=para address
	  push	  ax			;tempsave selector					      
	  les	  di,dword ptr REAL_MODE_BUFFER
	  add	  di,ssr_seg		;address of real buffer (es:di)
	  MOVE_ARGS_TO_BUF bx		;save segment to real mode
	  mov     dx,cx			;dx = length
	  pop	  ax			;restore selector
; save real memory selector in table
	  mov	  bx,mem_entry		;get entry number
	  inc	  mem_entry		;bump number of entries
	  shl	  bx,1			;index into memory table
	  mov	  mem_table[bx],ax	;save selector there

	  pop	  es
	  pop	  di			;es:di => string to copy
	  pop	  dx			;restore length
	  jmp	  ss_25
; We have a string length here, set ds:si to point to the real memory
; address. PRO2REAL will create a real window over this area, and copy
; the string to it.
ss_10:
	  cmp	  ax,dx 		;string len >= buffer len?
	  jae	  ss_20 		;yes, jump
	  mov	  dx,ax 		;dx = #chars to copy
ss_20:					
      	  mov	  di,si			;es:di = string to copy
	  mov	  si,bp
	  add	  si,ssr_offset		;ds:si => real memory address
	  xor	  ax,ax			;use ds:si to map address
ss_25:
	  call	  pro2real		;copy to real memory

	  push	  cx
	  push	  RPCXLISSR
	  jmp	  xesc_57
		     
xesc	  endp


; SOFTINT
;   Handler for the "software interrupt"
;
; Use:
;   call SOFTINT    7,op,intnum,return-type,ax,bx,cx,dx
;   where all arguments are pcs registers
;
; On exit:
;   The first register will contain the returned value
;
; Description:
;   All args are interrogated to determine the length of a buffer
;   required to hold the args. A buffer is allocated in real mode
;   (via function E8), the args are then copied into the buffer,
;   and the software interrupt is issued. Upon return, the return
;   value is processed, the buffer is deallocated, and the first
;   register is set with the return value.

;
; This following data will be allocated locally within SWINT
;
local_save struc
; Following is the machine state block for Issue Real Interrupt request
msb_ax	   dw ?			; ax register for interrupt
msb_bx	   dw ?			; bx register for interrupt
msb_cx	   dw ?			; cx register for interrupt
msb_dx	   dw ?			; dx register for interrupt
msb_si	   dw ?			; si register for interrupt
msb_di	   dw ?			; di register for interrupt
msb_flags  dw ?			; flags register for interrupt
msb_ds	   dw ?			; ds register for interrupt
msb_es	   dw ?			; es register for interrupt
; The following local data contains ptrs into the real segment
selector   dw ?			; selector for real segment
buf_ptr	   dw ?			; local pointer into real segment
msb_ptr	   dw ?			; local pointer into msb
stop	   dw ?			; temp data
work_spc   dd ?			; temp working storage
; Following definitions define the stack upon call
caller_bp  dw ?			; callers bp
farret	   dd ?			; far return address
dummy	   dw ?			; %esc first arg = # operands
arg4       dw ?			; arg4 = dx
arg3       dw ?			; arg4 = cx
arg2       dw ?			; arg4 = bx
arg1       dw ?			; arg4 = ax
ret_type   dw ?			; return type
intnum     dw ?			; interrupt number
op	   dw ?			; op-code
local_save ends

softint	  proc	  far
       	  push	  bp 				;save callers bp
	  sub	  sp,caller_bp			;allocate local storage
	  mov	  bp,sp				;and update BP

	  and	  xesc_func,0			;note sw-int 
	  lea	  bx,swi_err_exit		;error handler for sw-int
	  mov	  error_return,bx

; Sum up the space required to hold all the arguments

	  mov	  si,bp
	  add	  si,arg4-2			;SI => args
	  mov	  [bp].stop,si			;save for later
	  mov	  di,bp
	  add	  di,msb_dx			;DI => regs in msb
	  mov	  cx,4				;CX = number of args
	  xor	  dx,dx				;DX = space required
sum_spc:
	  push	  di				;temp save di
	  add	  si,2				;address arg
	  mov	  bx,[si]			;get vm reg
	  mov	  di,[bx].C_page		;get its page#
	  cmp	  ptype[di],STRTYPE*2		;Is it a string?
	  jne	  sum_010			; no,  jump
	  %LoadPage es,di			; yes, 
	  mov	  di,[bx].C_disp		;    es:di => string
	  inc	  di				;    skip tag	
	  mov	  ax,es:[di]        		;    get string object length
	  cmp	  ax,0				;    is it positive?
	  jge	  sum_005			;     yes, jump; normal string
	  add	  ax,str_ovhd*2 		;     no,  short string
sum_005:  sub	  ax,str_ovhd			;          subtract overhead
	  inc	  ax  				;          add 1 for null terminator
	  jmp	  short sum_020
sum_010:
	  mov	  ax,4				;non-string at least 4 bytes
	  cmp	  ptype[di],FLOTYPE*2		;floating point object?
	  jne	  sum_020			; no,  jump
	  add	  ax,4				; yes, floats are 8 bytes
sum_020:  
	  pop	  di				;msb register ptr
	  mov	  ds:[di],ax			; save length of object
	  sub	  di,2				; next msb register ptr
	  add	  dx,ax				;sum space required	
	  loop	  sum_spc			;and loop

; CX:DX = space required to buffer the args, SI => arg 1 at this point

	  mov	  ax,ALLOC_REAL			;Create real segment
	  int	  DOS				;Extended Dos Function request
	  jnc	  swi_07
	  mov	  bx,offset al_seg
	  mov	  cx,offset swi_txt
	  jmp	  fatal_pro_err			;control will not return here
swi_07:
	  mov	  [bp].selector,ax		;save segment selector
	  mov	  es,ax				;es = real buffer selector
	  mov	  [bp].msb_ds,bx		;save para address in msb
	  mov	  [bp].msb_es,bx		;save para address in msb
	  mov	  [bp].buf_ptr,0		;pointer within real segment
	  mov	  [bp].msb_ptr,bp		;pointer into msb regs

; Move each arg into the buffer, SI => arg1 at this point
;
swi_020:
	  cmp	  si,[bp].stop			;all args processed?
	  je	  swi_025			;  yes, jump

	  std
	  lods	  word ptr [si]			;pick up arg
	  mov	  bx,ax				;save in BX

	  mov	  di,[bp].msb_ptr		;di = ptr to reg in msb
	  add	  [bp].msb_ptr,2		;      set for next time
	  mov	  cx,ds:[di]			;cx = length of object
	  mov	  ax,[bp].buf_ptr		;ax = ptr into buffer
	  add	  [bp].buf_ptr,cx		;      set for next time
	  mov	  ds:[di],ax			;update msb reg with buf ptr
	  mov	  di,ax				;es:di => buffer

; Dispatch on argument type
	  push	  si				;tempsave arg ptr
	  mov	  si,[bx].C_page		;get page#
	  mov	  si,ptype[si]			;  and type
; BX=page #, CX=length, ES:DI=>buffer
	  call	  cs:word ptr do_arg[si]	;Handle each object.
	  pop	  si				;restore arg ptr
	  jmp	  swi_020 

; At this time all args are in the buffer, Issue the sofware interrupt

swi_025:
	  cld
	  mov	  bx,[bp].intnum		;get reg holding int
	  mov	  ax,[bx].C_disp		;AL = interrupt number
	  mov	  dx,bp				;DS:DX => machine state block 
	  mov	  bx,msb_es+2			;# bytes which may change
	  mov	  ah,REAL_INTRP			;AH = Issue Real Interrupt
	  int	  DOS				;Extended Dos Function Request
	  jnc	  swi_27
	  mov	  bx,offset rl_int
	  mov	  cx,offset swi_txt
	  jmp	  fatal_pro_err			;control will not return here
swi_27:
; We're back from software interrupt, lets get return value

	  mov	  bx,[bp].ret_type		;get vm reg
	  mov	  di,[bx].C_disp
	  shl	  di,1				;make index into valu table
	  cmp	  di,N_RV*2			;return value out of range?
	  jb 	  swi_070
	          				;bx = reg holding return type 
	  mov	  ax,SWI_ERR_VALUE_BAD_TYPE	;ax = error indicator
	  jmp	  swi_err_exit
swi_070:
; now go convert the return values
	  mov	  si,bp				;ds:si => address of ret value
	  mov	  bx,[bp].op			;bx = return register
	  call	  cs:word ptr do_val[di]	;handle one type of return value
	  mov	  ax,0				;AX=0 says no errors
; ax= error indicator (if nonzero, bx = irritant)
swi_err_exit label near
	  push	  ax				 ;push error number
	  push	  bx				 ;push irritant
	  mov	  es,[bp].selector
	  mov	  ax,DELETE_SEG			 ;Delete Real Segment
	  int	  DOS				 ;Extended Dos Function
	  jnc	  swi_077
	  mov	  bx,offset dl_seg
	  mov	  cx,offset swi_txt
	  jmp	  fatal_pro_err		;control will not return here
swi_077:
	  pop	  cx				 ;cx = irritant
	  pop	  ax				 ;ax = error indication
	  mov	  bx,ax				 ; move to bx
	  dec	  bx				 ; form index
	  js	  swi_ret			 ;negative - no error
	  shl	  bx,1				 ;form index
	  mov	  bx,swi_errs[bx]		 ;bx => error message
	  mov	  ax,1				 ;note non-restartable
; ax= error indicator (if nonzero bx=message address, cx = irritant)
swi_ret:
	  mov	  sp,bp
	  add	  sp,caller_bp
	  pop	  bp
	  ret
softint	  endp


	  subttl  Code segment:  Copy arguments to xfer buffer
	  page


;; Jump tables to handle arguments to the %xesc call
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

; On entry to all the argument handler routines:
;   ES:DI = pointer to real mode buffer to store data
;   BX = address of VM reg with page:offset of Scheme object
;   SI = Type of operand code
;
; On exit:
;   CX = number of bytes moved to the buffer pointed to by ES:DI

;
; Process list argument
;
do_lstarg label   near				;looking for false only
	  cmp	  [bx].C_page,NIL_PAGE*2
	  je	  do_lst01
	  jmp	  do_errarg
do_lst01:
	  xor	  ax,ax
	  jmp	  do_log

;
; Process fixnum argument
;
do_fixarg label   near
	  mov	  ax,[bx].C_disp		;get the fixnum data
	  shl	  ax,1				;deal with sign bit
	  sar	  ax,1				;ax = 16-bit signed int
; True and false are treated as the numbers 1 and 0, respectively.
; Boolean-argument processing merges into integer processing at this point.
do_log:   cwd					;dx:ax is 32-bit signed int
	  MOVE_ARGS_TO_BUF <dx,ax>,,autoincr,save
	  ret					;and return
;
; Process float argument
;
do_floarg label	  near
	  push	  ds				;preserve data seg

	  mov	  si,[bx].C_page		;get float's page #
	  mov	  ax,[bx].C_disp		; and offset
	  %LoadPage ds,si
	  mov	  si,ax				;ds:si => float
	  inc	  si				;bump past header
	  mov	  cx,8				;cx = length of float
	  MOVE_TO_REAL_BUF autoincr,save	;move float to buffer
	  pop	  ds				;restore data seg
	  ret					;and return

;
; Process bignum argument
;
do_bigarg label	  near
; Stage the conversion to longint on the stack
	  sub	  sp,4				;allocate stack space for long
	  mov	  ax,sp				;note its address
; ok to add to stack here because we've reserved space above.
	  push	  es				;save regs around call
	  push	  di
	  push	  bp 
	  mov	  bp,sp
	  push	  bx				;push VM reg@
	  push	  ax                   		;push buffer@
	  mov	  C_fn,offset pgroup:int2long   ;convert bignum to long
	  call	  far ptr far_C
	  pop	  bx				;dump buffer@
	  pop	  bx				;restore VM reg@
	  pop	  bp				;restore bp
	  pop	  di				;        di
	  pop	  es				;	 es
; above cleans stack up from calling C routine
	  cmp	  ax,0				;did bignum convert OK?
	  je	  do_big5			;yes, jump
; there was an error in converting the number
	  mov	  ax,XLI_ERR_BIG_TO_32_BITS	;ax = error # (default xli)
	  cmp	  xesc_func,0			;performing xli function?
	  jne	  do_bigerr			; yes, jump
	  mov	  ax,SWI_ERR_BIG_TO_32_BITS	;ax = error # (for sw-int)
; ax=error number, bx=irritant
do_bigerr:
	  jmp	  error_return
do_big5:
	  mov	  si,sp				;ds:si => long int
	  mov	  cx,8				;cx = length
	  MOVE_TO_REAL_BUF autoincr,save	;move float to buffer
	  add	  sp,4				;clean up stack
	  ret					;and return

;
; Process symbol argument
;
do_symarg label	  near				;looking for true only
	  cmp	  [bx].C_page,T_PAGE*2
	  jne	  do_errarg
	  cmp	  [bx].C_disp,T_DISP
	  jne	  do_errarg
	  mov	  ax,1
	  jmp	  do_log

;
; Process string arguments
;
do_strarg label	  near
	  or	  xesc_func,0			;doing xesc?
	  jz	  swi_strarg			; no,  jump
	  MOVE_ARGS_TO_BUF <-1>,,autoincr,save	; yes, indicate string
	  ret
swi_strarg:					;move string to swint buffer
	  push	  ds				;preserve regs
	  push	  si
	  mov	  ax,[bx].C_disp		;get offset
	  mov	  si,[bx].C_page		;get page #
	  %LoadPage ds,si
	  mov	  si,ax				;ds:si => string
	  inc	  si				;skip tag
	  cld
	  lods	  word ptr [si]			;get length
	  or 	  ax,ax				;is it positive?
	  jge	  swi_str05			;yes, jump; normal string
	  add	  ax,str_ovhd*2 		;no,  short string
swi_str05:
	  sub	  ax,str_ovhd			;subtract overhead
	  mov	  cx,ax				;CX = length of string
	  MOVE_TO_REAL_BUF autoincr    		;move string across
	  mov	  ax,ss
	  mov	  ds,ax
	  push	  cx				;save # bytes just written	  
	  MOVE_BYTE_TO_BUF 0,,autoincr		;write out null terminator
	  pop	  cx
	  inc	  cx				;cx = total # bytes written
	  pop	  si				;restore preserved regs
	  pop	  ds
	  ret

do_errarg label   near
	  mov	  ax,XLI_ERR_ARGN_BAD_TYPE      ;ax = error # (default xli)
	  cmp	  xesc_func,0			;performing xli function?
	  jne	  do_errerr			; yes, jump
	  mov	  ax,SWI_ERR_ARGN_BAD_TYPE	;ax = error # (for sw-int)
; ax = error number, bx=irritant
do_errerr:
	  jmp	  error_return


	  subttl  Code segment:  Copy return value back into Scheme
	  page

;; Jump tables to handle values returned from the real routine
; indexed by value type (SW-INT return types)
do_val	  dw	  do_intval			 ;0=integer
	  dw	  do_TFval			 ;1=true/false
	  dw	  do_strval			 ;2=string
	  dw	  do_floval			 ;3=flonum


; On entry to all the value handler routines:
;	  BX    = result register address
;	  DS:SI = pointer to return value

;
; Process integer return value
;
do_intval proc	  near
do_int10: 
	  push	  bp
          mov	  bp,sp 			;get BP set for C call
	  or	  xesc_func,0			;doing xesc?
	  jnz	  doint_05			; yes, jump
	  push	  [si]				;si=> msb_ax on stack. remember
	  push	  [si]+2	  		;lattice's return conventions
	  jmp	  doint_07
doint_05: push	  [si]+2			;push longint
	  push	  [si]
doint_07: push	  bx				;push vm reg address
	  mov	  C_fn,offset pgroup:long2int	;allocate integer
	  call	  far ptr far_C 		;C longint -> PCS integer
						;(bignum or fixnum)
	  mov	  sp,bp 			;pop C args
	  pop	  bp				;restore callers bp
	  ret					; and return
do_intval endp

;
; Process true/false return value
;
do_TFval  proc	  near
	  mov	  cx,0
	  or	  xesc_func,0			 ;doing xesc?
	  jnz	  dotf_05			 ; yes, jump
	  mov	  ax,[si]+2			 ;si=> msb_ax on stack. remember
	  jmp	  dotf_07			 ;lattice's return convention
dotf_05:  mov	  ax,[si]			 ;get value
dotf_07:  or	  ax,ax				 ;zero?
	  jz	  do_TF10			 ; yes (false object)
	  mov	  ax,T_DISP			 ; no  (true object)
	  mov	  cx,T_PAGE*2
do_TF10:
	  mov	  [bx].C_disp,ax
	  mov	  [bx].C_page,cx
	  ret
do_TFval  endp

;
; Process float return value
;
do_floval proc	  near
	  push	  bp
	  mov	  bp,sp
	  or	  xesc_func,0			;doing xesc?
	  jnz	  doflo_05			; yes, jump
	  push	  [si]				;si=> msb_ax on stack. remember
	  push	  [si]+2	  		;lattice's return conventions
	  push	  [si]+4	  		;and push args appropriately.
	  push	  [si]+6
	  jmp	  doflo_07
doflo_05: push	  [si]+6			;push float values
	  push	  [si]+4
	  push	  [si]+2
	  push	  [si]
doflo_07: push	  bx				;push vm return reg
	  mov	  C_fn,offset pgroup:alloc_fl	;allocate float
	  call	  far ptr far_C 		;C double -> PCS flonum
	  mov	  sp,bp 			;pop args from stack
	  pop	  bp
	  ret
do_floval endp

;
; Process string return values
;
do_strval proc	  near
	  or	  xesc_func,0			;doing xesc?
	  jz	  swi_strval			; no,  jump
;
; Do it for xli
;
	  push	  bp
	  mov	  bp,sp

	  mov	  cx,[si]           		;get string length
	  cmp	  cx,16380			;string length short enough?
	  jbe	  do_stv15			;yes, jump
	  mov	  cx,16380 			;no, truncate at max
do_stv15: 
; allocate the space for the return value string object
	  push	  cx				;save length for later
	  push	  si				;     pointer to buffer
	  push	  bx				;     return value VM reg
	  push	  bp
	  mov	  bp,sp 			;get BP set for C call
	  push	  cx				;push length
	  push	  STRTYPE			;push type
	  push	  bx				;push return value VM reg @
	  mov	  C_fn,offset pgroup:alloc_bl   ;allocate block
	  call	  far ptr far_C 		;go do it
	  mov	  sp,bp 			;pop C args
	  pop	  bp
	  pop	  bx				;return VM reg
	  mov	  di,[bx].C_disp
	  mov	  bx,[bx].C_page
	  %LoadPage es,bx
	  add	  di,3				;es:si => destination
	  pop	  si
	  add	  si,2				;ds:si => real mode address
	  pop	  dx				;dx = length
	  call	  real2pro			;xfer from real mem to pro mem
	  mov	  sp,bp				;clean up stack
	  pop	  bp				;restore caller's bp
	  ret					;and return
;
; Do it for software interrupt
;
swi_strval:
	
	  push	  ds				;tempsave ds
	  mov	  si,[bp].msb_ax
	  mov	  ds,[bp].selector		;DS:SI points to string

	  push	  ss
	  pop	  es
	  mov	  di,bp	
	  add	  di,work_spc			;ES:DI => destination

	  mov	  ax,BLOCK_XFER			;grab one byte and test zero
	  mov	  cx,1
	  mov	  dx,0FFFFh
swi_str01:
	  inc	  dx				;# bytes read
	  int	  DOS				;xfer 1 byte
	  inc	  si				;next byte to read
	  cmp	  byte ptr es:[di],0		;is it zero?
	  jne	  swi_str01			;no, get next char
swi_stv15:
	  pop	  ds				;restore ds
	  push	  dx				;save length for later
;
; allocate the space for the return value string object
;
	  mov	  ax,[bp].op			 ;get return vm reg

	  push	  bp				 ;tempsave around call
	  mov	  bp,sp 			 ;get BP set for C call
	  push	  dx				 ;push length
	  push	  STRTYPE
	  push	  ax				 ;push vm reg
	  mov	  C_fn,offset pgroup:alloc_bl
	  call	  far ptr far_C 		 ;allocate string object;
						 ;"alloc_block" takes care
						 ;of overhead matters
	  mov	  sp,bp 			 ;pop C args
	  pop	  bp

	  mov	  bx,[bp].op    		 ;return value VM reg
	  mov	  di,[bx].C_disp
	  mov	  bx,[bx].C_page
	  %LoadPage es,bx			 ;ES:DI is dest object @
	  add	  di,3				 ;skip past string's overhead

	  mov	  si,[bp].msb_ax		 
	  mov	  ds,[bp].selector		 ;DS:SI is string in buffer
	  pop	  cx				 ;CX = length
	  mov	  ax,BLOCK_XFER			 ;copy into scheme heap
	  int	  DOS				 ;Extended Dos function call

	  mov	  ax,ss
	  mov	  ds,ax
	  ret
do_strval endp


do_errval proc	  near
	  mov	  ax,XLI_ERR_VALUE_BAD_TYPE
	  jmp	  error_return
do_errval endp


	  public  pro2real,real2pro,map_real_mem
; REAL2PRO
;  
;  On entry:
;   DS:SI => address of real mode buffer
;   ES:DI => scheme heap
;   DX    =  length
;
;  On exit:
;   CX is number of chars xfered

real2pro  proc	  near
	  push	  ds			; save data segment
	  call	  map_real_mem		; create real window (selector in ax)
; Error Checks here
	  mov	  cx,dx			; cx = length
; WARNING: DS addresses real memory below
	  mov	  ds,ax			; real mode selector
	  xor	  si,si			; ds:si = source (real data)
	  mov	  ax,BLOCK_XFER		; do block xfer
	  int	  DOS
	  mov	  ax,ds
	  mov	  es,ax			; es = mapped selector
	  mov	  ax,DELETE_SEG		; Delete Segment
	  int	  DOS
	  jnc	  r2p_next
	  xor	  bx,bx
	  mov	  bl,ss:xesc_func
	  shl	  bx,1
	  mov	  cx,ss:which_func[bx]
	  mov	  bx,offset dl_seg
	  jmp	  fatal_pro_err		;control will not return here
r2p_next:
; WARNING:  DS does not address scheme's data segment above
	  pop	  ds			; restore data segment
	  ret
real2pro  endp


; PRO2REAL
;  Copy data from protected mode memory to real mode memory. If ax is
;  non-zero, then it already contains a real selector where we can move
;  the data - in this case we don't create a real window and delete the
;  segment selector after the copy. 
;
;  On entry:
;   if AX = 0
;     then DX =     length
;	   DS:SI => address of real mode buffer
;	   ES:DI => scheme heap
;     else
;	   AX =	    selector to real mode buffer
;          DX =     length
;	   ES:DI => scheme heap
;
;  On exit:
;   CX is number of chars xfered

pro2real  proc	  near
	  push	  ds			; callers data segment
	  push	  ax			; indicator
	  push	  di			; offset to data
	  or	  ax,ax			; do we have a selector already?
	  jnz	  p2r_010		; yes, don't create real window (jump)
	  call	  map_real_mem		; no,  create real window 
				        ;      selector returned in ax
; Error Checks here
p2r_010:
	  mov	  cx,dx			; cx = length
; WARNING: DS addresses scheme heap below
	  mov	  bx,es
	  mov	  ds,bx
	  pop	  si			; ds:si = source      (in scheme heap)

	  mov	  es,ax			; real mode selector
	  xor	  di,di			; es:di = destination (in real mode)
mode_xfer:
	  mov 	  dx,ax			; tempsave selector
	  mov	  ax,BLOCK_XFER		; do block xfer
	  int	  DOS
	  pop	  ax   			; restore indicator
	  or	  ax,ax			; was a selector passed in?
	  jnz	  mode_xf01		;  yes,  then don't delete it
	  mov	  es,dx			; es = mapped selector
	  mov	  ax,DELETE_SEG		; Delete Segment
	  int	  DOS
	  jnc	  mode_next
	  xor	  bx,bx
	  mov	  bl,ss:xesc_func
	  shl	  bx,1
	  mov	  cx,ss:which_func[bx]
	  mov	  bx,offset dl_seg
	  jmp	  fatal_pro_err		;control will not return here
mode_next:
; WARNING:  DS does not address scheme's data segment above
mode_xf01:
	  mov	  ax,ds
	  mov	  es,ax			; restore ptr to scheme heap
	  pop	  ds			; restore data segment
	  ret
pro2real  endp

; MAP_REAL_MEM
;   Map a real memory address into a selector for use in protected memory.
;
;   DS:SI => address of real mode buffer
;   DX    =  length
;
;  On exit:
;   Carry flag set on error
;   AX = selector for real memory or error if carry flag set
;
;  Regs used: ax,bx,cx,si - all destroyed

map_real_mem proc near
					; create real mode window
	  xor	  ax,ax
	  mov	  cx,4			; shift count
	  mov	  bx,[si]+2      	; bx = real segment address
	  mov	  al,bh			; create 32 bit address in SI:BX
	  shr	  ax,cl
	  shl	  bx,cl			; shift for physical mem calc
	  add	  bx,[si]  		; add effective memory address
	  jnc	  mr_25
	  inc	  ax			; SI:BX = real memory address
mr_25:
	  mov	  si,ax			; si:bx = real memory address
	  xor	  cx,cx			; CX:DX = length
	  mov	  ax,CREATE_WIN		; Create Window function request
	  int	  DOS			; Return selector in AX
	  jnc	  mr_ret
	  xor	  bx,bx
	  mov	  bl,ss:xesc_func
	  shl	  bx,1
	  mov	  cx,ss:which_func[bx]
	  mov	  bx,offset cr_win
	  jmp	  fatal_pro_err		;control will not return here
mr_ret:
	  ret
map_real_mem endp

progx	  ends



	  subttl  Prog segment code definitions
	  page

prog	  segment byte public 'PROG'
	  assume  cs:pgroup
	  extrn   next_SP:near,src_err:near
   	  extrn	  fix_intr:near
	  public  pcinit,set_crtint,reset_crtint,xli_ldall,xli_term,xli_xesc

; PC_INIT
;  Perform initializations, some of which are PC specific.
;
pcinit	  proc	  near
	  call	bid_rpc			;bid the real mode code
	  cmp	pcs_sysd,0		;have we found the system directory?
	  jz	pcini_00		; no,  skip loading of xli
	  call	xli_ldall		; yes, load xli stuff
pcini_00:
	  call	pc_init			;get specific pc info
	  call	fix_intr		;take over interrupts
	  ret				;return to caller
pcinit	  endp


; The following routines are gateways to routines in the progx segment
; for real procedure calls (RPC) and external language interface (XLI).
; Note that the progx routines are jumped to via the FAR_RPC table, however 
; they return to the caller of this routine because we fix up the stack.
; 
bid_rpc   proc	  near
	  mov	  bx,frpc_bid  		;initialize real procedure
	  jmp	  rpc_call
pc_init:
	  mov	  bx,frpc_init 		;get machine type
	  jmp	  rpc_call
set_crtint:
	  mov	  bx,frpc_setcrt	;set crt interrupt
	  jmp	  rpc_call
reset_crtint:
	  mov	  bx,frpc_resetcrt	;reset crt interrupt
	  jmp	  rpc_call
xli_ldall:
	  mov	  bx,frpc_ldall  	;load xli files
	  jmp	  rpc_call
xli_term:
	  mov	  bx,frpc_unld    	;unload xli files
	  jmp	  rpc_call
xli_xesc: 
	  mov	  bx,frpc_xesc 		;perform xesc 
	  jmp	  rpc_call
rpc_call:
	  pop	dx			;pop return address
	  push	prog			;push segment return
	  push	dx			;then offset
	  jmp	dword ptr FAR_RPC+[bx]	;jump to progx routine
	  ret				;we'll never return here
bid_rpc   endp

; Far linkage *from* XLI
; (all the memory allocation routines are written in C).
; The caller of this should have set BP from SP before pushing the C args,
; then restore SP from BP afterwards to remove them from the stack.
; We don't preserve ES across the call.
     	  public  far_C
far_C	  proc	  far
	  push	  ds			;C likes ES=DS
	  pop	  es
	  pop	  C_retadr	        ;get far @ off stack so C sees its args
	  pop	  C_retadr+2
	  call	  [C_fn]
	  push	  C_retadr+2
	  push	  C_retadr
	  ret				;C returns with return value in AX..DX
far_C	  endp

prog	  ends

	  end
