;
;***************************************
;*     TIPC Scheme  Runtime Support    *
;*				       *
;*    (C) Copyright 1985 by Texas      *
;*     Instruments Incorporated.       *
;*	  All rights reserved.	       *
;*				       *
;* Date Written:  5 June 1985	       *
;* Last Modification:  15 May 1986     *
;***************************************
	  page	  60,132
	  .286c

	  include smmu.mac

MSDOS	   equ	  021h		   ; MS-DOS service call interrupt
BIDTASK    equ	  04Bh		   ; Load/Execute program

DGROUP	  group   data
data	  segment word public 'DATA'
	  assume  DS:DGROUP
	  extrn   _psp:dword	   ; program segment prefix paragraph address

cmd_      db	  "COMSPEC="
cmd_1     equ	  $
ENVPTR	  dw	  0		   ; DOS EXEC parameter block
CMDOFF	  dw	  0		   ; 		"
CMDSEG	  dw	  0		   ; 		"
FCB1OFF   dw	  5Ch		   ; 		"
FCB1SEG   dw	  0		   ; 		"
FCB2OFF   dw	  6Ch		   ; 		"
FCB2SEG   dw	  0		   ; 		"

INSTALLED dw	  0		   ; Whether crt interrupt is installed or not

data	  ends

XGROUP	  group   PROGX
PROGX	  segment byte public 'PROGX'
	  assume  CS:XGROUP

;************************************************************************
;*			     Bid another Task				*
;************************************************************************

;
; BP is set up by bid (the caller of this routine)
;
bid_args  struc
	  dw	  ?		   ; caller's BP
	  dw	  ?		   ; return address (caller of bid)
bid_file  dw	  ?		   ; program's file name
bid_parm  dw	  ?		   ; parameters
free_req  dw	  ?		   ; requested # of free paragraphs
display   dw	  ?		   ; Indicates if screen should be disturbed
bid_args  ends


bid_task  proc	  far
	  push	  ES

;     Set up parameter block
	  mov	  AX,[BP].bid_parm ; Set up dword pointer to command line
	  mov	  CMDOFF,AX
	  mov	  CMDSEG,DS

	  mov	  AX,word ptr _psp+2 ; Point to FCBs in program segment prefix
	  mov	  FCB1SEG,AX
	  mov	  FCB2SEG,AX

	  mov	  ES,AX
	  mov	  AX,ES:[02Ch]	   ; copy current environment ptr to
	  mov	  ENVPTR,AX	   ;  parameter area

;     Set ES:BX to address of parameter block
	  mov	  AX,DS
	  mov	  ES,AX
	  mov	  BX,offset ENVPTR

;     Set DS:DX to address of ASCIZ pathname (of file to be loaded)
	  push	  DS		   ; save DS segment register
	  mov	  DX,[BP].bid_file
	  mov	  DI,DX
	  cmp	  byte ptr [di],0    ; check if pt'ed to string is empty
	  jne	  bid_it

;     No filename-- bid up a new command interpreter;
;     have to search environment for COMSPEC= string
	  mov	  ES,ENVPTR	   ; ES:DI points to 1st string in environment
	  xor	  DI,DI

;     Test for end of environment
get_plop: cmp	  byte ptr ES:[DI],0 ; last entry in environment?
	  je	  cmd_err 	   ; if so, COMSPEC= not found
	  mov	  SI,offset cmd_   ; load address of comparison string
	  mov	  CX,cmd_1-cmd_    ;  and length of same
     repe cmps	  cmd_,ES:[DI]     ; does this entry begin "COMSPEC="?
	  je	  found 	   ; if so, found it! (jump)
	  xor	  AX,AX 	   ; clear AX for search
	  mov	  CX,-1 	   ; set CX for maximum length
    repne scas	  byte ptr ES:[DI] ; find \0 which terminates string
	  jmp	  get_plop	   ; loop

;     No command interpreter found
cmd_err:  mov	  AX,10		   ; treat as bad-environment error
	  stc
	  jmp	  short get_out

;     Found COMSPEC=
found:	  mov	  DX,DI		   ; DS:DX is ptr to command interpreter
	  push	  DS		   ; (swap DS and ES)
	  push	  ES
	  pop	  DS
	  pop	  ES

;     issue load task function call
bid_it:   
	  xor	  AL,AL 	   ; load and execute condition
	  mov	  AH,BIDTASK
	  ; load "load and execute" ftn id
	  int	  MSDOS 	   ; perform service call
get_out:  pop	  DS		   ; restore DS segment register
	  jc	  exit   	   ; branch if error in bidding task
	  xor	  AX,AX 	   ; indicate no error
exit:
	  pop	  ES		   ; restore ES segment register
	  ret			   ; return to caller
bid_task  endp


PROGX	  ends

PGROUP	  group   prog
prog	  segment byte public 'PROG'
	  assume  CS:PGROUP
	  extrn   unfixint:near,fix_intr:near
	  extrn   zcuron:near,zcuroff:near
	  extrn   set_crtint:near,reset_crtint:near	  


	  public  bid
bid	  proc	  near
	  push	  bp
	  mov	  bp,sp		   ;address local arguments

	  call	  unfixint	   ;reset shift-break vector
	  call	  zcuron	   ;turn the cursor back on
	  cmp	  [bp].display,0   ;can we disturb the screen? 
	  je	  no_install	   ; yes, jump
	  call	  set_crtint	   ; no,  take over crt interrupt
no_install:
	  call	  bid_task	   ;go bid the task
	  push	  AX		   ;save its error return code

	  cmp	  [bp].display,0   ;crt interrupt taken over
	  je	  no_uninstall	   ; no,  jump
	  call	  reset_crtint	   ; yes, reset the crt interrupt
no_uninstall:
	  call	  zcuroff	   ;turn the cursor back off
	  call	  fix_intr	   ;set shift-break vector
	  pop	  AX		   ;restore error code
	  pop	  bp		   ;dump args from stack
	  ret			   ;return to caller
bid	  endp
prog	  ends
	  end
