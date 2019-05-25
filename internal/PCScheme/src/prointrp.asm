;							=====> PROINTRP.ASM
;***************************************
;*   TIPC Scheme '84 Runtime Support   *
;*	Special Keyboard Handlers      *
;*				       *
;*  (C) Copyright 1984,1985 by Texas   *
;*     Instruments Incorporated.       *
;*	  All rights reserved.	       *
;*				       *
;* Date Written:  Feb 1988  	       *
;* 				       *
;* This file is basically INTRUP.ASM   *
;* modified to run in protected mode   *
;* under AI Architects OSx86.	       *
;* 				       *
;***************************************
	  .286c
	  page	  66,132
	  include dos.mac
	  include pcmake.equ
	  include smmu.mac	   ; Protected mode Macros

DOS	    equ	  021h		   ; Dos function request
EXT_ERR	    equ	  059h		   ; Dos get extended error
GET_VEC	    equ	  035h		   ; Dos get interrupt vector
SET_VEC	    equ	  025h		   ; Dos set interrupt vector
SET_AIA_VEC equ	  0E4h		   ; AIA set interrupt vector

TI_PBI	    equ	  05Dh		   ; TI  Program Break Interrupt
IBM_PBI     equ	  01Bh		   ; IBM Program Break Interrupt
ERR_INT	    equ	  024h		   ; Fatal error abort address
CTRLC_INT   equ	  023h		   ; Control C exit interrupt


	  DSEG
	  extrn   PC_MAKE:word
	  ENDDS

PGROUP	  GROUP   PROG
PROG	  SEGMENT BYTE PUBLIC 'PROG'
	  ASSUME  CS:PGROUP
	  extrn   shft%brk:far
PROG	  ends

XGROUP	  GROUP   PROGX
PROGX	  SEGMENT BYTE PUBLIC 'PROGX'
	  ASSUME  CS:XGROUP,DS:DGROUP

				   ; Sorry guys, but this has gotta be in CS:
kbmi_off  dw	  ?		   ; Keyboard Mapping Interrupt (offset)
kbmi_seg  dw	  ?		   ; Keyboard Mapping Interrupt (segment)
;******************

	  public  PROG_BRK
PROG_BRK  proc	  far		   ; Handler for Keynoard Break Interrupt
    	  push	  ax		   ; Save keystroke across call
	  call	  PGROUP:shft%brk  ; Flag to force debugger on next VM inst
    	  pop	  ax		   ; Restore keystroke
    	  mov	  ax,0FFh	   ; Ignore keystroke (IBM'll ignore this)
	  stc			   ; Tell TI keyboard DSR no key was pressd
				   ; again, IBM BIOS won't care about this.
          iret			   ; interrupt return
PROG_BRK  endp

;******************
          public CTLC_INT
CTLC_INT  proc	 far		   ; Handle detection of CTRL-C (INT 23H)
	  iret			   ;   Just return like nothing happened 'cept
				   ;   that a ^C<CR><LF> trio is displayed.
CTLC_INT  endp

;*******************
	  public  FAT_ERR
FAT_ERR   proc	  far		   ; Handle for fatal error interrupt (24H)
	  mov	  ax,di		   ;di = err number. add 19 to form err number
	  add	  ax,19		   ;you would get from Get Extended Error (59h)
	  iret	 		   ;just return for now
comment ~
;
; First release of AI Architect's OSx86 didn't support fatal error
; interrupts as specified by DOS. Also, couldn't issue Get Extended
; Error (Dos function 59h) from within here. Above code will have
; to suffice for now.
;
	  ; remove ip,cs, and flags of system regs from int 24h
	  pop	  AX
	  pop	  AX
	  pop	  AX

	  ; get extended error codes
	  xor	  BX,BX
	  mov	  AH,EXT_ERR
	  int	  DOS		    ; Extended Error Code returned in AX

	  ; restore user registers at time of original function request 21h
	  pop	  BX		    ; Ignore old AX
	  pop	  BX
	  pop	  CX
	  pop	  DX
	  pop	  SI
	  pop	  DI
	  pop	  BP
	  pop	  DS
	  pop	  ES

	  ; Set the carry bit in the caller's flags and return
	  ; The original dos requestor should see that carry is set and
	  ; that ax contains the error code

	  or	  byte ptr [BP-02], CARY_FLG
	  iret
~

FAT_ERR   endp

;*******************
; Reassign program break interrupt (5Dh=ti, 1Bh=ibm), and "fix" Dos's
; CTRL-C Exit int (23h)
	  public  fix%intr
fix%intr  proc	  far		   
	  push	  es			;tempsave off some regs
	  push	  dx
	  push	  bx
	  push	  ax
;
; WARNING: DS does not point to the local data segment below
;
	  mov	  ax,cs			 
	  mov	  ds,ax 	        ;set ds=cs for dos calls below
 
; take over program break interrupt
					;no need to get interrupt vector
					;since AIA will clean up on exit					
	  mov	  ah,SET_VEC		;ah = set interrupt vector
	  mov	  al,IBM_PBI		;al = ibm program break interrupt
	  cmp	  SS:PC_MAKE,TIPC	;if not running on a TIPC
	  jne	  short fix_010         ;  then jump
	  mov	  al,TI_PBI		;  else set TI program break interrupt
fix_010:
	  mov	  dx,offset PROG_BRK	;ds:ax => interrupt handler
	  int	  DOS		        ;take over the handler	

; take over ctl-c interrupt
	  mov	  ah,SET_VEC		;ah = set interrupt vector
	  mov	  al,CTRLC_INT		;al = CTRL-C Interrupt (23H)
	  mov	  dx,offset PROG_BRK	;ds:ax => interrupt handler
	  int	  DOS		        ;take over the handler
 
; take over fatal error interrupt
	  mov	  ah,SET_VEC		;ah = set interrupt vector
	  mov	  al,ERR_INT		;al = Fatal error interrupt
	  mov	  dx,offset FAT_ERR     ;ds:dx => interrupt handler
	  int	  DOS			;take over handler

	  mov	  ax,ss			;restore local data seg
	  mov	  ds,ax
;
; WARNING: DS does not point to the local data segment above
;
	  pop	  ax   			;restore saved regs
	  pop	  bx
	  pop	  dx
	  pop	  es
	  ret			        ;get the heck outta here
fix%intr  endp

;******************
; Restore Keyboard Mapping Interrupt (5BH)
; (DOS should take care of 23H)
	  public  unfix%
unfix%	  proc	  far		   

; 
; We don't do anything cuz AI Architects OSx86 will clean up upon exit.
; However, we'll leave this dummy routine here in case something pops
; up in the future
;
	  ret			   ; Get the heck outta here
unfix%	  endp
PROGX	  ends

;**********************************************************************
;*	      Link routines					      *
;**********************************************************************
PROG	  SEGMENT BYTE PUBLIC 'PROG'
	  ASSUME  CS:PGROUP
	  Public  fix_intr, unfixint

fix_intr  proc	  near
	  call	  fix%intr
	  ret
fix_intr  endp

unfixint  proc	  near
	  call	  unfix%
	  ret
unfixint  endp
prog	  ends
	  end

	  end
