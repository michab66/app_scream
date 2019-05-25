;                                                       =====> INTRUP.ASM
;***************************************
;*   TIPC Scheme '84 Runtime Support   *
;*      Special Keyboard Handlers      *
;*                                     *
;*  (C) Copyright 1984,1985 by Texas   *
;*     Instruments Incorporated.       *
;*        All rights reserved.         *
;*                                     *
;* Date Written:  March 1985           *
;* Last Modification:		       *	
;*    16 Mar 87 - tc		       *
;*	Changed int24 fatal error int  *
;*      handler to pass extended error *
;*	code back to originator.       *		
;***************************************
;
; A not-so-ingenius patch to keep PC Scheme from prematurely
; exitting to MS-DOS.   CTRL-C now just echos ^C<CR><LF> trio
; and this should disappear once the READER is re-written.
;
          page    66,132
          include dos.mac
          include pcmake.equ

DOS       equ     21h
SHIFT     equ     04h              ; SHIFT in mode keys
META      equ     02h              ; ALT   "   "    "
CNTRL     equ     01h              ; CTRL  "   "    "
C_KEY     equ     54h              ; Scan code for 'C' key (84 decimal)
BROKEY    equ     64h              ; Scan code for 'PAUS/BRK' key (100 decimal)

ERR_INT   equ     24h              ; Fatal error abort address
EXT_ERR	  equ	  59h		   ; Get Extended Error Code
TI_KMI    equ     5Bh              ; TIPC Keyboard Mapping Interrupt
IBM_PBI   equ     1Bh              ; IBM Program Break Interrupt
BIOS_BRK  equ     0071h            ;   If CTRL-BREAK is pressed on IBM then
BRK_BIT   equ     80h              ;   this bit is set at BIOS_BRK in BIOS data

CARY_FLG  equ	  01h		   ; Carry flag

          DSEG
          extrn   PC_MAKE:word     ; =1 for TIPC, > 0F0h for IBM-PC, =0 for ???
get_vec   dw      3500h+TI_KMI
set_vec   dw      2500h+TI_KMI
          ENDDS

PGROUP    GROUP   PROG
PROG      SEGMENT BYTE PUBLIC 'PROG'
          ASSUME  CS:PGROUP
          extrn   shft%brk:far
PROG      ends

XGROUP    GROUP   PROGX
PROGX     SEGMENT BYTE PUBLIC 'PROGX'
          ASSUME  CS:XGROUP,DS:DGROUP

                                   ; Sorry guys, but this has gotta be in CS:
kbmi_off  dw      ?                ; Keyboard Mapping Interrupt (offset)
kbmi_seg  dw      ?                ; Keyboard Mapping Interrupt (segment)
ferr_off  dw      ?                ; Fatal Error Interrupt (offset)
ferr_seg  dw      ?                ; Fatal Error Interrupt (segment)
;******************
TI_BRK    proc    far                  ; BREAK pressed by (ab)user
          cmp     AL,BROKEY            ; PAUS/BRK key pressed?
          jne     TI_020
          test    AH,SHIFT             ; SHIFT pressed with PAUS/BRK?
          jz      TI_020         ;   if no then SHIFT-BRK not possible
          test    AH,META+CNTRL        ; CTRL or ALT pressed with PAUS/BRK?
          jnz     TI_020         ;   if yes then ALT or CTRL has priority
;         jmp     short TI_010

IBM_BRK   label   far              ; Entry point for IBM's Keyboard Break Int.
TI_010    label   near
          push    AX               ; Save AX across call
          call    PGROUP:shft%brk  ; Flag to force debugger on next VM instruct
          pop     AX               ; Restore AX
          mov     AL,0FFh          ; Ignore this keystroke (IBM'll ignore this)

TI_020    label   near             ; Jump here & return like nothing happened
          stc                      ; Tell TI keyboard DSR that no key was pressd
                                   ; again, IBM BIOS won't care about this.
          jmp     dword ptr CS:kbmi_off ; Go off and perform task that
                                   ; may have had control of Int 5Bh before
                                   ; we did (e.g. RDClock, etc.).
TI_BRK    endp

;******************
CTLC_INT  proc  far                ; Handle detection of CTRL-C (INT 23H)
          iret                     ;   Just return like nothing happened 'cept
                                   ;   that a ^C<CR><LF> trio is displayed.
CTLC_INT  endp

;*******************
	  public  FAT_ERR
FAT_ERR   proc    far              ; Handle for fatal error interrupt (24H)

	  ; remove ip,cs, and flags of system regs from int 24h
          pop     AX
          pop     AX
          pop     AX
	
	  ; get extended error codes
	  xor	  BX,BX
	  mov	  AH,EXT_ERR
	  int	  DOS		    ; Extended Error Code returned in AX

          ; restore user registers at time of original function request 21h			   
          pop     BX		    ; Ignore old AX
          pop     BX
          pop     CX
          pop     DX
          pop     SI
          pop     DI
          pop     BP
          pop     DS
          pop     ES
	
	  ; Set the carry bit in the caller's flags and return
	  ; The original dos requestor should see that carry is set and
	  ; that ax contains the error code

	  or 	  byte ptr [BP-02], CARY_FLG 
          iret
FAT_ERR   endp
;******************
fix%intr  proc    far              ; Re-assign Keyboard Mapping Interrupt (5BH)
          push    ES               ; and "fix" DOS's CTRL-C Exit Interrupt (23H)
          push    DX
          push    BX
          push    AX
          cmp     PC_MAKE,TIPC     ; We running on a TIPC or (yuck) IBM?
          je      short fix_010    ; Jump as already setup for TIPC
          mov     al,IBM_PBI
          mov     byte ptr set_vec,al  ; LSB of word in first byte
          mov     byte ptr get_vec,al
fix_010   label   near             ; NO CHANGES if you jumped to here
          mov     AX,get_vec       ; get the interrupt vector
          int     DOS
;
          mov     word ptr CS:kbmi_seg,ES  ; save it
          mov     word ptr CS:kbmi_off,BX
;
          mov     AX,set_vec           ; Load AX with DOS func # and INT #
          mov     DX,offset TI_BRK     ;   for replacing vector with my own
          cmp     PC_MAKE,TIPC
          je      short fix_020        ; Jump if we're running on a TIPC
          mov     DX,offset IBM_BRK    ; Use different entry point for IBM

fix_020   label   near
          push    DS
          mov     CX,CS                ; Do this now as I needed the DS
          mov     DS,CX                ; register back at "cmp PC_MAKE,0"
          int     DOS
;
          mov     DX,offset CTLC_INT   ; CTRL-C Handler Interrupt (23H)
          mov     AX,2523h             ; This one doesn't need to be restored
          int     DOS                  ; and is the same for ALL MS-DOS machines
;**************************************************
;*   Install the handler for fatal error interrupt
;**************************************************
          pop     DS
          mov     al,ERR_INT
          mov     AH,35H               ; get the original entry
          int     DOS

          mov     word ptr CS:ferr_seg,ES  ; save it
          mov     word ptr CS:ferr_off,BX
          mov     AH,25H               ; set the new entry point
          mov     AL,ERR_INT
          mov     DX,offset FAT_ERR    ; new address of handler
          push    DS
          mov     CX,CS
          mov     DS,CX
          int     DOS

          pop     DS
          pop     AX
          pop     BX
          pop     DX
          pop     ES
;
          ret                      ; Get the heck outta here
fix%intr  endp

;******************
unfix%    proc    far              ; Restore Keyboard Mapping Interrupt (5BH)
                                   ; (DOS should take care of 23H)
          push    DS
          push    DX
;
          mov     AX,set_vec
          lds     DX,dword ptr CS:kbmi_off ; get old interrupt vector
          int     DOS
                                   ; Restore fatal error interrupt (24H)
          mov     AH,25H
          mov     AL,ERR_INT
          lds     DX,dword ptr CS:ferr_off
          int     DOS
;
          pop     DX
          pop     DS
;
          ret                      ; Get the heck outta here
unfix%    endp
PROGX     ends

;**********************************************************************
;*            Link routines                                           *
;**********************************************************************
PROG      SEGMENT BYTE PUBLIC 'PROG'
          ASSUME  CS:PGROUP
          Public  fix_intr, unfixint

fix_intr  proc    near
          call    fix%intr
          ret
fix_intr  endp

unfixint  proc    near
          call    unfix%
          ret
unfixint  endp
prog      ends
          end

; **NOTE**
;         Let it be known to the world that this programmer
;         believes that IBM stands for Immense Bowel Movement!!!
;         Or possibly a law firm named Idiots, Bumblers, & Morons.

          end
