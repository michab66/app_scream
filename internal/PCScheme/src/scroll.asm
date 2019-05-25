;                                                       =====> SCROLL.ASM
;***************************************
;*   TIPC Scheme Runtime Support       *
;*      Window Support Routine         *
;*                                     *
;*  (C) Copyright 1985 by Texas        *
;*     Instruments Incorporated.       *
;*        All rights reserved.         *
;*                                     *
;* Date Written:  October 1985         *
;* Last Modification:                  *
;***************************************
          include pcmake.equ

TI_CRT    equ     049h
IBM_CRT   equ     010h

DGROUP    group   DATA
DATA      segment word public 'DATA'
          assume  DS:DGROUP
          extrn   PC_MAKE:word
DATA      ends

XGROUP    group   PROGX
PROGX     segment byte public 'PROGX'
          assume  CS:XGROUP,DS:DGROUP
          extrn   crt_dsr:far

;************************************************************************
;*                   Scroll Window Down one line                        *
;************************************************************************
s_args    struc
          dw      ?                ; caller's BP
          dd      ?                ; return address
          dw      ?
s_line    dw      ?                ; upper left hand corner line number
s_col     dw      ?                ; upper left hand corner column number
s_nline   dw      ?                ; number of lines
s_ncols   dw      ?                ; number of columns
s_attr    dw      ?                ; text attributes (used for blanking)
s_args    ends

scroll%d  proc    far
          push    BP               ; save caller's BP
          mov     BP,SP
;     scroll window's text down one line
          mov     CL,byte ptr [BP].s_nline ; load number of lines
          dec     CL               ; decrease number of lines by one
          jz      blank            ; Jump if scrolling 1-line and just blank it
          mov     CH,byte ptr [BP].s_ncols    ; load number of columns
          mov     DL,byte ptr [BP].s_line     ; load upper left line number
          mov     DH,byte ptr [BP].s_col      ; load upper left column number
          mov     AX,0701h         ; load "scroll text" code with no blanking
          cmp     DGROUP:PC_MAKE,TIPC
          je      ti_down

          push    AX               ; else
          mov     AH,0Fh
          int     IBM_CRT          ; Are we in graphics mode?
          cmp     AL,4             ;   If we are then fix blank fill attributes
          jl      text_m           ;   so that the bar characters don't show up
          cmp     AL,7
          je      text_m
          xor     BH,BH            ; zero attribute for fill blanks
          jmp     short wrte_atr
text_m:   mov     BH,byte ptr [BP].s_attr ; Blanked lines' attribute txt mode

wrte_atr: pop     AX
          xchg    CX,DX            ; CX=Upper left corner
          xchg    CH,CL            ; Row,column instead of TI's column,row
          xchg    DH,DL            ; ditto
          add     DX,CX            ; DX=Lower right corner
          dec     DL               ; adjust column count (0 is first column)
          int     IBM_CRT
          jmp     short quit       ; IFF IBM is in graphics mode weird char's
                                   ; are used for blanks when scrolling.  Do
                                   ; as TIPC does and "manual" blank 'em.
;
ti_down:  mov     BX,DX            ; copy destination coordinates
          inc     BL               ; compute dest by incrementing line number
          int     TI_CRT           ; perform the block move
;     paint the first line of the window with blank of proper attributes
blank:    mov     DH,byte ptr [BP].s_col ; load starting column number
          mov     DL,byte ptr [BP].s_line ; load upper line number
          mov     AH,02h           ; load the "put cursor" code
          xor     BH,BH            ; IBMism
          call    crt_dsr          ; position cursor for write
          mov     AX,0920h         ; load "write char/attr" code, write a blank
          mov     BL,byte ptr [BP].s_attr ; load attribute bit setting
          xor     BH,BH            ; IBMism
          mov     CX,[BP].s_ncols  ; load line length
          call    crt_dsr          ; write a line of blanks
;     return to caller
quit:     pop     BP               ; restore caller's BP
          ret
scroll%d  endp
PROGX     ends

;****************************************************************************
;*             Link routine                                                 *
;****************************************************************************

PGROUP    GROUP   PROG
PROG      SEGMENT BYTE PUBLIC 'PROG'
          assume  CS:PGROUP
          public  scroll_d

scroll_d  proc    near
          call    scroll%d         ; link to window scroll down routine
          ret
scroll_d  endp
PROG      ends
          end
