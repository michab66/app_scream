;                                                       =====> CPRINT.ASM
;******************************************
;*     TIPC Scheme  Runtime Support       *
;*   Scheme Interpreter Support for write *
;*                                        *
;*    (C) Copyright 1985 by Texas         *
;*     Instruments Incorporated.          *
;*        All rights reserved.            *
;*                                        *
;* Date Written:  21 March 1986           *
;* Last Modification: 21 March 1986       *
;******************************************
            page    60,132
            include scheme.equ
            include sinterp.arg
LF          equ     0Ah
SPACE       equ     20h

DGROUP      group   data
data        segment word public 'DATA'
            assume  DS:DGROUP
            extrn   display:word
            extrn   show:word
;;;         extrn   detail:word
sp1_er      db      "WRITE",0
spc_er      db      "DISPLAY",0
spt_er      db      "PRINT",0
new_er      db      "NEWLINE",0
data        ends

PGROUP      group   prog
prog        segment byte public 'PROG'
            assume  CS:PGROUP
prn_proc    proc    near
            extrn   next_SP:near
            extrn   src_err:near
            extrn   get_port:near
            extrn   sprint:near
;;;
;;;  Does not set the value for flag "detail" (which is removed in CPRINT1.ASM)
;;;
;;;****************************************************************************
;;;            Print an S-Expression (w/ slashification)
;;;  Purpose:  Scheme interpreter support to output an s-expression to
;;;            a port.
;;;****************************************************************************
            public  spprin1
spprin1:    lods    word ptr ES:[SI]          ; load register operand
            save    <SI>
            xor     BX,BX
            mov     BL,AH
            add     BX,offset reg0            ; BX = port object
            xor     AH,AH
            add     AX,offset reg0            ; AX = s-expression pointer
            mov     DI,AX
            save    <DI>
            mov     CX,1                      ; write indicator
            pushm   <CX, BX>
            C_call  get_port,,Load_ES         ; get port address
            mov     SP,BP
            test    AX,AX                     ; check return status
            jz      sp1_010
            lea     BX,sp1_er
            jmp     src_err                   ; link to error handler
sp1_010:
;;;         mov     detail,AX
            inc     AX
            mov     display,AX
            mov     show,AX
            pushm   <tmp_disp, tmp_page>
            restore <DI>
            mov     BX,[DI].C_page
            shr     BX,1
            pushm   <[DI].C_disp, BX>
            call    sprint                    ; write
            mov     SP,BP
sp1_020:    restore <DI>                      ; get the register pointer
            mov     [DI].C_page,NPR_PAGE*2    ; return as non-printable object
            mov     [DI].C_disp,NPR_DISP
            jmp     next_SP                   ; return to interpreter
;;;****************************************************************************
;;;            Print an S-Expression (w/o slashification)
;;;  Purpose:  Scheme interpreter support to output an s-expression to
;;;            a port.
;;;****************************************************************************
            public  spprinc
spprinc:    lods    word ptr ES:[SI]          ; load register operand
            save    <SI>
            xor     BX,BX
            mov     BL,AH
            add     BX,offset reg0            ; BX = port object
            xor     AH,AH
            add     AX,offset reg0            ; AX = s-expression pointer
            mov     DI,AX
            save    <DI>
            mov     CX,1
            pushm   <CX, BX>
            C_call  get_port,,Load_ES         ; get port address
            mov     SP,BP
            test    AX,AX                     ; check return status
            jz      spc_010
            lea     BX,spc_er
            jmp     src_err                   ; link to error handler
spc_010:    mov     display,AX
;;;         mov     detail,AX
            inc     AX
            mov     show,AX
            pushm   <tmp_disp, tmp_page>
            restore <DI>
            mov     BX,[DI].C_page
            shr     BX,1
            pushm   <[DI].C_disp, BX>
            call    sprint                    ; display
            mov     SP,BP
            jmp     sp1_020
;;;****************************************************************************
;;;            Print an S-Expression (w/ spacing control)
;;;  Purpose:  Scheme interpreter support to output an s-expression to
;;;            a port.
;;;****************************************************************************
            public  spprint
spprint:    lods    word ptr ES:[SI]          ; load register operand
            save    <SI>
            xor     BX,BX
            mov     BL,AH
            add     BX,offset reg0            ; BX = port object
            xor     AH,AH
            add     AX,offset reg0            ; AX = s-expression pointer
            mov     DI,AX
            save    <DI>
            mov     CX,1
            pushm   <CX, BX>
            C_call  get_port,,Load_ES         ; get port address
            mov     SP,BP
            test    AX,AX                     ; check return status
            jz      spt_010
            lea     BX,spt_er
            jmp     src_err                   ; link to error handler
spt_010:    mov     display,AX
;;;         mov     detail,AX
            inc     AX
            mov     show,AX
            mov     DX,SPECCHAR
            mov     BX,LF                     ; line feed
            pushm   <tmp_disp, tmp_page, BX, DX>
            call    sprint                    ; print it
            mov     SP,BP
            xor     AX,AX
;;;         mov     detail,AX
            inc     AX
            mov     show,AX
            mov     display,AX
            pushm   <tmp_disp, tmp_page>
            restore <DI>
            mov     BX,[DI].C_page
            shr     BX,1
            pushm   <[DI].C_disp, BX>
            call    sprint                    ; print the s-expression
            mov     SP,BP
            mov     BX,SPACE
            mov     DX,SPECCHAR               ; space
            xor     AX,AX
;;;         mov     detail,AX
            mov     display,AX
            inc     AX
            mov     show,AX
            pushm   <tmp_disp, tmp_page, BX, DX>
            call    sprint                    ; print it
            mov     SP,BP
            jmp     sp1_020
;;;****************************************************************************
;;;            Print a "newline" character
;;;  Purpose:  Scheme interpreter support to output a newline character
;;;            to a port.
;;;****************************************************************************
            public  spnewlin
spnewlin:   lods    byte ptr ES:[SI]          ; load register operand
            save    <SI>
            add     AX,offset reg0            ; AX = port object
            mov     CX,1
            pushm   <CX, AX>
            C_call  get_port,,Load_ES         ; get port address
            mov     SP,BP
            test    AX,AX                     ; check return status
            jz      new_010
            lea     BX,new_er
            jmp     src_err                   ; link to error handler
new_010:    mov     display,AX
;;;         mov     detail,AX
            inc     AX
            mov     show,AX
            mov     BX,SPECCHAR
            mov     DX,LF                     ; linefeed
            pushm   <tmp_disp, tmp_page, DX, BX>
            call    sprint
            mov     SP,BP
            jmp     next_SP                   ; return to interpreter
;;;****************************************************************************
;;;            Find Print-length of an S-Expression
;;;  Purpose:  Scheme interpreter support to determine the print length
;;;            of a scheme object.
;;;****************************************************************************
            public  prt_len
prt_len:    lods    byte ptr ES:[SI]          ; load register operand
            save    <SI>
            add     AX,offset reg0            ; AX = port object
            mov     DI,AX
            xor     CX,CX
            mov     display,CX                ; no display and show
            mov     show,CX
;;;         inc     CX
;;;         mov     detail,CX
            save    <DI>
            mov     DX,OUT_PAGE*2
            mov     CX,OUT_DISP
            mov     BX,[DI].C_page
            shr     BX,1                      ; correct page number
            pushm   <CX, DX, [DI].C_disp, BX>
            call    sprint
            mov     SP,BP                     ; AX = print length
            restore <DI>
            mov     [DI].C_page,SPECFIX*2
            mov     [DI].C_disp,AX            ; get the print length
            jmp     next_SP                   ; return to interpreter
prn_proc    endp
prog        ends
            end

