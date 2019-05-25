;                                                       =====> CWINDOW.ASM
;***************************************
;*     TIPC Scheme  Runtime Support    *
;*       Window I/O support            *
;*                                     *
;*    (C) Copyright 1985 by Texas      *
;*     Instruments Incorporated.       *
;*        All rights reserved.         *
;*                                     *
;* Date Written:  24 March 1986        *
;* Last Modification: 24 March 1986    *
;*   7 Jan 1987 - dbs                  *
;*       added random I/O              *
;***************************************
          page    60,132
          include scheme.equ
          include sinterp.arg

BUFFSIZE  equ     256                       ; input/output buffer
WINDSIZE  equ     32-BLK_OVHD
PORTATTR  equ     62
LABEL     equ     32+BUFFSIZE               ; window label field
P_FLAGS   equ     6
W_FLAGS   equ     26
WINDOW    equ     4
B_ATTR    equ     22
T_ATTR    equ     24
CUR_LINE  equ     10
CUR_COL   equ     12
UL_LINE   equ     14
UL_COL    equ     16
N_LINES   equ     18
N_COLS    equ     20
NUM_FLDS  equ     12
CHUNK     equ     14
STR_PTR   equ     3
OPEN      equ     8

DGROUP    group   data
data      segment word public 'DATA'
          assume  DS:DGROUP
          public  MAX_ROWS,MAX_COLS
; from ????
	  extrn   port_r:word

bad_port  db      "[VM INTERNAL ERROR] Bad port for window output",CR,LF,0
mk_win_st db      "%MAKE_WINDOW",0
sv_win_st db      "WINDOW-SAVE-CONTENTS",0
rt_win_st db      "WINDOW-RESTORE-CONTENTS",0
gt_win_st db      "%REIFY-PORT",0
cl_win_st db      "WINDOW_CLEAR",0

defaults  dw      0,0,0,0	; default values of window object
max_rows  db	  25,0
max_cols  db	  80,0
	  dw	  -1,15,1,0,0 

wnlines   dw      0                         ; number of lines
wncols    dw      0                         ; number of columns
wulline   dw      0                         ; upper-left line number
wulcol    dw      0                         ; upper-left column number
branchtab dw      setw_20                   ; [0] : cursor line
          dw      setw_20                   ; [1] : cursor column
          dw      setw_30                   ; [2] : upper left corner line
          dw      setw_40                   ; [3] : upper left corner column
          dw      setw_50                   ; [4] : number of lines
          dw      setw_60                   ; [5] : number of columns
          dw      setw_100                  ; [6] : border attribute
          dw      setw_100                  ; [7] : text attribute
          dw      setw_100                  ; [8] : flags
          dw      setw_100                  ; [9] : buffer position
          dw      setw_100                  ; [10] : buffer end
          dw      setw_100                  ; [11] : port flag
          dw      setw_70                   ; [12] : # of chunks
data      ends

XGROUP    group   progx
progx     segment word public 'progx'
          extrn   rest%scr:far
          extrn   save%scr:far
progx     ends

PGROUP    group   prog
prog      segment byte public 'PROG'
          assume  CS:PGROUP

win_proc  proc    near
;;;************************************************************************
;;;               Allocate a window object
;;;************************************************************************
          extrn   zero_blk:near
          extrn   next_SP:near
          extrn   src_err:near
          extrn   adj4bord:near
          public  make_win
make_win: lods    byte ptr ES:[SI]         ; load the operand register
          save    <SI>
          add     AX,offset reg0           ; compute register address
          mov     BX,AX
          mov     SI,[BX].C_disp           ; get displacement
          mov     BX,[BX].C_page           ; get page number
          mov     tmp_disp,SI              ; save window label pointer
          mov     tmp_page,BX
          cmp     byte ptr ptype+[BX],STRTYPE*2 ; check string type
          jne     make_err
          jmp     short make_020

make_err: test    BX,BX
          jz      make_020                 ; null window label
          lea     BX,mk_win_st             ; load address of text
          jmp     src_err                  ; display error message

make_020: mov     BX,BUFFSIZE+WINDSIZE     ; get object length
          mov     CX,PORTTYPE              ; port type
          pushm   <BX,CX,AX>
          C_call  alloc_bl,,Load_ES        ; allocate block for window object
          pop     BX
          mov     DI,[BX].C_disp           ; get displacement
          save    <DI>
          mov     BX,[BX].C_page           ; get page numbe of window object
          LoadPage ES,BX                   ; get page address
          shr     BX,1
          pushm   <DI, BX>
          call    zero_blk                 ; zero window object
          restore <DI>
          mov     word ptr ES:[DI+6],PORTATTR ; store port attribute
          mov     AX,DI
          add     DI,10                    ; position to move default values
          lea     SI,defaults              ; address of default values
          mov     CX,NUM_FLDS-1            ; length of defaults
rep       movsw                            ; move defaults into object
          mov     DI,AX
          mov     AX,tmp_page
          mov     BX,tmp_disp
          mov     byte ptr ES:[DI+STR_PTR],AL ; store window label pointer
          mov     word ptr ES:[DI+STR_PTR+1],BX
          jmp     next_SP
;;;************************************************************************
;;;            Get Window Attributes
;;; Get Window Attributes was translated from C. The following C comments
;;; show the mappings of the arguments to get-window-attributes to their
;;; actual locations within the port object.
;;;
;;;
;;;#define NUM_FIELDS 12
;;;static int defaults[NUM_FIELDS] = {0,   /* cursor line number */
;;;                                   0,   /* cursor column number */
;;;                                   0,   /* upper left corner line number */
;;;                                   0,   /* upper left corner column number */
;;;                                  25,   /* number of lines */
;;;                                  80,   /* number of columns */
;;;                                  -1,   /* no border */
;;;                                  15,   /* text high intensity, enable */
;;;                                   1,   /* wrap enabled */
;;;                                   0,   /* current buffer position */
;;;                                   0,   /* current buffer end */
;;;TRANSCRIPT+BINARY+WINDOW+OPEN+READ_WRITE}; /* port attributes */
;;;static int map_attr[NUM_FIELDS] = {10,12,14,16,18,20,22,24,26,28,30,6};
;;;
;;;************************************************************************
          public  get_wind
get_wind: lods    word ptr ES:[SI]         ; load register operand
          save    <SI>                     ; save the location pointer
          xor     BX,BX
          mov     BL,AH
          add     BX,offset reg0           ; compute address of register
          xor     AH,AH
          add     AX,offset reg0
          save    <AX>                     ; save registers
          save    <BX>
          mov     CX,1
          pushm   <CX, AX>
          C_call  get_port,,Load_ES        ; get the port object
          mov     SP,BP
          mov     SI,tmp_page
          cmp     byte ptr ptype+[SI],PORTTYPE*2
          jne     get_err
          restore <BX>
          cmp     [BX].C_page,SPECFIX*2
          jne     get_err
          mov     BX,word ptr [BX].C_disp  ; get the value
          shl     BX,1
          sar     BX,1
          cmp     BX,0
          jl      get_err
          cmp     BX,NUM_FLDS
          jg      get_err                  ; used to be jge - dbs
          LoadPage ES,SI                   ; get page address
          mov     SI,tmp_disp
          restore <AX>
          mov     DI,AX
          mov     word ptr [DI].C_page,SPECFIX*2
          cmp     BX,12
          jne     get_05
          mov     AX,word ptr ES:[SI+CHUNK]; get chunk number
          jmp     get_20
get_05:   cmp     BX,11
          jne     get_10
          mov     AX,word ptr ES:[SI+6]
          jmp     get_20
get_10:   shl     BX,1                     ; get the word offset
          mov     AX,word ptr ES:[SI+10+BX]
get_20:
	  test	  word ptr ES:[SI+P_FLAGS],WINDOW ; Port a window?
	  jz	  get_25			  ; No,  jump
	  and	  AX,07FFFh			  ; Yes, return integer
	  mov	  word ptr [DI].C_disp,AX
	  jmp	  next_SP		 	  ; Return to interpreter
get_25:
	  xor 	  BX,BX
	  push	  BX			   ; push long integer value
	  push	  AX
	  push	  DI			   ; register to store value
          C_call  long2int,,Load_ES        ; convert to scheme integer
	  mov	  SP,BP
	  jmp	  next_SP
get_err:  lea     BX,gt_win_st
          jmp     src_err                  ; link to error handler
;;;************************************************************************
;;;                  Modify Transcript File Status
;;;************************************************************************
          public  trns_chg
trns_chg: lods    byte ptr ES:[SI]         ; load register operand
          save    <SI>
          add     AX,offset reg0           ; compute address of register
          mov     BX,AX
          mov     SI,[BX].C_disp
          mov     BX,[BX].C_page
          cmp     byte ptr ptype+[BX],PORTTYPE*2 ; check type
          jne     trns_10
          LoadPage ES,BX                   ; get page address
          mov     AX,word ptr ES:[SI+P_FLAGS]
          mov     CX,AX
          and     AX,OPEN                  ; open?
          jz      trns_10
          and     CX,3                     ; read and write?
          jz      trns_10
          mov     TRNS_pag,BX
          mov     TRNS_dis,SI
          jmp     next_SP
trns_10:  xor     AX,AX
          mov     TRNS_pag,AX
          mov     TRNS_dis,AX
          jmp     next_SP
;;;************************************************************************
;;;                  Save Window Contents
;;;************************************************************************
          public  save_win
save_win: lods    byte ptr ES:[SI]         ; load register operand
          save    <SI>
          add     AX,offset reg0           ; compute address of register
          xor     BX,BX
          pushm   <BX, AX>
          save    <AX>
          C_call  get_port,,Load_ES        ; get port object
          mov     SP,BP
          mov     BX,tmp_page
          cmp     byte ptr ptype+[BX],PORTTYPE*2 ; check port type
          je      save_01
save_err: lea     BX,sv_win_st
          jmp     src_err                  ; link to error handler
save_01:  LoadPage ES,BX                   ; get page address
          mov     DI,tmp_disp
          mov     AX,word ptr ES:[DI+P_FLAGS]
          and     AX,WINDOW                ; window object?
          jz      save_err
          mov     AX,word ptr ES:[DI+UL_LINE]
          mov     BX,word ptr ES:[DI+UL_COL]
          mov     CX,word ptr ES:[DI+N_LINES]
          mov     DX,word ptr ES:[DI+N_COLS]
          mov     wulline,AX
          mov     wulcol,BX
          mov     wnlines,CX
          mov     wncols,DX
          mov     AX,word ptr ES:[DI+B_ATTR] ; border attribute
          cmp     AX,-1                    ; bordered?
          je      save_10                  ; no, jump
          lea     AX,wulline
          lea     BX,wulcol
          lea     CX,wnlines
          lea     DX,wncols
          pushm   <DX, BX, CX, AX>
          call    adj4bord                 ; adjust window region
save_10:  mov     AX,wnlines
          mov     BX,wncols
; compute the length of string to save window contents
          mul     BL
          shl     AX,1                     ; * 2
          add     AX,2                     ; + 2
          push    AX
          restore <AX>
          mov     CX,STRTYPE               ; string type
          pushm   <CX, AX>
          C_call  alloc_bl,,Load_ES        ; alloc_block
          mov     SP,BP
          pushm   <wncols,wnlines,wulcol,wulline>
          restore <AX>
          push    AX
          call    save%scr                 ; save screen
          jmp     next_SP                  ; return to interpreter
;;;************************************************************************
;;;                  Restore Window Contents
;;;************************************************************************
          public  rest_win
rest_win: lods    word ptr ES:[SI]         ; load register operand
          save    <SI>                     ; save the location pointer
          xor     BX,BX
          mov     BL,AH
          add     BX,offset reg0           ; compute address of register
          xor     AH,AH
          add     AX,offset reg0
          save    <BX>
          xor     CX,CX
          pushm   <CX, AX>
          C_call  get_port,,Load_ES        ; get the port object
          mov     SP,BP
          restore <BX>                     ; BX = data to be restored
          mov     SI,[BX].C_page
          cmp     byte ptr ptype+[SI],STRTYPE*2 ; check type
          jne     rest_err
          mov     DI,tmp_page
          cmp     byte ptr ptype+[DI],PORTTYPE*2 ; check type
          jne     rest_err
          LoadPage ES,DI                   ; get page address
          mov     DI,tmp_disp
          mov     AX,word ptr ES:[DI+P_FLAGS]
          and     AX,WINDOW                ; window object?
          jz      rest_err
          mov     AX,word ptr ES:[DI+UL_LINE]
          mov     BX,word ptr ES:[DI+UL_COL]
          mov     CX,word ptr ES:[DI+N_LINES]
          mov     DX,word ptr ES:[DI+N_COLS]
          mov     wulline,AX
          mov     wulcol,BX
          mov     wnlines,CX
          mov     wncols,DX
          mov     AX,word ptr ES:[DI+B_ATTR] ; border attribute
          cmp     AX,-1
          je      rest_10
          lea     AX,wulline
          lea     BX,wulcol
          lea     CX,wnlines
          lea     DX,wncols
          pushm   <DX, BX, CX, AX>
          call    adj4bord                 ; adjust window region
rest_10:  pushm   <wncols, wnlines, wulcol, wulline>
          restore <BX>
          push    BX
          call    rest%scr                 ; restore screen
          jmp     next_SP                  ; return to interpreter
rest_err: lea     BX,rt_win_st
          jmp     src_err                  ; link to error handler
win_proc  endp
;;;************************************************************************
;;;                  Set Window Attribute
;;;************************************************************************
setw_arg  struc
          dw      ?                        ; caller's BP
          dw      ?                        ; caller's ES
          dw      ?                        ; caller's return address
setw_reg  dw      ?
setw_att  dw      ?
setw_val  dw      ?
setw_arg  ends
          public  set_wind
set_wind  proc    near
          push    ES
          push    BP
          mov     BP,SP
          mov     AX,1
          pushm   <AX, [BP].setw_reg>
          C_call  get_port,,Load_ES        ; get port address
          mov     SP,BP
          mov     BX,tmp_page
          cmp     byte ptr ptype+[BX],PORTTYPE*2 ; check type
          jne     setw_err
          mov     SI,[BP].setw_att
          cmp     word ptr [SI].C_page,SPECFIX*2 ; check attribute type
          jne     setw_err
          mov     AX,[SI].C_disp           ; get attribute value
          shl     AX,1
          sar     AX,1
          cmp     AX,0                     ; check attribute value
          jl      setw_err
          cmp     AX,NUM_FLDS
          jge     setw_err
          mov     SI,[BP].setw_val         ; get the value pointer
          cmp     word ptr [SI].C_page,SPECFIX*2 ; check type
          je      setw_10
setw_err: lea     BX,gt_win_st             ; address of error message
          pushm   <[BP].setw_val, [BP].setw_att, [BP].setw_reg>
          mov     AX,3
          pushm   <AX, BX>
          C_call  set_src_,,Load_ES        ; set_src_err
          mov     SP,BP
          mov     AX,-1                    ; return error status
          jmp     setw_ret
setw_10:  mov     CX,[SI].C_disp           ; get the value
          shl     CX,1
          sar     CX,1
          LoadPage ES,BX                   ; get page address of port
          mov     SI,tmp_disp              ; displacement of port object
          mov     BX,AX
          shl     BX,1                     ; get the word offset
          jmp     branchtab+[BX]
; cursor line/cursor column
setw_20:  cmp     CX,0
          jl      setw_err                 ; negative value, error
          jmp     setw_100
; upper left hand corner line number
setw_30:  xor     AX,AX
	  xor	  DH,DH
          mov     DL,MAX_ROWS
          dec     DX                       ; MAX_ROWS - 1
          call    fit_in_r
          mov     AX,word ptr ES:[SI+N_LINES]
          inc     DX
          sub     DX,CX                    ; MAX_ROWS - value
          cmp     AX,DX
          jle     setw_35
          mov     word ptr ES:[SI+N_LINES],DX
setw_35:  jmp     setw_100
; upper left hand corner column number
setw_40:  xor     AX,AX
	  xor	  DH,DH
          mov     DL,MAX_COLS
          dec     DX                       ; MAX_COLUMNS - 1
          call    fit_in_r
          mov     AX,word ptr ES:[SI+N_COLS]
          inc     DX
          sub     DX,CX                    ; MAX_COLUMNS - value
          cmp     AX,DX
          jle     setw_35
          mov     word ptr ES:[SI+N_COLS],DX
          jmp     setw_35
; number of lines
setw_50:  mov     AX,word ptr ES:[SI+UL_LINE]
	  xor	  DH,DH
          mov     DL,MAX_ROWS
          sub     DX,AX                    ; MAX_ROWS - UL_LINE
          mov     AX,1
          call    fit_in_r
          jmp     setw_100
; number of columns
setw_60:  mov     AX,word ptr ES:[SI+P_FLAGS]
          and     AX,WINDOW                ; window?
          jz      setw_100                 ; no, jump
          mov     AX,word ptr ES:[SI+UL_COL]
	  xor	  DH,DH
          mov     DL,MAX_COLS
          sub     DX,AX                    ; MAX_COLUMNS - UL_COL
          mov     AX,1
          call    fit_in_r
          jmp     setw_100
; chunk#
setw_70:  mov     BX,CHUNK
          jmp     setw_120
; store the value
setw_100: sar     BX,1
          cmp     BX,11
          jne     setw_110
          mov     BX,6
          jmp     setw_120
setw_110: shl     BX,1                     ; word offset
          add     BX,10
setw_120: mov     word ptr ES:[SI+BX],CX   ; store the value
          xor     AX,AX
setw_ret: pop     BP
          pop     ES
          ret
set_wind  endp
;;;************************************************************************
;;;                  Force Value into Range
;;;  Purpose: To test a value (in CX) to determine if it falls within a
;;;           range of values, as specified by an lower (in AX) and
;;;           upper (in DX) bounds. If the value is within the range,
;;;           the value is returned (in CX) unchanged. If it is outside
;;;           the range, the value of the endpoint nearest its value
;;;           is returned (in CX).
;;;************************************************************************
fit_in_r  proc    near
          pop     DI                       ; get the return address
          cmp     CX,AX                    ; value < lower?
          jge     fit_10
          mov     CX,AX                    ; yes, return lower
fit_01:   jmp     DI                       ; return to caller
fit_10:   cmp     CX,DX                    ; value > upper?
          jle     fit_01                   ; no, return
          mov     CX,DX                    ; yes, return upper
          jmp     DI                       ; return to caller
fit_in_r  endp
;;;************************************************************************
;;;                  Write message to the who-line
;;;************************************************************************
who_arg   struc
pg        dw      ?
dis       dw      ?
who_BP    dw      ?                        ; caller's BP
          dw      ?                        ; caller's ES
          dw      ?                        ; caller's return address
str       dw      ?                        ; pointer to message string
who_arg   ends
          extrn   ssetadr:near
          extrn   printstr:near
          public  who_writ
who_writ  proc    near
          push    ES
          push    BP
          sub     SP,offset who_BP         ; allocate local storage
          mov     BP,SP
          lea     SI,port_r
          mov     AX,[SI].C_page
          mov     [BP].pg,AX
          mov     AX,[SI].C_disp
          mov     [BP].dis,AX
          mov     AX,WHO_DISP
          mov     BX,WHO_PAGE*2
          pushm   <AX, BX>
          call    ssetadr                  ; get port address
          mov     SP,BP
; compute the length of message string
          xor     BX,BX
          mov     SI,[BP].str
who_010:  cmp     byte ptr [SI+BX],0       ; end of string?
          je      who_020
          inc     BX
          jmp     who_010
; Write message to the who line
who_020:  push    BX                       ; BX = strlen(str)
          push    SI
          call    printstr
          mov     SP,BP
; Restore the port which was in effect when started
          mov     BX,[BP].pg
          cmp     byte ptr ptype+[BX],PORTTYPE*2 ; check port type
          jne     who_ret
          LoadPage ES,BX                   ; get page address
          mov     SI,[BP].dis
          cmp     byte ptr ES:[SI],PORTTYPE ; check port type
          jne     who_ret
          pushm   <SI, BX>
          call    ssetadr                  ; get port address
          mov     SP,BP
who_ret:  add     SP,offset who_BP         ; release local storage
          pop     BP
          pop     ES
          ret
who_writ  endp

prog      ends
          end

