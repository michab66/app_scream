;                                                       =====> REALIO.ASM
;*****************************************************
;*   		TIPC Scheme Runtime Support          *
;*      	  Real Mode I/O Routines             *
;*                                     		     *
;*  		(C) Copyright 1985 by Texas          *
;*     		    Instruments Incorporated.        *
;*        	      All rights reserved.           *
;*                                     		     *
;* Date Written:  November 1987 (tc)   		     *
;* Last Modification:                  		     *
;*****************************************************
		  include schemed.equ    ;for port defs
	          include screen.equ
	  	  .286c
;
; Local equates
;

RETURN	  equ	  0Dh		   ; Carriage Return Character
LINEFEED  equ	  0Ah		   ; Line Feed Character
DOS	  equ	  021h		   ; Dos function request
WRITEFILE equ	  040h		   ; Write Fiel function request
DISK_FULL equ	  -1		   ; Disk full error designator

TIPC	  equ	  1		   ; Designates machine type as TIPC

TI_CRT    equ     049h
IBM_CRT   equ     010h

;
; Segment Definitions
;

PGROUP	  group   PROG
DGROUP	  group   DATA

; This stack is used for the standard XLI interface. However, a different
; stack (i.e. PCS's) is used during calls to a graphics driver.
STACK   segment word stack 'STACK'
stackstart =	$
	dw	16 dup (?)
stacksize  =	$ - stackstart
STACK   ends

;
; Data definitions
;
DATA	  segment byte public 'DATA'
	  assume  DS:DGROUP
datastart  =	$

callers_ds dw   ?
callers_es dw   ?

;-----------------------------------------------------------------------------
; Type/Monitor info
;-----------------------------------------------------------------------------
PC_MAKE	  dw	0
VID_MODE  dw	3
MAX_ROWS  db	DEFAULT_NUM_ROWS
MAX_COLS  db	DEFAULT_NUM_COLS
CHAR_HGT  dw	8 

;-----------------------------------------------------------------------------
;	  Jump table for handler based on op_code
;-----------------------------------------------------------------------------
OP_CODE   dw	  BELL		;  0 - Sound Bell
	  dw	  CLEAR		;  1 - Clear Screen
	  dw	  BORDER	;  2 - Draw Border
	  dw	  SAVE_SCR	;  3 - Save Screen
	  dw	  REST_SCR	;  4 - Restore Screen
	  dw	  ?		;  5 - was turn cursor on
	  dw	  ?		;  6 - was turn cursor off
	  dw	  ?     	;  7 - was position cursor
	  dw	  PUTCHAR	;  8 - Put Character on screen
	  dw	  SCROLLUP	;  9 - Scroll Screen Up 1 Line
	  dw	  SCROLLDN	; 10 - Scroll Screen Dn 1 Line
	  dw	  ?       	; 11 - was ega cursor emulation
	  dw	  CHGMODE	; 12 - Note Change to Video Mode
	  dw	  WSTRING	; 13 - Write string to output port
	  dw	  WBLOCK	; 14 - Write string to display
table_len equ	  $ - OP_CODE

;-----------------------------------------------------------------------------
;	XLI Setup
;-----------------------------------------------------------------------------
;;; ----- Equates -----
; offsets into the PSP
term_addr equ	0Ah
fb_addr	  equ	5Ch
;;; ----- Data structures -----
; file block
file_block label word
	dw	4252h
	dw	10011b		             ;flags = sysint,0,0,16-bit,near
	dw	offset lookup_table, seg lookup_table
	dw	offset parm_block, seg parm_block
; reserved area of file block
	dw	100h		             ;sysint#  (256=%graphics)
	dw	offset handler, seg handler  ;ISR entry point
	dw	0,0,0,0,0
; parameter block
parm_block label word		;not used 
	dw	0
; lookup table
lookup_table label word
	db	'//'		;not used
; other needed values
psp	dw	?		;PSP segment address
psize   dw	?		;size of program in paragraphs
xwait   dw	2 dup (?)	;XLI wait address
xbye	dw	2 dup (?)	;XLI bye address

;				       ___		      __     __
;		    +	-|   |-   _|_	|  --	|    __|  |__	|   |	(extra)
map_tab   db	  0c5h,0b4h,0c3h,0c1h,0c2h,0c4h,0b3h,0d9h,0c0h,0bfh,0dah,0dah
map_tabx  equ	  $

trns_tab  db	  0dah,0c2h,0c3h,0c5h,0c3h,0c2h,0c2h,0c5h,0c3h,0c5h,0c5h
	  db	  0c2h,0bfh,0c5h,0b4h,0b4h,0c2h,0c2h,0c5h,0c5h,0b4h,0c5h
	  db	  0c3h,0c5h,0c0h,0c1h,0c3h,0c1h,0c5h,0c1h,0c3h,0c5h,0c5h
	  db	  0c5h,0b4h,0c1h,0d9h,0b4h,0c1h,0c5h,0c1h,0c5h,0b4h,0c5h
	  db	  0c3h,0b4h,0c3h,0b4h,0b3h,0c5h,0c5h,0c5h,0c3h,0b4h,0c5h
	  db	  0c2h,0c2h,0c1h,0c1h,0c5h,0c4h,0c2h,0c1h,0c5h,0c5h,0c5h

m14_attr equ $
;33-60	    ! " # $ % & ' ( ) * + , - . / 0 1 2 3 4 5 6 7 8 9 : ; <
	 db 1,0,2,2,1,4,2,0,0,3,3,7,3,5,6,2,6,6,5,3,5,2,1,2,2,1,7,3

;61-88      = > ? @ A B C D E F G H I J K L M N O P Q R S T U V W X
	 db 2,6,1,2,2,0,3,0,0,0,3,1,0,4,0,0,1,1,3,0,3,0,2,0,1,3,1,1

;89-116     Y Z [ \ ] ^ _ ` a b c d e f g h i j k l m n o p q r s t
	 db 0,1,0,0,0,3,7,1,5,0,3,4,3,3,3,0,6,6,0,0,2,2,3,2,3,2,3,2

;117-126    u v w x y z { | } ~
	 db 2,3,3,2,2,2,3,1,0,1

;127-191
	 db 64 dup (0)

;192-197
	 db 5,2,0,0,0,4
	      
;198-218
	 db 20 dup (0)
;219-220
	 db 2,5

m16_attr equ $
;33-60	    ! " # $ % & ' ( ) * +  , - . / 0  1  2 3 4 5 6 7 8 9 :  ; <
	 db 3,1,4,4,4,7,4,2,2,6,6,11,6,9,9,4,10,10,9,6,9,4,2,4,4,3,10,6

;61-88      =  > ? @ A B C D E F G H I J K L M N O P Q R S T U V W X
	 db 5,10,3,4,5,2,5,2,2,2,5,2,2,8,2,2,2,2,5,2,5,2,4,2,2,5,2,2

;89-116     Y Z [ \ ] ^  _ ` a b c d e f g h  i  j k l m n o p q r s t
	 db 2,2,2,2,2,3,12,2,9,2,6,7,6,6,6,2,10,11,2,2,5,5,6,5,6,5,6,4

;117-126    u v w x y z { | } ~
	 db 5,5,5,5,5,5,6,2,2,3
	 db 64 dup (0)
	 db 7,2,0,0,0,7
	 db 20 dup (0)
	 db 2,7

	  public  m18_attr
m18_attr equ $
;33-60	    ! " # $ % & ' ( ) * +  , -  .  / 0  1  2  3 4  5 6 7 8 9  :  ; <
	 db 2,1,4,4,8,8,4,2,2,6,6,10,9,10,05,4,10,10,10,6,10,4,2,4,4,10,10,6

;61-88      =  > ? @ A B C D E F G H I J K L M N O P Q R S T U V W X
	 db 9,06,2,4,4,2,6,2,2,2,6,2,2,8,2,2,2,2,6,2,6,2,4,2,2,6,2,2

;89-116     Y Z [ \ ] ^  _ `  a b c d e f g h  i  j k l m n o p q r s t
	 db 2,2,2,3,2,3,13,2,10,2,6,8,6,6,6,2,10,12,2,2,5,5,6,5,6,5,6,4

;117-126    u v w x y z { | } ~
	 db 5,6,6,5,5,5,6,2,2,2
	 db 64 dup (0)
	 db 10,4,0,0,0,8
	 db 20 dup (0)
	 db 8,8

;
; The following jump table is used wstring to handle control characters
; written to the screen.
;
wn_handle dw	 wwin_default 	   ;00 - Null character
	  dw	 wwin_default	   ;01 - 
	  dw	 wwin_default	   ;02 - 
	  dw	 wwin_default	   ;03 - 
	  dw	 wwin_default	   ;04 - 
	  dw	 wwin_default	   ;05 - 
	  dw	 wwin_default	   ;06 - 
	  dw	 wwin_bell	   ;07 - Bell
	  dw	 wwin_backspace	   ;08 - Backspace
	  dw	 wwin_tab	   ;09 - Tab
	  dw	 wwin_linefeed	   ;0A - Linefeed
	  dw	 wwin_default 	   ;0B - 
	  dw	 wwin_default	   ;0C - 
	  dw	 wwin_creturn	   ;0C - Carriage Return

;
; The following jump table is used by wstring to handle control characters
; written to disk.
;
ds_handle dw	 wfil_default 	   ;00 - Null character
	  dw	 wfil_default	   ;01 - 
	  dw	 wfil_default	   ;02 - 
	  dw	 wfil_default	   ;03 - 
	  dw	 wfil_default	   ;04 - 
	  dw	 wfil_default	   ;05 - 
	  dw	 wfil_default	   ;06 - 
	  dw	 wfil_default	   ;07 -
	  dw	 wfil_backspace	   ;08 - Backspace
	  dw	 wfil_tab	   ;09 - Tab
	  dw	 wfil_newline 	   ;0A - Linefeed
	  dw	 wfil_default 	   ;0B - 
	  dw	 wfil_default	   ;0C - 
	  dw	 wfil_newline 	   ;0D - Carriage Return


last_char db 0dbh

c_col	 dw 0
c_row	 dw 0
c_len	 dw 0
c_nrows  dw 0

sav_di	 dw  0

datasize =	$-datastart
DATA	  ends

	  page
;-----------------------------------------------------------------------------
;-----------------------------------------------------------------------------
PROG	  segment byte public 'PROG'
	  assume  CS:PGROUP,DS:DGROUP
progstart =	  $

;-----------------------------------------------------------------------------
; name	  HANDLER -- Interface Handler to IO routines
;
; AX will contain error indicator (-1) if error encountered
;
hand_args struc
	  dw	  ?		   ; callers bp
	  dd	  ?		   ; return address (far)
opcode	  dw	  ?		   ; IO operation to perform
hand_args ends

	  public  handler
handler   proc	  far
	  mov	  BX,DS		   ; temp save caller's DS
	  mov	  AX,data
	  mov	  DS,AX		   ; establish local data segment
	  mov	  callers_ds,bx    ; save callers data seg
	  mov	  callers_es,es    ; and extra seg
	  mov	  ES,AX
;     Load sub opcode
	  push	  bp
	  mov	  bp,sp
	  mov	  BX,[BP].opcode   ; load operation code
	  pop	  bp
	  add	  BX,BX 	   ; adjust for index into jump table
	  cmp	  BX,table_len	   ; bad op_code?
	  jae	  bad_op

;     Jump desired IO handler
	  call	  OP_CODE[BX]
	  jmp	  short hand_ret

bad_op:   mov	  AX,-1

;     Return to caller
hand_ret: 
	  push	  callers_ds
	  pop	  ds		   ; restore caller's data seg
	  push	  callers_es
	  pop	  es		   ; restore caller's data seg
	  ret			   ; return to caller
handler   endp


;************************************************************************
;*                      Generate a Bell Character                       *
;*                                                                      *
;* Purpose:  To generate a "bell character" (i.e., make a noise) to     *
;*              simulate the effect of outputting a bell character      *
;*              (control-G) in the output stream.                       *
;*                                                                      *
;* Calling Sequence:  zbell();                                          *
;*                                                                      *
;* Input Parameters:  None.                                             *
;*                                                                      *
;* Output Parameters:  None.                                            *
;*                                                                      *
;************************************************************************
          public  bell
bell      proc    near
          cmp     PC_MAKE,TIPC	   ; If not tipc
          jne     zbmbell	   ;  then jump
zbwait:   mov     AH,1             ; Get speaker status
          int     48h
          jnz     zbwait           ; wait for bell to turn off
          mov     AH,2             ; Set speaker frequency
          mov     CX,1563          ; Value for 1.25MHz/800Hz (system beep)
          int     48h
          mov     AX,000Ah         ; Turn speaker on for AL*25-ms. 0Ah = .25-sec
          int     48h
	  xor	  ax,ax
          ret                      ; return to caller
;
zbmbell:  mov     BX,080h          ; ****Copied from IBM-PC/XT BIOS listing****
          in      AL,61h
          push    AX               ; Save
beep_cycle: and   AL,0FCh          ; Turn off timer gate and speaker data
          out     61h,AL           ; output to control
          mov     CX,48h           ; Half cycle time for TONE
here:     loop    here             ; speaker off
          or      AL,2             ; Turn speaker on
          out     61h,AL
          mov     CX,48h
here2:    loop    here2
          dec     BX               ; Decrease cycle count
          jnz     beep_cycle
          pop     AX
          out     61h,AL
	  xor	  ax,ax
          ret
bell      endp

;************************************************************************
;*                           Clear a Window                             *
;************************************************************************
cl_args   struc
          dw      ?                ; caller's BP
          dw      ?                ; return address
	  dd	  ?		   ; caller's return address
cl_op	  dw	  ?		   ; clear window op code
cl_row    dw      ?                ; upper left hand corner row number
cl_col    dw      ?                ; upper left hand corner column number
cl_nrows  dw      ?                ; number of rows
cl_len    dw      ?                ; line length (number of characters)
cl_attrib dw      ?                ; character attributes
cl_args   ends

         public  clear
clear    proc    near
         push    BP               ; save caller's BP
         mov     BP,SP
;     Put cursor at beginning of next row
c_loop:  mov     DL,byte ptr [BP].cl_row ; load current row number
         mov     DH,byte ptr [BP].cl_col ; load starting column number
         xor     BH,BH            ; page number (0 if in graphics mode)
         mov     AH,02H           ; load "put cursor" code
         call    crt_dsr          ; position the cursor
;     Write line of blanks at current cursor position
         mov     AX,0920h         ; load write char/attr code + blank (= 20h)
         xor     BH,BH            ; (for IBM-PC BH=display page #)
         mov     BL,byte ptr [BP].cl_attrib ; load attribute flag

         cmp    vid_mode,14        ; IBM EGA modes?
         jl     c_01
         cmp    BL,87h             ; attribute is rv white?
         jne    c_22
         mov    AX,09dbh           ; use the block character not the blank
         and    BL,7fh             ; strip off the xor bit

c_01:    mov     CX,[BP].cl_len    ; load number of times to write the blank
         call    crt_dsr          ; perform the write
;     Increment row number, decrement row count, test, loop
         inc     [BP].cl_row       ; increment row number
         dec     [BP].cl_nrows     ; decrement row count
         jg      c_loop           ; if more rows, loop (jump)
;     Return to caller
c_end:   pop     BP               ; restore caller's BP
	 xor	 ax,ax		  ; return status
         ret                      ; return

  ; clear out the line by writing directly to the graphics planes
c_22:    
	 mov	AX,[BP].cl_nrows
	 mov	c_nrows,ax
	 mov    AX,[BP].cl_row          ; set AX to the row
         mov    c_row,AX
         mov    AX,[BP].cl_col          ; add in the starting column
         mov    c_col,AX
         mov    AX,[BP].cl_len          ; number of columns to blank
         mov    c_len,AX
         call   z_ega                   ; restore counter

         jmp    c_end                  ; return

clear    endp

;************************************************************************
;*			     Draw Border				*
;************************************************************************
b_args  struc
	 dw	  ?		   ; caller's BP
	 dw	  ?		   ; return address
	 dd	  ?		   ; caller's return address
b_opcode dw	  ?		   ; border opcode
b_line   dw	  ?		   ; upper left corner line number
b_col	 dw	  ?		   ; upper left corner column number
b_nlines dw	  ?		   ; number of lines
b_ncols  dw	  ?		   ; number of columns
b_battr  dw	  ?		   ; border attributes
b_label  dw	  ?		   ; pointer to label text
b_args   ends

	  public  border
border    proc	  near
;;;	  int	  3	  
	  push	  BP		   ; save caller's BP
	  mov	  BP,SP

;     output corners
	  mov	  BL,byte ptr [BP].b_battr ; load attribute bits
	  mov	  DH,byte ptr [BP].b_col ; load left column number
	  mov	  DL,byte ptr [BP].b_line ; load left line number
	  dec	  DL
	  dec	  DH
	  mov	  AL,0DAh	   ; load upper left corner character
	  call	  zcorner
	  inc	  DH
	  add	  DH,byte ptr [BP].b_ncols
	  mov	  AL,0BFh	   ; load upper right corner character
	  call	  zcorner
	  inc	  DL
	  add	  DL,byte ptr [BP].b_nlines
	  mov	  AL,0D9h	   ; load lower right corner character
	  call	  zcorner
	  dec	  DH
	  sub	  DH,byte ptr [BP].b_ncols
	  mov	  AL,0C0h	   ; load lower left corner character
	  call	  zcorner

;     output sides
	  mov	  DH,byte ptr [BP].b_col   ; reload upper left column number
	  mov	  DL,byte ptr [BP].b_line  ;  and line number
	  dec	  DH		   ; decrement column number
	  mov	  CX,[BP].b_nlines
	  call	  zside 	   ; draw the left hand border
	  mov	  DH,byte ptr [BP].b_col   ; reload upper left column number
	  mov	  DL,byte ptr [BP].b_line  ;  and line number
	  add	  DH,byte ptr [BP].b_ncols ;  add in line length
	  mov	  CX,[BP].b_nlines
	  call	  zside 	   ; draw the right hand border

;     Output the top of the border
	  mov	  DL,byte ptr [BP].b_line  ; load upper left row number
	  dec	  DL
	  jl	  z_no_top	   ; if row negative, skip write
	  mov	  DH,byte ptr [BP].b_col   ; load upper left column number
	  mov	  CX,[BP].b_ncols
	  call	  ztop
;     Put the label in the top left corner of the border, if it'll fit
	  mov 	  BX,[BP].b_label  ; get segment of label
	  cmp	  BX,0
	  je	  z_no_top	   ; jump, if NULL pointer
	  mov	  ES,BX
	  mov	  BX,0		   ; ES:BX => label
	  mov	  DX,[BP].b_ncols  ; load window width
	  xor	  CX,CX 	   ; zero the character counter
b_loop:  cmp	  byte ptr ES:[BX],0  ; end of string?
	  je	  b_eos	           ; if end of string, jump
	  inc	  CX		   ; increment the character count
	  inc	  BX		   ; increment the character string pointer
	  cmp	  CX,DX 	   ; compare to window width
	  jl	  b_loop	   ; if label still shorter than window, loop
b_eos:   jcxz	  z_no_top	   ; if no label, jump
	  push	  CX		   ; save label length
;     Write the label
	  mov	  DL,byte ptr [BP].b_line ; load upper left row number
	  mov	  DH,byte ptr [BP].b_col  ; load upper left column number
	  dec	  DL		   ; decrement row number
	  xor	  BH,BH 	   ; IBMism (page 0 for text-mode)
	  mov	  AH,02h	   ; load "put cursor" code
	  call	  CRT_DSR	   ; put cursor in upper left corner of border
	  pop	  CX		   ; restore label's character count
	  cmp	  PC_MAKE,TIPC
	  jne	  ibm_cblk
	  mov	  AH,011h	   ; load "write block of characters" code
	  mov	  DX,ES 	   ; load segment address
	  mov	  BX,0		   ; load label offset
	  int	  TI_CRT	   ; write the label
	  jmp	  short z_no_top
;
ibm_cblk: mov	  AL,byte ptr [BP].b_col
	  add	  AL,CL
	  cmp	  AL,MAX_COLS
	  jle	  b_sml	           ; jump if label length is OK
	  sub	  AL,MAX_COLS
	  sub	  CL,AL 	   ; force label to remain within 80-col screen
b_sml:   mov	  DI,0             ; load label offset
lbl_loop: mov	  AH,0Eh	   ; Write ASCII Teletype
	  mov	  AL,byte ptr ES:[DI]
	  mov	  BL,byte ptr [BP].b_battr ; load attribute bits just in case
	  xor	  BH,BH 	   ; page # for alpha mode
	  push	  CX
	  push	  DI
	  int	  IBM_CRT
	  pop	  DI
	  pop	  CX
	  inc	  DI
	  loop	  lbl_loop	   ; DECrement CX and jump if != 0
;     Output the bottom of the border
z_no_top: 
	  mov	  dx,ds
	  mov	  es,dx
	  mov	  BL,byte ptr [BP].b_battr ; load attribute bits
	  mov	  DL,byte ptr [BP].b_line
	  add	  DL,byte ptr [BP].b_nlines
	  mov	  DH,byte ptr [BP].b_col   ; load upper left column number
	  mov	  CX,[BP].b_ncols
	  call	  ztop

;     return to caller
	  pop	  BP		   ; restore caller's BP
	  xor	  ax,ax
	  ret			   ; return
border  endp

;************************************************************************
;* Local Support:  Draw a single character at cursor position		*
;*									*
;* Input Registers:  AL - the character to be output			*
;*		     BL - the character attributes for the write	*
;*		     DH - column					*
;*		     DL - row						*
;*									*
;* Registers Modified:	AX,CX,SI,DI					*
;************************************************************************
zcorner   proc	  near		   ; draw a single corner character
	  cmp	  DH,MAX_COLS
	  jae	  zcornret
	  cmp	  DL,MAX_ROWS
	  jae	  zcornret
	  push	  DX		   ; save cursor coordinates
	  push	  AX		   ; save character to be output
	  xor	  BH,BH 	   ; page number (=0 for graphics mode also)
	  mov	  AH,02h	   ; load "put cursor" code
	  call	  CRT_DSR	   ; position the cursor
;     read the character in this screen position
;      ** This is tricky 'cause DH/DL are correct but
;      ** will be swapped back (to incorrect) by CRT_DSR proc
;      ** if using an IBM!!!
	  cmp	  PC_MAKE,TIPC
	  je	  no_swap
	  xchg	  DH,DL
	  xor	  BH,BH 	   ; IBM display page
no_swap:  mov	  AH,08h
	  call	  CRT_DSR
;     see if it's one of the borderline characters
	  call	  map_char
	  mov	  SI,AX
	  pop	  AX		   ; recover character to be output
	  cmp	  SI,0
	  jl	  zcornput
;     map corner to border character
	  call	  map_char
	  mov	  DL,map_tabx-map_tab-1
	  mul	  DL
	  add	  SI,AX
	  mov	  AL,trns_tab+[SI]
;     output the corner character
zcornput: mov	  AH,09h	   ; load "write character/attribute" code
	  mov	  CX,1		   ; number of characters = 1
	  xor	  BH,BH 	   ; Display page for IBM text mode (=0)
	  call	  CRT_DSR	   ; write it to the screen at cursor position
	  pop	  DX		   ; restore cursor coordinates
zcornret: ret			   ; return
zcorner   endp

;************************************************************************
;* Local Support:  Draw a border sides					*
;*									*
;* Input Registers:  DH - column					*
;*		     DL - row						*
;*		     CX - number of rows				*
;*									*
;* Registers Modified:	AX,CX,DL					*
;************************************************************************
zside	  proc	  near
	  cmp	  DH,MAX_COLS	   ; is column within the CRT's boundaries?
	  jae	  zsideret	   ; if not, jump
zside_lp: mov	  AL,0B3h	   ; load "|" border character
	  push	  CX		   ; save line count
	  push	  DX		   ; save next cursor position
	  call	  zcorner	   ; output the border character
	  pop	  DX		   ; restore current cursor position
	  pop	  CX		   ; restore line counter
	  inc	  DL		   ; increment the row number
	  loop	  zside_lp	   ; loop until side is drawn
zsideret: ret
zside	  endp

;************************************************************************
;* Local Support:  Draw a border - Top or Bottom			*
;*									*
;* Input Registers:  DH - column					*
;*		     DL - row						*
;*		     CX - number of columns				*
;*									*
;* Registers Modified:	AX,CX						*
;************************************************************************
ztop	  proc	  near
	  cmp	  DL,MAX_ROWS	   ; is row within the CRT's boundaries?
	  jae	  ztopret	   ; if not, jump
ztop_lp:  mov	  AL,0C4h	   ; load "-" border character
	  push	  CX		   ; save line count
	  push	  DX		   ; save next cursor position
	  call	  zcorner	   ; output the border character
	  pop	  DX		   ; restore current cursor position
	  pop	  CX		   ; restore line counter
	  inc	  DH		   ; increment the column number
	  loop	  ztop_lp	   ; loop until top/bottom is drawn
ztopret:  ret
ztop	  endp

;************************************************************************
;* Local Support:  return character mapping     			*
;*									*
;* Input Registers:  AL = character					*
;*									*
;* Registers Modified:	CX,DI						*
;************************************************************************
map_char  proc	  near
	  mov	  CX,map_tabx-map_tab
	  mov	  DI,offset map_tab
repne	  scasb
	  mov	  AX,CX
	  dec	  AX
	  ret
map_char  endp

;************************************************************************
;*			Save Screen Contents				*
;*									*
;* Purpose:  To save a rectangular region of the CRT in a string data	*
;*		object. 						*
;*									*
;* Calling Sequence:  save_scr(str_reg, ul_row, ul_col, n_rows, ncols)	*
;*			where str_reg - pointer to string data object	*
;*					which is to receive the screen	*
;*					contents			*
;*			      ul_row - row number of the upper left	*
;*					corner of the region to be	*
;*					saved				*
;*			      ul_col - column number of the upper left	*
;*					corner of the region to be	*
;*					saved				*
;*			      n_rows - number of rows in the region to	*
;*					be saved			*
;*			      n_cols - number of columns in the region	*
;*					to be saved			*
;************************************************************************
sv_args   struc
	  dw	  ?		   ; caller's BP
	  dw	  ?		   ; caller's ES
	  dw	  ?		   ; return address
	  dd	  ?		   ; caller's return address
sv_op	  dw	  ?		   ; save screen opcode
sv_str	  dw	  ?		   ; address of register pointing to string
sv_ulrow  dw	  ?		   ; upper left hand corner's row number
sv_ulcol  dw	  ?		   ; upper left hand corner's column number
sv_nrow   dw	  ?		   ; number of rows
sv_ncol   dw	  ?		   ; number of columns
sv_args   ends

	  public  save_scr
save_scr  proc	  near
;;;	  int     3
	  push	  ES
	  push	  BP		   ; save the caller's BP register
	  mov	  BP,SP 	   ;  and establish local addressability
;     create a pointer to the string object
;;;	  mov	  BX,[BP].sv_str   ; load address of register
;;;	  mov	  DI,[BX].C_disp   ; load the string
;;;	  mov	  BX,[BX].C_page   ;  pointer
;;;	  %LoadPage ES,BX	   ; load string page's paragraph address
;;;	  mov	  ES,pagetabl+[BX] ; load string page's paragraph address
;;;	  add	  DI,BLK_OVHD	   ; advance pointer past string header

	  mov	  ES,[BP].sv_str   
	  xor	  DI,DI		   ;ES:DI => string

;     store number of rows and columns into the first two bytes of the string
	  mov	  AL,byte ptr [BP].sv_nrow
	  stosb
	  mov	  AL,byte ptr [BP].sv_ncol
	  stosb
;     adjust number of lines/columns for test conditions
	  mov	  AX,[BP].sv_ulrow
	  add	  [BP].sv_nrow,AX
	  mov	  AX,[BP].sv_ulcol
	  add	  [BP].sv_ncol,AX
;     loop until all rows processed
	  mov	  DL,byte ptr [BP].sv_ulrow
rw_loop:  mov	  DH,byte ptr [BP].sv_ulcol
;     position cursor
cl_loop:  push	  DX		   ; save current position
	  mov	  AH,02h	   ; load "put cursor" function id
	  xor	  BH,BH 	   ; IBMism (page number for cursor)
	  call	  crt_dsr	   ; position the cursor
;     read character/attributes at current screen position
	  mov	  AH,08h	   ; load "read char/attribute" function id
	  xor	  BH,BH 	   ; IBMism (display page #)
	  call	  crt_dsr	   ; read said
;*******
	 cmp	vid_mode,14
	 jl	sav_01		   ; not graphics modes
	 cmp	AL,0		   ; don't bother with attributes if nul
	 je	sav_01
;	 cmp	AL,07fh 	   ; is it above the first 128 characters ?
;	 jno	sav_00		   ; no
	 cmp	AL,0dah
	 jbe	sav_00
;	 test	AL,010h 	   ; look for D0-DF
;	 je	sav_00
	 xor	AL,AL		   ; set to nul
	 jmp	sav_01
sav_00:  call	graph_attr	   ; mode 14 and 16 attribute function
;******
sav_01:   stosw 		   ; store char/attr into output string
;     increment column number, test, branch
	  pop	  DX
	  inc	  DH
	  cmp	  DH,byte ptr [BP].sv_ncol
	  jl	  cl_loop
;     increment row number, test, branch
	  inc	  DL
	  cmp	  DL,byte ptr [BP].sv_nrow
	  jl	  rw_loop

;     return to caller
	  pop	  BP
	  pop	  ES
	  xor	  ax,ax
	  ret			   ; return to caller
save_scr  endp

;************************************************************************
;*			Restore Screen Contents 			*
;*									*
;* Purpose:  To restore a rectangular region of the CRT from a string	*
;*		data object.						*
;*									*
;* Calling Sequence:  rest_scr(str_reg, ul_row, ul_col) 		*
;*			where str_reg - pointer to string data object	*
;*					which contains the screen	*
;*					contents			*
;*			      ul_row - row number of the upper left	*
;*					corner of the region to be	*
;*					restored			*
;*			      ul_col - column number of the upper left	*
;*					corner of the region to be	*
;*					restored			*
;************************************************************************
rs_args   struc
rs_nrow   dw	  ?		   ; number of rows in saved data
rs_ncol   dw	  ?		   ; number of columns in saved data
rs_BP	  dw	  ?		   ; caller's BP
	  dw	  ?		   ; caller's ES
	  dw	  ?		   ; return address
	  dd	  ?		   ; callers return address
rs_op	  dw	  ?		   ; restore screen opcode
rs_str	  dw	  ?		   ; segment address of string
rs_ulrow  dw	  ?		   ; upper left hand corner's row number
rs_ulcol  dw	  ?		   ; upper left hand corner's column number
rs_mrow   dw	  ?		   ; number of rows in new window
rs_mcol   dw	  ?		   ; number of columns in new window
rs_args   ends

	  public  rest_scr
rest_scr  proc	  near
   	  int	  3
	  push	  ES
	  push	  BP		   ; save the caller's BP register
	  sub	  SP,offset rs_BP
	  mov	  BP,SP 	   ;  and establish local addressability

;     address the string

	  mov	  ES,[BP].rs_str
	  xor	  SI,SI		   ; ES:SI => string

;     get number of rows and columns from screen object
	  xor	  AH,AH
	  lods	  byte ptr ES:[SI]
	  add	  AX,[BP].rs_ulrow
	  mov	  [BP].rs_nrow,AX 
	  xor	  AH,AH
	  lods	  byte ptr ES:[SI]
	  add	  AX,[BP].rs_ulcol
	  mov	  [BP].rs_ncol,AX
;     adjust number of lines/columns for test conditions
	  mov	  AX,[BP].rs_ulrow
	  add	  [BP].rs_mrow,AX
	  mov	  AX,[BP].rs_ulcol
	  add	  [BP].rs_mcol,AX
;     loop until all rows processed
	  mov	  DL,byte ptr [BP].rs_ulrow
xw_loop:  mov	  DH,byte ptr [BP].rs_ulcol
;     position cursor
xl_loop:  cmp	  DH,byte ptr [BP].rs_mcol ; column too long for new window?
	  jge	  x_long	   ; if too long, jump
	  push	  DX		   ; save current position
	  mov	  AH,02h	   ; load "put cursor" function id
	  xor	  BH,BH 	   ; IBMism (page number/0 in graphic mode)
	  call	  crt_dsr	   ; position the cursor
;     read character/attributes at current screen position
	  lods	  word ptr ES:[SI] ; fetch the character and attribute

;;;;;;;; cmp	AL,20h
;;;;;;;; je	x_sp		   ; if a space skip

	  mov	  BL,AH 	   ;  and copy attribute to BL
	  mov	  AH,09h	   ; load "write char/attribute" function id
	  xor	  BH,BH 	   ; IBMism (page number)
	  mov	  CX,1		   ; character count = 1
	  call	  crt_dsr	   ; read said
;     increment column number, test, branch
x_sp:	  pop	  DX		   ; recover the row/column coordinates
x_more:   inc	  DH		   ; increment the column number
	  cmp	  DH,byte ptr [BP].rs_ncol ; more characters in this row?
	  jl	  xl_loop	   ; if so, jump
;     increment row number, test, branch
	  inc	  DL		   ; increment the row number
	  cmp	  DL,byte ptr [BP].rs_mrow ; check against new window boundary
	  jge	  rs_fin	   ; if all rows filled, jump
	  cmp	  DL,byte ptr [BP].rs_nrow ; check against saved data
	  jl	  xw_loop	   ; if more lines, jump

;     return to caller
rs_fin:   add	  SP,offset rs_BP  ; deallocate local storage
	  pop	  BP		   ; restore the caller's BP register
	  pop	  ES		   ; restore the caller's ES register
	  xor	  ax,ax
	  ret			   ; return to caller
;
x_long:   inc	  SI		   ; increment index into saved screen
	  inc	  SI		   ;  buffer
	  jmp	  short x_more	   ; continue processing row
rest_scr  endp


;************************************************************************
;*                         Output Character To Window                   *
;************************************************************************
pch_args  struc
         dw ?                       ; caller's BP
         dw ?                       ; return address
	 dd ?			    ; caller's return address
p_op	 dw ?			    ; putchar opcode
p_line   dw ?                       ; cursor position - line number
p_col    dw ?                       ; cursor position - column number
p_char   dw ?                       ; character to write
p_attr   dw ?                       ; character's attributes
pch_args ends

         public putchar
putchar  proc   near
         push   BP                  ; save caller's BP
         mov    BP,SP
;     position cursor for write
         mov    DL,byte ptr [BP].p_line ; load line number
         mov    DH,byte ptr [BP].p_col  ; load column number
         xor    BH,BH               ; IBMism
         mov    AH,02h              ; load "put cursor" code
         call   crt_dsr             ; positio the cursor

         mov    BL,byte ptr [BP].p_attr ; load its attributes
         cmp    vid_mode,14         ; only attribute for EGA modes is a
         jl     pchar_1             ; simulated reverse video

         mov    BH,BL               ; save the attribute
         and    BH,80h              ; reverse video?
         jz     pchar_1             ; zero indicates bit 8 not set

pchar_2: and    BL,7fh              ; strip off high bit
         mov    CX,1                ; character count
         xor    BH,BH               ; video page number
         mov    AL,0dbh             ; block character
         mov    AH,09h
         call   crt_dsr
         or     BL,80h              ; set xor bit
; write the characters with attributes
pchar_1: mov    AL,byte ptr [BP].p_char ; load the character
         xor    BH,BH               ; IBMism
         mov    CX,1                ; repeat count  = 1
         mov    AH,09h              ; load write char/attribute code
         call   crt_dsr
;     return to caller
	 xor	ax,ax
         pop    BP
         ret
putchar  endp
	
;************************************************************************
;*                   Scroll Window Up one line                          *
;************************************************************************
su_args   struc
          dw      ?                ; caller's BP
          dw      ?                ; return address
	  dd	  ?		   ; caller's return address
su_op	  dw	  ?		   ; scroll opcode
su_line   dw      ?                ; upper left hand corner line number
su_col    dw      ?                ; upper left hand corner column number
su_nline  dw      ?                ; number of lines
su_ncols  dw      ?                ; number of columns
su_attr   dw      ?                ; text attributes (used for blanking)
su_args   ends

          public  scrollup
scrollup  proc    near
          push    BP               ; save caller's BP
          mov     BP,SP
;     scroll window's text up one line
          mov     CL,byte ptr [BP].su_nline ; load number of lines
          dec     CL               ; decrease number of lines by one
          jz      blank1           ; Jump if scrolling 1-line and just blank it
          mov     CH,byte ptr [BP].su_ncols    ; load number of columns
          mov     DL,byte ptr [BP].su_line     ; load upper left line number
          mov     DH,byte ptr [BP].su_col      ; load upper left column number
          mov     AX,0601h         ; load "scroll text" code with no blanking
          cmp     DGROUP:PC_MAKE,TIPC
          je      ti_scrl
          cmp     vid_mode,4       ; Are we in graphics mode?
          jl      txt_mod          ;   If we are then fix blank fill attributes
          cmp     vid_mode,7       ;   so that the bar characters don't show up
          je      txt_mod
          xor     BH,BH            ; zero attribute for fill blanks
          jmp     short rite_atr
txt_mod:  mov     BH,byte ptr [BP].su_attr ; Blanked lines' attribute txt mode
rite_atr: xchg    CX,DX            ; CX=Upper left corner
          xchg    CH,CL            ; Row,column instead of TI's column,row
          xchg    DH,DL            ; ditto
          add     DX,CX            ; DX=Lower right corner
          dec     DL               ; adjust column count (0 is first column)
          int     IBM_CRT
          jmp     short z_quit     ; IFF IBM is in graphics mode weird char's
                                   ; are used for blanks when scrolling.  Do
                                   ; as TIPC does and "manual" blank 'em.
;
ti_scrl:  mov     BX,DX            ; copy destination coordinates
          inc   DL                 ; compute source by incrementing line number
          int   TI_CRT             ; perform block move
;     paint the last line of the window with blank of proper attributes
blank1:  mov    DH,byte ptr [BP].su_col ; load starting column number
         mov    DL,byte ptr [BP].su_line ; load upper line number
         add    DL,byte ptr [BP].su_nline ; add the number of lines and
         dec    DL                  ; subtract offf one
         mov    AH,02h              ; load "put cursor" code
         xor    BH,BH               ; IBMism
         call   crt_dsr             ; position cursor for write
         mov    AX,0920h            ; load "write char/attr" code, write a blank
         mov    BL,byte ptr [BP].su_attr ; load attribute bit setting

         cmp    vid_mode,14         ; ega mode?
         jl     z_scr01
         mov    BH,BL
         and    BH,80h
         cmp    BH,80h              ; reverse video?
         jne    z_scr01
         mov    AX,09dbh            ; change for block character
         and    BL,7fh              ; strip off xor bit
z_scr01: xor    BH,BH               ; IBMism
         mov    CX,[BP].su_ncols    ; load line length
         call   crt_dsr             ; write a line of blanks
; return to caller
z_quit:  pop    BP
	 xor    ax,ax
         ret
scrollup endp


;************************************************************************
;*                   Scroll Window Down one line                        *
;************************************************************************
sd_args   struc
          dw      ?                ; caller's BP
          dw      ?                ; return address
	  dd	  ?   		   ; caller's return address
sd_op     dw      ?		   ; op code
sd_line   dw      ?                ; upper left hand corner line number
sd_col    dw      ?                ; upper left hand corner column number
sd_nline  dw      ?                ; number of lines
sd_ncols  dw      ?                ; number of columns
sd_attr   dw      ?                ; text attributes (used for blanking)
sd_args   ends

scrolldn  proc    near
          push    BP               ; save caller's BP
          mov     BP,SP
;     scroll window's text down one line
          mov     CL,byte ptr [BP].sd_nline ; load number of lines
          dec     CL               ; decrease number of lines by one
          jz      blank            ; Jump if scrolling 1-line and just blank it
          mov     CH,byte ptr [BP].sd_ncols    ; load number of columns
          mov     DL,byte ptr [BP].sd_line     ; load upper left line number
          mov     DH,byte ptr [BP].sd_col      ; load upper left column number
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
text_m:   mov     BH,byte ptr [BP].sd_attr ; Blanked lines' attribute txt mode

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
blank:    mov     DH,byte ptr [BP].sd_col ; load starting column number
          mov     DL,byte ptr [BP].sd_line ; load upper line number
          mov     AH,02h           ; load the "put cursor" code
          xor     BH,BH            ; IBMism
          call    crt_dsr          ; position cursor for write
          mov     AX,0920h         ; load "write char/attr" code, write a blank
          mov     BL,byte ptr [BP].sd_attr ; load attribute bit setting
          xor     BH,BH            ; IBMism
          mov     CX,[BP].sd_ncols  ; load line length
          call    crt_dsr          ; write a line of blanks
;     return to caller
quit:     pop     BP               ; restore caller's BP
	  xor	  ax,ax
          ret
scrolldn  endp

;****************************************************************************
;*                                                                          *
;*                            Change Video Mode                             *
;*                                                                          *
;*    Purpose: to note changes to video mode.                               *
;*                                                                          *
;****************************************************************************
chgvmode  struc
          dw      ?                ; caller's BP
          dw      ?                ; return address
	  dd	  ?   		   ; caller's return address
chg_op    dw      ?		   ; op code
chg_chgt  dw      ?                ; new character height
chg_mode  dw      ?                ; new video mode
chg_rows  dw	  ?		   ; new max rows for screen
chgvmode  ends

         public chgmode
chgmode  proc   near
	 int	3
	 push	bp
	 mov	bp,sp
	 mov	ax,[bp].chg_mode
	 mov	VID_MODE,ax
	 mov    ax,[bp].chg_chgt
	 mov	CHAR_HGT,ax
	 mov    ax,[bp].chg_rows
	 mov	MAX_ROWS,al
	 pop	bp
	 xor	ax,ax
	 ret
chgmode	 endp



;****************************************************************************
;*                                                                          *
;*                            WRITE BLOCK OF CHARACTERS                     *
;*                                                                          *
;*    Purpose: Write a character string to the display                      *
;*                                                                          *
;****************************************************************************
wblk_args struc
	  dw	?		;caller's BP
	  dw	?		;return address
	  dd	?		;caller's return address
blk_op	  dw	?		;op code
blk_len   dw	?		;length of block to write
blk_buf   db	100 dup (?)	;buffer to write
blk_txt   dw	?		;text attributes
blk_cur   dw	?		;cursor position
wblk_args ends

	  public  wblock	
wblock	  proc	  near
	  int	  3
	  push	  bp
	  mov	  bp,sp

	  mov	  ah,02h	   ;load "put cursor" code
	  xor	  bh,bh		   ;IBMism - page 0 for text mode
	  mov	  dx,[bp].blk_cur  ;dx = cursor coordinates
	  call	  crt_dsr	   ;position the cursor

	  mov	  cx,[bp].blk_len  ;cx = number of characters

	  cmp	  PC_MAKE,TIPC	   ;on what flavor PC are we running?
	  jne	  blk_ibm	   ;if an IBM, jump
;     Write line to TIPC's screen
	  mov	  al,byte ptr [bp].blk_txt ;load text attributes
	  mov	  ah,010h	   ;load "write block w/ attr" code
	  mov	  dx,ss		   ;load segment address
	  mov	  bx,bp		   ;load buffer offset in segment
	  add	  bx,blk_buf	   
	  int	  TI_CRT
	  jmp	  blk_end

;     Write line to IBM's screen
blk_ibm:  mov	  di,bp
	  add	  di,blk_buf	   ;load buffer offset	
	  mov	  dx,[bp].blk_cur  ;reverse row/column coordinates
	  xchg	  dl,dh
	  mov	  [bp].blk_cur,dx
	  push	  cx		   ; save
	  jmp	  short blk_imidl  ;jump into middle of loop

blk_iloop: 
	  push	  cx		   ;save the character counter
	  mov	  dx,[bp].blk_cur  ;load the previous cursor coordinates,
	  inc	  dl		   ;increment the column number
	  mov	  [bp].blk_cur,dx  ;and save new coordinates
	  xor	  bh,bh		   ;page number (0 for graphics mode) IBMism
	  mov	  ah,02h	   ;load "put cursor" code
	  push	  di
	  int	  IBM_CRT	   ;position the cursor
	  pop	  di
blk_imidl: 
	  mov	  ah,09h	   ;load "write char w/ attributes" code
	  mov	  al,byte ptr ss:[di] ;load character from buffer
	  mov	  BL,byte ptr [bp].blk_txt  ;load attribute bits
	  xor	  bh,bh		   ;page # for alpha mode
	  mov	  cx,1		   ;load repeat count = 1
	  pop	  dx		   ;restore character count
;     test to see if we buy anything by using a repeat count
blk_imore: 
	  cmp	  dx,1		   ;more characters to display?
	  jle	  blk_ibotm	   ;if no more characters, jump
	  cmp	  al,byte ptr ss:[di]+1   ;is next character the same as previous?
	  jne	  blk_ibotm	   ;if not same character, jump
	  inc	  cx		   ;increment the repeat count
	  inc	  di		   ;increment the output buffer index
	  inc	  byte ptr [bp].blk_cur  ;increment the cursor position
	  dec	  dx		   ;decrement the character count
	  jmp	  blk_imore	   ;try for another
blk_ibotm: 
;     output the character(s)
	  push	  dx		   ;save the adjusted character count
	  push	  di		   ;save the output buffer index
	  int     IBM_CRT	   ;output the char(s)
	  pop	  di		   ;restore the output buffer index
	  pop	  cx		   ;restore character counter
	  inc	  di		   ;increment buffer pointer
	  loop	  blk_iloop	   ;continue 'til all characters output

blk_end:
	  pop	  bp		   ;restore stack
	  ret			   ; and return
wblock	  endp

;****************************************************************************
;*                                                                          *
;*                            WRITE CHARACTER STRING                        *
;*                                                                          *
;*    Purpose: Write a character string to the given port object            *
;*                                                                          *
;****************************************************************************
wstr_args struc
          dw      ?                ; caller's BP
          dw      ?                ; return address
	  dd	  ?   		   ; caller's return address
wrt_op    dw      ?		   ; op code
;;; wrt_newl  dw	  ?		   ; newline before writing
wrt_wrap  dw	  ?		   ; check wrap before writing string
wrt_offs  dw	  ?		   ; string offset
wrt_seg	  dw	  ?		   ; string segment
wrt_len   dw      ?                ; # characters
wrt_port  dw	  ?		   ; port object
wstr_args ends

	  public  wstring
wstring   proc	  near
	  int	  3
	  push	  bp
	  mov	  bp,sp
	  cmp	  [bp].wrt_len,0		;anything to write?
	  jg	  wst_10			; yes, continue
	  jmp	  wst_fin
wst_10:

	  cmp 	  [bp+wrt_wrap],0	        ;check wrap before writing?
	  je	  wst_15    		        ; no,  continue
	  xor	  ax,ax				;ax = wrap indicator (0 = no)
	  mov	  bx,[bp+wrt_port].pt_ncols     ;bx = line length
	  cmp	  bx,0				;maintaining line length?
	  je	  wst_15    			; no,  jump
	  mov	  cx,[bp+wrt_port].pt_ccol 	;cx = current column
	  cmp	  cx,1				;in first column already?
	  jle	  wst_15       			; yes, jump
	  sub	  bx,cx				;determine space remaining
	  cmp	  bx,[bp+wrt_len]		;room left on current line?
	  jge	  wst_15    		        ; yes,  jump
	  inc	  ax				;set wrap indicator
wst_15:						
	  mov	  [bp+wrt_wrap],ax		;update wrap indicator
	  mov	  bx,[bp+wrt_port].pt_pflgs 	;get window flags
	  test	  bx,OPEN			;is port open for writing?
	  jz	  wst_esc			; no, get outa here
	  test	  bx,STRIO			;is port a string?
	  jnz	  wst_esc			; yes, return
	  test	  bx,WINDOW			;is port a window?
	  jnz	  wst_win			; yes, go to window code
          jmp	  wst_fil			; no,  go to file code
wst_esc:  jmp	  wst_fin
;*****************************************************************************
; We have a valid window port. Write the string to the display.
;*****************************************************************************
wst_win:
	  les	  di,dword ptr [bp+wrt_offs]	;es:di => string buffer

	  mov     bx,[bp+wrt_port].pt_cline     ;bx = current line
          mov     ax,[bp+wrt_port].pt_ccol	;ax = current column
          mov     dx,[bp+wrt_port].pt_ullin     ;dx = upper left line #
   	  cmp 	  [bp+wrt_wrap],0	        ;wrap before writing string?
   	  je	  wwin_start		        ; no, jump
          xor     ax,ax			        ;clear current column
          inc     bx			        ;bump current line
          cmp     bx,[bp+wrt_port].pt_nline     ;exceeded number of lines?
	  jl	  wwst_w10			;no, skip scroll
	  int	  3
	  call	  scrollit		        ;scroll the display
wwst_w10:
	  push	  ax
	  push	  di
	  push	  es:[di]		        ;next character
	  call	  isspace		        ;determine if whitespace
	  pop	  di
	  or	  ax,ax			        ;is it?
	  pop	  ax
	  jz	  wwin_start 		        ; no,  jump
	  inc	  di
	  dec	  [bp+wrt_len]		        ;decrement string length
	  jnz	  wwin_start		        ;if non-zero, go
          mov     [bp+wrt_port].pt_cline,bx     ;save current cursor line number
          mov     [bp+wrt_port].pt_ccol,ax      ;save current cursor column number
	  jmp	  wst_fin 		        ; else return
wwin_start:
	  mov	  cx,[bp+wrt_len]
;
; loop through the chars, writing them to the display
;
wwin_loop:
	  push	  cx
	  push	  di

          mov     cl,es:[di]
	  cmp	  cl,RETURN             	;if char = carriage return
;;;;;;;;  je	  wwin_creturn			;  then jump
          ja	  wwin_default			;if not control char, jump
	  xor	  ch,ch
	  mov	  si,cx
	  shl	  si,1				;get index into jump table
	  jmp	  [wn_handle+si]		;go to handler

; default character handler
;
wwin_default:
	  cmp     ax,[bp+wrt_port].pt_ncols     ;are we in the last column?
          jl      wwin_linechk			; no, jump
          test    [bp+wrt_port].pt_wflgs,W_WRAP	;wrap option on?
	  jnz	  wwin_scrl			; yes, jump
	  inc	  ax				;clip - bump column
	  jmp	  wwin_100			; continue	
wwin_linechk:
	  cmp	  bx,[bp+wrt_port].pt_nline	;out of lines?
	  jl      wwin_wchar			;no, return
wwin_scrl:
          inc     bx				;bump current line
	  xor	  ax,ax				;clear column
          cmp     bx,[bp+wrt_port].pt_nline     ;exceeded number of lines?
          jl	  wwin_wchar			; no, jump
	  call	  scrollit
wwin_wchar:
	  push	  ax				;save current column
	  push	  bx				;save current line

	  cmp	  vid_mode,3
	  jne	  wwin_gen
	  cmp	  pc_make,1
	  je	  wwin_gen

	  xchg	  ax,bx
          add     ax,[bp+wrt_port].pt_ullin
	  mov	  ah,80
	  mul	  ah
	  add	  ax,bx
          add     ax,[bp+wrt_port].pt_ulcol
	  shl	  ax,1
	  mov	  si,ax
	  mov	  ch,byte ptr [bp+wrt_port].pt_text
	  push	  ds
	  push	  0B800h
	  pop	  ds
	  mov	  [si],cx
	  pop	  ds
	  jmp	  wwin_nxt
wwin_go:
	  jmp	  wwin_loop
wwin_gen:
	  push	  [bp+wrt_port].pt_text	        ;push text character attribute
	  push	  es:[di]			;push the character
          add     ax,[bp+wrt_port].pt_ulcol	
          push    ax                    	;column number to console
          add     bx,[bp+wrt_port].pt_ullin
          push    bx                    	;line number to console
	  sub	  sp,6
          call    putchar               	;display character
	  add	  sp,14				;dump args off stack
wwin_nxt:
	  pop	  bx             		;restore current line
	  pop	  ax                		;restore current column
          inc     ax                    	;increment current column
wwin_100: mov     [bp+wrt_port].pt_cline,bx     ;save current cursor line number
wwin_120: mov     [bp+wrt_port].pt_ccol,ax      ;save current cursor column number
	  pop	  di
          inc	  di
	  pop	  cx
	  loop	  wwin_go
;;;;	  loop	  wwin_loop			;if more chars, loop
	  jmp	  wst_fin			; else go home

;
;  Handlers for special characters (RETURN,LINEFEED,TAB,BACKSPACE) 
;

; Carriage return character handler
wwin_creturn:
	  mov	  cl,LINEFEED			;fall thru to linefeed handler
; Line Feed character handler
wwin_linefeed:
          xor     ax,ax				;clear column
          inc     bx                    	;bump line number
          cmp     bx,[bp+wrt_port].pt_nline	;exceeded number of lines?
          jl	  wwin_100  			; no, jump
	  call	  scrollit			; yes, scroll
          jmp     wwin_100
; Backspace character handler
wwin_backspace:
          dec     AX				;decrement current column
          cmp     AX,0				;if column now positive
	  jge	  wwin_120			; then return
          xor     AX,AX                 	;cur_col = 0
          jmp     wwin_120			;return
; Bell character handler
wwin_bell:  
	  push	  ax
	  push	  bx
          call    bell                 		;sound the alarm
	  pop	  bx
	  pop	  ax
          jmp     wwin_120			;and return
; Tab character handler
wwin_tab:
          mov     cx,ax				;cx = current column
          mov     dx,8                  	;dl = 8
          div     dl                    	;ah = (cur_col % 8)
          sub     dl,ah
          add     cx,dx
          mov     ax,cx				;update current column
          jmp     wwin_120			;and return

;*****************************************************************************
; We have a valid file port. Write the string to the disk.
;*****************************************************************************
wst_fil:
	  int	  3
	  xor	  ax,ax				;clear ax for char later
	  les	  di,dword ptr [bp+wrt_offs]	;es:di => string buffer

	  test	  [bp+wrt_port].pt_pflgs,BINARY ;binary file?
	  jnz	  wfil_start			; yes, skip newline
	  cmp 	  [bp+wrt_wrap],0		;wrap before writing string?
	  je	  wfil_start			; no,  go write chars to disk
	  mov	  al,RETURN			; yes, write CR/LF to file
	  call	  write_char
	  xor	  ax,ax				;set current column to 0
	  call	  upd_port			;update port object
	  mov	  al,LINEFEED
	  call	  write_char
	  xor	  ax,ax				;set current column to 0
	  call	  upd_port			;update port object
	  push	  di
	  push	  es:[di]
	  call	  isspace			;determine next char whitespace
	  pop	  di
	  or	  ax,ax				;is it?
	  jz	  wfil_start 			; no, jump
	  inc	  di				;
	  dec	  [bp+wrt_len]			;decrement string length
	  jnz	  wfil_start			;if non-zero, continue
	  jmp	  wst_fin			; else return
wfil_start:
	  mov	  cx,[bp+wrt_len]
;
; loop through the chars, writing them to the display
;
wfil_loop:
	  push	  cx				;save length
	  push	  di				;save index to string

          mov     cl,es:[di]			;get character just written
	  cmp	  cl,RETURN             	;test for control char
	  ja	  wfil_default			; if not, handle as default case
	  je	  wfil_newline			; if return, go handle
	  xor	  ch,ch				;clear high byte
	  mov	  si,cx				;move char to index reg
	  shl	  si,1				;get index into jump table
	  jmp	  [ds_handle+si]		;go to handler

wfil_default:
	  mov	  cx,1				;cx = length
	  mov	  bx,[bp+wrt_port].pt_handl	;bx = handle
	  mov	  dx,di				;dx = offset
	  mov	  ax,es				;ax = segment
	  call	  diskout
          mov     ax,[bp+wrt_port].pt_ccol	;ax = current column
          cmp     ax,[bp+wrt_port].pt_ncols	;have we exceeded line length
          jl	  wfil_10 			; no, jump
	  xor	  ax,ax				;clear current column
	  jmp	  wfil_upd			;go update port data
wfil_10:  inc     ax				;bump current column
wfil_upd: call	  upd_port
	  pop	  di				;restore string index
	  inc	  di				;and increment 
	  pop	  cx				;restore length
	  loop	  wfil_loop			;loop if more
wst_fin:
	  xor	  ax,ax
	  pop	  bp
	  ret					;return to caller

;
;  Handlers for special characters (RETURN,LINEFEED,TAB,BACKSPACE)
;

; Carriage return or linefeed character handler
wfil_newline:
	  test	  [bp+wrt_port].pt_pflgs,BINARY ;binary file?
	  jnz	  wfil_default			; yes, output char
	  mov	  al,RETURN			; no,  output cr/lf
	  call	  write_char
	  xor	  ax,ax				;set current column to 0
	  call	  upd_port			;update port object
	  mov	  al,LINEFEED
	  call	  write_char
	  xor	  ax,ax
	  jmp	  wfil_upd

; backspace character handler
wfil_backspace:
	  test	  [bp+wrt_port].pt_pflgs,BINARY ;binary file?
	  jnz	  wfil_10			;yes, output char
	  mov	  al,byte ptr es:[di]		;al = backspace char
	  call	  write_char			;write it out
          mov     ax,[bp+wrt_port].pt_ccol	;ax = current column
          dec     ax				;decrement it
          cmp     ax,0
          jge     wfil_upd
          xor     ax,ax
          jmp     wfil_upd
; tab character handler
wfil_tab: 
	  test	  [bp+wrt_port].pt_pflgs,BINARY ;binary file?
	  jnz	  wfil_10			;yes, jump
	  mov	  al,byte ptr es:[di]		;al = tab char
	  call	  write_char			;write it out
          mov     ax,[bp+wrt_port].pt_ccol	;ax = current column
          mov     cx,ax
          mov     dx,8
          div     dl                       	; ah = (cur_col % 8)
          sub     dl,ah
          add     cx,dx
          mov     ax,cx
          jmp     wfil_upd


;*****************************************************************************
; Utilty routines for writing characters to display or disk
;*****************************************************************************

;UPD_PORT - update port object
;  entry: ax = current column
upd_port  proc    near
          cmp	  [bp+wrt_port].pt_ncols,0	;if line length = 0
	  je	  upd_05			; then don't maintain column
	  mov     [bp+wrt_port].pt_ccol,ax	;save current column
upd_05:
	  mov	  ax,[bp+wrt_port].pt_bfpos     ;get current buffer position
          inc     ax				;bump the position
          cmp     ax,256			;crossed chunk boundary?
          jle     upd_10  			; no, jump
	  sub	  ax,256			;ax=excess above chunk
	  inc	  [bp+wrt_port].pt_chunk	;update chunk number
upd_10:   mov	  [bp+wrt_port].pt_bfpos,ax	;update buffer position
	  ret
upd_port  endp

;WRITE_CHAR - write character to port
; entry: ax = character to write to port
write_char proc	  near
	  mov	  cx,1				;length
	  mov	  bx,[bp+wrt_port].pt_handl	;handle
	  push	  ax
	  mov	  dx,sp
	  mov	  ax,ss
	  call	  diskout
	  pop	  ax
	  ret
write_char endp


;DISKOUT - output char(s) to disk
; entry: ax=segment, bx=handle, cx=buffer length, dx=offset
; exit:  carry set = error, ax=status 
;	 errors will return to the caller of wstring
diskout	  proc	  near
	  push	  ds
	  mov	  ds,ax
	  mov	  ah,WRITEFILE		   
	  int	  DOS			   ;perform disk write
	  pop	  ds
	  jc	  diskerr
	  cmp	  ax,cx			   ;everything written?
	  je	  diskret		   ; yes, return	
	  mov	  ax,DISK_FULL        	   ;note disk full error
	  jmp	  derr_ret
diskerr:  
	  int	  3
;;;
;;; For some reason, the following doesn't work. Must be a failure by
;;; AI Architects support of Get Extended Error. In the interim, just
;;; use the return value in ax. Returning is easy, just dump local
;;; storage from the stack, pop the callers bp, and return.
;;;
;;;	  xor	  bx,bx
;;;	  mov	  AH,059h		   ;GET_EXTENDED_ERROR
;;;	  int	  DOS			   ;Extended error code in AX
derr_ret: mov	  sp,bp			   ;dump everything off stack
	  pop	  bp			   ;return to handler, error in ax
diskret:
	  ret
diskout	  endp

;ISSPACE - determine if character is whitespace
;
isspace	  proc	  near
	  pop	  di
	  pop	  ax
	  cmp	  al,' '
	  je	  issp
	  cmp	  al,9
	  jb	  isntsp
	  cmp	  al,13
	  jbe	  issp
isntsp:   xor	  ax,ax
issp:	  jmp	  di		  
isspace   endp


;Scrollit - local support to scroll window
; entry: ax = column, bx = line
; exit:  ax = column, bx = line
scrollit  proc	  near
	  cmp	  bx,[bp+wrt_port].pt_nline	;out of lines?
	  jl      scrl_ret 			;no, return
          push    [bp+wrt_port].pt_text
	  push	  [bp+wrt_port].pt_ncols
	  push	  [bp+wrt_port].pt_nline
	  push	  [bp+wrt_port].pt_ulcol
	  push	  [bp+wrt_port].pt_ullin
	  sub	  sp,6			 	;dummy args for scroll
          call    scrollup              	;scroll window up one line
          add     sp,16		        	;dump args off stack
	  mov	  bx,[bp+wrt_port].pt_nline     ;bx = number of lines
          dec     bx				;bx = line number
          xor     ax,ax				;ax = column
scrl_ret:
	  ret
scrollit  endp

wstring   endp

;************************************************************************
;*		 Perform appropriate VIDEO I/O interrupt		*
;*     Any difference in register definition should be handled by	*
;*     the caller except where DH,DL contain row,col information.	*
;************************************************************************
	  public  crt_dsr
crt_dsr   proc	  near
	  cmp	  PC_MAKE,TIPC
	  jne	  ibm_dsr
	  int	  TI_CRT
	  ret
ibm_dsr:  xchg	  DH,DL       ; Do this now instead of making special checks
	  int	  IBM_CRT     ; IBM's row,col is diff'rnt from TI's col,row
	  ret
crt_dsr   endp

z_ega    proc   near

         mov    AX,0A000h
         mov    ES,AX                   ; set ES to the video plane
         mov    AX,c_row                ; set AX to the row
         mul    char_hgt                ; multiply by the character height
         mov    BX,80                   ; multiply by 80 bytes per line
         mul    BX
         add    AX,c_col                ; add in the starting column
         mov    sav_di,AX               ; save the starting value
         xor    BX,BX                   ; use BX as a counter
         mov    DX,c_len                ; number of columns to blank

zc_03:   mov    CX,DX                   ; restore counter
         mov    DI,sav_di               ; restore index
         mov    AH,0fh
         xor    AX,AX                   ; clear AX
         cld
rep      stosb

         add    sav_di,80               ; next line
         inc    BX                      ; increment counter
         cmp    BX,char_hgt             ; done with this row?
         jne    zc_03

         xor    BX,BX                   ; clear counter
         dec    c_nrows                 ; decrement row count
         jg     zc_03                   ; if more rows, loop (jump)
         ret
z_ega    endp

;************************************************************************
;*			  Graphics Character Attribute			*
;*									*
;* Purpose:  To retrieve the attribute of a character on an IBM screen	*
;*		in a graphics mode, either 14 or 16.			*
;*									*
;************************************************************************

	 public     graph_attr
graph_attr  proc    near

	 cmp	AL,20h		; skip if a space
	 je	grphend

	 cmp	AL,00h		; skip if a null
	 je	grphend

	 cmp	AL,0dbh 	; block character?
	 je	grphend

	 push	ES
	 push	SI
	 push	AX		; save character
	 push	DX		; save row and column
	 xor	AH,AH		; clear AH
	 mov	SI,AX		; use SI as an index
	 sub	SI,21h

	 mov	AL,DL		; row
	 mul	char_hgt	; pixels per character
	 xor	BX,BX

	 mov	BL,byte ptr m18_attr[SI] ; default mode 18 adjustment
	 cmp	vid_mode,18		 ; are we in mode 18?
	 je	grph_02		   	 ;   yes, jump
	 mov	BL,byte ptr m16_attr[SI] ; default mode 16 adjustment
	 cmp	vid_mode,16		 ; are we in mode 16?
	 je	grph_02			 ;   yes, jump
	 mov	BL,byte ptr m14_attr[SI] ; must be mode 14
grph_02: 
	 add	AX,BX
	 mov	BX,80		; 80 bytes per line
	 mul	BX

	 pop	DX		; restore the column
	 xor	DL,DL		; clear the row
	 xchg	DH,DL		; set AX to the row
	 add	AX,DX
	 mov	SI,AX		; put result in SI

	 mov	AX,0a000h	; load in graphics plane
	 mov	ES,AX

	 xor	CX,CX		; clear CX

	 mov	CH,01
	 mov	AH,0
grph_03: call	get_val

	 shl	CH,1		; shift mask one bit to the left
	 inc	AH		; next plane
	 cmp	AH,3
	 jbe	grph_03

	 pop	AX		; retrieve character
	 mov	AH,CL		; set attribute byte
	 pop	SI
	 pop	ES
grphend: ret
graph_attr  endp

get_val  proc	near
	 push	AX			; save AH
	 mov	DX,3ceh 		; port addr of sequencer
	 mov	AL,04h			; index to other map mask register
	 out	DX,AL			; set index register
	 inc	DX
	 xchg	AL,AH
	 out	DX,AL			; enable bank
	 pop	AX			; restore AH
	 mov	AL,ES:[SI]
	 or	AL,AL
	 jz	get_end
	 or	CL,CH		; set attribute bit
get_end: ret
get_val  endp

; PCTYPE
;  Determine type of PC we are running on and initialize screen.
;
;  Returns upon exit:
;    Machine Type
;     	1 for TIPC or Business Pro in TI mode
;    	FF for IBM-PC
;    	FE for IBM-PC/XT
;    	FD for IBM-PC/jr
;    	FC for IBM-PC/AT or B-P in IBM mode
;    	F8 for PS2 Model 80
;     	0 for undeterminable
;    Video Mode
;    Character Height
;
pctype    proc	  near
	  push	  es		   ; preserve regs for later
	  push	  ds

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
; tipc, initialize graphics
	  mov     di,0DF01h
	  mov	  es,di		   	    ; clear graphics planes
	  xor	  di,di
	  mov	  byte ptr es:[di],0AAh	    ; set red palette
	  mov	  byte ptr es:[di]+16,0CCh  ; set green palette
	  mov	  byte ptr es:[di]+32,0F0h  ; set blue palette

	  mov     ax,0DF82h
	  mov	  es,ax
	  mov	  byte ptr es:[di],040h     ; turn text on

	  mov	  ax,3			    ; ax = video mode
	            			    ; bx = pc type code
	  mov	  cx,8			    ; cx = character height
	  jmp	  pc_020			
; ibm, (assumed) get current video mode
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
	  pop	  es		   ; es:di addresses transaction buffer
	  xor	  di,di

	  mov	  PC_MAKE,bx	 ; put PC_MAKE in transaction buffer
	  mov	  VID_MODE,ax	 ; ditto video mode
	  mov	  CHAR_HGT,cx    ; ditto char height
	  ret
pctype    endp


	  page
;-----------------------------------------------------------------------------
;	The XLI interface.
;-----------------------------------------------------------------------------

main    proc    far			;this file's initial entry point
	mov	AX,data
	mov	DS,AX
;	mov	AX,stack		;establish local stack
;	mov	SS,AX

	call	pctype			;initialize type/monitor info

	mov	psp,ES			;save PSP@
	mov	word ptr ES:fb_addr,offset file_block	;poke file block@
	mov	word ptr ES:fb_addr+2,seg file_block    ;into PSP
	mov	AX,ES:term_addr		;calc ptrs in PCS to jump to
	add	AX,3
	mov	xwait,AX
	add	AX,3
	mov	xbye,AX
	mov	AX,ES:term_addr+2
	mov	xwait+2,AX
	mov	xbye+2,AX
	mov	psize,plen		;calc program size
	push	psp
	push	psize
	call	dword ptr [xwait]	;connect with PCS
; Since this is a XLI SYSINT routine, no XCALL's ever cause a return.
; The only time we return is to terminate.
	pop	AX
	pop	AX
	call	dword ptr [xbye]	;disconnect from PCS
main    endp

progsize =	$-progstart
plen	equ	(progsize+datasize+stacksize+100h+10h)/16
PROG	ends
	end	main
