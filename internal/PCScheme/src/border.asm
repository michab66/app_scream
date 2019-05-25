;							=====> BORDER.ASM
;***************************************
;*     TIPC Scheme  Runtime Support    *
;*	 Window Support Routines       *
;*				       *
;*    (C) Copyright 1985 by Texas      *
;*     Instruments Incorporated.       *
;*	  All rights reserved.	       *
;*				       *
;* Date Written:  16 May 1985	       *
;* Last Modification:		       *
;*   14 April 1986 :		       *
;*	 Make references to pagetabl   *
;*	 call Memory Manager for use   *
;*	 with extended/expanded mem.   *
;*   26 Sept  1986 :		       *
;*	 added EGA support	       *
;*   13 May 1987 :		       *
;*	 Fixed Save/restore problem.   *
;***************************************
	  page	  60,132
	  include scheme.equ
	  include pcmake.equ

MSDOS	  equ	  021h
TI_CRT	  equ	  049h
IBM_CRT   equ	  010h


DGROUP	  group   data
data	  segment word public 'DATA'
	  assume  DS:DGROUP
	  extrn	  MAX_ROWS:byte,MAX_COLS:byte


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

last_char   db	    0dbh

	 extrn	char_hgt:byte
	 extrn	vid_mode:word
data	  ends

XGROUP	  group   PROGX
PROGX	  segment byte public 'PROGX'
	  assume  CS:XGROUP

;************************************************************************
;*		 Perform appropriate VIDEO I/O interrupt		*
;*     Any difference in register definition should be handled by	*
;*     the caller except where DH,DL contain row,col information.	*
;************************************************************************
	  public  crt_dsr
crt_dsr   proc	  far
	  cmp	  PC_MAKE,TIPC
	  jne	  ibm_dsr
	  int	  TI_CRT
	  ret
ibm_dsr:  xchg	  DH,DL       ; Do this now instead of making special checks
	  int	  IBM_CRT     ; IBM's row,col is diff'rnt from TI's col,row
	  ret
crt_dsr   endp

;************************************************************************
;*			     Draw Border				*
;************************************************************************
zb_args  struc
	  dw	  ?		   ; caller's BP
	  dd	  ?		   ; return address (far linkage)
	  dw	  ?		   ; return address (original)
zb_line   dw	  ?		   ; upper left corner line number
zb_col	  dw	  ?		   ; upper left corner column number
zb_nlines dw	  ?		   ; number of lines
zb_ncols  dw	  ?		   ; number of columns
zb_battr  dw	  ?		   ; border attributes
zb_label  dw	  ?		   ; pointer to label text
zb_args   ends

	  public  z%border
z%border  proc	  far
	  push	  BP		   ; save caller's BP
	  mov	  BP,SP

;     output corners
	  mov	  BL,byte ptr [BP].zb_battr ; load attribute bits
	  mov	  DH,byte ptr [BP].zb_col ; load left column number
	  mov	  DL,byte ptr [BP].zb_line ; load left line number
	  dec	  DL
	  dec	  DH
	  mov	  AL,0DAh	   ; load upper left corner character
	  call	  zcorner
	  inc	  DH
	  add	  DH,byte ptr [BP].zb_ncols
	  mov	  AL,0BFh	   ; load upper right corner character
	  call	  zcorner
	  inc	  DL
	  add	  DL,byte ptr [BP].zb_nlines
	  mov	  AL,0D9h	   ; load lower right corner character
	  call	  zcorner
	  dec	  DH
	  sub	  DH,byte ptr [BP].zb_ncols
	  mov	  AL,0C0h	   ; load lower left corner character
	  call	  zcorner

;     output sides
	  mov	  DH,byte ptr [BP].zb_col ; reload upper left column number
	  mov	  DL,byte ptr [BP].zb_line ;  and line number
	  dec	  DH		   ; decrement column number
	  mov	  CX,[BP].zb_nlines
	  call	  zside 	   ; draw the left hand border
	  mov	  DH,byte ptr [BP].zb_col ; reload upper left column number
	  mov	  DL,byte ptr [BP].zb_line ;  and line number
	  add	  DH,byte ptr [BP].zb_ncols ;  add in line length
	  mov	  CX,[BP].zb_nlines
	  call	  zside 	   ; draw the right hand border

;     Output the top of the border
	  mov	  DL,byte ptr [BP].zb_line ; load upper left row number
	  dec	  DL
	  jl	  z_no_top	   ; if row negative, skip write
	  mov	  DH,byte ptr [BP].zb_col ; load upper left column number
	  mov	  CX,[BP].zb_ncols
	  call	  ztop
;     Put the label in the top left corner of the border, if it'll fit
	  mov	  BX,[BP].zb_label ; load pointer to the label's text
	  cmp	  BX,0		   ; if pointer NULL, no label
	  je	  z_no_top	   ; jump, if NULL pointer
	  mov	  DX,[BP].zb_ncols ; load window width
	  xor	  CX,CX 	   ; zero the character counter
zb_loop:  cmp	  byte ptr [BX],0  ; end of string?
	  je	  zb_eos	   ; if end of string, jump
	  inc	  CX		   ; increment the character count
	  inc	  BX		   ; increment the character string pointer
	  cmp	  CX,DX 	   ; compare to window width
	  jl	  zb_loop	   ; if label still shorter than window, loop
zb_eos:   jcxz	  z_no_top	   ; if no label, jump
	  push	  CX		   ; save label length
;     Write the label
	  mov	  DL,byte ptr [BP].zb_line ; load upper left row number
	  mov	  DH,byte ptr [BP].zb_col ; load upper left column number
	  dec	  DL		   ; decrement row number
	  xor	  BH,BH 	   ; IBMism (page 0 for text-mode)
	  mov	  AH,02h	   ; load "put cursor" code
	  call	  CRT_DSR	   ; put cursor in upper left corner of border
	  pop	  CX		   ; restore label's character count
	  cmp	  PC_MAKE,TIPC
	  jne	  ibm_cblk
	  mov	  AH,011h	   ; load "write block of characters" code
	  mov	  DX,DS 	   ; load segment address
	  mov	  BX,[BP].zb_label ; load label offset
	  int	  TI_CRT	   ; write the label
	  jmp	  short z_no_top
;
ibm_cblk: mov	  AL,byte ptr [BP].zb_col
	  add	  AL,CL
	  cmp	  AL,MAX_COLS
	  jle	  zb_sml	   ; jump if label length is OK
	  sub	  AL,MAX_COLS
	  sub	  CL,AL 	   ; force label to remain within 80-col screen
zb_sml:   mov	  DI,[BP].zb_label ; load label offset
lbl_loop: mov	  AH,0Eh	   ; Write ASCII Teletype
	  mov	  AL,byte ptr [DI]
	  mov	  BL,byte ptr [BP].zb_battr ; load attribute bits just in case
	  xor	  BH,BH 	   ; page # for alpha mode
	  push	  CX
	  push	  DI
	  int	  IBM_CRT
	  pop	  DI
	  pop	  CX
	  inc	  DI
	  loop	  lbl_loop	   ; DECrement CX and jump if != 0
;     Output the bottom of the border
z_no_top: mov	  BL,byte ptr [BP].zb_battr ; load attribute bits
	  mov	  DL,byte ptr [BP].zb_line
	  add	  DL,byte ptr [BP].zb_nlines
	  mov	  DH,byte ptr [BP].zb_col ; load upper left column number
	  mov	  CX,[BP].zb_ncols
	  call	  ztop

;     return to caller
	  pop	  BP		   ; restore caller's BP
	  ret			   ; return
z%border  endp

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
	  dd	  ?		   ; return address (long)
;	  dw	  ?		   ; original return address (short)
sv_str	  dw	  ?		   ; address of register pointing to string
sv_ulrow  dw	  ?		   ; upper left hand corner's row number
sv_ulcol  dw	  ?		   ; upper left hand corner's column number
sv_nrow   dw	  ?		   ; number of rows
sv_ncol   dw	  ?		   ; number of columns
sv_args   ends

	  public  save%scr
save%scr  proc	  far
	  push	  ES
	  push	  BP		   ; save the caller's BP register
	  mov	  BP,SP 	   ;  and establish local addressability
;     create a pointer to the string object
	  mov	  BX,[BP].sv_str   ; load address of register
	  mov	  DI,[BX].C_disp   ; load the string
	  mov	  BX,[BX].C_page   ;  pointer
	  %LoadPage ES,BX	   ; load string page's paragraph address
;;;	  mov	  ES,pagetabl+[BX] ; load string page's paragraph address
	  add	  DI,BLK_OVHD	   ; advance pointer past string header
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
sav_00:  call	graph_attr	   ; mode 14,16, and 18 attribute function
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
	  ret			   ; return to caller
save%scr  endp

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
	  dd	  ?		   ; return address (long)
;	  dw	  ?		   ; original return address (short)
rs_str	  dw	  ?		   ; address of register pointing to string
rs_ulrow  dw	  ?		   ; upper left hand corner's row number
rs_ulcol  dw	  ?		   ; upper left hand corner's column number
rs_mrow   dw	  ?		   ; number of rows in new window
rs_mcol   dw	  ?		   ; number of columns in new window
rs_args   ends

	  public  rest%scr
rest%scr  proc	  far
	  push	  ES
	  push	  BP		   ; save the caller's BP register
	  sub	  SP,offset rs_BP
	  mov	  BP,SP 	   ;  and establish local addressability
;     create a pointer to the string object
	  mov	  BX,[BP].rs_str   ; load address of register
	  mov	  SI,[BX].C_disp   ; load the string
	  mov	  BX,[BX].C_page   ;  pointer
	  %LoadPage ES,BX	   ; load string page's paragraph address
;;;	  mov	  ES,pagetabl+[BX] ; load string page's paragraph address
	  add	  SI,BLK_OVHD	   ; advance pointer past string header
;     recover number of rows and columns from screen object
	  xor	  AX,AX
	  lods	  byte ptr ES:[SI]
	  add	  AX,[BP].rs_ulrow
	  mov	  [BP].rs_nrow,AX
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
	  ret			   ; return to caller
;
x_long:   inc	  SI		   ; increment index into saved screen
	  inc	  SI		   ;  buffer
	  jmp	  short x_more	   ; continue processing row
rest%scr  endp

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
	 cmp	vid_mode,18	; are we in mode 18?
	 je	grph_02		;   yes, jump
	 mov	BL,byte ptr m16_attr[SI] ; default mode 16 adjustment
	 cmp	vid_mode,16	; are we in mode 16?
	 je	grph_02		;   yes, jump
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

PROGX	  ends
	  end
