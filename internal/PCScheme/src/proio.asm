;                                                       =====> PROIO.ASM
;********************************************************
;*     	        Scheme Runtime Support         	        *
;*     	    Low level I/O Support Routines    	        *	
;*                                      		*
;*    		(C) Copyright 1985 by Texas       	*
;*     		 Instruments Incorporated.        	*
;*        	   All rights reserved.         	*
;*			                                *
;* Date Written:  09 November 1987      		*
;* Last Modification:                   		*
;********************************************************
          page    60,132
	  .286c
	  include sinterp.arg
	  include memtype.equ
          include scheme.equ
          include pcmake.equ
	  include rpc.equ
	  include realio.equ
	  include xli_pro.mac

;
; local equates
;
EXT_ERR		equ	059h		 ;get extended error
TI_CRT	  	equ	049h		 ;ti  video bios interrupt
IBM_CRT   	equ	010h		 ;ibm video bios interrupt

CURSMASK  	equ     10011111b        ;zeros are the bits that disable cursor
NOCURSOR  	equ     00100000b        ;byte mask to disable cursor

;------------------------------------------------------------------------------
;
; Data Definitions
;
;-------------------------------------------------------------------------------

DGROUP    group   data
data      segment word public 'DATA'
          assume  DS:DGROUP
;from xli_pro.asm
	  extrn  rpc_handle:byte
          extrn  REAL_MODE_BUFFER:dword
	  extrn  REAL_BUF_OFFSET:word,REAL_BUF_SELECTOR:word
	  extrn	 REAL_BUF_PARA:word,REAL_BUF_TOP:WORD
;from ???
	  extrn	 port_pg:word,port_ds:word,port_r:dword
;from proio.asm
	  extrn	 char_hgt:word,cur_off:word
;from prowin.asm
	  extrn	 MAX_ROWS:byte,MAX_COLS:byte

	  public  zapcurs
zapcurs   dw	  0		   ; for disabling cursor altogether
curs_sav  dw	400Ch		   ; Cache for cursor size


local_pds dw	0		    ; Local copy of port disp
local_ppg dw	0		    ; Local copy of port page

    	  public pro_msb
pro_msb	  dw 0,0,0,0,0,0,0,0,0	    ; Machine State Block for crt_dsr

sfp_err   db      "SET-FILE-POSITION!",0

;
; Graphics are implemented via the RPC mechanism, the following data
; structures support the %graphics primitives.
;
	     public vid_mode
vid_mode     dw	 3	
graphic_go   db  3,0,0,0,1,1,0,0,0	; graphics functions which return vals
m_graph      db	  "%GRAPHICS",0

data      ends


;------------------------------------------------------------------------------
;
; Code Definitions
;
;-------------------------------------------------------------------------------

PGROUP    group   prog
prog      segment byte public 'PROG'
          assume  CS:PGROUP

	  extrn	  next_sp:near, src_err:near

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
no_args   struc
          dw      ?                ; caller's BP
          dw      ?                ; return address
no_arg	  dw	  ?		   ; designates no args on stack
no_args	  ends
          public  zbell
zbell     proc    near
	  push	  bp
	  mov	  bp,sp
	  REALIO  REAL_BELL,no_arg,no_arg,continue
	  pop	  bp
          ret
zbell     endp

;************************************************************************
;*                           Clear a Window                             *
;************************************************************************
zc_args   struc
          dw      ?                ; caller's BP
          dw      ?                ; return address
zc_row    dw      ?                ; upper left hand corner row number
zc_col    dw      ?                ; upper left hand corner column number
zc_nrows  dw      ?                ; number of rows
zc_len    dw      ?                ; line length (number of characters)
zc_attrib dw      ?                ; character attributes
zc_args   ends

          public  zclear
zclear    proc    near
          push    BP               ; save caller's BP
          mov     BP,SP
	  pusha
	  REALIO  REAL_CLEAR,zc_row,zc_attrib,continue
	  popa
	  pop	 bp
	  ret
zclear    endp

;************************************************************************
;*                           Draw Border                                *
;************************************************************************
zb_args  struc
          dw      ?                ; caller's BP
          dw      ?                ; return address
zb_line   dw      ?                ; upper left corner line number
zb_col    dw      ?                ; upper left corner column number
zb_nlines dw      ?                ; number of lines
zb_ncols  dw      ?                ; number of columns
zb_battr  dw      ?                ; border attributes
zb_label  dw      ?                ; pointer to label text
zb_args   ends

          public  zborder
zborder   proc    near
	  push	  bp
	  mov	  bp,sp
	  mov	  si,[bp].zb_label 		;ds:si => label
	  cmp	  byte ptr [si],0		;is it null?
	  jne	  zb_010			; no,  jump
	  mov	 [bp].zb_label,0		; yes, note null	
	  jmp	  zb_020	  		;      and skip
zb_010:
	  mov	  ax,REAL_BUF_PARA
	  mov	  [bp].zb_label,ax 		;seg addr of real buffer
;determine length of label
	  xor	  ax,ax		   
	  xor	  cx,cx				;hold count
zb_loop:
	  inc	  cx
	  lodsb	  
	  cmp	  ah,al
	  jnz	  zb_loop
	  sub	  si,cx				;ds:si => label
;move label to real mode buffer @ address 0
	  les	  di,REAL_MODE_BUFFER		;es:si => real buffer
	  xor	  di,di
	  mov	  ax,BLOCK_XFER
	  int	  DOS
	  push	  ds
	  pop	  es
;now do the real mode I/O call
zb_020:
	  REALIO  REAL_BORDER zb_line,zb_label,continue
	  pop	  bp
          ret
zborder   endp

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
sv_str	  dw	  ?		   ; address of register pointing to string
sv_ulrow  dw	  ?		   ; upper left hand corner's row number
sv_ulcol  dw	  ?		   ; upper left hand corner's column number
sv_nrow   dw	  ?		   ; number of rows
sv_ncol   dw	  ?		   ; number of columns
sv_args   ends

          public  save_scr
save_scr  proc    near
	  push	  es
	  push	  bp
	  mov	  bp,sp

	  push	  [bp].sv_str		;save register for later
	  mov	  ax,REAL_BUF_PARA
	  mov	  [bp].sv_str,ax	;seg addr of real mode buffer

	  REALIO  REAL_SAVESCR,sv_str,sv_ncol

	  pop	  bx			;restore register ptr
	  mov	  di,[bx].C_disp
	  mov	  bx,[bx].C_page
	  loadPage es,bx
	  add	  di,BLK_OVHD			;es:di => string

	  mov	  ax,[bp].sv_ncol		;determine # chars to copy
	  mul	  [bp].sv_nrow
	  mov	  cx,2
	  mul	  cx
	  add	  ax,2				;add for row/col info
	  mov	  cx,ax

	  mov	  ax,REAL_BUF_SELECTOR
	  mov	  ds,ax
	  xor	  si,si			;ds:si => real mode buffer
	  mov	  AX,BLOCK_XFER		;move real string into heap
	  int	  dos

	  mov	  bx,ss			;restore local data seg
	  mov	  ds,bx
	  pop	  bp 			;restore regs
	  pop	  es
          ret				;return
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
   	  dw	  ?		   ; caller's BP
	  dw	  ?		   ; caller's ES
	  dw	  ?		   ; return address
rs_str	  dw	  ?		   ; address of register pointing to string
rs_ulrow  dw	  ?		   ; upper left hand corner's row number
rs_ulcol  dw	  ?		   ; upper left hand corner's column number
rs_mrow   dw	  ?		   ; number of rows in new window
rs_mcol   dw	  ?		   ; number of columns in new window
rs_args   ends

       public  rest_scr
rest_scr  proc    near
	  push	  es
	  push	  bp
	  mov	  bp,sp
	  
	  mov	  bx,[bp].rs_str		;register holding string ptr
	  mov	  si,[bx].C_disp
	  mov	  bx,[bx].C_page
	  loadpage ds,bx
	  mov	  cx,word ptr ds:[si]+1		;cx = string length
	  add	  si,BLK_OVHD			;ds:si => string object
	  mov	  es,ss:REAL_BUF_SELECTOR
	  xor	  di,di				;es:di => real mode buffer
	  mov	  ax,BLOCK_XFER
	  int	  dos	

	  mov	  ax,ss
	  mov	  ds,ax				;restore data seg

	  mov	  ax,REAL_BUF_PARA		;replace string reg with addr
	  mov	  [bp].rs_str,ax		;of real mode buffer
	  
          REALIO  REAL_RESTSCR,rs_str,rs_mcol,continue

	  pop	  bp
	  pop	  es
          ret
rest_scr  endp

;************************************************************************
;*                              Cursor On                               *
;************************************************************************
          public  zcuron
zcuron    proc    near
	  cmp	  zapcurs,0	   ; if cursor disabled
	  jne	  zcon_ret	   ;  then return
          mov     cx,curs_sav      ; attributes for cursor on
          mov     ah,01h           ; load "set cursor type" code
          call    far ptr crt_dsr  ; turn the cursor on
zcon_ret:
          ret                      ; return to caller
zcuron    endp

;************************************************************************
;*                              Cursor Off                              *
;************************************************************************
          public  zcuroff
zcuroff   proc    near
	  push	  bp
	  mov	  bp,sp

          call   ega_curs

          mov     ah,03
          xor     bh,bh            ;IBM page number/must be 0 for graphics mode
          call    far ptr crt_dsr      ;get the cursor position/mode
	  cmp	  zapcurs,0	   ; if cursor disabled
	  jne	  zcoff_01	   ;  then jump
          mov     curs_sav,cx      ;save it for restoration
zcoff_01:
          and     ch,CURSMASK      ;mask off bits to select cursor type
          or      ch,NOCURSOR      ;disables cursor (turns it off)
          mov     ah,01h           ;load "set cursor type" code
          call    far ptr crt_dsr      ;turn the cursor off
	  pop	  bp
          ret                      ;return to caller
zcuroff   endp

;************************************************************************
;*                           Put Cursor                                 *
;************************************************************************
zpc_args  struc
          dw      ?                ; caller's BP
          dw      ?                ; return address
zpc_row   dw      ?                ; upper left hand corner row number
zpc_col   dw      ?                ; upper left hand corner column number
zpc_args  ends
          public  zputcur
zputcur   proc    near
          push    bp               ; save caller's BP
          mov     bp,sp
;     put cursor in desired location
          mov     dh,byte ptr [bp].zpc_col   ;load column number
          mov     dl,byte ptr [bp].zpc_row    ;load row number
          xor     bh,bh            ;IBMism: page number (0 if in graphics mode)
          mov     ah,02H           ;load "put cursor" code
          call    far ptr crt_dsr      ;position the cursor (DSR swaps DH/DL)
          call    ega_curs          ;display cursor for ega mode
;     Return to caller
          pop     bp               ; restore caller's BP
          ret                      ; return
zputcur   endp

;************************************************************************
;*                         Output Character To Window                   *
;************************************************************************
zp_args  struc
         dw ?                       ; caller's BP
         dw ?                       ; return address
zp_line  dw ?                       ; cursor position - line number
zp_col   dw ?                       ; cursor position - column number
zp_char  dw ?                       ; character to write
zp_attr  dw ?                       ; character's attributes
zp_args  ends

         public zputc
zputc    proc   near
         push   BP                      ; save caller's BP
         mov    BP,SP
	 pusha
	 REALIO REAL_PUTC,zp_line,zp_attr,continue
	 popa
	 pop	BP
         ret
zputc    endp

;************************************************************************
;*                         Scroll a Window                              *
;************************************************************************
zs_args   struc
          dw      ?                ; caller's BP
          dw      ?                ; return address
zs_line   dw      ?                ; upper left hand corner line number
zs_col    dw      ?                ; upper left hand corner column number
zs_nline  dw      ?                ; number of lines
zs_ncols  dw      ?                ; number of columns
zs_attr   dw      ?                ; text attributes (used for blanking)
zs_args   ends

          public  zscroll
zscroll   proc    near
          push    BP               ; save caller's BP
          mov     BP,SP
	  pusha
	  REALIO  REAL_SCROLLUP,zs_line,zs_attr,continue
	  popa
          pop    BP
          ret
zscroll  endp

;************************************************************************
;*                   Scroll Window Down one line                        *
;************************************************************************
s_args    struc
          dw      ?                ; caller's BP
          dw      ?		   ; return address
s_line    dw      ?                ; upper left hand corner line number
s_col     dw      ?                ; upper left hand corner column number
s_nline   dw      ?                ; number of lines
s_ncols   dw      ?                ; number of columns
s_attr    dw      ?                ; text attributes (used for blanking)
s_args    ends

          public  scroll_d
scroll_d  proc    near
          push    BP               ; save caller's BP
          mov     BP,SP
	  pusha
	  REALIO  REAL_SCROLLDN,s_line,s_attr,continue
	  popa
          pop    BP
          ret
scroll_d endp


;************************************************************************
;*                   Emulate cursor in EGA mode                         *
;*									*
;* On Entry: ES:SI points to port object				*
;************************************************************************
	  public  ega_curs
ega_curs  proc	  near
	  push	  bp
	  mov	  bp,sp

          cmp     vid_mode,14      ; are we in in EGA mode?
          jl      ega_03	   ;   no, return

          test    cur_off,7fh      ; cursor already off? (bit one zero)
          jz      ega_02           ;   yes, jump
          and     cur_off,0feh     ; turn off bit one
          jmp    ega_03

ega_02:  cmp    es:[si].pt_text,0  ; black attribute?
         je     ega_03             ; forget it

; set up BIOS call for ega cursor
	 mov	AX,09DBh	   ; reverse-video block 
	 mov	BX,8Fh		   ; attr = xor,white
	 mov	CX,1		   ; repetition count = 1
	 int	10h
ega_03:  
	 pop	  bp
	 ret
ega_curs endp

;************************************************************************
;*                   Note Changes to Video Mode                         *
;************************************************************************
vm_chg    struc
          dw      ?                ; caller's BP
          dw      ?		   ; return address
vm_chgt   dw      ?                ; new video mode
vm_mode   dw      ?                ; new character height
vm_rows   dw	  ?		   ; new # rows for screen
vm_chg    ends

	  public  chg_vmode
chg_vmode proc	  near
	  push	  bp
	  mov	  bp,sp
	  REALIO  REAL_CHGVMODE,vm_chgt,vm_rows,continue
	  pop	  bp
	  ret
chg_vmode endp


;************************************************************************
;GVCHARS - display characters
;
; Upon Entry:
;	cx    =  number of characters
;	dx    =  wrap flag (0 = don't check for wrap, else check for wrap)
;	es:di => print buffer
;
;************************************************************************

	public  gvchars
gvchars proc	near
	push	cx			;character count
	push	REAL_BUF_PARA		;buffer segment
	push	di			;buffer offset
	push	dx			;wrap indicator
	push	REAL_WRTSTRNG		;op code
;
; Warning: DS does not reference data segment in code below
;
	mov	cx,pt_bfend		;cx = number bytes to write
	mov	si,port_ds
	LoadPage ds,port_pg		;ds:si => port object
gv_again:
	mov	ss:local_pds,si		;save port address locally
	mov	ss:local_ppg,ds
	mov	di,ss:REAL_BUF_TOP      ;get top address of buffer
	sub	di,cx        		;es:di => buffer area
	mov	ax,BLOCK_XFER		;xfer port object to real memory	
	int	DOS
	mov	ax,ss			;restore local data segment
	mov	ds,ax
; stack at this point contains opcode, wrap, buffer offset/seg and length
	mov	cx,10			;move 10 bytes
	sub	di,cx			;es:di => real buffer
	mov	si,sp			;ds:si => args
	mov	ax,BLOCK_XFER		;xfer arg data to real memory	
	int	DOS
; issue call to real mode I/O handler
	mov	al,rpc_handle		;real procedure handle
	mov	ah,RPC			;rpc function call
	push	di			;stack pointer
	push	XLI_REALIO	        ;real i/o function designator
	mov	dx,sp			;ds:dx => rpc buffer
	mov	cx,4			;cx = # bytes in rpc buffer
	mov	bx,2  			;bx = number return bytes
	int	DOS 			;issue RPC - I/O request
	pop	ax			;ax = result status
	add	sp,2			;dump other arg from stack
	or	ax,ax			;test result status
	jnz	disk_err		;go report error
; update port object
	mov	cx,pt_bfend		;cx = number bytes to fetch
	mov	si,REAL_BUF_TOP
	sub	si,cx
	mov	bx,es
	les	di,dword ptr local_pds	;es:di => port object
	mov	ds,bx			;ds:si => updated port data
	mov	ax,BLOCK_XFER		;xfer block to scheme heap
	int	DOS
; everything is written, is there a transcript file?
	cmp	ss:TRNS_pag,0		    ;transcript file associated?
	je	gvch_ret		    ; no, return
	test	es:[di].pt_pflgs,TRANSCRI   ;this port have bit set?
	jz	gvch_ret		    ; no, return

	mov	bx,ds
	mov	cx,pt_bfend		    ;cx = number bytes to write
	mov	es,bx			    ;es => real buffer
	mov	si,ss:TRNS_dis
	LoadPage ds,ss:TRNS_pag		    ;ds:si => port object
	jmp	gv_again		    ;stack still contains orig argments
;
; Warning: DS does not reference data segment in above code
;
gvch_ret:
	mov	bx,ss			;restore data segment
	mov	ds,bx

	add	sp,10	     		;dump args off stack
	xor	ax,ax			;return status = 0
	ret				;return to caller

public disk_err
; ax= dos error, or if negative - disk full
disk_err:
	jns	der_01
	mov	ax,DISK_FULL_ERROR
	jmp	der_02
der_01:	add	ax,(IO_ERRORS_START - 1);make into scheme error
der_02: mov	bx,1			;non-restartable
	lea	cx,port_r		;port object
	pushm  <cx,ax,bx>		;invoke scheme error handler
        call    dos_err		        ;control will not return

gvchars   endp


MSDOS     equ     021h
TI_CRT    equ     049h
IBM_CRT   equ     010h
TI_KEYBD  equ     04Ah
IBM_KEYB  equ     016h

;************************************************************************
;*                      Character at Keyboard ?                         *
;*                                                                      *
;* Our equivalent to Lattic C's kbhit function                          *
;*									*
;************************************************************************
	  public  char_rdy
char_rdy  proc    near

          mov     ah,01h           ; load "check keyboard status" function code

	  IFNDEF  PROMEM	   ;;;; PROTECTED MODE will ignore
          cmp     pc_make,TIPC     ; TI or IBM flavored PC?
          jne     zch_IBM
          int     TI_KEYBD         ; issue TI keyboard DSR service call
          jz      zch_no           ; is character buffered? if not, jump
	  ELSE
	  jmp	  zch_IBM
	  ENDIF
zch_yes:  xor     AH,AH            ; clear high order byte of AX
          cmp     AL,0             ; test next character to be read
          jne     zch_ret          ; binary zero?  if not, jump
          mov     AX,256           ; if character is 0, make it non-zero
zch_ret:  ret                      ; return (true)

zch_IBM:  int     IBM_KEYB         ; issue IBM keyboard DSR service call
          jnz     zch_yes          ; is character buffered?  if so, jump
zch_no:   xor     AX,AX            ; set result = false
          ret                      ; return (false)
char_rdy  endp

;************************************************************************
;*                      Buffered Keyboard Input                         *
;*                                                                      *
;* Calling Sequence:  ch = getch();                                     *
;*                      where ch - the character read from the keyboard *
;************************************************************************
          public  getch
getch     proc    near
	  push	  si
	  push	  di
          mov     AH,07h           ; function code = Direct Console Input
          int     MSDOS            ; do it
          xor     AH,AH            ; clear the high order byte
	  pop	  di
	  pop	  si
          ret                      ; return to caller
getch     endp

;************************************************************************
;*                      Get Extended Error Information                  *
;*                                                                      *
;*  Use the Dos function to get extended error information when error   *
;*  reported on DOS I/O.						*
;************************************************************************
;;          public  get_io_err
;;get_io_err proc    near
;;
;;          mov     AH,EXT_ERR       ; function code = get extended error
;;          int     MSDOS            ; ax will contain error number
;;          stc			   ; set carry flag
;;          ret                      ; return to caller
;;get_io_err endp

;************************************************************************
;*                           Create a File                              *
;*                                                                      *
;* Calling sequence:  stat = zcreate(handle, pathname)                  *
;*                      where:  int *handle - location to store handle  *
;*                                              returned by open request*
;*                              char *pathname - zero terminated string *
;*                                              containing the file's   *
;*                                              pathname                *
;*                              int stat - the completion code          *
;*                                              0=no errors             *
;*                                              3=path not found        *
;*                                              4=too many open files   *
;*                                              5=access denied         *
;************************************************************************
zop_args  struc
          dw      ?                ; caller's BP
          dw      ?                ; return address
zhandle   dw      ?                ; address of handle
zpathnam  dw      ?                ; address of string containing file pathname
zmode     dw      ?                ; mode:  0=read, 1=write, 2=read/write
zhigh     dw      ?                ; address of high word of file size
zlow      dw      ?                ; address of low word of file size
zop_args  ends

          public  zcreate
zcreate   proc    near
          push    BP               ; save caller's BP
          mov     BP,SP
          mov     AH,03Ch          ; load function request id
          mov     DX,[BP].zpathnam ; load pointer to pathname
          mov     CX,020h          ; create with "archive" attribute
          int     MSDOS            ; issue create request
          jc      zcr_ret          ; if error, jump
          mov     BX,[BP].zhandle  ; load address of handle
          mov     [BX],AX          ;  and store returned handle value
          xor     AX,AX            ; set return code for normal return
zcr_ret:  pop     BP               ; restore caller's BP
          ret                      ; return
zcreate   endp

;************************************************************************
;*                              Open a File                             *
;*                                                                      *
;* Calling sequence:  stat = zopen(handle, pathname, access_code)       *
;*                      where:  int *handle - location to store handle  *
;*                                              returned by open request*
;*                              char *pathname - zero terminated string *
;*                                              containing the file's   *
;*                                              pathname                *
;*                              int access_code - 0=read, 1=write,      *
;*                                              2=read and write        *
;*                              int stat - the completion code          *
;*                                              0=no errors             *
;*                                              2=file not found        *
;*                                              4=too many open files   *
;*                                              5=access denied         *
;*                                              12=invalid access       *
;************************************************************************

          public  zopen
zopen     proc    near
          push    BP               ; save caller's BP
          mov     BP,SP
          mov     AH,03Dh          ; load function request id
          mov     AL,byte ptr [BP].zmode ; load access code (mode)
          mov     DX,[BP].zpathnam ; load pointer to pathname
          int     MSDOS            ; issue open request
          jc      zop_ret          ; if error, jump
          mov     BX,[BP].zhandle  ; load address of handle
          mov     [BX],AX          ;  and store returned handle value
;
          push    AX               ; save file handle
          mov     BX,AX            ; set bx to file handle
          xor     CX,CX
          xor     DX,DX
          mov     AX,4202h         ; poisition file pointer at eof
          int     MSDOS
	  jc	  zop_ret
;
          mov     BX,[BP].zhigh    ; load address of hsize
          mov     [BX],DX          ;  and store returned hsize value
          mov     BX,[BP].zlow     ; load address of lsize
          mov     [BX],AX          ;  and store returned lsize value
;
          pop     BX               ; retrieve file handle
          xor     CX,CX
          xor     DX,DX
          mov     AX,4200h         ; reset file pointer to begining of file
          int     MSDOS
	  jc	  zop_ret
;
          xor     AX,AX            ; set return code for normal return
zop_ret:  pop     BP               ; restore caller's BP
          ret                      ; return
zopen     endp

;************************************************************************
;*                           Close a File                               *
;*                                                                      *
;* Calling sequence:  stat = zclose(handle)                             *
;*                      where:  int handle - handle returned by open    *
;*                                           request                    *
;*                              int stat - the completion code          *
;*                                              0=no errors             *
;*                                              6=invalid handle        *
;************************************************************************
          public  zclose
zclose    proc    near
          push    BP               ; save caller's BP
          mov     BP,SP
          mov     AH,03Eh          ; load function request id
          mov     BX,[BP].zhandle  ; load handle of file to close
          int     MSDOS            ; issue close request
          jc      zcl_ret          ; if error, jump
          xor     AX,AX            ; set return code for normal return
zcl_ret:  pop     BP               ; restore caller's BP
          ret                      ; return
zclose    endp

;************************************************************************
;*                           Read From a File                           *
;*                                                                      *
;* Calling sequence:  stat = zread(handle, buffer, length)              *
;*                      where:  int handle - handle returned by open    *
;*                                           request                    *
;*                              char *buffer - address of character     *
;*                                              buffer into which data  *
;*                                              is to be read           *
;*                              int *length - on input, the maximum     *
;*                                              number of characters    *
;*                                              which the buffer will   *
;*                                              hold.  On output, the   *
;*                                              number of characters    *
;*                                              actually read.  Note:   *
;*                                              a return value of zero  *
;*                                              characters read         *
;*                                              indicates end of file.  *
;*                              int stat - the completion code          *
;*                                              0=no errors             *
;*                                              5=access denied         *
;*                                              6=invalid handle        *
;************************************************************************
zrw_args  struc
          dw      ?                ; caller's BP
          dw      ?                ; return address
          dw      ?                ; zhandle (use previous equate)
zbuffer   dw      ?                ; input/output buffer
zlength   dw      ?                ; address of length value
zrw_args  ends

          public  zread
zread     proc    near
          push    BP               ; save caller's BP
          mov     BP,SP
          mov     AH,03Fh          ; load function request id
          mov     DX,[BP].zbuffer  ; load address of input buffer
          mov     BX,[BP].zlength  ; load address of length value
          mov     CX,[BX]          ;  then load length for read
          mov     BX,[BP].zhandle  ; load file's handle
          int     MSDOS            ; issue create request
          jc      zrd_ret          ; if error, jump
          mov     BX,[BP].zlength  ; load address of length parameter
          mov     [BX],AX          ;  and store number of characters read
          xor     AX,AX            ; set return code for normal return
zrd_ret:  pop     BP               ; restore caller's BP
          ret                      ; return
zread     endp

;************************************************************************
;*                           Write to a File                            *
;*                                                                      *
;* Calling sequence:  stat = zwrite(handle, buffer, length)             *
;*                      where:  int handle - handle returned by open    *
;*                              char *buffer - address of character     *
;*                                              buffer from which data  *
;*                                              is to be written        *
;*                              int *length - on input, the number of   *
;*                                              characters to write.    *
;*                                              The actual number of    *
;*                                              characters which were   *
;*                                              written is returned in  *
;*                                              "length"                *
;*                              int stat - the completion code          *
;*                                              0=no errors             *
;*                                              5=access denied         *
;*                                              6=invalid handle        *
;************************************************************************
          public  zwrite
zwrite    proc    near
          push    BP               ; save caller's BP
          mov     BP,SP
          mov     AH,040h          ; load function request id
          mov     DX,[BP].zbuffer  ; load address of input buffer
          mov     BX,[BP].zlength  ; load address of length value
          mov     CX,[BX]          ;  then load length for write
          mov     BX,[BP].zhandle  ; load file's handle
          int     MSDOS            ; issue write request
          jc      zwr_ret          ; if error, jump
          mov     BX,[BP].zlength  ; load address of length parameter
          mov     [BX],AX          ;  and store number of characters written
          xor     AX,AX            ; set return code for normal return
zwr_ret:  pop     BP               ; restore caller's BP
          ret                      ; return
zwrite    endp

;************************************************************************
;* Read characters from a string                                        *
;*                                                                      *
;* Calling Sequence:  stringrd(page, disp, buffer, &length)             *
;*   where page,disp:  location of string-fed port                      *
;*         buffer and length  are as in ZREAD (see above)               *
;*                                                                      *
;* Note: The passing parameter `page' is page #                         *
;************************************************************************
strd      struc
          dw      ?                ;caller's BP
	  dw	  ?		   ;return address
strdpg    dw      ?                ;Page, displacement of port
strdds    dw      ?
strdbuf   dw      ?                ;Buffer address
strdlen   dw      ?                ;Length address
strd      ends
          public  stringrd
stringrd  proc    near
          push    bp
          mov     bp,sp
          push    ds               ;save caller's ds
          mov     ax,es            ;save caller's es (making ax nonzero as well)
          mov     bx,[bp].strdlen  ;cx = number of chars to transfer
          mov     cx,[bx]
          mov     di,[bp].strdpg   ;get port page
          mov     dx,di            ;  and save for later
          LoadPage ds,di
          mov     di,[bp].strdds               ;ds:di => port object
          mov     si,word ptr[di+car].pt_ptr   ;point DS:SI to string
          mov     bl,[di+car_page].pt_ptr
          xor     bh,bh
          LoadPage ds,bx
          cmp     byte ptr[si],STRTYPE         ;is this a string?
          jne     nostr                        ;  no, jump (error)
          mov     bx,[si].str_len  	       ;fetch string length
          cmp     bX,0                         ;check for small string
          jge     strn_01
          add     bx,BLK_OVHD+PTRSIZE
strn_01:  LoadPage es,dx           	       ;restore ptr to port
          mov     dx,es:[di].pt_ullin          ;fetch position within string
          sub     bx,dx                        ;bx = #chars left
          jns     notpast                      ;if not negative, skip
          xor     bx,bx                        ; else #chars = 0
notpast:  cmp     bx,cx                        
          jae     max              	       ;set CX to # of chars left or max
          mov     cx,bx                        ;called for, whichever is smaller
max:      add     si,dx                        ;adjust si into string
          add     dx,cx                        ;reset pointer into string
          mov     es:[di].pt_ullin,dx
          mov     es,ax                        ;restore for C
          mov     di,[bp].strdbuf              ;point di to buffer
          xor     ax,ax                        ;prepare to return 0 (all's well)
          jmp     short storlen                ;store # of chars
nostr:    xor     cx,cx                        ;when not a string, move no chars
storlen:  mov     bx,[bp].strdlen  	       ;set length to # of chars read
          mov     es:[bx],cx
          rep     movsb                        ;transfer bytes
          pop     ds                           ;restore caller's ds
          pop     bp
          ret
stringrd endp

;********************************************************************
;Set File Position						    *
;                                                                   *
;     set_pos will set the file position, determing which chunk     *
;     of the file to read and then setting the file position to     *
;     the appropriate place. A chunk is a multiple of 256 bytes.    *
;                                                                   *
;********************************************************************
set_arg struc
         dw     ?                       ;callers bp
         dw     ?			;callers es
         dw     ?                       ;return addres
set_prt  dw     ?                       ;port
set_amt  dw     ?                       ;chunk 
set_buf  dw     ?                       ;position within chunk
set_arg ends

         public set_pos
set_pos  proc   near
         push   es
         push   bp
         mov    bp,sp			;set up stack

         mov    ax,1
         pushm  <ax, [bp].set_prt>
         C_call get_port,,Load_ES       ;get port object
         mov    sp,bp			;dump args
	 test	ax,ax			;check return status
	 jz	set_010			;jump if we have a port
setferr: 
	 lea    bx,sfp_err              ;address of error message
         pushm  <[bp].set_buf, [bp].set_amt, [bp].set_prt>
         mov    ax,3
         pushm  <ax, bx>
         C_call set_src_,,Load_ES       ;set_src_err
         mov    sp,bp
         mov    ax,-1
         jmp    set_don
set_010: 
	 mov    bx,tmp_page
         LoadPage es,bx                 ;get page address of port
         mov    si,tmp_disp
	 test	es:[si].pt_pflgs,WINDOW ;window port?		
	 jnz	setferr		
;we have a file
         mov    di,[bp].set_amt
         mov    dx,[di]			;dx = chunk number
         inc    dx
	 mov	word ptr es:[si].pt_chunk,dx ;update chunk # in port
         dec    dx
         mov    cl,8			;make chunk number into # bytes
         xor    bx,bx
         mov    bl,dh
         xor    dh,dh
         shl    dx,cl                   ;multiply dx by 256
         mov    cx,bx			;cx:dx = # bytes (32bit int)

         test   byte ptr es:[si].pt_pflgs,READWRITE+WRITE_ONLY ;test port flags
	 pushf				         ;save flags for later
	 jz	set_015			         ;if input port, jump
         or     byte ptr es:[si].pt_pflgs,DIRTY	 ;else set dirty bit
         mov    bx,[bp].set_buf			 ;     get chunk offset
         add    dx,[bx]			         ;     and add to file position
set_015: 				;cx:dx = distance to move (bytes)
	 mov    bx,es:[si].pt_handl	;bx = file handle
         mov    ah,42h                  ;move file pointer
         mov    al,0                    ;position from file start
         int    MSDOS                   ;move it
	 popf	   			;restore flags 
	 jnz	set_020			;jump if output port

         mov    cx,256                  ;cx = length of buffer
         mov    bx,es:[si].pt_handl	;bx = file handle
         mov    dx,si
	 add	dx,pt_buffr		;dx = start of buffer
         push   ds
         push   es
         pop    ds                      ;ds:dx => buffer
         mov    ah,3fh                  ;read from a file
         int    MSDOS
         pop    ds			;restore ds
	 jc	set_don			;return on error
	 mov	es:[si].pt_bfend,ax     ;update number of bytes read
set_020: 
	 mov    bx,[bp].set_buf
         mov    ax,[bx]
	 mov	es:[si].pt_bfpos,ax	;update buffer position
set_don: 
	 pop    bp		
         pop    es
         ret
set_pos  endp

;********************************************************************
;SGRAPH           						    *
;  Interface to Graphic Primitives     (%graphics  arg1 ... arg7)   *
;                                                                   *
;********************************************************************
	 public	  sgraph
BUFFER_IS_STACK	   		   ; denote emulate stack with real buffer
sgraph:   mov	  CX,7		   ; load counter-- seven arguments
	  xor	  DX,DX 	   ; set error flag = FALSE
	  lods	  byte ptr ES:[SI] ; load first argument
	  save	  <AX>		   ;  and save as destination register
	  GET_REAL_BUFFER_STACK	   ; es:di => top of buffer
	  jmp	  short sgraph0
; loop thru args, moving to real mode buffer
sgraph1:  lods	  byte ptr ES:[SI] ; load next argument
sgraph0:  xor	  AH,AH 	   ; be sure high byte is zero
	  mov	  BX,AX 	   ; copy register number to BX
	  cmp	  byte ptr reg0_pag+[BX],SPECFIX*2 ; is arg a fixnum?
	  je	  sgraph2	   ; if arg *is* a fixnum, o.k. (jump)
	  inc	  DX		   ; indicate an invalid argument
sgraph2:  mov	  AX,reg0_dis+[BX] ; expand 15-bit signed int to 16-bit signed int
	  shl	  AX,1
	  sar	  AX,1
	  push	  ES
	  push	  SI
	  push	  CX		   ; save around following
	  MOVE_ARGS_TO_BUF <ax>,REAL_MODE_BUFFER,autodecr,save
	  pop	  CX		   ; restore count
	  pop	  SI
	  pop	  ES
	  loop	  sgraph1	   ; continue 'til all arguments processed
; all args moved to buffer
 	  cmp	  DX,0		   ; any argument errors?
	  je	  sg_005
	  jmp	  sgraph3	   ; if errors encountered, jump
sg_005:
	  save	  <SI>		   ; save the location pointer
	  mov     BX,[BP].save_AX  ; restore first argument register (op-code)
; use graphics op-code as index into graphics-go table to indicate whether
; return values are expected; on hboard parallel processing can exist.
	  mov	  BX,reg0_dis+[BX] ; get value
	  shl	  BX,1		   ; expand to 16 bit signed integer
	  sar	  BX,1
	  mov	bl,[graphic_go+bx] ; index into return value table
	  push	bx		   ; save # return values for later
	  or	bl,bl		   ; does it return a value?
	  jz    sg_010		   ;  no, jump
	  mov	bx,2		   ; bx = # bytes to return
; build rpc buffer on the local stack and issue the rpc call
sg_010:
	  GET_REAL_BUFFER	   ; es:di => next loc in stack buffer
	  add	di,2		   ; make last loc top of stack
	  push	di       	   ; pass stack ptr 
	  mov	cx,4		   ; cx = # bytes to pass
	  push	XLI_GRAPH	   ; Type code - %graphics
	  mov	dx,sp		   ; ds:dx => transaction buffer
	  mov	al,rpc_handle
	  mov	ah,RPC		   ;Issue RPC
	  int	DOS
	  xor	ax,ax		   ;default return result to zero
	  pop	bx		   ;bx = return result
	  pop	cx		   ;adjust stack
; if return value not expected exit back to interpreter loop, otherwise
; if set-video-mode op code get additional return values from transaction
; buffer
	  pop	cx		   ;if no result expected
	  jcxz	sg_030     	   ; then return
	  mov	ax,bx		   ;ax = return result
	  or	ax,ax		   ;If negative result
	  jl	sg_030		   ;  then some kind of error 
	  cmp   cx,1		   ;Additional values expected?
	  je   sg_030		   ; no,  jump
				   ; yes, must be set-video-mode
	  push	ax		   ;      save return result around call
	  add	di,8		   ;      address buffer for 3 return
	  MOVE_ARGS_FROM_BUF <AX,CHAR_HGT,VID_MODE>
	  mov	MAX_ROWS,AL
	  push	AX
	  push	vid_mode
	  push	char_hgt
	  call	chg_vmode	   ; tell real mode i/o code about changes
	  add	sp,6		   ; dump args off stack

;
;The following must be done so that OS/386 recognizes mode change has been made.
;
	  cmp	pc_make,1	   ;tipc?
	  je	sg_028		   ;  yes,  jump
	  mov	ax,VID_MODE	
	  xor	ah,ah
	  int   10h

sg_028:
	  pop	ax		   ;      restore return result
; at this point, ax contains the return result
sg_030:
	  shl	  AX,1		   ; clear high order bit of result
	  shr	  AX,1		   ;  (convert to immediate value)
	  mov     BX,[BP].save_AX  ; restore destination register number
	  mov	  reg0_dis+[BX],AX ; store returned result into destination reg
not_pc:   jmp	  next_SP	   ; return to interpreter
sgraph3:  mov	  BX,offset m_graph ; load addr of "%graphics" text
	  jmp	  src_err	   ; link to Scheme debugger
BUFFER_IS_BUFFER	   	   ;subsequent uses of buffer as buffer

;***************************************************************************
;*            Link for routines in PROGX                                   *
;***************************************************************************
          extrn   shft_brk:near
          extrn   dos_err:near
          public  shft%brk
          public  dos%err
shft%brk  proc    far
          call    shft_brk         ;link to SHF BREAK process
          ret
shft%brk  endp

dos%err   proc    far
          call    dos_err          ;link to DOS fatal error process
          ret
dos%err   endp

prog      ends


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

	  IFDEF   PROMEM		;;; PROTECTED MODE
	  mov	  pro_msb,ax			;Save Machine State Block
	  mov	  pro_msb+2,bx
	  mov	  pro_msb+4,cx
	  mov	  pro_msb+6,dx

	  lea	  dx,pro_msb		;;; Do real mode interrupt
	  xor	  bx,bx
	  cmp	  ah,3			;;; Read Cursor position
	  je	  crt_d02
	  cmp	  ah,8			;;; Read Char and Attribute
	  jne	  crt_d04	    
crt_d02:  mov	  bx,8	 		;;; Wait for return value
crt_d04:
	  mov	  ax,0E349h
	  int	  21h

	  mov	  ax,pro_msb		;;; restore ax
	  mov	  bx,pro_msb+2		;;; restore bx
	  mov	  cx,pro_msb+4		;;; restore cx
	  ret
	   
	  ELSE
	  int	  TI_CRT
	  ret
	  ENDIF
ibm_dsr:  xchg	  DH,DL       ; Do this now instead of making special checks
	  int	  IBM_CRT     ; IBM's row,col is diff'rnt from TI's col,row
	  ret
crt_dsr   endp

PROGX	  ends
          end
