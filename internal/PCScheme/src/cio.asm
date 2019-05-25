;                                                       =====> CIO.ASM
;***************************************
;*     TIPC Scheme  Runtime Support    *
;*            I/O support              *
;*                                     *
;* (C) Copyright 1985, 1986 by Texas   *
;*     Instruments Incorporated.       *
;*        All rights reserved.         *
;*                                     *
;* Date Written:  24 March 1986        *
;* Last Modification:                  *
;*   14 April 1986                     *
;*     Change references to pagetabl   *
;*     to call memory manager for use  *
;*     with extended/expanded mem.     *
;*   9 Sept 1986 - ds                  *
;*     Add EGA support                 *
;*  21 Nov 1986 - rb                   *
;*     Detect disk full error correctly*
;*   7 Jan 1987 - dbs                  *
;*     Added support for random I/O    *
;*  10 Feb 1987 - tc                   *
;*     EOF-DISP modified to reflect    *
;*     other changes in Page 5 symbols *
;*  16 Mar 1987 - tc		       *
;*     Added Binary I/O, Error handling*
;*     for Disk Full		       *			
;*  21 Jan 1988 - rb                   *
;*     binary I/O uses line-length=0;  *
;*     do EGA cursor with BIOS call;   *
;*     use dirty bit of port flags     *
;*     (commented out)                 *
;*                                     *
;***************************************
          page    60,132
          include scheme.equ
          include sinterp.arg

P_FLAGS   equ     6
W_FLAGS   equ     26
HANDLE    equ     8
CUR_LINE  equ     10
CUR_COL   equ     12
UL_LINE   equ     14
UL_COL    equ     16
N_LINES   equ     18
N_COLS    equ     20
T_ATTR    equ     24
BUF_POS   equ     28
BUF_END   equ     30
BUFR      equ     32
CHUNK     equ     14
BACKSP    equ     08
WRAP      equ     1
TAB       equ     09
RETURN    equ     0Dh
LF        equ     0Ah
CTRL_Z    equ     1Ah
LEFT_AR   equ     4Bh
RIGHT_AR  equ     4Dh
F3        equ     3Dh
F5        equ     3Fh
INSERT    equ     52h
DELETE    equ     53h
ENTER     equ     0Dh
NULL_CH   equ     0
BELL_CH   equ     07
BLANK     equ     0020h
buf_len   equ     253
MSDOS    equ        21h

DGROUP    group   data
data      segment word public 'DATA'
          assume  DS:DGROUP
          public  port_r, port_seg, port_d
          public  prn_hand
          public  direct,nlines, ncols, ulline, ulcol
          public  curcol, row, column, cur_off, char_hgt
bad_set   db      "[VM INTERNAL ERROR] setadr: bad port",CR,LF,0
push_er   db      "[VM INTERNAL ERROR] pushchar: failed",CR,LF,0
rd_st_er  db      "[VM INTERNAL ERROR] takechar: source not a string",CR,LF,0
ch_rd     db      "CHAR-READY?",0
rch_er    db      "READ-CHAR",0
sfp_err   db      "SET-FILE-POSITION!",0
port_r    dw      0                        ; port_reg
          dw      0
port_seg  dw      0                        ; port_page segment
port_d    dw      0                        ; port_disp
prn_hand  dw      0                        ; printer handle
win_p     dw      0                        ; window_p
str_p     dw      0                        ; string_p
handlee   dw      0                        ; handle
direct    dw      0                        ; direction
nlines    dw      0                        ; n_lines
ncols     dw      0                        ; n_cols
ulline    dw      0                        ; ul_line
ulcol     dw      0                        ; ul_col
t_attrib  dw      0                        ; text attribute
insert_m  dw      0                        ; insert mode
curcol    dw      0                        ; cur_col
curline   dw      0                        ; cur_line
index     dw      0                        ; index of buffer
sh_ptr    dw      0                        ; pointer of shadow buffer
sh_len    dw      0                        ; length of shadow buffer
sh_bufer  db      256 dup (0)              ; shadow buffer for characters
row       dw      256 dup (0)              ; row vector
column    dw      256 dup (0)              ; column vector

scan     dw ?
endscan  dw ?
cur_off  dw 0
char_hgt dw 8

         extrn  vid_mode:word
         extrn  ega_col:byte
         extrn  ega_row:byte

data      ends

XGROUP   group      progx
progx    segment    word public 'progx'
         assume     CS:XGROUP

          extrn   zbell:far
          extrn   zch_rdy:far
          extrn   sch_err:near
	  extrn   dos_err:near
;
;     For the Ega
; This routine first outputs a byte to the sequencer register to point to
; the map mask register, and then uses the map mask register to enable
; all banks for writing.
;
         public enable
enable   proc   far
	 comment ~
         push   DX
         push   AX
         mov    DX,3c4h                 ; port addr of sequencer
         mov    AL,2                    ; index to other map mask register
         out    DX,AL                   ; set index register
         inc    DX                      ; set DX to map mask register
         xchg   AL,AH
         out    DX,AL                   ; enable all banks
         pop    AX
         pop    DX
	 ~
         ret
enable   endp

;****************************************************************************
;*                                                                          *
;*                            EGA Cursor Emulator                           *
;*                                                                          *
;*    Purpose: to simulate a cursor for the IBM EGA modes.                  *
;*                                                                          *
;****************************************************************************
         public ega_curs
ega_curs proc   far

         cmp    vid_mode,14             ; don't bother unless in EGA mode
         jl     ega_03

         mov    CX,cur_off
         and    CX,7fh                  ; is bit one on?
         jz     ega_02                  ; cursor not turned off
         and    cur_off,0feh            ; turn off bit one
         jmp    ega_03

ega_02:  cmp    t_attrib,00h            ; black attribute?
         je     ega_03                  ; forget it

; set up BIOS call
	 mov	AX,09DBh		; reverse-video block 
	 mov	BX,8Fh			; attr = xor,white
	 mov	CX,1			; repetition count = 1
	 int	10h

	 comment ~
         push   ES
         mov    AX,0a000h
         mov    ES,AX

         mov    char_hgt,8
         cmp    vid_mode,14
         je     ega_01
         mov    char_hgt,14
;
; start scan line = row * height
;
ega_01:  mov    AL,ega_row                  ; current line number
         xor    AH,AH
         mul    char_hgt
         mov    scan,AX
;
; end scan line = row * height + height - 1
;
         add    AX,char_hgt
         dec    AX
         mov    endscan,AX

show_loop:
         mov    CX,80
         mul    CX
         mov    BX,AX
         xor    AX,AX
         mov    AL,ega_col
         add    BX,AX               ; current column
         mov    AH,18h
         call   logical
         mov    DL,0ffh
         call   clrbyte
         inc    scan
         mov    AX,scan
         cmp    AX,endscan
         jl     show_loop
         mov    AH,0
         call   logical
         mov    AH,0
         call   enable
         pop    ES
	 ~

ega_03:  ret
ega_curs endp

	comment ~
; signal to the graphics processor that we want to do a logical operation
; (and or xor) with the latched data.
; on entry ah = function selected
logical  proc   near
         push   DX
         push   AX
         mov    DX,3ceh                 ; port addr of graphics address reg
         mov    AL,3                    ; data rotate function
         out    DX,AL
         inc    DX
         xchg   AL,AH
         out    DX,AL
         pop    AX
         pop    DX
         ret
logical  endp
;
;on entry: DL contains bit mask for clearing ES:[BX] points to byte in
; CRT memory
;
clrbyte  proc   near
         mov    AH,0fh
         call   enable                  ; enable all banks
         mov    AL,ES:[BX]              ; latch data
         xor    AL,AL                   ; zero
         mov    ES:[BX],AL              ; clear byte
         mov    AH,0ffh
         call   enable
         mov    AL,ES:[BX]              ; ????
         mov    AL,DL                   ; bit mask for character
         mov    ES:[BX],AL              ; set the value
         ret
clrbyte  endp
	~

progx    ends


PGROUP    group   prog
prog      segment byte public 'PROG'
          assume  CS:PGROUP
          extrn   zscroll:near
          extrn   zputcur:near
          extrn   zputc:near
          extrn   getch:near
          extrn   zcuron:near
          extrn   zcuroff:near
          extrn   zread:near
          extrn   toblock:near
          extrn   stringrd:near
          extrn   printstr:near

;********************************************************************
;                                                                   *
;     set_pos will set the file position, determing which chunk     *
;     of the file to read and then setting the file position to     *
;     the appropriate place.                                        *
;                                                                   *
;********************************************************************

set%_arg struc
         dw     ?                       ; caller's CS and IP
         dw     ?
         dw     ?                       ; ES
set_prt  dw     ?                       ; port #
set_amt  dw     ?                       ; chunk #
set_buf  dw     ?                       ; new buffer pointer
set%_arg ends

         public set_pos
set_pos  proc   near

         push   ES
         push   BP
         mov    BP,SP
         mov    AX,1
         pushm  <AX, [BP].set_prt>
         C_call get_port,,Load_ES           ; get port address
         mov    SP,BP
         mov    BX,tmp_page            ; check return status
         cmp    byte ptr ptype+[BX],PORTTYPE*2  ; check type
         je     set_010
setferr: lea    BX,sfp_err                  ; address of error message
         pushm  <[BP].set_buf, [BP].set_amt, [BP].set_prt>
         mov    AX,3
         pushm  <AX, BX>
         C_call set_src_,,Load_ES           ; set_src_err
         mov    SP,BP
         mov    AX,-1
         jmp    set_don
;
set_010: mov    BX,tmp_page
         LoadPage ES,BX                 ; get page address of port
         mov    SI,tmp_disp
         mov    DX,word ptr ES:[SI+P_FLAGS]
         and    DX,04h                  ; port a file or a window?
         cmp    DX,04h
         je     setferr

         mov    DI,[BP].set_amt
         mov    DX,[DI]
         inc    DX
         mov    word ptr ES:[SI+CHUNK],DX  ; update chunk #
         dec    DX
         mov    CL,8
         xor    BX,BX
         mov    BL,DH
         xor    DH,DH
         shl    DX,CL                   ; multiply DX by 256
         mov    CX,BX

	 test   byte ptr ES:[SI+P_FLAGS],READWRITE+WRITE_ONLY ;test port flags
	 pushf					;save flags for later
	 jz	set_015				;if input port jump
	 or	byte ptr ES:[SI+P_FLAGS],DIRTY	;else set dirty bit
         mov    BX,[BP].set_buf			;     get chunk offset
         add    DX,[BX]				;     and add fo file position
set_015: 
	 mov    BX,word ptr ES:[SI+HANDLE]      ;get file handle
         mov    AH,42h                          ;move file pointer to file
         mov    AL,0                            ;start plus offset in dx
         int    MSDOS
	 popf					;restore flags
	 jnz	set_020				;jump if output port

         mov    CX,256                  	;get buffer length
         mov    BX,word ptr ES:[SI+HANDLE] 	;get file handle
         mov    DX,SI
         add    DX,32				;start of port buffer
         push   DS
         push   ES
         pop    DS                      	;ds:dx => port buffer
         mov    AH,3fh                  	;read from a file
         int    MSDOS				;go do it
         pop    DS
         mov    word ptr ES:[SI+BUF_END],AX 	;save #bytes read in port
set_020: 
	 mov    BX,[BP].set_buf			;address of chunk offset
         mov    AX,[BX]				;get offset 
         mov    word ptr ES:[SI+BUF_POS],AX     ;and save in port

set_don: pop    BP
         pop    ES
         ret
set_pos  endp
;;;**************************************************************************
;;;                     Set Port Address
;;;**************************************************************************
set_arg   struc
          dw      ?                         ; caller's BP
          dw      ?                         ; caller's ES
          dw      ?                         ; return address
pg        dw      ?                         ; adjusted page number
dis       dw      ?
set_arg   ends
          public  ssetadr                   ; temporary
ssetadr   proc    near                      ; assembly routine for setadr
          push    ES
          push    BP
          mov     BP,SP
          push    DI
          push    SI
          push    BX
          mov     BX,[BP].pg                ; adjusted page number
          cmp     byte ptr ptype+[BX],PORTTYPE*2 ; check port type
          jne     set_err
; get port information
          lea     DI,port_r                 ; get port register address
          mov     [DI].C_page,BX
          mov     SI,[BP].dis
          mov     [DI].C_disp,SI
          mov     port_d,SI
          LoadPage ES,BX                    ; get page address
;;;       mov     ES,word ptr pagetabl+[BX] ; get page address
          mov     port_seg,ES               ; save the page paragraph
          mov     AX,word ptr ES:[SI+HANDLE] ; handler
          mov     handlee,AX
          mov     AX,word ptr ES:[SI+P_FLAGS] ; port flag
          mov     direct,AX
          mov     BX,AX
          and     AX,WINDOW
          mov     win_p,AX
          and     BX,STRIO
          mov     str_p,BX
          xor     AX,AX                     ; return status
set_ret:  pop     BX
          pop     SI
          pop     DI
          pop     BP
          pop     ES
          ret
; Display error message
set_err:  lea     SI,bad_set                ; address of error message
          push    SI
          C_call  printf,,Load_ES           ; print error message
          mov     SP,BP
          C_call  force_de                  ; force_debug
          mov     SP,BP
          mov     AX,1                      ; return error status
          jmp     set_ret
ssetadr   endp
;;;**************************************************************************
;;;                  Input a Single Character
;;;**************************************************************************
take_arg  struc
leng      dw      256
new_bpos  dw	  0
take_BP   dw      ?                        ; caller's BP
          dw      ?                        ; caller's ES
          dw      ?                        ; caller's return address
take_arg  ends
          public  take_ch
take_ch   proc    near
          push    ES
          push    BP
          sub     SP,offset take_BP        ; allocate local storage
          mov     BP,SP
	  mov	  [BP].new_bpos,0	   ; buf position after refilling buf
          mov     [BP].leng,256            ; set up buffer length
          lea     SI,port_r
          mov     BX,[SI].C_page
          LoadPage ES,BX
          mov     SI,port_d                ; get displacement

; Fix for random I/O - read preceeded by a write
	  test	  byte ptr ES:[SI+P_FLAGS],READWRITE+WRITE_ONLY
	  jz  	  take_c00			      ;skip if input port
	  mov	  BL,byte ptr ES:[SI+P_FLAGS]	      ;get port flags
	  and	  BL,DIRTY+STRIO+OPEN+WINDOW	      ;isolate appropriate flags
	  cmp	  BL,DIRTY+OPEN		              ;buffer modified?
	  jne	  take_c00		   	      ; no, jump
	  and     byte ptr ES:[SI+P_FLAGS],NOT DIRTY  ;clear flag

; this read was preceded by at least one write, so reposition file pointer
; so it rereads the buffer
	  mov	 BX,word ptr ES:[SI+HANDLE]
	  dec       word ptr ES:[SI+CHUNK]
	  mov	 CX,word ptr ES:[SI+CHUNK]
	  xor	 DL,DL
	  mov	 DH,CL
	  mov	 CL,CH
	  xor	 CH,CH
  	  mov	 AX,4200h		     ; reposition file pointer
	  int	 MSDOS
	  mov	 BX,ES:[SI+BUF_POS]	     ; after re-reading file, restore
	  mov	 [BP].new_bpos,BX	     ; current buffer position
	  jmp	 take_fil		     ; go re-read the file

take_c00: mov     BX,word ptr ES:[SI+BUF_POS]
          cmp     BX,word ptr ES:[SI+BUF_END]
          jge     take_c01
          jmp     take_nxt                 ; get the next character from buffer
; buffer empty -- fill it up
take_c01:
          cmp     win_p,0                  ; window object?
          jne     take_c02
          jmp     take_fil                 ; no, jump
take_c02: cmp     str_p,0                  ; read from string?
          je      take_win                 ; no, jump
; read character from string
          lea     BX,[BP].leng
          push    BX
          lea     BX,row                   ; buffer for characters
          push    BX
          lea     SI,port_r
          pushm   <[SI].C_disp,[SI].C_page> ; port object
          mov     AX,DS
          mov     ES,AX                    ; ES segment points to DS
          call    stringrd
          mov     SP,BP
          test    AX,AX                    ; check return status
          jnz     take_ser                 ; error, jump
          lea     SI,port_r
          mov     BX,[SI].C_page
          LoadPage ES,BX
;;;       LoadPage ES,port_seg             ; restore port page
;;;       mov     ES,port_seg              ; reset ES segment
          mov     SI,port_d                ; restore SI register
          jmp     take_10
take_ser: lea     BX,rd_st_er              ; address of error message
          push    BX
          C_call  printf                   ; display error message
          mov     SP,BP
          jmp     take_10
; read from window
take_win: call    read_win
          mov     BX,AX
          jmp     short take_11
;
take_10:  mov     BX,[BP].leng
take_11:  mov     ES:[SI+BUF_END],BX       ; save buffer length
          test    BX,BX                    ; length zero?
          jnz     take_20                  ; no, jump
          mov     ES:[SI+BUF_POS],BX
          jmp     take_30
take_20:  cmp     win_p,0                 ; window object?
          je      take_22                 ; no, copy string
          cmp     str_p,0                 ; string?
          je      take_25                 ; no, jump
; copy characters from buffer to file object
take_22:  push    SI                      ; save SI register
          mov     DI,SI
          add     DI,BUFR
          lea     SI,row
          mov     CX,BX                   ; length of characters to move
          cld                             ; direction forward
      rep movsb
          pop     SI                      ; restore SI register
take_25:  mov	  BX,[BP].new_bpos	  ; BX = buffer position
; Return the next character from the input buffer
take_nxt: xor     AH,AH
          mov     AL,byte ptr ES:[SI+BUFR+BX]
          inc     BX
          mov     word ptr ES:[SI+BUF_POS],BX
          cmp     AL,CTRL_Z                ; test for control-Z
          jne     take_ret                 ; no, return
	  test	  direct,BINARY
          jnz     take_ret                 ; no, return
take_30:  mov     AX,256                   ; text file, send EOF
take_ret: add     SP,offset take_BP        ; release local storage
          pop     BP
          pop     ES
          ret

; Read from file
	  public  take_fil
take_fil: 
   	  cmp	  word ptr ES:[SI+CHUNK],1    ; operating on first chunk?
   	  jne	  take_f05		      ; no, jump	
   	  cmp	  word ptr ES:[SI+BUF_POS],0  ; Have we filled the buffer yet?
   	  je	  take_f10		      ; yes, jump	
take_f05:
	  inc	  word ptr ES:[SI+CHUNK]      ; bump the chunk number
take_f10:
	  mov     BX,handlee              ; file handle
          lea     CX,[BP].leng            ; address of length of bytes to read
          lea     AX,row                  ; input buffer
          pushm   <CX,AX,BX>
          call    zread
          mov     SP,BP
          test    AX,AX                    ; error status
          jz      take_50                  ; no, jump
; We will not return from call to dos_err
	  add	 AX,(IO_ERRORS_START - 1)  ; Make Dos I/O error number
	  mov	 BX,1
	  lea    CX,port_r
	  pushm  <CX,AX,BX>	   	   ; 1 = non-restartable
	  call   dos_err		   ; invoke scheme error handler
take_50:  jmp	 take_10		;relative jump not long enough
take_ch   endp
;**************************************************************************
;         Read a "record" from window
;                        ES:SI points to the window object
;                        Return AX = number of characters read
;**************************************************************************
read_arg  struc
read_SI   dw      ?
read_BX   dw      ?
sav_p     dw      ?
sav_d     dw      ?
read_BP   dw      ?                        ; caller's BP
          dw      ?                        ; caller's return address
read_arg  ends
          public  read_win
read_win  proc    near
          push    BP
          sub     SP,offset read_BP        ; allocate for local storage
          mov     BP,SP
          xor     BX,BX                    ; initialization
          mov     index,BX
          mov     sh_ptr,BX
          mov     insert_m,BX
          mov     BX,word ptr ES:[SI+CUR_LINE] ; get window information
          mov     curline,BX
          mov     DX,word ptr ES:[SI+CUR_COL]
          mov     curcol,DX
          mov     DX,word ptr ES:[SI+UL_LINE]
          mov     ulline,DX
          mov     DX,word ptr ES:[SI+UL_COL]
          mov     ulcol,DX
          mov     DX,word ptr ES:[SI+N_LINES]
          mov     nlines,DX
          mov     DX,word ptr ES:[SI+N_COLS]
          mov     ncols,DX
          mov     DX,word ptr ES:[SI+T_ATTR]
          mov     t_attrib,DX
          call    zcuron                   ; turn on the cursor
read_001: mov     BX,curline               ; get the current line number
          cmp     BX,nlines                ; check out of lines
          jl      read_put
          pushm   <t_attrib,ncols,nlines,ulcol,ulline>
          call    zscroll                  ; scroll up one line
          mov     SP,BP
          mov     BX,nlines
          dec     BX                       ; cur_line = n_lines - 1
          mov     curline,BX
          mov     curcol,0                 ; cur_col = 0
read_put: mov     DX,curcol
          add     DX,ulcol
          add     BX,ulline
          pushm   <DX,BX>
          call    zputcur                  ; show the cursor
          mov     SP,BP
          call    getch                    ; character returned in AL
          test    AL,AL                    ; extended character?
          jz      read_ex
          jmp     read_100
;;; Process extended key sequence
read_ex:  call    getch                    ; character returned in AL
          cmp     AL,LEFT_AR               ; left arrow key?
          jne     read_ra
          jmp     read_bs                  ; as backspace
;
read_ra:  cmp     AL,RIGHT_AR              ; right arrow key?
          jne     read_f3
          mov     insert_m,0               ; turn off insert mode
          mov     BX,sh_ptr
          cmp     BX,sh_len
          jl      read_030                 ; get character from shadow buffer
          jmp     read_001
read_030: lea     DI,sh_bufer
          mov     AL,byte ptr [DI+BX]
          jmp     read_one
;
read_f3:  cmp     AL,F3                    ; F3 key?
          jne     read_f5
          mov     insert_m,0               ; turn off insert mode
read_041: mov     CX,index
          cmp     CX,buf_len               ; index < len?
          jl      read_043
          jmp     read_001                 ; no room for more chars
read_043: mov     BX,sh_ptr
          cmp     BX,sh_len                ; sh_ptr < sh_length?
          jl      read_045
          jmp     read_001                 ; buffer empty
read_045: lea     DI,sh_bufer
          mov     AL,byte ptr [DI+BX]
          call    echo_ch                  ; AL = character
          mov     SP,BP
          jmp     read_041
;
read_f5:  cmp     AL,F5                   ; F5 key?
          jne     read_ins
          call  ega_curs                  ; turn off the EGA cursor
          mov     insert_m,0              ; turn off insert mode
          cmp     index,0
          jne     read_051
          jmp     read_001
read_051: call    str_str                 ; copy characters to shadow buffer
          mov     BX,index
          mov     sh_len,BX
          mov     byte ptr [DI+BX],0      ; end of string
          dec     BX
          mov     [BP].read_SI,SI         ; save SI register
          lea     DI,row                  ; address of row vector
          lea     SI,column               ; address of column vector
read_053: cmp     BX,0
          jl      read_055
          cmp     byte ptr [DI+BX],0
          jl      read_055
          mov     [BP].read_BX,BX         ; save BX
          mov     CX,BLANK
          pushm   <t_attrib,CX>
          xor     CH,CH
          mov     CL,byte ptr [SI+BX]
          mov     curcol,CX
          add     CX,ulcol                ; ul_col + cur_col
          push    CX
          mov     CL,byte ptr [DI+BX]
          mov     curline,CX
          add     CX,ulline               ; ul_line + cur_line
          push    CX
          call    zputc
          mov     SP,BP
          mov     BX,[BP].read_BX         ; restore BX
          dec     BX
          jmp     read_053
read_055: mov     SI,[BP].read_SI         ; restore SI register
          mov     index,0
          mov     sh_ptr,0
          jmp     read_001
;
read_ins: cmp     AL,INSERT               ; insert key?
          jne     read_del
          call  ega_curs                  ; turn off the EGA cursor
          mov     insert_m,1              ; turn on insert mode
          jmp     read_001
;
read_del: cmp     AL,DELETE               ; delete key?
          jne     read_EN
          mov     insert_m,0              ; turn off insert mode
          mov     BX,sh_ptr
          cmp     BX,sh_len               ; sh_ptr < sh_len?
          jl      read_d01
          jmp     read_001
read_d01: inc     sh_ptr                  ; sh_ptr++
          jmp     read_001
;
read_EN:  cmp     AL,ENTER                ; enter key?
          je      read_RT                 ; as carriage return
          jmp     read_001
;;; Process ascii character
read_100: cmp     AL,BACKSP               ; backspace?
          jne     read_200
read_bs:  mov     insert_m,0              ; turn off insert mode
          call    ega_curs                ; turn off the EGA cursor
          mov     BX,index
          cmp     BX,0
          jle     read_150
          lea     DI,row
          dec     BX
          cmp     byte ptr [DI+BX],0
          jl      read_150
          mov     index,BX
          cmp     sh_ptr,0
          je      read_120
          dec     sh_ptr                  ; decrement sh_ptr pointer
read_120: lea     DI,column
          xor     CH,CH
          mov     CL,byte ptr [DI+BX]     ; update cur_line and cur_col
          mov     curcol,CX
          lea     DI,row
          xor     DH,DH
          mov     DL,byte ptr [DI+BX]
          mov     curline,DX
          mov     BX,BLANK
          add     CX,ulcol                ; ul_col + cur_col
          add     DX,ulline               ; ul_line + cur_line
          pushm   <t_attrib,BX,CX,DX>
          call    zputc
          mov     SP,BP
          jmp     read_001
read_150: call    zbell
          jmp     read_001
;
read_200: cmp     AL,RETURN              ; carriage return?
          je      read_RT
          jmp     read_300               ; no, jump
;;; Process return key
read_RT: cmp    vid_mode,14
         jl     read_rt1
         call   ega_curs                ; turn off the ega cursor
         or     cur_off,1

read_rt1: mov     BX,index
          mov     byte ptr ES:[SI+BUFR+BX],RETURN ; insert carriage return
          inc     BX
          mov     byte ptr ES:[SI+BUFR+BX],LF ; insert line feed
          inc     BX
          mov     index,BX
          mov     DX,curline
          mov     curcol,0               ; cur_col = 0
          inc     DX                     ; cur_line++
          cmp     DX,nlines              ; out of lines?
          jl      read_220
          pushm   <t_attrib,ncols,nlines,ulcol,ulline>
          call    zscroll                ; scroll up one line
          mov     SP,BP
          mov     DX,nlines              ; yes, cur_line = n_lines - 1
          dec     DX
read_220: mov     curline,DX             ; restore cur_line
          call    str_str                ; copy string into buffer
          cmp     TRNS_pag,0             ; check transcript file
          je      read_250
          mov     BX,direct
          and     BX,TRANSCRI
          jz      read_250
; transcript file "on"
          lea     BX,port_r
          mov     DX,[BX].C_page
          mov     [BP].sav_p,DX
          mov     DX,[BX].C_disp
          mov     [BP].sav_d,DX
          pushm   <TRNS_dis,TRNS_pag>
          call    ssetadr                ; set transcript file address
          mov     SP,BP
          mov     AX,index
          dec     AX
          push    AX
          lea     BX,sh_bufer
          push    BX
          mov     [BP].read_SI,SI        ; save SI register
          call    printstr               ; output to transcript file
          mov     SP,BP
          mov     SI,[BP].read_SI        ; restore SI register
          pushm   <[BP].sav_d, [BP].sav_p>
          call    ssetadr                ; set current port address
          mov     SP,BP
          lea     DI,sh_bufer
read_250: mov     BX,index
          dec     BX
          mov     byte ptr [DI+BX],0     ; end of string
          dec     BX
          mov     sh_len,BX
          jmp     read_off
;
read_300: cmp     AL,LF                  ; line feed?
          jne     read_one
          jmp     read_001               ; ignore line feed key
;
read_one: mov     BX,index               ; default
          cmp     BX,buf_len             ; index >= len?
          jl      read_420
          call    zbell
          jmp     read_001
read_420: call    echo_ch                ; AL = character
          jmp     read_001
;
read_off: call    zcuroff                ; turn off the cursor
          mov     BX,curline
          mov     CX,curcol
          mov     ES:[SI+CUR_LINE],BX    ; save cur_line and cur_col
          mov     ES:[SI+CUR_COL],CX
          mov     AX,index               ; return length
;
read_ret: add     SP,offset read_BP
          pop     BP
          ret
read_win  endp

;*****************************************************************************
; Move the string in port object to buffer sh_bufer
;*****************************************************************************
str_str   proc    near
          lea     DI,sh_bufer            ; address of shadow buffer
;         xor     BX,BX
; Clear the buffer
;str_01:   cmp     BX,sh_len
;         jge     str_10
;         mov     byte ptr [DI+BX],0
;         inc     BX
;         jmp     str_01
; Move the characters
str_10:   push    SI                     ; save SI
          add     SI,BUFR                ; address of input buffer
          mov     CX,index
          mov     AX,ES
          mov     BX,DS
          mov     ES,BX                  ; ES:DI points to destination string
          mov     DS,AX                  ; DS:SI points to source string
rep       movsb
          mov     ES,AX                  ; reset segment registers
          mov     DS,BX
          pop     SI                     ; restore SI
          lea     DI,sh_bufer
          ret
str_str   endp
;*****************************************************************************
;                          Echo single character
;*****************************************************************************
echo_ch   proc    near
          push    BP
          mov     BP,SP

         mov    BX,word ptr ES:[SI+T_ATTR]  ; get attribute
         mov    t_attrib,BX

          mov     BX,index
          mov     byte ptr ES:[SI+BX+BUFR],AL ; store character
          inc     BX                     ; index++
          mov     index,BX               ;
          cmp     insert_m,0             ; insert mode?
          jne     echo_10
          inc     sh_ptr                 ; sh_ptr++
echo_10:  mov     DX,curcol
          mov     CX,curline
          cmp     DX,ncols               ; end of line?
          jl      echo_20
          inc     CX                     ; yes, cur_line++
          xor     DX,DX                  ;      cur_col = 0
echo_20:  lea     DI,row
          cmp     CX,nlines              ; out of lines?
          jl      echo_50
          pushm   <t_attrib,ncols,nlines,ulcol,ulline>
          call    zscroll                ; scroll up one line
          mov     SP,BP
          mov     CX,nlines
          dec     CX                     ; cur_line = n_lines - 1
          xor     DX,DX                  ; cur_col = 0
; Decrement the contents of row vector
          push    AX                      ; save the character
          push    BX                      ; save the index
          push    CX
          mov     AX,BX                   ; AX = index
          xor     BX,BX
echo_30:  cmp     BX,AX                   ; j < index?
          jge     echo_40
;         mov     CL,byte ptr [DI+BX]
          dec     byte ptr [DI+BX]        ; row[j]--
;         mov     byte ptr [DI+BX],CL
          inc     BX                      ; j++
          jmp     echo_30
echo_40:  pop     CX
          pop     BX                      ; restore information
          pop     AX
echo_50:  dec     BX                      ; update row and column vectors
          mov     byte ptr [DI+BX],CL
          lea     DI,column
          mov     byte ptr [DI+BX],DL
          cmp     AL,TAB                  ; tab key?
          jne     echo_100
; Process the TAB key
          mov     AX,DX
          mov     BX,8
          div     BL                     ; AH = cur_col % 8
          sub     BL,AH
          add     DX,BX
          cmp     DX,ncols               ; end of line?
          jle     echo_60
          mov     DX,ncols
echo_60:  mov     BX,DX
          add     BX,ulcol
          cmp     BX,80                  ; out of screen?
          jl      echo_200
          mov     BX,79
          pushm   <BX,CX>
          call    zputcur
          mov     SP,BP
          jmp     echo_200
; Process the non-TAB key
echo_100: mov     curline,CX              ; save the information
          mov     curcol,DX
          add     DX,ulcol
          add     CX,ulline
          pushm   <t_attrib,AX,DX,CX>
          call    zputc
          mov     SP,BP
          mov     DX,curcol               ; restore the information
          mov     CX,curline
          inc     DX
echo_200: mov     curline,CX
          mov     curcol,DX
          pop     BP
          ret
echo_ch   endp
;*************************************************************************
;      Push a single character back into the input buffer
;*************************************************************************
          public  pushchar
pushchar  proc    near
          push    ES
          push    BP
          mov     BP,SP
          push    SI
          push    BX
;;;       LoadPage ES,port_seg           ; Get port page
          lea     SI,port_r
          mov     BX,[SI].C_page
          LoadPage ES,BX
;;;       mov     ES,port_seg            ; get address of page
          mov     SI,port_d
          mov     BX,word ptr ES:[SI+BUF_POS] ; input buffer starting position
          cmp     BX,0                   ; any character available?
          jle     push_err               ; no, error
          dec     BX
          mov     word ptr ES:[SI+BUF_POS],BX ; decrement the starting position
push_ret: pop     BX
          pop     SI
          pop     BP
          pop     ES
          ret
push_err: lea     BX,push_er
          push    BX
          C_call  printf,,Load_ES        ; print error message
          mov     SP,BP
          C_call  force_de,,Load_ES      ; force_debug()
          mov     SP,BP
          jmp     push_ret
pushchar  endp

rd_proc   proc    near
;*************************************************************************
;                  Support for read-char-ready?
;*************************************************************************
          extrn   next_SP:near
          extrn   src_err:near
          public  rd_ch_rd
          public  read_cha

rd_ch_rd: lods    byte ptr ES:[SI]
          save    <SI>
          add     AX,offset reg0         ; compute register address
          mov     DI,AX
          save    <DI>                   ; save DI register
          xor     CX,CX
          push    CX
          push    AX
          C_call  get_port,,Load_ES      ; get port object
          mov     SP,BP
          test    AX,AX                  ; check return status
          jz      rd_010
          jmp     rd_err
;
rd_010:   restore <DI>
          mov     [DI].C_page,SPECCHAR*2 ; prepare to return a character
          mov     SI,tmp_disp
          mov     BX,tmp_page
          LoadPage ES,BX                    ; get page address
;;;       mov     ES,word ptr pagetabl+[BX] ; get address of page
          mov     BX,word ptr ES:[SI+BUF_POS] ; input buffer starting position
          cmp     BX,word ptr ES:[SI+BUF_END] ; compare with ending position
          jge     rd_020
          xor     AH,AH
          mov     AL,byte ptr ES:[SI+BUFR+BX] ; get the character
rd_T:     cmp     AL,CTRL_Z              ; control-Z?
          jne     rd_015
          mov     BX,word ptr ES:[SI+P_FLAGS]
          and     BX,BINARY              ; binary file?
          jnz     rd_015
rd_eof:   mov     [DI].C_page,EOF_PAGE*2 ; return eof character
          mov     [DI].C_disp,EOF_DISP
          jmp     next_SP
;
rd_015:   mov     [DI].C_disp,AX         ; return the character
          jmp     next_SP
; no character in input buffer
rd_020:   mov     AX,word ptr ES:[SI+P_FLAGS]
          mov     BX,AX
          and     AX,WINDOW              ; window?
          jz      rd_030
          call    zch_rdy                ; any character?
          test    AX,AX
          jz      rd_no
          xor     AH,AH                  ; yes
          jmp     rd_T
; no character available -- return '()
rd_no:    xor     AX,AX
          mov     [DI].C_page,AX
          mov     [DI].C_disp,AX
          jmp     next_SP
; not a window
rd_030:   and     BX,OPEN                ; open?
          jz      rd_no                  ; no, return '()
          pushm   <tmp_disp,tmp_page>
          call    ssetadr
          mov     SP,BP
          call    take_ch                ; get one character
          mov     SP,BP
          restore <DI>
          cmp     AX,256                 ; eof?
          je      rd_eof
          call    pushchar               ; no, put it back
          mov     SP,BP
          jmp     rd_015

; Wrong port object, display error message
rd_err:   lea     BX,ch_rd
          jmp     src_err                ; link to error handler

;;;************************************************************************
;;;                Support for read-char
;;;************************************************************************
read_cha: lods    byte ptr ES:[SI]
          save    <SI>
          add     AX,offset reg0         ; compute register address
          mov     DI,AX
          save    <DI>                   ; save DI register
          xor     CX,CX
          push    CX
          push    AX
          C_call  get_port,,Load_ES      ; get port object
          mov     SP,BP
          test    AX,AX                  ; check return status
          jz      rc_010
          jmp     rc_err
;
rc_010:   restore <DI>
          mov     [DI].C_page,SPECCHAR*2
          mov     BX,tmp_page
          LoadPage ES,BX                    ; get page address
;;;       mov     ES,word ptr pagetabl+[BX] ; get address of page
          mov     SI,tmp_disp
          mov     AX,word ptr ES:[SI+P_FLAGS] ; get port flags
          mov     BX,AX
          and     AX,WINDOW              ; window object?
          jz      rc_050
          and     BX,STRIO               ; string object?
          jnz     rc_050
          mov     CX,word ptr ES:[SI+BUF_POS]
          cmp     CX,word ptr ES:[SI+BUF_END] ; any character in buffer?
          jl      rc_050
          mov     CX,word ptr ES:[SI+CUR_LINE]
          add     CX,word ptr ES:[SI+UL_LINE]
          mov     DX,word ptr ES:[SI+CUR_COL]
          add     DX,word ptr ES:[SI+UL_COL]

          push    AX
          mov     AX,word ptr ES:[SI+T_ATTR]
          mov     t_attrib,AX
          pop     AX

          pushm   <DX,CX>
          call    zputcur                ; cursor position
          mov     SP,BP
          call    zcuron                 ; cursor on
          mov     SP,BP
          call    getch                  ; get character
          mov     [DI].C_disp,AX
          mov     byte ptr ES:[SI+BUFR],AL ; store in port object
          call    zcuroff                ; cursor off
          mov     SP,BP
          mov     BX,1
          mov     word ptr ES:[SI+BUF_POS],BX
          mov     word ptr ES:[SI+BUF_END],BX
          jmp     next_SP
;
rc_050:   pushm   <tmp_disp,tmp_page>
          call    ssetadr               ; set port address
          mov     SP,BP
          call    take_ch               ; take one character
          mov     SP,BP
          restore <DI>
          cmp     AX,256                ; eof?
          je      rc_060
          jmp     rd_015                ; return the character
rc_060:   jmp     rd_eof
;
rc_err:   lea     BX,rch_er             ; address of error message
          jmp     src_err               ; jump to error handler
rd_proc   endp
;;;****************************************************************
;;;            Output a single character
;;;****************************************************************
give_arg  struc
lenn      dw      ?                        ; character string length
lenn2     dw      ?                        ; second copy of length
sav_pg    dw      ?
sav_ds    dw      ?
give_SI   dw      ?
give_DX   dw      ?
give_CX   dw      ?
give_BX   dw      ?
give_BP   dw      ?                        ; caller's BP
          dw      ?                        ; caller's ES
          dw      ?                        ; caller's return address
char      dw      ?                        ; the character to be output
give_arg  ends
          extrn   zscroll:near
          extrn   force_de:near
          extrn   zputc:near
          extrn   printf:near
          extrn   zwrite:near
          extrn   force_re:near
          public  givechar
givechar  proc    near
          push    ES
          push    BP
          sub     SP,offset give_BP
          mov     BP,SP
          mov     [BP].give_SI,SI          ; save registers
          mov     [BP].give_DX,DX
          mov     [BP].give_CX,CX
          mov     [BP].give_BX,BX
          cmp     TRNS_pag,0               ; transcript file?
          je      give_010
          mov     BX,direct
          and     BX,TRANSCRI
          jz      give_010
; transcript file "on"
          lea     BX,port_r
          mov     DX,[BX].C_page
          mov     [BP].sav_pg,DX
          mov     DX,[BX].C_disp
          mov     [BP].sav_ds,DX
          pushm   <TRNS_dis,TRNS_pag>
          call    ssetadr                  ; set transcript file
          mov     SP,BP
          push    [BP].char
          call    givechar                 ; output to transcript file
          mov     SP,BP
          pushm   <[BP].sav_ds,[BP].sav_pg>
          call    ssetadr                  ; set port address
          mov     SP,BP
;
give_010: mov     CX,[BP].char
          cmp     win_p,0                  ; window?
          jne     give_015
          jmp     give_fil                 ; no, jump
give_015: cmp     str_p,0                  ; string?
          je      give_018
          jmp     give_030                 ; yes, return
; Output to window
give_018: cmp     CL,RETURN                ; carriage return?
          jne     give_020
          mov     CL,LF                    ; yes, change to LF
give_020:
;;;       call    putc_win                 ; putc_window
;;;********************************************************************
;;;                     Output Character to Window
;;;
;;; Description:This routine writes a character to the current cursor
;;;             position, then increments the cursor location.
;;;             If the current cursor position is now within the bounds
;;;             of the window, the character is output in the first
;;;             column of the next line, scrolling the window, if
;;;             necessary.  The current text attributes are used to
;;;             write the character.
;;; Note: CX = character
;;;********************************************************************
          mov     SI,port_d                ; get displacement
          lea     BX,port_r
          mov     BX,[BX].C_page
          LoadPage ES,BX
;;;       LoadPage ES,port_seg             ; get port page
;;;       mov     ES,port_seg              ; get page segment
          mov     AX,direct                ; get the port flag
          and     AX,OPEN                  ; open for write?
          jnz     putc_002
          jmp     give_ret
putc_002: mov     BX,word ptr ES:[SI+CUR_LINE] ; BX = cur_line
          mov     AX,word ptr ES:[SI+CUR_COL]  ; AX = cur_col
          mov     DX,word ptr ES:[SI+UL_LINE]
          mov     ulline,DX
          mov     DX,word ptr ES:[SI+UL_COL]
          mov     ulcol,DX
          mov     DX,word ptr ES:[SI+N_LINES]
          mov     nlines,DX
          mov     DX,word ptr ES:[SI+N_COLS]
          mov     ncols,DX
          mov     DX,word ptr ES:[SI+T_ATTR]
          mov     t_attrib,DX
; Check for the character
          cmp     CL,NULL_CH               ; null character?
          jne     putc_010
          jmp     give_ret                 ; do nothing
;
putc_010: cmp     CL,BACKSP                ; backspace?
          jne     putc_020
          dec     AX
          cmp     AX,0
          jl      putc_015
          jmp     putc_120
putc_015: xor     AX,AX                    ; cur_col = 0
          jmp     putc_120
;
putc_020: cmp     CL,BELL_CH               ; bell character?
          jne     putc_030
          call    zbell                    ; sound the alarm
          mov     SP,BP
          jmp     give_ret
;
putc_030: cmp     CL,TAB                   ; tab character?
          jne     putc_050
          mov     CX,AX
          mov     DX,8                     ; DL = 8
          div     DL                       ; AH = (cur_col % 8)
          sub     DL,AH
          add     CX,DX
          mov     AX,CX
          jmp     putc_120
;
;putc_040: cmp    CL,RETURN                ; carriage return?
;         jne     putc_050
;         xor     AX,AX                    ; cur_col = 0
;         jmp     putc_100
;
putc_050: cmp     CL,LF                    ; line feed?
          jne     putc_060
          xor     AX,AX
          inc     BX
          cmp     BX,nlines                ; out of lines?
          jge     putc_055
          jmp     putc_100
putc_055: pushm   <t_attrib,ncols,nlines,ulcol,ulline>
          call    zscroll                  ; scroll window up one line
          mov     SP,BP
          mov     BX,nlines
          dec     BX
          xor     AX,AX
          jmp     putc_100
; default
putc_060: cmp     AX,ncols                 ; check end of line
          jl      putc_080
          mov     DX,word ptr ES:[SI+W_FLAGS]
          and     DX,WRAP
          jz      putc_070
          inc     BX                       ; wrap
          xor     AX,AX
          jmp     putc_080
putc_070: inc     AX                       ; clip
          jmp     putc_100                 ; no display
putc_080: cmp     BX,nlines                ; check out of lines?
          jl      putc_090
          pushm   <t_attrib,ncols,nlines,ulcol,ulline>
          call    zscroll                  ; scroll window up one line
          mov     SP,BP
          mov     BX,nlines
          dec     BX                       ; set up current line number
          xor     AX,AX                    ; and current column number
putc_090: mov     curcol,AX
          mov     curline,BX
          push    t_attrib                 ; text character attribute
          push    [BP].char                ; character
          add     AX,ulcol
          push    AX                       ; column number to console
          add     BX,ulline
          push    BX                       ; line number to console
          call    zputc                    ; write on cursor position
          mov     SP,BP
          mov     AX,curcol
          mov     BX,curline
          inc     AX                       ; increment current column
putc_100: mov     ES:[SI+CUR_LINE],BX      ; save current cursor line number
putc_120: mov     ES:[SI+CUR_COL],AX       ; save current cursor column number
give_030: jmp     give_ret
; Output to file
give_fil: lea     BX,[BP].lenn
          mov     word ptr [BX],1          ; lenn <- 1
          mov     word ptr [BX+2],1        ; lenn2 <- 1
          lea     SI,[BP].char
          mov     AX,handlee
	  test	  direct,BINARY		   ; Binary file?
	  jnz	  give_50		   ; Yes, jump
          cmp     CL,LF                    ; Line feed?
          jne     give_50                  ; no, jump
          mov     word ptr [SI],RETURN     ; output carriage return
          pushm   <BX, SI, AX>
          call    zwrite
          mov     SP,BP
          test    AX,AX                    ; check return status
          jnz     give_er                  ; error, jump
          mov     AX,[BP].lenn             ; #chars spec'd = #chars written?
          cmp     AX,[BP].lenn2
	  jne     give_disk
	  mov     AX,handlee
          jmp     give_80
;
give_50:  pushm   <BX, SI, AX>
          call    zwrite
          mov     SP,BP
          test    AX,AX
          jnz     give_er
          mov     AX,[BP].lenn             ; #chars spec'd = #chars written?
          cmp     AX,[BP].lenn2
          cmp     AX,[BP].lenn2
	  jne	  give_disk
	  test	  direct,BINARY		   ; Binary file?
	  jnz	  give_100		   ; yes, jump
          cmp     word ptr [SI],RETURN     ; carriage return?
          jne     give_100                 ; no, jump
          mov     AX,handlee
;;;       cmp     AX,prn_hand              ; printer?
;;;       je      give_100                 ; yes, jump
give_80:  lea     SI,[BP].char
          mov     word ptr [SI],LF         ; output line feed
          lea     BX,[BP].lenn
          mov     word ptr [BX],1
          pushm   <BX,SI,AX>
          call    zwrite
          mov     SP,BP
          test    AX,AX                    ; check return status
          jnz     give_er
          mov     AX,[BP].lenn             ; #chars spec'd = #chars written?
          cmp     AX,[BP].lenn2
          cmp     AX,[BP].lenn2
	  je	  give_100
give_disk:
	  mov	  ax,DISK_FULL_ERROR  	   ; Note disk full error
	  jmp	  short give_er1
give_er:  add	 ax,(IO_ERRORS_START - 1)  ; make dos i/o error number  
give_er1: mov	 BX,1
	  lea	 CX,port_r
	  pushm  <CX,AX,BX>		   ; 1 = non-restartable
	  ; We will not return from call to dos_err
          call    dos_err		   ; invoke scheme error handler

give_100: lea     BX,port_r
          mov     BX,[BX].C_page
          LoadPage ES,BX
;;;       LoadPage ES,port_seg
;;;       mov     ES,port_seg
          mov     BX,word ptr [SI]         ; get the character
          mov     SI,port_d
          mov     AX,word ptr ES:[SI+CUR_COL]
	  test	  direct,BINARY		   ; Binary file?
	  jnz     give_200
          cmp     BL,BACKSP                ; back space?
          jne     give_110
          dec     AX
          cmp     AX,0
          jge     give_200
give_rt:  xor     AX,AX
          jmp     give_200
give_110: cmp     BL,TAB                   ; tab?
          jne     give_120
          mov     CX,AX
          mov     DX,8
          div     DL                       ; AH = (cur_col % 8)
          sub     DL,AH
          add     CX,DX
          mov     AX,CX
          jmp     give_200
;
give_120: cmp     BL,RETURN                ; carriage return?
          jne     give_130		   ; no,  continue
	  mov	  BL,LF			   ; yes, make it a linefeed
          jmp     give_rt
;
give_130: cmp     BL,LF                    ; line feed?
          jne     give_140
          jmp     give_rt
; default
give_140: cmp     AX,word ptr ES:[SI+N_COLS]
          jge     give_rt
          inc     AX
;
give_200: 
	  cmp     word ptr ES:[SI+N_COLS],0	; Line length = 0 ?
	  je	  give_20a			; Yes, don't maintain column
	  mov     ES:[SI+CUR_COL],AX
give_20a: mov     AX,word ptr ES:[SI+BUF_POS]
          inc     AX
	  test    direct,BINARY			; Binary file?
	  jnz	  give_20b			;   yes, jump
	  cmp	  BX,LF				; CR or LF just output?
	  jne	  give_20b			;   no,  jump
	  inc	  AX				;   yes  bump # bytes written
give_20b:
          cmp     AX,256			; Exceed chunk boundary?
          jle     give_201			;   no,  jump
	  sub	  AX,256			; AX = excess above chunk
          inc     word ptr ES:[SI+CHUNK]	; bump chunk #
give_201: mov     word ptr ES:[SI+BUF_POS],AX   ; set the buffer position

give_ret: xor     AX,AX
          add     SP,offset give_BP        ; release local storage
          mov     SI,[BP].give_SI          ; restore registers
          mov     DX,[BP].give_DX
          mov     CX,[BP].give_CX
          mov     BX,[BP].give_BX
          pop     BP
          pop     ES
          ret
givechar  endp

prog      ends
          end
