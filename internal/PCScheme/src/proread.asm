;                                                             =====> PROREAD.ASM
;*****************************************************************
;*     			Lowlevel Read Support			 *
;*                                     				 *
;* 		(C) Copyright 1985, 1986 by Texas   		 *
;*		     Instruments Incorporated.       		 *
;*        		All rights reserved.         		 *
;*				                                 *
;* Date Written:  24 March 1986        			 	 *
;* Last Modification:                  				 *
;*				                                 *
;*   14 Apr 86 (tc) Change references to pagetabl to call        *
;*     		    memory manager for use with ext/exp memory.  *
;*    9 Sep 86 (ds) EGA support.                		 *
;*   21 Nov 86 (rb) Detect disk full error correctly.		 *
;*    7 Jan 87 (ds) Added support for random I/O.   		 *
;*   10 Feb 87 (tc) EOF-DISP modified to reflect other changes.  *
;*		    in Page 5 symbols.				 *
;*   16 Mar 87 (tc) Added Binary I/O, Error handling, better     *
;*     		    handling for Disk Full		         *
;*****************************************************************
          page    60,132
          include scheme.equ
          include sinterp.arg

MSDOS		equ	21h

BACKSP    	equ     08
TAB       	equ     09
RETURN    	equ     0Dh
LF        	equ     0Ah
CTRL_Z    	equ     1Ah
LEFT_AR   	equ     4Bh
RIGHT_AR  	equ     4Dh
F3        	equ     3Dh
F5        	equ     3Fh
INSERT    	equ     52h
DELETE    	equ     53h
ENTER     	equ     0Dh
NULL_CH   	equ     0
BELL_CH      	equ     07
BLANK        	equ     0020h

SCREEN_WIDTH 	equ  80
buf_len      	equ     253

DGROUP    group   data
data      segment word public 'DATA'
          assume  DS:DGROUP
;from iosuport.asm
	  extrn   port_r:word, port_pg:word, port_ds:word, port_seg:word
;from ???
          extrn  vid_mode:word


          public  cur_off, char_hgt
;
; Local error messages
;
ch_rd     db      "CHAR-READY?",0
rch_er    db      "READ-CHAR",0
push_er   db      "[VM INTERNAL ERROR] pushchar: failed",CR,LF,0
rd_st_er  db      "[VM INTERNAL ERROR] takechar: source not a string",CR,LF,0


cur_off   dw      0
char_hgt  dw      8

;
; The following data is used to capture and restore data entered from
; the console. All characters entered are saved in a shadow buffer
; so that they may be recalled via the F3, and F5 keys
;
insert_m  dw      0                        ;insert mode flag
index     dw      0                        ;index into port buffer
sh_ptr    dw      0                        ;pointer into shadow buffer
sh_len    dw      0                        ;length of shadow buffer
sh_bufer  db      256 dup (0)              ;shadow buffer for characters
row       dw      256 dup (0)              ;row vector
column    dw      256 dup (0)              ;column vector

data      ends


PGROUP    group   prog
prog      segment byte public 'PROG'
          assume  CS:PGROUP
;from basicio.asm
	  extrn   zbell:near,zscroll:near,zputcur:near
          extrn   zputc:near,zcuron:near,zcuroff:near
          extrn   zread:near,stringrd:near,char_rdy:near
	  extrn   ega_curs:near
;from ???
          extrn   getch:near,ssetadr:near
;from 
          extrn   toblock:near
;from sprint.asm
          extrn   printtxt:near
;from sinterp.asm
          extrn   next_SP:near,src_err:near,sch_err:near,dos_err:near

;;;**************************************************************************
;;;                  Input a Single Character
;;;**************************************************************************

take_buf_len equ  256

take_arg  struc
tk_leng   dw	  take_buf_len
new_bpos  dw	  0
tk_bp     dw      ?                        ;caller's BP
          dw      ?                        ;caller's ES
          dw      ?                        ;caller's return address
take_arg  ends

          public  take_ch
take_ch   proc    near
          push    es
          push    bp
          sub     sp,offset tk_bp         ;allocate local storage
          mov     bp,sp
	  mov	  [BP].new_bpos,0	  ;buf position after refilling buf
	  LoadPage es,port_pg
	  mov	   si,port_ds		  ;es:si => port object

; Fix for random I/O - read preceeded by a write
	  test	  byte ptr es:[si].pt_pflgs,READWRITE+WRITE_ONLY
	  jz  	  take_c00			      ;skip if input port
	  mov	  bl,byte ptr es:[si].pt_pflgs	      ;get port flags
	  and	  bl,DIRTY+STRIO+OPEN+WINDOW	      ;isolate appropriate flags
	  cmp	  bl,DIRTY+OPEN		              ;buffer modified?
	  jne	  take_c00		   	      ; no, jump
	  and     byte ptr es:[si].pt_pflgs,NOT DIRTY ;clear flag
; this read was preceeded by at least one write, so reposition file pointer
; so it rereads the buffer
	  mov	  bx,word ptr es:[si].pt_handl
	  dec	     word ptr es:[si].pt_chunk
	  mov	  cx,word ptr es:[si].pt_chunk
	  xor	  dl,dl
	  mov	  dh,cl
	  mov	  cl,ch
	  xor     ch,ch
  	  mov	  ax,4200h		     ; reposition file pointer
	  int	  MSDOS
	  mov	  bx,es:[si].pt_bfpos	     ; after re-reading file, restore
	  mov	  [BP].new_bpos,bx	     ; current buffer position
	  jmp	  take_fill		     ; go re-read the file
take_c00:
          mov     bx,es:[si].pt_bfpos
          cmp     bx,es:[si].pt_bfend	  ;have we exceeded port's buffer?
          jge     take_fill		  ;  yes, go fill it again
take_nxt: 				  
          xor     ah,ah
          mov     al,byte ptr es:[si+pt_buffr+bx] ;get next char from buffer
          inc     bx			   ;bump buffer position pointer
          mov     es:[si].pt_bfpos,bx	   ; and update in port object
          cmp     al,CTRL_Z                ;control-z?
          jne     take_ret                 ; no, return
	  test	  es:[si].pt_pflgs,BINARY  ;binary file?
          jnz     take_ret                 ; no, return
take_eof: mov     AX,256                   ;text file, send EOF
take_ret: add     sp,offset tk_bp          ; release local storage
          pop     bp
          pop     es
          ret

; buffer empty -- fill it up
take_fill:
	  mov     [BP].tk_leng,take_buf_len  ;set up buffer length
	  test    es:[si].pt_pflgs,WINDOW    ;window port?
	  jz	  take_fil		     ; no, jump
	  test    es:[si].pt_pflgs,STRIO     ;string port?
	  jnz	  take_str		     ; yes, jump
; read from window
	  call    read_win		   ;read from window
          mov     BX,AX
          jmp     take_11
; Read from file
	  public  take_fil
take_fil: 
   	  cmp	  word ptr es:[si].pt_chunk,1 ; operating on first chunk?
   	  jne	  take_f05		      ; no, jump	
   	  cmp	  word ptr es:[si].pt_bfpos,0 ; Have we filled the buffer yet?
   	  je	  take_f10		      ; yes, jump	
take_f05:
	  inc	  word ptr es:[si].pt_chunk   ; bump the chunk number
take_f10:
	  push	  bp			   ;<this is tk_leng>
          lea     ax,row
          push    ax			   ;address of input buffer
	  push	  es:[si].pt_handl	   ;file handle
          call    zread			   ;read from file
          mov     sp,bp			   ;dump args from stack
          test    ax,ax                    ;error?
          jnz     take_err                 ; yes, jump
	  jmp	  take_10
; read character from string
take_str:
          mov     ax,ds
          mov     es,ax                    ;es = ds
	  push	  bp			   ;<this is tk_leng>
          lea     bx,row
          push    bx			   ;buffer for characters
	  push	  port_ds		   ;port displacement
	  push	  port_pg		   ;port page
          call    stringrd		   ;read from string
          mov     sp,bp			   ;dump args off stack
          test    ax,ax                    ;error encountered?
	  jz	  take_05  		   ; no,  jump
          lea     bx,rd_st_er              ;address of error message
          push    bx
          C_call  printf                   ;display error message
          mov     sp,bp			   ;dump args from stack
take_05:
	  LoadPage es,port_pg		   ;restore port addressability
	  mov	  si,port_ds
;
take_10:  mov     bx,[bp].tk_leng	  ;bx = length	
take_11:  mov     es:[si].pt_bfend,bx     ;update buffer length
          test    bx,bx                   ;length = zero?
          jnz     take_20                 ; no,  jump
          mov     es:[si].pt_bfpos,bx	  ; yes, position = end
          jmp     take_eof		  ;      note eof
take_20:  
	  test	  es:[si].pt_pflgs,WINDOW ;window port?
	  jz	  take_22		  ; no, jump
	  test	  es:[si].pt_pflgs,STRIO  ;string port?
	  jz	  take_25		  ; no, jump
; copy characters from buffer to file object
take_22:  
	  push    si                      ;tempsave si
          mov     di,si
          add     di,pt_buffr		  ;es:di => port buffer
          lea     si,row		  ;ds:si => char buffer
          mov     cx,bx                   ;# characters to move
          cld                             ;direction forward
rep       movsb				  ;do it
          pop     si                      ;restore si
take_25:  
	  mov	  bx,[bp].new_bpos	  ;BX = buffer position
	  jmp	take_nxt
take_err:
; We will not return from call to dos_err
	  add	 ax,(IO_ERRORS_START - 1)  ;make Dos I/O error number
	  mov	 bx,1			   ;non-restartable
	  lea    cx,port_r		   ;port object
	  pushm  <cx,ax,bx>	   	   ;invoke scheme error handler
	  call   dos_err		   ;control will not return here
take_ch   endp

;**************************************************************************
;         Read a "record" from window
;                        ES:SI points to the window object
;                        Return AX = number of characters read
;**************************************************************************
read_arg  struc
          dw      ?                        ;caller's bp
          dw      ?                        ;return address
read_arg  ends

          public  read_win
read_win  proc    near
          push    bp
          mov     bp,sp
          mov     index,0		   ;clear index into port buffer
          mov     sh_ptr,0		   ;clear index into shadow buffer
          mov     insert_m,0		   ;clear insert flag
          call    zcuron                   ;turn on the cursor
	  mov	  bx,es:[si].pt_text	   ;get text attribute for window
read_001: 
	  mov     bx,es:[si].pt_cline      ;bx = current line number
          cmp     bx,es:[si].pt_nline      ;have we exceeded number of lines?
          jl      read_put		   ; no, jump
	  push	  es:[si].pt_text
	  push	  es:[si].pt_ncols
	  push	  es:[si].pt_nline
	  push	  es:[si].pt_ulcol
	  push	  es:[si].pt_ullin
          call    zscroll                  ;scroll up one line
          mov     sp,bp			   ;dump args off stack
          mov     bx,es:[si].pt_nline
          dec     bx
          mov     es:[si].pt_cline,bx	   ;current line = #lines - 1
          mov     es:[si].pt_ccol,0        ;current column = 0
read_put: 
	  mov     dx,es:[si].pt_ccol
          add     dx,es:[si].pt_ulcol
          add     bx,es:[si].pt_ullin
          pushm   <dx,bx>
          call    zputcur                  ;show the cursor
          mov     sp,bp			   ;bump args off stack

          call    getch                    ;get character from console
          test    al,al                    ;extended character?
          jz      read_ex
          jmp     read_100		   ;  no, go process ascii character
;
; Process extended key sequence
;
read_ex:  
	  call    getch                    ;get extended character from console
          cmp     al,LEFT_AR               ;left arrow key?
          jne     read_ra		   ;  no,  jump	
          jmp     read_bs                  ;  yes, treat as backspace
; Check for RIGHT ARROW key
read_ra:  
	  cmp     al,RIGHT_AR              ;right arrow key?
          jne     read_f3		   ;  no, jump
          mov     insert_m,0               ;turn off insert mode
          mov     bx,sh_ptr		   ;bx => shadow buffer
          cmp     bx,sh_len		   ;if more chars in shadow buffer
          jl      read_030                 ;  then go fetch
          jmp     read_001		   ;  else go read next char from window
read_030: 
	  lea     di,sh_bufer		   ;ds:di => shadow buffer
          mov     al,byte ptr [di+bx]	   ;fetch character from buffer
          jmp     read_one		   ;and go echo to screen
; Check for F3 key
read_f3:  
	  cmp     AL,F3                    ;F3 key?
          jne     read_f5		   ; no, jump
          mov     insert_m,0               ;turn off insert mode
read_041: mov     cx,index
          cmp     cx,buf_len               ;have we exceeded port buffer?
          jl      read_043		   ;  no, jump
          jmp     read_001                 ;no room for more chars
read_043: 
	  mov     bx,sh_ptr		   ;bx => shadow buffer
          cmp     bx,sh_len                ;have we exceeded length of buffer?
          jl      read_045		   ; no, jump
          jmp     read_001
read_045: lea     di,sh_bufer		   ;ds:di => shadow buffer
          mov     al,byte ptr [di+bx]	   ;get character from buffer
          call    echo_ch                  ;echo to screen
          mov     sp,bp			   ;bump args from stack
          jmp     read_041		   ;go get next character
; Check for F5 key
read_f5:  cmp     AL,F5                    ;F5 key?
          jne     read_ins		   ; no, jump
          call  ega_curs                   ;turn off the EGA cursor
          mov     insert_m,0               ;disable insert mode
          cmp     index,0
          jne     read_051
          jmp     read_001
read_051: 
	  call    str_str                  ;copy from port buf to shadow buf
          mov     bx,index		   ;bx = index into port buffer
          mov     sh_len,bx		   ;update shadow buffer length
          mov     byte ptr [di+bx],0       ;note end of string
          dec     bx			   ;bx => last char in shadow buffer
          lea     di,row                   ;di => row vector
read_053: 
	  cmp     bx,0			   ;reached start of shadow buffer?
          jl      read_055		   ; yes, exit loop
          cmp     byte ptr [di+bx],0	   ;at top of screen?
          jl      read_055		   ; yes, exit loop
          mov     ax,BLANK		   ;blank character for write
          lea     si,column                ;si => column vector
	  xor	  ch,ch
	  mov	  cl,byte ptr [si+bx]	   ;cl = column for character
	  xor	  dh,dh
          mov     dl,byte ptr [di+bx]	   ;dl = row for character
	  mov	  si,port_ds		   ;si => port object
	  mov	  es:[si].pt_ccol,cx	   ;update column
	  mov	  es:[si].pt_cline,dx	   ; and row
	  add	  cx,es:[si].pt_ulcol	   ;cx = column within window
	  add	  dx,es:[si].pt_ullin	   ;dx = row within window
          push	  bx			   ;tempsave bx around call

          push    es:[si].pt_text	   ;text attribute
	  push	  ax			   ;blank character
	  push	  cx			   ;column
	  push	  dx			   ;row
          call    zputc			   ;clear character from window
          add	  sp,8			   ;dump args off stack

	  pop	  bx			   ;restore shadow buffer index
          dec     bx			   ;and decrement for next character
          jmp     read_053		   ;go clear next character
read_055: 
          mov     index,0		   ;clear index into port buffer
          mov     sh_ptr,0		   ;clear index into shadow buffer
          jmp     read_001		   ;go read the next character
; Check for INSERT key
read_ins: cmp     al,INSERT                ;insert key?
          jne     read_del
          call  ega_curs                   ;turn off the EGA cursor
          mov     insert_m,1               ;turn on insert mode
          jmp     read_001
; Check for DELETE key
read_del: cmp     al,DELETE                ;delete key?
          jne     read_EN
          mov     insert_m,0               ;turn off insert mode
          mov     bx,sh_ptr
          cmp     bx,sh_len                ;ensure still within shadow buffer
	  jg	  read_d02
read_d01: inc     sh_ptr
read_d02: jmp     read_001
; Check for ENTER key
read_EN:  cmp     al,ENTER                 ;enter key?
          je      read_RT                  ; yes, treat as carriage return
          jmp     read_001		   ;ignore all other extended keys
;
; Process ascii character key
;

; Check for BACKSPACE key
read_100: 
	  cmp     al,BACKSP               ;backspace?
          jne     read_200		  ; no, try next
read_bs:  mov     insert_m,0              ;disable insert mode
          call    ega_curs                ;disable EGA cursor
          mov     bx,index		  ;bx = port buffer index
          cmp     bx,0			  ;if already at buffer start
          jle     read_150		  ;  then jump
          dec     bx			  ;decrement port buffer index
          lea     di,row		  ;ds:di => row vector
          cmp     byte ptr [di+bx],0	  ;if at screen start
          jl      read_150		  ;  then jump
          mov     index,bx		  ;save buffer index
          cmp     sh_ptr,0		  ;if at start of shadow buffer
          je      read_120		  ; then jump
          dec     sh_ptr                  ; else backspace one character
read_120: lea     di,column		  ;ds:di => column vector
          xor     ch,ch
          mov     cl,byte ptr [di+bx]     ;get column of prior character
          mov     es:[si].pt_ccol,cx	  ; and update within port object
	  add	  cx,es:[si].pt_ulcol     ;cx = col within window
          xor     dh,dh			
          lea     di,row
          mov     dl,byte ptr [di+bx]     ;get line of prior character
          mov     es:[si].pt_cline,dx	  ; and update within port object
	  add	  dx,es:[si].pt_ullin     ;dx = line within window

          mov     bx,BLANK
          push    es:[si].pt_text	  ;text attribute
	  push	  bx			  ;blank character
	  push    cx			  ;column
	  push    dx			  ;line
          call    zputc			  ;blank out char on screen
          mov     sp,bp			  ;dump args off stack
          jmp     read_001
read_150: 
	  call    zbell			  ;beep
          jmp     read_001
; Check for BACKSPACE key
read_200: cmp     al,RETURN              ;carriage return?
          je      read_RT		 ; yes
          jmp     read_300               ; no, jump
; Process return key
read_RT: 
	  cmp    vid_mode,14		 ;if not in ega mode
          jl     read_rt1		 ;  then jump
          call   ega_curs                ;  else turn off the ega cursor
          or     cur_off,1		 ;       and note cursor off
read_rt1: 
	  mov     bx,index		 ;bx = port buffer index
          mov     byte ptr es:[si+pt_buffr+bx],RETURN  ;move CR to buffer
          inc     bx
          mov     byte ptr es:[si+pt_buffr+BX],LF      ;move LF to buffer
          inc     bx
          mov     index,bx		 ;update port buffer pointer
          mov     es:[si].pt_ccol,0	 ;clear current column
          mov     dx,es:[si].pt_cline    ;get current line
          inc     dx                     ; and increment
          cmp     dx,es:[si].pt_nline    ;if still on screen
          jl      read_220		 ;  then jump
          push    es:[si].pt_text
          push    es:[si].pt_ncols
          push    es:[si].pt_nline
          push    es:[si].pt_ulcol
          push    es:[si].pt_ullin
          call    zscroll                ;scroll up one line
          mov     sp,bp			 ;dump args off stack
          mov	  dx,es:[si].pt_nline
	  dec	  dx
read_220: mov     es:[si].pt_cline,dx    ;update current line
          call    str_str                ;copy shadow buffer into port buffer
	  cmp	  TRNS_pag,0
	  je	  read_250
	  test	  es:[si].pt_pflgs,TRANSCRI
	  jz	  read_250
; transcript file "on", write buffer to transcript file
	  push	  si			 ;save current port disp
	  push	  port_pg		 ;save current port page number

          pushm   <TRNS_dis,TRNS_pag>
          call    ssetadr                ;set transcript file address
          add	  sp,4			 ;bump args off stack
          mov     ax,index		 
          dec     ax
          push    ax		         ;index into buffer
          lea     bx,sh_bufer
          push    bx			 ;buffer address
          call    printtxt               ;output to transcript file
	  add 	  sp,4			 ;dump args off stack
          				 ;use port args saved above
          call    ssetadr                ;restore current port address
	  pop	  bx			 ;restore port page number
	  LoadPage es,bx		 ;es:si => port object
	  pop	  si			 ;restore port displacement
          lea     di,sh_bufer		 ;ds:di => shadow buffer
read_250: 
	  mov     bx,index		 ;bx = index into port buffer
          dec     bx			 ;decrement
          mov     byte ptr [di+bx],0     ;note end of string in shadow buffer
          dec     bx
          mov     sh_len,bx		 ;update shadow length
          jmp     read_done
; Check for LINEFEED key
read_300: 
	  cmp     al,LF                  ;line feed?
          jne     read_one		 ; no,  jump
          jmp     read_001               ; yes, ignore
; Default character encountered
read_one: 
	  mov     bx,index               ;bx = port buffer index
          cmp     bx,buf_len             ;have we exceeded buffer boundary?
          jl      read_420		 ;  no,  jump
          call    zbell			 ;  yes, sound beep
          jmp     read_001		 ;       and continue
read_420: 
	  call    echo_ch                ;echo character to display
          jmp     read_001		 ;go handle next read
; finished reading from window
read_done:
	  call    zcuroff                ;turn off the cursor
          mov     ax,index               ;return length
          pop     bp
          ret
read_win  endp

;*****************************************************************************
; Move the string in port object to buffer sh_bufer
;*****************************************************************************
str_str   proc    near
          lea     di,sh_bufer            ;di=address of shadow buffer
; Move the characters
	  push    si                     ;tempsave si
	  add     si,pt_buffr            ;port buffer address
          mov     cx,index		 ;cx = buffer length
          mov     AX,ES
          mov     BX,DS
          mov     ES,BX                  ;es:di => shadow buffer
          mov     DS,AX                  ;ds:si => port buffer
rep       movsb				 ;move 'em out
          mov     es,ax                  ;reset segment registers
          mov     ds,bx
          lea     di,sh_bufer		 ;di => shadow buffer
          pop     si                     ;si => port object
          ret
str_str   endp
;*****************************************************************************
;                          Echo single character
; Entry : al    = character to display
;         es:si => current port object
;*****************************************************************************
echo_ch   proc    near
          mov     bx,index		 ;bx = index within port buffer
          mov     byte ptr es:[si+bx+pt_buffr],al  ;store character
          inc     bx                     ;bump index
          mov     index,bx		 ; and update
          cmp     insert_m,0             ;insert mode?
          jne     echo_10		 ; yes, jump
          inc     sh_ptr                 ;bump shadow buffer index
echo_10:  
          mov     cx,es:[si].pt_cline	 ;cx = current column
	  mov     dx,es:[si].pt_ccol	 ;dx = current line
          cmp     dx,es:[si].pt_ncols    ;reached end of line?
          jl      echo_20		 ; no, jump
          inc     cx                     ;bump  current line
          xor     dx,dx                  ;clear current col
echo_20:  
	  lea     di,row		 ;ds:di => row vector
          cmp     cx,es:[si].pt_nline    ;exceed number lines?
          jl      echo_50		 ; no, jump
          push    es:[si].pt_text	 ;text attribute
          push    es:[si].pt_ncols	 ;number columns
          push    es:[si].pt_nline	 ;number lines
          push    es:[si].pt_ulcol	 ;upper left col
          push    es:[si].pt_ullin	 ;upper left line
          call    zscroll                ;scroll up one line
	  add	  sp,10			 ;dump args
          mov     cx,es:[si].pt_nline
          dec     cx                     ;update current line
          xor     dx,dx                  ;clear current column
; Decrement the contents of row vector
          push    ax                     ;tempsave character
          push    bx                     ;tempsave buffer index
          mov     ax,bx                  ;ax = port buffer index
          xor     bx,bx			 ;bx = buffer start
echo_30:  cmp     bx,ax                  ;have we reached buffer end
          jge     echo_40		 ; yes, jump
          dec     byte ptr [di+bx]       ;decrement row for character
          inc     bx                     ;index for next character
          jmp     echo_30		 ;loop till done
echo_40:  pop     bx                     ;restore buffer index
          pop     ax			 ;restore character
;update row/col vector for this character
echo_50:  
	  dec     bx			 ;create index into row/col vectors
          mov     byte ptr [di+bx],cl	 ;update row
          lea     di,column
          mov     byte ptr [di+bx],dl	 ;update col
          cmp     al,TAB                 ;is character the tab key?
          jne     echo_100		 ; no, jump
; Process the TAB key
          mov     ax,dx			 ;ax = current column
          mov     bx,8			 ;bx = tab spacing
          div     bl                     ;ah = remainder (cur_col % 8)
          sub     bl,ah			 ;bx = 8 - remainder
          add     dx,bx			 ;dx = (new) current column
          cmp     dx,es:[si].pt_ncols    ;exceeded line length?
          jle     echo_60		 ; no,  jump
          mov     dx,es:[si].pt_ncols    ; yes, current col = end of line
echo_60:  
	  mov	  es:[si].pt_ccol,dx     ;update current col
	  mov	  es:[si].pt_cline,cx	 ;update current line

	  mov     bx,dx			 ;bx = current column
          add     bx,es:[si].pt_ulcol	 ;bx = column within window
          cmp     bx,SCREEN_WIDTH        ;off of screen?
	  jl	  echo_ret		 ; no, jump
          mov     bx,(SCREEN_WIDTH - 1)	 ; yes, current col = last col  
          pushm   <bx,cx>
          call    zputcur		 ;position cursor
	  add	  sp,4			 ;dump args
          jmp     echo_ret		 ;return
; Process the non-TAB key
echo_100: 
	  mov	  es:[si].pt_cline,cx	 ;update current line
	  add	  cx,es:[si].pt_ullin	 ;cx = current lin relative to window
	  mov	  es:[si].pt_ccol,dx     ;update current line
	  add	  dx,es:[si].pt_ulcol    ;dx = current col relative to window

	  push	  es:[si].pt_text	 ;text attribute
	  push	  ax			 ;character to display
	  push	  dx			 ;column
	  push	  cx			 ;line
          call    zputc			 ;display character
	  add	  sp,8			 ;dump args
	  inc	  es:[si].pt_ccol	 ;update port's current column
echo_ret:
          ret
echo_ch   endp
;*************************************************************************
;      Push a single character back into the input buffer
;*************************************************************************
          public  pushchar
pushchar  proc    near
          push    es
          push    si

	  LoadPage es,port_pg
	  mov	  si,port_ds		;es:si => port object

          cmp     es:[si].pt_bfpos,0    ;any chars in buffer?
          jle     push_err              ;  no, error
          dec     es:[si].pt_bfpos      ;position to prio character
push_ret:
          pop     si
          pop     es
          ret
push_err: 
	  lea     bx,push_er		 ;bx = address of error msg
          push    bx			 ;pass to print routine
          C_call  printf,,Load_ES        ;print error message
          add	  sp,2			 ;dump args
          C_call  force_de,,Load_ES      ;envoke debugger
          add	  sp,2			 ;will we ever return here???
          jmp     push_ret
pushchar  endp

rd_proc   proc    near
;*************************************************************************
;                  Support for read-char-ready?
;*************************************************************************
          public  rd_ch_rd
          public  read_cha
rd_ch_rd: 
	  lods    byte ptr es:[si]	 ;get register
          save    <si>			 ;save vm instruction pointer
          add     ax,offset reg0         ;compute register address
          mov     di,ax
          save    <di> 			 ;save register argument for later
          xor     cx,cx
          push    cx
          push    ax
          C_call  get_port,,Load_ES      ;get port object
					 ;port returned in tmp_page:tmp_disp
          mov     sp,bp			 ;dump args
          test    ax,ax                  ;check return status
          jz      rd_010		 ;  no errors, continue
          jmp     rd_err		 ;  else jump to error handler
rd_010:   
	  restore <di>			 ;restore register argument
          mov     [di].C_page,SPECCHAR*2 ;prepare to return a character
          mov     si,tmp_disp
          LoadPage es,tmp_page           ;get page address
          mov     bx,es:[si].pt_bfpos    ;bx = buffer index
          cmp     bx,es:[si].pt_bfend    ;if at buffer end
          jge     rd_020		 ;  then go fill the buffer
;get character from port object buffer
          xor     ah,ah
          mov     al,byte ptr es:[si+pt_buffr+bx]  ;get the character
rd_T:     
	  cmp     al,CTRL_Z               ;control-z character?
          jne     rd_015		  ; no, continue
	  test	  es:[si].pt_pflgs,BINARY ;binary file?
          jnz     rd_015		  ; yes, continue
	  jmp	  rd_eof		  ; no,  return eof char
rd_015:   mov     [di].C_disp,ax          ;return the character
          jmp     next_SP
; no character in input buffer
rd_020:   
	  test	  es:[si].pt_pflgs,WINDOW ;window port?
	  jz	  rd_030		  ;  no, jump
          jz      rd_030
          call    char_rdy                ;check for character at console
          test    ax,ax			  ;was one there?
          jz      rd_no			  ; no, jump
          xor     ah,ah
          jmp     rd_T 			  ;go process
; no character available -- return '()
rd_no:    xor     ax,ax
          mov     [DI].C_page,ax
          mov     [DI].C_disp,ax
          jmp     next_SP
; not a window
rd_030:
	  test	  es:[si].pt_pflgs,OPEN   ;is the port open?
          jz      rd_no                   ; no, return '()
          pushm   <tmp_disp,tmp_page>
          call    ssetadr	          ;set up port address
          mov     sp,bp			  ;dump args
          call    take_ch                 ;get a character
          mov     sp,bp			  ;dump args
          restore <di>			  ;di => register for return
          cmp     ax,256                  ;eof?
	  jne	  rd_033		  ; no,  continue
	  jmp	  rd_eof		  ; yes, go process it
rd_033:
          call    pushchar                ; no,  put it back
          mov     sp,bp
          jmp     rd_015

; Wrong port object, display error message
rd_err:   lea     BX,ch_rd
          jmp     src_err                ; link to error handler

;;;************************************************************************
;;;                Support for read-char
;;;************************************************************************
read_cha: 
	  lods    byte ptr es:[si]	 ;get register
          save    <si>			 ;save vm instruction pointer
          add     ax,offset reg0         ;compute register address
          mov     di,ax
          save    <di> 			 ;save register argument for later
          xor     cx,cx
          push    cx
          push    ax
          C_call  get_port,,Load_ES      ;get port object
					 ;port returned in tmp_page:tmp_disp
          mov     sp,bp			 ;dump args
          test    ax,ax                  ;check return status
          jz      rc_010		 ; no errors, continue
          jmp     rc_err		 ; else jump to error handler
rc_010:   
	  restore <di>
          mov     [di].C_page,SPECCHAR*2 ;prepare to return character
          mov     si,tmp_disp
	  LoadPage es,tmp_page		 ;es:si => port object
          mov     bx,es:[si].pt_pflgs    ;get port flags
	  test	  bx,WINDOW		 ;window port?
	  jz	  rc_050		 ; no,  jump
	  test	  bx,STRIO		 ;string port?
	  jnz	  rc_050		 ; yes, jump
;read from window
          mov     cx,es:[si].pt_bfpos	 ;cx = port buffer index
          cmp     cx,es:[si].pt_bfend    ;any character in buffer?
          jl      rc_050		 ; no, jump
          mov     cx,es:[si].pt_cline
          add     cx,es:[si].pt_ullin	 ;cx = line
          mov     dx,es:[si].pt_ccol
          add     dx,es:[si].pt_ulcol	 ;dx = column
          pushm   <dx,cx>
          call    zputcur                ;position cursor
          mov     sp,bp			 ;dump args
          call    zcuron                 ;enable cursor
          call    getch                  ;get character from console
          mov     [di].C_disp,ax	 ;return character in reg
          mov     byte ptr es:[si].pt_buffr,al  ;store also in port object
          call    zcuroff                ;disable cursor
          mov     bx,1
          mov     es:[si].pt_bfpos,bx	 ;update port position
          mov     es:[si].pt_bfend,bx
          jmp     next_SP
;read from port object
rc_050:   
	  pushm   <tmp_disp,tmp_page>
          call    ssetadr                ;set port address
          mov     sp,bp
          call    take_ch                ;take one character
          mov     sp,bp
          restore <di>
          cmp     ax,256                 ;eof character?
          je      rd_eof		 ; yes, jump
          jmp     rd_015                 ; no,  return the character
;
rd_eof:   mov     [di].C_page,EOF_PAGE*2  ; no,  return eof character
          mov     [di].C_disp,EOF_DISP
          jmp     next_SP
;
rc_err:   lea     BX,rch_er             ; address of error message
          jmp     src_err               ; jump to error handler
rd_proc   endp

prog      ends
          end
