;                                                       =====> ZIO.ASM
;****************************************
;*     TIPC Scheme  Runtime Support     *
;*     File IO - MS-DOS Version 2.1     *
;*                                      *
;*    (C) Copyright 1985 by Texas       *
;*     Instruments Incorporated.        *
;*        All rights reserved.          *
;*                                      *
;* Date Written:  21 January 1985       *
;* Last Modification: 26 September 1986 *
;****************************************
          page    60,132
          include scheme.equ
          include pcmake.equ

MSDOS     equ     021h
TI_CRT    equ     049h
IBM_CRT   equ     010h
TI_KEYBD  equ     04Ah
IBM_KEYB  equ     016h

MAX_COLS  equ     80
MAX_ROWS  equ     25

CURSMASK  equ     10011111b        ; The zeros are the bits that disable cursor
NOCURSOR  equ     00100000b        ; byte mask to disable cursor

DGROUP    group   data
data      segment word public 'DATA'
          assume  DS:DGROUP
          public  zapcurs,curs_sav, ega_col, ega_row

zapcurs   dw	  0		   ; for disabling cursor altogether
curs_sav  dw      400Ch            ; For saving the cursor size when it's
                                   ; disabled.   Default value just in case...

ega_col  db ?
ega_row  db ?
c_row    dw ?
c_col    dw ?
c_len    dw ?

banka    dw 0a000h

sav_di   dw ?

         extrn  vid_mode:word
         extrn  cur_off:byte
         extrn  char_hgt:word

data      ends

XGROUP    group   progx
progx     segment word public 'progx'
          assume  CS:XGROUP
          extrn   z%border:far     ; border drawer
          extrn   crt_dsr:far      ; use machine appropriate VIDEO interrupt
          extrn   save%scr:far     ; save screen
          extrn   rest%scr:far     ; restore screen

         extrn  ega_curs:far        ; display an ega cursor
         extrn  enable:far          ; part of the ega cursor routine

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
          public  zbell
zbell     proc    far
          cmp     DGROUP:PC_MAKE,TIPC
          jne     zbmbell
zbwait:   mov     AH,1             ; Get speaker status
          int     48h
          jnz     zbwait           ; wait for bell to turn off
          mov     AH,2             ; Set speaker frequency
          mov     CX,1563          ; Value for 1.25MHz/800Hz (system beep)
          int     48h
          mov     AX,000Ah         ; Turn speaker on for AL*25-ms. 0Ah = .25-sec
          int     48h
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
          ret
zbell     endp

          public  zch_rdy
zch_rdy   proc    far

;         IFDEF   extmem           ; Kludge to fix hanging keyboard
;         mov     AL,0AEh          ; Ensure keyboard enabled
;         out     64h,AL           ; Output to 8042 controller
;         ENDIF

          mov     AH,01h           ; load "check keyboard status" function code
          cmp     pc_make,TIPC     ; TI or IBM flavored PC?
          jne     zch_IBM
          int     TI_KEYBD         ; issue TI keyboard DSR service call
          jz      zch_no           ; is character buffered? if not, jump
zch_yes:  xor     AH,AH            ; clear high order byte of AX
          cmp     AL,0             ; test next character to be read
          jne     zch_ret          ; binary zero?  if not, jump
          mov     AX,256           ; if character is 0, make it non-zero
zch_ret:  ret                      ; return (true)
zch_IBM:  int     IBM_KEYB         ; issue IBM keyboard DSR service call
          jnz     zch_yes          ; is character buffered?  if so, jump
zch_no:   xor     AX,AX            ; set result = false
          ret                      ; return (false)
zch_rdy   endp

zop_args  struc
          dd      ?             ; far CS and IP
          dw      ?                ; caller's BP
          dw      ?                ; return address
zhandle   dw      ?                ; address of handle
zpathnam  dw      ?                ; address of string containing file pathname
zmode     dw      ?                ; mode:  0=read, 1=write, 2=read/write
zhigh     dw      ?                ; address of high word of file size
zlow      dw      ?                ; address of low word of file size
zop_args  ends

          public  z%open
z%open    proc    far
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
;
          xor     AX,AX            ; set return code for normal return
zop_ret:  pop     BP               ; restore caller's BP
          ret                      ; return
z%open    endp

          public  z%create
z%create  proc    far
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
z%create  endp

          public  z%close
z%close   proc    far
          push    BP               ; save caller's BP
          mov     BP,SP
          mov     AH,03Eh          ; load function request id
          mov     BX,[BP].zhandle  ; load handle of file to close
          int     MSDOS            ; issue close request
          jc      zcl_ret          ; if error, jump
          xor     AX,AX            ; set return code for normal return
zcl_ret:  pop     BP               ; restore caller's BP
          ret                      ; return
z%close   endp

zrw_args  struc
          dd        ?               ; far cs and ip
          dw      ?                ; caller's BP
          dw      ?                ; return address
          dw      ?                ; zhandle (use previous equate)
zbuffer   dw      ?                ; input/output buffer
zlength   dw      ?                ; address of length value
zrw_args  ends

          public  z%read
z%read    proc    far
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
z%read    endp

          public  z%write
z%write   proc    far
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
z%write   endp

strd      struc
          dd    ?                   ; far cs and ip
          dw      ?,?              ;Caller's BP, Return address
strdpg    dw      ?                ;Page, displacement of port
strdds    dw      ?
strdbuf   dw      ?                ;Buffer address
strdlen   dw      ?                ;Length address
strd      ends
          public  string%rd
string%rd proc    far
          push    BP
          mov     BP,SP
          push    DS               ;Save caller's DS, ES
          mov     AX,ES            ;  (and make AX nonzero as well)
          mov     BX,[BP].strdlen  ;Load CX with number of chars to transfer
          mov     CX,[BX]
          mov     DI,[BP].strdpg   ;Get port page
          mov     DX,DI            ;  and save for later
          %LoadPage DS,DI           ;Get para address
          mov     DI,[BP].strdds   ;DS:DI point to port
          mov     SI,word ptr[DI+car].pt_ptr   ;Point DS:SI to string
          mov     BL,[DI+car_page].pt_ptr
          xor     BH,BH
          %LoadPage DS,BX
;;;       mov     DS,ES:pagetabl+[BX]
          cmp     byte ptr[SI],STRTYPE  ;Is this a string?
          jne     nostr            ;Jump if not (error)
          mov     BX,[SI].str_len  ;Else fetch string length
          cmp     BX,0             ;;; check for small string
          jge     strn_01
          add     BX,BLK_OVHD+PTRSIZE
strn_01:  %LoadPage ES,DX           ;Restore ptr to port
          mov     DX,ES:[DI].pt_ullin  ;Fetch position within string
          sub     BX,DX            ;Set BX to # of chars left
          jns     notpast          ;If not negative, skip
          xor     BX,BX            ;Set # of chars left to 0
notpast:  cmp     BX,CX            ;Set CX to # of chars left or maximum
          jae     max              ;  called for, whichever is smaller
          mov     CX,BX
max:      add     SI,DX            ;Adjust SI into string
          add     DX,CX            ;Reset pointer into string
          mov     ES:[DI].pt_ullin,DX
          mov     ES,AX            ;Restore C's ES
          mov     DI,[BP].strdbuf  ;Point DI to buffer
          xor     AX,AX            ;Prepare to return 0 (all's well)
          jmp     short storlen    ;Store # of chars
nostr:    xor     CX,CX            ;When not a string, move no chars
storlen:  mov     BX,[BP].strdlen  ;Set LENGTH to # of chars read
          mov     ES:[BX],CX
          rep     movsb            ;Transfer bytes
          pop     DS               ;Restore caller's DS
          pop     BP
          ret
string%rd endp

;************************************************************************
;*                      Buffered Keyboard Input                         *
;*                                                                      *
;* Calling Sequence:  ch = getch();                                     *
;*                      where ch - the character read from the keyboard *
;************************************************************************
          public  get%ch
get%ch    proc    far

;         IFDEF   extmem           ; Kludge to fix hanging keyboard
;         mov     AL,0AEh          ; Ensure keyboard enabled
;         out     64h,AL           ; Output to 8042 controller
;         ENDIF

          mov     AH,07h           ; function code = Direct Console Input
          int     MSDOS            ; do it
          xor     AH,AH            ; clear the high order byte
          ret                      ; return to caller
get%ch    endp

z%ega    proc   far

         mov    AX,banka
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
         call   enable                  ; enable all banks
         xor    AX,AX                   ; clear AX
         cld
rep      stosb

         add    sav_di,80               ; next line
         inc    BX                      ; increment counter
         cmp    BX,char_hgt             ; done with this row?
         jne    zc_03

         xor    BX,BX                   ; clear counter
         dec    [BP].zc_nrows           ; decrement row count
         jg     zc_03                   ; if more rows, loop (jump)
         ret
z%ega    endp

progx     ends


PGROUP    group   prog
prog      segment byte public 'PROG'
          assume  CS:PGROUP

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
          public  zcreate
zcreate   proc    near
          call    z%create
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
          call    z%open
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
          call    z%close
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

          public  zread
zread     proc    near
          call    z%read
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
          call    z%write
          ret                      ; return
zwrite    endp

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
         push   ES
         push   DI
         push   AX
         push   BX
         push   CX
         push   DX
;     Put cursor at beginning of next row
zc_loop:  mov     DL,byte ptr [BP].zc_row ; load current row number
          mov     DH,byte ptr [BP].zc_col ; load starting column number
          xor     BH,BH            ; page number (0 if in graphics mode)
          mov     AH,02H           ; load "put cursor" code
          call    crt_dsr          ; position the cursor
;     Write line of blanks at current cursor position
          mov     AX,0920h         ; load write char/attr code + blank (= 20h)
          xor     BH,BH            ; (for IBM-PC BH=display page #)
          mov     BL,byte ptr [BP].zc_attrib ; load attribute flag
         cmp    vid_mode,14        ; IBM EGA modes?
         jl     zc_01
         cmp    BL,87h             ; attribute is rv white?
         jne    zc_22
         mov    AX,09dbh           ; use the block character not the blank
         and    BL,7fh             ; strip off the xor bit
zc_01:    mov     CX,[BP].zc_len   ; load number of times to write the blank
          call    crt_dsr          ; perform the write
;     Increment row number, decrement row count, test, loop
          inc     [BP].zc_row      ; increment row number
          dec     [BP].zc_nrows    ; decrement row count
          jg      zc_loop          ; if more rows, loop (jump)
;     Return to caller
zc_end:  pop    DX
         pop    CX
         pop    BX
         pop    AX
         pop    DI
         pop    ES
          pop     BP               ; restore caller's BP
          ret                      ; return

; clear out the line by writing directly to the graphics planes
zc_22:   mov    AX,[BP].zc_row          ; set AX to the row
         mov    c_row,AX
         mov    AX,[BP].zc_col          ; add in the starting column
         mov    c_col,AX
         mov    AX,[BP].zc_len          ; number of columns to blank
         mov    c_len,AX
         call   z%ega                   ; restore counter

         jmp    zc_end                  ; return

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
          call    z%border
          ret
zborder   endp

;************************************************************************
;*                      Link to Save Screen Support                     *
;************************************************************************
       public  save_scr
save_scr  proc    near
          call    save%scr
          ret
save_scr  endp

;************************************************************************
;*                      Link to Restore Screen Support                  *
;************************************************************************
       public  rest_scr
rest_scr  proc    near
          call    rest%scr
          ret
rest_scr  endp

;************************************************************************
;*                              Cursor Off                              *
;************************************************************************
          public  zcuroff
zcuroff   proc    near

         call   ega_curs

          mov     AH,03
          xor     BH,BH            ; IBM page number/must be 0 for graphics mode
          call    crt_dsr          ; get the cursor position/mode

	  cmp	  zapcurs,0
	  jne	  zcoff_01
          mov     curs_sav,CX      ; save it for restoration
zcoff_01:
          and     CH,CURSMASK      ; mask off bits to select cursor type
          or      CH,NOCURSOR      ; disables cursor (turns it off)
          mov     AH,01h           ; load "set cursor type" code
          call    crt_dsr          ; turn the cursor off
          ret                      ; return to caller
zcuroff   endp


;************************************************************************
;*                              Cursor On                               *
;************************************************************************
          public  zcuron
zcuron    proc    near

	  cmp	  zapcurs,0	   ; if cursor disabled
	  jne	  zcon_ret	   ;  then return

          mov     CX,curs_sav      ; attributes for cursor on
          mov     AH,01h           ; load "set cursor type" code
          call    crt_dsr          ; turn the cursor on
zcon_ret:
          ret                      ; return to caller
zcuron    endp

;************************************************************************
;*                           Put Cursor                                 *
;************************************************************************
          public  zputcur
zputcur   proc    near

          push    BP               ; save caller's BP
          mov     BP,SP
;     put cursor in desired location
          mov     DH,byte ptr [BP].zc_col ; load column number
          mov     ega_col,DH
          mov     DL,byte ptr [BP].zc_row ; load row number
          mov     ega_row,DL
          xor     BH,BH            ; IBMism: page number (0 if in graphics mode)
          mov     AH,02H           ; load "put cursor" code
          call    crt_dsr          ; position the cursor (DSR swaps DH/DL)

         call   ega_curs            ; display cursor for ega mode

;     Return to caller
          pop     BP               ; restore caller's BP
          ret                      ; return
zputcur   endp

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
          push    AX
          push    BX
          push    CX
          push    DX
;     scroll window's text up one line
          mov     CL,byte ptr [BP].zs_nline ; load number of lines
          dec     CL               ; decrease number of lines by one
          jz      blank1           ; Jump if scrolling 1-line and just blank it
          mov     CH,byte ptr [BP].zs_ncols    ; load number of columns
          mov     DL,byte ptr [BP].zs_line     ; load upper left line number
          mov     DH,byte ptr [BP].zs_col      ; load upper left column number
          mov     AX,0601h         ; load "scroll text" code with no blanking
          cmp     DGROUP:PC_MAKE,TIPC
          je      ti_scrl
;;;;;;;;; cmp     vid_mode,14
;;;;;;;;; jge     txt_mod          ; treat ega modes as text
          cmp     vid_mode,4       ; Are we in graphics mode?
          jl      txt_mod          ;   If we are then fix blank fill attributes
          cmp     vid_mode,7       ;   so that the bar characters don't show up
          je      txt_mod
          xor     BH,BH            ; zero attribute for fill blanks
          jmp     short rite_atr
txt_mod:  mov     BH,byte ptr [BP].zs_attr ; Blanked lines' attribute txt mode
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
blank1:  mov    DH,byte ptr [BP].zs_col ; load starting column number
         mov    DL,byte ptr [BP].zs_line ; load upper line number
         add    DL,byte ptr [BP].zs_nline ; add the number of lines and
         dec    DL                  ; subtract offf one
         mov    AH,02h              ; load "put cursor" code
         xor    BH,BH               ; IBMism
         call   crt_dsr             ; position cursor for write
         mov    AX,0920h            ; load "write char/attr" code, write a blank
         mov    BL,byte ptr [BP].zs_attr ; load attribute bit setting

         cmp    vid_mode,14         ; ega mode?
         jl     z_scr01
         mov    BH,BL
         and    BH,80h
         cmp    BH,80h              ; reverse video?
         jne    z_scr01
         mov    AX,09dbh            ; change for block character
         and    BL,7fh              ; strip off xor bit
z_scr01: xor    BH,BH               ; IBMism
         mov    CX,[BP].zs_ncols    ; load line length
         call   crt_dsr             ; write a line of blanks
; return to caller
z_quit:  pop    DX                  ; restore caller's BP
         pop    CX
         pop    BX
         pop    AX
         pop    BP
         ret
zscroll  endp

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
         push   BP                  ; save caller's BP
         mov    BP,SP
         push   DX
         push   CX
         push   BX
         push   AX
;     position cursor for write
         mov    DL,byte ptr [BP].zp_line ; load line number
         mov    DH,byte ptr [BP].zp_col  ; load column number
         xor    BH,BH               ; IBMism
         mov    AH,02h              ; load "put cursor" code
         call   crt_dsr             ; positio the cursor

         mov    BL,byte ptr [BP].zp_attr ; load its attributes
         cmp    vid_mode,14         ; only attribute for EGA modes is a
         jl     zchar_1             ; simulated reverse video

         mov    BH,BL               ; save the attribute
         and    BH,80h              ; reverse video?
         jz     zchar_1             ; zero indicates bit 8 not set

zchar_2: and    BL,7fh              ; strip off high bit
         mov    CX,1                ; character count
         xor    BH,BH               ; video page number
         mov    AL,0dbh             ; block character
         mov    AH,09h
         call   crt_dsr
         or     BL,80h              ; set xor bit
; write the characters with attributes
zchar_1: mov    AL,byte ptr [BP].zp_char ; load the character
         xor    BH,BH               ; IBMism
         mov    CX,1                ; repeat count  = 1
         mov    AH,09h              ; load write char/attribute code
         call   crt_dsr
;     return to caller
         pop    AX
         pop    BX
         pop    CX
         pop    DX
         pop    BP
         ret
zputc    endp

;************************************************************************
;*                      Buffered Keyboard Input                         *
;*                                                                      *
;* Calling Sequence:  ch = getch();                                     *
;*                      where ch - the character read from the keyboard *
;************************************************************************
          public  getch
getch     proc    near
          call    get%ch
          ret                      ; return to caller
getch     endp

;************************************************************************
;* Read characters from a string                                        *
;*                                                                      *
;* Calling Sequence:  stringrd(page, disp, buffer, &length)             *
;*   where page,disp:  location of string-fed port                      *
;*         buffer and length  are as in ZREAD (see above)               *
;*                                                                      *
;* Note: The passing parameter `page' is page #                         *
;************************************************************************

          public  stringrd
stringrd  proc    near
          call    string%rd
          ret
stringrd  endp
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
;
	  public  char_rdy
char_rdy  proc    near		   ;our equivalent of Lattice C's kbhit fn
	  call	  zch_rdy
	  ret
char_rdy  endp

prog      ends

          end
