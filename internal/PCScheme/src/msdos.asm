;***************************************
;*	    MS-DOS Utilities	       *
;*				       *
;*  (C) Copyright 1984,1985 by Texas   *
;*     Instruments Incorporated.       *
;*	  All rights reserved.	       *
;*				       *
;* Date Written:  21 June 1984	       *
;* Last Modification:	9 June	1986   *
;***************************************

MSDOS	  equ	  021h		   ; MS-DOS interrupt number
GETTIME   equ	  02Ch		   ; "get_time" function request id
READ	  equ	  0
WRITE	  equ	  1
SELDISK   equ	  0EH		   ; select disk
CURDISK   equ	  019H		   ; get the current disk
SETADDR   equ	  01AH		   ; set disk transfer address
CHNGDIR   equ	  03BH		   ; change the current directory
CRFILE	  equ	  03CH		   ; create a file
OPENFILE  equ	  03DH		   ; open a file
CLFILE	  equ	  03EH		   ; close a file
RFILE	  equ	  03FH		   ; read from a file
WFILE	  equ	  040H		   ; write to a file
DELFILE   equ	  041H		   ; delete a file function request id
MOVPTR	  equ	  042H		   ; move file pointer
CURRDIR   equ	  047H		   ; return text of current directory
FINDFILE  equ	  04EH		   ; find match file
FINDNEXT  equ	  04FH		   ; step through a directory, matching files
CHGNAME   equ	  056H		   ; move a directory entry

DGROUP	  group   data
data	  segment word public 'DATA'
	  assume  DS:DGROUP
curdrv	  db	  0
	  db	  3AH		   ; ':'
	  db	  5CH		   ; '\'
curdir	  db	  64 dup (0)
dta	  db	  43 dup (0)
filespec  db	  13 dup (0)
data	  ends

XGROUP	  group   progx
progx	  segment byte public 'PROGX'
	  assume  CS:XGROUP,DS:DGROUP
;;;
;;; Delete a file
;;;
del_arg   struc
	  dw	  ?		   ; caller's BP
	  dd	  ?		   ; return address (long)
	  dw	  ?		   ; original return address (short)
filename  dw	  ?		   ; file name
del_arg   ends

dos%del   proc	  far
	  push	  BP
	  mov	  BP,SP
	  mov	  DX,[BP].filename ; DX points to ASCIZ pathname
	  mov	  AH,DELFILE	   ; delete a file
	  int	  MSDOS
	  jc	  del_ret
	  xor	  AX,AX 	   ; carry not set, return zero
del_ret:  pop	  BP
	  ret
dos%del   endp
;;;
;;; Copy a file
;;;
copy_arg  struc
handle1   dw	  ?		   ; source file handle
handle2   dw	  ?		   ; destination file handle
copy_buf  db	  128 dup (0)	   ; temporary buffer for copy
copy_BP   dw	  ?		   ; caller's BP
	  dd	  ?		   ; return address
	  dw	  ?		   ; original return address
file1	  dw	  ?		   ; source file name
file2	  dw	  ?		   ; destination file name
copy_arg  ends

dos%copy  proc	  far
	  push	  BP
	  sub	  SP,offset copy_BP ; allocate local storage
	  mov	  BP,SP
	  mov	  DX,[BP].file1
	  mov	  AH,OPENFILE	   ; open a file (source)
	  mov	  AL,READ	   ; access mode: read
	  int	  MSDOS
	  jc	  copy_ret	   ; carry set, return
	  mov	  [BP].handle1,AX
	  mov	  DX,[BP].file2
	  mov	  CX,0		   ; file attribute
	  mov	  AH,CRFILE	   ; create a file (destination)
	  int	  MSDOS
	  jc	  copy_ret
	  mov	  [BP].handle2,AX
; copy bytes from source file to destination file
copy_01:  lea	  DX,[BP].copy_buf
	  mov	  CX,128
	  mov	  BX,[BP].handle1
	  mov	  AH,RFILE	   ; read from file
	  int	  MSDOS
	  cmp	  AX,0		   ; end of file?
	  je	  copy_10	   ; yes, jump
	  mov	  CX,AX 	   ; number of bytes to move
	  lea	  DX,[BP].copy_buf
	  mov	  BX,[BP].handle2
	  mov	  AH,WFILE	   ; write to a file
	  int	  MSDOS
	  jmp	  copy_01
; close source file and destination file
copy_10:  mov	  BX,[BP].handle1
	  mov	  AH,CLFILE	   ; close a file
	  int	  MSDOS
	  mov	  BX,[BP].handle2
	  mov	  AH,CLFILE	   ; close a file
	  int	  MSDOS
	  xor	  AX,AX
copy_ret: add	  SP,offset copy_BP ; release local storage
	  pop	  BP
	  ret
dos%copy  endp
;;;
;;; Rename files under current directory
;;;
ren%mov  proc	near
	 cmp	byte ptr [DI],2ah   ; an '*'
	 je	renmv1
	 cmp	byte ptr [DI],3fh   ; a '?'
	 je	renmv1
	 mov	AL,byte ptr [DI]    ; otherwise move in the new file char
renmv1:  mov	byte ptr [BX],AL
	 ret
ren%mov  endp

ren_arg   struc
ren_BP	  dw	  ?		   ; caller's BP
	  dd	  ?		   ; return address
	  dw	  ?		   ; original return address
oldfile   dw	  ?		   ; old file name
newfile   dw	  ?		   ; new file name
ren_arg   ends

dos%ren   proc	  far
	  push	  BP
	  mov	  BP,SP

	  mov	  DX,offset DGROUP:dta
	  mov	  AH,SETADDR	   ; set disk transfer address
	  int	  MSDOS

	  mov	  DX,[BP].oldfile
	  mov	  CX,0		   ; search attribute
	  mov	  AH,FINDFILE	   ; find match file
	  int	  MSDOS
	  jc	  ren_ret

ren_01:   mov	  SI,offset DGROUP:dta
	  add	  SI,29 	   ; points to filespec
	  mov	  DI,[BP].newfile
	  mov	  BX,offset DGROUP:filespec

ren_02:   inc	  SI
	  cmp	  byte ptr ES:[SI],00h ; end of the string
	  je	  ren_03
	  cmp	  byte ptr ES:[SI],2eh ; an '.'?
	  je	  ren_03
	  mov	  AL,byte ptr ES:[SI]
	  call	  ren%mov
	  inc	  DI
	  inc	  BX
	  cmp	  byte ptr [DI-1],2ah
	  jne	  ren_02
	  cmp	  byte ptr [SI+1],2eh	;next char a '.'?
	  je	  ren_02
	  dec	  DI
	  jmp	  ren_02
;
ren_03:
	  cmp	  byte ptr [DI],00h ; end of the string
	  je	  ren_04
	  cmp	  byte ptr [DI],3fh ; a '?'
	  je	  ren_04
	  cmp	  byte ptr [DI],2ah ; an '*'
	  je	  ren_04
	  cmp	  byte ptr [DI-1],2eh ; previous character a '.'?
	  je	  ren_02
	  mov	  AL,byte ptr ES:[SI-1]
	  call	  ren%mov
	  inc	  DI
	  inc	  BX
	  jmp	  ren_03
;
; rename the file
;
ren_04:   mov	  byte ptr [BX],0
	  mov	  DI,offset DGROUP:filespec
	  mov	  DX,offset DGROUP:dta
	  add	  DX,30
	  mov	  AH,CHGNAME	   ; move a directory entry
	  int	  MSDOS

	  mov	  AH,FINDNEXT	   ; find next match file
	  int	  MSDOS
	  jnc	  ren_01	    ; carry not set, do next file
ren_100:  xor	  AX,AX
ren_ret:  pop	  BP
	  ret
dos%ren   endp

;;;
;;; Get the file size
;;;
size_arg  struc
	  dw	  ?		   ; caller's BP
	  dd	  ?		   ; caller's return address
	  dw	  ?
file	  dw	  ?
size_arg  ends

dos%size  proc	  far
	  push	  BP
	  mov	  BP,SP
	  mov	  DX,offset DGROUP:dta
	  mov	  AH,SETADDR	   ; set disk transfer address
	  int	  MSDOS
	  mov	  DX,[BP].file
	  mov	  CX,0		   ; search attribute
	  mov	  AH,FINDFILE	   ; find match file
	  int	  MSDOS
	  jnc	  size_01
	  xor	  BX,BX 	   ; return 0 for invalid access
	  xor	  AX,AX
	  jmp	  size_ret
size_01:  mov	  DI,offset DGROUP:dta
	  mov	  AX,word ptr [DI+28] ; high word of file size
	  mov	  BX,word ptr [DI+26] ; low word of file size
size_ret: pop	  BP
	  ret
dos%size  endp
;;;
;;; Change the current directory
;;;
cd_arg	  struc
cd_BP	  dw	  ?		   ; caller's BP
	  dd	  ?		   ; caller's return address
	  dw	  ?
dir	  dw	  ?
cd_arg	  ends
dos%cd	  proc	  far
	  push	  BP
	  mov	  BP,SP
	  mov	  AH,CURDISK	   ; current disk
	  int	  MSDOS
	  inc	  AL
	  mov	  DL,AL 	   ; drive number
	  add	  AL,40H	   ; drive character
	  mov	  curdrv,AL
	  mov	  SI,offset DGROUP:curdir
	  mov	  AH,CURRDIR	   ; return current directory
	  int	  MSDOS
	  mov	  DX,[BP].dir
	  mov	  AH,CHNGDIR	   ; change the current directory
	  int	  MSDOS
	  mov	  AX,offset DGROUP:curdrv
cd_ret:   pop	  BP
	  ret
dos%cd	  endp
;;;
;;; Change the current drive
;;;
drv_arg   struc
	  dw	  ?		   ; caller's BP
	  dd	  ?		   ; caller's return address
	  dw	  ?
drive	  db	  ?
drv_arg   ends
dos%drv   proc	  far
	  push	  BP
	  mov	  BP,SP
	  mov	  DL,[BP].drive
	  mov	  AH,CURDISK	   ; current disk
	  int	  MSDOS
	  mov	  [BP].drive,AL
	  sub	  DL,41H	   ; get the drive number
	  cmp	  DL,0
	  jl	  drv_ret
	  cmp	  DL,10 	   ; maximum nuber of drive?
	  jg	  drv_ret
	  mov	  AH,SELDISK	   ; select disk
	  int	  MSDOS
	  cmp	  DL,AL 	   ; AL = number of drives
	  jl	  drv_01
	  mov	  DL,[BP].drive    ; get the current disk
	  mov	  AH,SELDISK	   ; select disk
	  int	  MSDOS
	  jmp	  drv_ret
drv_01:   xor	  AX,AX
	  jmp	  drv_ret1
drv_ret:  mov	  AX,-1 	   ; error
drv_ret1: pop	  BP
	  ret
dos%drv   endp
;;;
;;; Move the file pointer right before EOF character and overwrite it
;;; to fix the bug in open-extend-file
;;;
mov_arg   struc
m_buffer  dw	  0
mov_BP	  dw	  ?		   ; caller's BP
	  dd	  ?		   ; caller's return address
	  dw	  ?
fhandle   dw	  ?		   ; file handle
mov_arg   ends

mov%fptr  proc	  far
	  push	  BP
	  sub	  SP,offset mov_BP ; allocate for local variable
	  mov	  BP,SP
	  mov	  AL,2		   ; move the pointer to end of file
	  mov	  DX,-128	   ;  and with offset (one record size)
	  mov	  CX,-1
	  mov	  AH,MOVPTR
	  mov	  BX,[BP].fhandle  ; file handle
	  int	  MSDOS
	  jc	  mov_ret
	  cmp	  DX,0		   ; small file?
	  jge	  mov_001
	  mov	  AL,0
	  xor	  CX,CX
	  xor	  DX,DX
	  mov	  AH,MOVPTR
	  int	  MSDOS
	  jc	  mov_ret
mov_001:  lea	  DX,[BP].m_buffer ; address of buffer
mov_01:   mov	  CX,1		   ; read one character at a time
	  mov	  AH,RFILE	   ; read it
	  int	  MSDOS
	  jc	  mov_ret
	  mov	  CL,byte ptr [BP].m_buffer
	  cmp	  CL,1AH	   ; reach eof character?
	  je	  mov_05	   ; yes, go overwrite it
	  cmp	  AX,0		   ; at eof, but no eof char?
	  je	  mov_ret	   ;  Yes, return
	  jmp short mov_01	   ;  No,  loop
; file pointer right after the EOF character
mov_05:	  mov	  AL,1		   ; move the pointer to the current
	  mov	  DX,-1 	   ;  location plus offset
	  mov	  CX,-1
	  mov	  AH,MOVPTR
	  int	  MSDOS
	  jc	  mov_ret
; file pointer points to EOF character
	  mov	  CX,1		   ; write one byte
	  mov	  BX,[BP].fhandle  ; file handle
	  mov	  [BP].m_buffer,0
	  lea	  DX,[BP].m_buffer ; address of buffer
	  mov	  AH,WFILE	   ; write it
	  int	  MSDOS
	  jc	  mov_ret
	  mov	  AL,1		   ; move the pointer to the current
	  mov	  DX,-1 	   ;  location plus offset
	  mov	  CX,DX
	  mov	  AH,MOVPTR
	  mov	  BX,[BP].fhandle  ; file handle
	  int	  MSDOS
	  jc	  mov_ret
	  xor	  AX,AX
mov_ret:  add	  SP,offset mov_BP ; release local storage
	  pop	  BP
	  ret
mov%fptr  endp
progx	  ends

PGROUP	  group   prog
prog	  segment byte public 'PROG'
	  assume  CS:PGROUP

get_args  struc
	  dw	  ?		   ; caller's BP
	  dw	  ?		   ; return address
get_ary   dw	  ?		   ; pointer to result array
get_args  ends

time_fmt  struc 		   ; format of data returned by get_time()
tim_hour  dw	  ?		   ; hour
tim_min   dw	  ?		   ; minute
tim_sec   dw	  ?		   ; seconds
tim_hnds  dw	  ?		   ; hundredths
time_fmt  ends

	  public  get_time
get_time  proc	  near
	  push	  BP		   ; save caller's BP
	  mov	  BP,SP 	   ; establish operand addressability

	  mov	  AH,GETTIME	   ; load "get_time" service call id
	  int	  MSDOS 	   ; request service from MS-DOS
	  mov	  BX,[BP].get_ary  ; load pointer to result array
	  xor	  AX,AX 	   ; clear AX
	  mov	  AL,CH 	   ; copy hours
	  mov	  [BX].tim_hour,AX ;  and store into result array
	  mov	  AL,CL 	   ; copy minutes
	  mov	  [BX].tim_min,AX  ;  and store into result array
	  mov	  AL,DH 	   ; copy seconds
	  mov	  [BX].tim_sec,AX  ;  and store into result array
	  mov	  AL,DL 	   ; copy hundredths
	  mov	  [BX].tim_hnds,AX ;  and store into result array

	  pop	  BP
	  ret
get_time  endp
;*************************************************************************
;		  Link to Delete a file support
;*************************************************************************
	  public  delete
delete	  proc	  near
	  call	  dos%del
	  ret
delete	  endp
;*************************************************************************
;		  Link to Copy a file support
;*************************************************************************
	  public  copy_fil
copy_fil  proc	  near
	  call	  dos%copy
	  ret
copy_fil  endp
;*************************************************************************
;		  Link to Rename a file support
;*************************************************************************
	  public  rename
rename	  proc	  near
	  call	  dos%ren
	  ret
rename	  endp
;*************************************************************************
;		  Link to file size support
;*************************************************************************
	  public  filesize
filesize  proc	  near
	  call	  dos%size
	  ret
filesize  endp
;*************************************************************************
;		  Link to Change directory support
;*************************************************************************
	  public  chgdir
chgdir	  proc	  near
	  call	  dos%cd
	  ret
chgdir	  endp
;*************************************************************************
;		  Link to Change drive support
;*************************************************************************
	  public  chgdrv
chgdrv	  proc	  near
	  call	  dos%drv
	  ret
chgdrv	  endp
;
	  public  mov_fptr
mov_fptr  proc	  near
	  call	  mov%fptr
	  ret
mov_fptr  endp


prog	  ends
	  end
