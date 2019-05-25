;							=====> GET_PATH.ASM
;***************************************
;*     TIPC Scheme  Runtime Support    *
;*  Get PATH= String From Environment  *
;*				       *
;*    (C) Copyright 1985 by Texas      *
;*     Instruments Incorporated.       *
;*	  All rights reserved.	       *
;*				       *
;* Date Written:  8 July 1985	       *
;* Last Modification:  6 November 1985 *
;***************************************
	  include scheme.equ

DGROUP	  group   data
XGROUP	  group   PROGX
PGROUP	  group   prog

MSDOS	  equ	  021h

data	  segment word public 'DATA'
	  assume  DS:DGROUP
	  extrn   _psp:dword
path_	  db	  "PATH="
path_1	  equ	  $
ret_sav1  dw	  0		   ; return address save area
ret_sav2  dw	  0		   ; return address save area
data	  ends

prog	  segment byte public 'PROG'
	  assume  CS:PGROUP
;************************************************************************
;*		    Far Linkage to "getmem" Routine                     *
;************************************************************************
%getmem   proc	  far
	  pop	  ret_sav1	   ; save far return address
	  pop	  ret_sav2
	  push	  DS		   ; update ES to point to the current
	  pop	  ES		   ;  data segment
	  extrn   getmem:near
	  call	  getmem	   ; allocate memory
	  push	  ret_sav2	   ; push the far return address back
	  push	  ret_sav1	   ;  on the TIPC's stack
	  ret			   ; return
%getmem   endp
prog	  ends

PROGX	  segment byte public 'PROGX'
	  assume  CS:XGROUP
;************************************************************************
;*		    Get PATH= String From Environment			*
;************************************************************************
get_args  struc
get_base  dw	  ?		   ; paragraph address of environment
get_strt  dw	  ?		   ; starting offset of string
get_len   dw	  ?		   ; length of PATH= string + 1
str_end   dw	  ?		   ; ending offset of directory pathname
indx_sav  dw	  ?		   ; PATH= index save area
buffer	  db	  128 dup (?)	   ; local character buffer
get_BP	  dw	  ?		   ; caller's BP register
	  dw	  ?		   ; callle's DS register
	  dw	  ?		   ; caller's ES register
	  dd	  ?		   ; return address (far call)
	  dw	  ?		   ; return address (near call)
filespec  dw	  ?		   ; file specification (ASCIZ string pointer)
get_args  ends

%getpath  proc	  far
	  push	  ES		   ; save the caller's ES register
	  push	  DS		   ; save the caller's DS register
	  push	  BP		   ; save the caller's BP register
	  sub	  SP,offset get_BP ; allocate local storage
	  mov	  BP,SP 	   ; establish local addressability

;     Test to see if file is in the default directory
	  mov	  DX,[BP].filespec ; load pointer to the filespec
	  xor	  CX,CX 	   ; zero the search attributes
	  mov	  AH,04Eh	   ; load the function code (Find File)
	  int	  MSDOS 	   ; service call
	  jc	  not_curr	   ; if not in default directory, jump
	  mov	  AH,19h	   ;	else get current pathname
	  int	  MSDOS 	   ; Get current disk drive
	  inc	  AL		   ; adjust for further calls
	  mov	  AH,AL 	   ; make an upper case letter out of it
	  add	  AH,40h	   ; 40h => '@', 41h => 'A', etc
	  mov	  byte ptr [BP].buffer,AH ; Put drive letter into pathname
	  mov	  byte ptr [BP].buffer+1,':'
	  mov	  byte ptr [BP].buffer+2,'\'
	  lea	  SI,[BP].buffer+3 ; offset just below E:\ or similar
	  mov	  DL,AL 	   ; drive letter
	  mov	  AH,47h	   ; Get current directory path
	  int	  MSDOS
	  mov	  DI,SI 	   ; SI shouldn't have changed
	  mov	  CX,64 	   ; max length of pathname
	  xor	  AL,AL 	   ; search for a NUL char
    repne scasb 		   ; Find end of string  DS:[DI]
	  mov	  [BP].str_end,DI  ; copy offset of end of pathname
	  jmp	  foundit	   ; return directory name to caller in a string

;     Load a pointer to the current environment (offset 02C in PSP)
not_curr: mov	  ES,word ptr _psp+2
	  mov	  ES,ES:02Ch
	  mov	  [BP].get_base,ES ; save paragraph address of environment
	  xor	  DI,DI 	   ; initialize environment offset to zero

;     Test for end of environment
get_plop: cmp	  byte ptr ES:[DI],0 ; last entry in environment?
	  je	  error 	   ; if so, PATH= not found
	  mov	  SI,offset path_  ; load address of comparison string
	  mov	  CX,path_1-path_  ;  and length of same
repe	  cmps	  path_,ES:[DI]    ; does this entry begin "PATH="?
	  je	  found 	   ; if so, found it! (jump)
	  xor	  AX,AX 	   ; clear AX for search
	  mov	  CX,-1 	   ; set CX for maximum length
repne	  scas	  byte ptr ES:[DI] ; find \0 which terminates string
	  jmp	  get_plop	   ; loop

;     PATH= found!-- begin searching its directories
found:	  mov	  SI,DI 	   ; copy address of PATH= string
next_one: lea	  DI,[BP].buffer   ; load address of output buffer
	  mov	  DX,DS 	   ; save current DS value in DX
	  mov	  ES,DX 	   ; ES <- current data segment
	  mov	  DS,[BP].get_base ; DS <- environment object base
	  lodsb 		   ; load 1st char from path directory string
	  cmp	  AL,0		   ; end of PATH list?
	  je	  error 	   ; if so we didn't find filespec in path
here:	  cmp	  AL,';'           ; semicolon?
	  je	  end_semi	   ; if semicolon, jump
	  cmp	  AL,0		   ; zero? (superfluous test 1st time in loop)
	  je	  end_0 	   ; if zero, jump
	  stosb 		   ; store character into output buffer
	  lodsb 		   ; load next char from PATH string
	  jmp	  short here	   ; loop 'til end of string

;     Error-- PATH= not found, getmem failed, or filespec not found
error:	  xor	  AX,AX 	   ; prepare to return a null pointer
	  jmp	  short get_ret    ; return

;     Directory path copied-- append filespec to directory pathname
end_0:	  dec	  SI		   ; back up pointer for end of string condition
end_semi: mov	  [BP].str_end,DI  ; save ending offset of directory pathname
	  mov	  DS,DX 	   ; reset DS to point to data segment
	  mov	  AL,'\'
	  cmp	  AL,[DI]-1
	  je	  b_slash
	  stosb
b_slash:  mov	  [BP].indx_sav,SI ; save pointer to next character in PATH=
	  mov	  SI,[BP].filespec ; load address of input filespec
fs_loop:  lodsb 		   ; load next character in filespec
	  stosb 		   ;  and move it to the output buffer
	  cmp	  AL,0		   ; end of string?
	  jne	  fs_loop	   ; if not end of string, loop (jump)

;     Search directory for file
	  lea	  DX,[BP].buffer   ; load address of the complete file
	  mov	  AH,04Eh	   ; load function code
	  int	  MSDOS 	   ; search for the file
	  jnc	  foundit	   ; if file found, jump

;     File not found-- search next directory
	  mov	  SI,[BP].indx_sav ; load offset of next character in PATH=
	  jmp	  next_one	   ; search next directory

;     File found in this directory-- return directory name in string
foundit:  mov	  DI,[BP].str_end  ; load ending offset of directory path
	  xor	  AX,AX 	   ; put a zero end-of-string terminator
	  stosb 		   ;  at end of directory path
	  lea	  BX,[BP].buffer   ; load beginning offset of buffer
	  sub	  DI,BX 	   ; compute string length + 1
	  push	  DI		   ;  and push as argument to getmem
	  call	  %getmem	   ; allocate a string
	  cmp	  AX,0		   ; getmem successful?
	  je	  error 	   ; if getmem failed, error (jump)
	  pop	  CX		   ; reload string length
	  mov	  DX,DS 	   ; ES <- current data segment
	  mov	  ES,DX
	  mov	  DI,AX 	   ; DI <- address of newly allocated string
	  lea	  SI,[BP].buffer   ; SI <- address of local buffer
rep	  movsb 		   ; copy string from local buffer

;     Return to calling program
get_ret:  mov	  SP,BP 	   ; drop arguments off TIPC's stack
	  add	  SP,offset get_BP ; deallocate local storage
	  pop	  BP		   ; restore caller's BP
	  pop	  DS		   ; restore caller's DS
	  pop	  ES		   ; restore caller's ES
	  ret			   ; return

%getpath  endp
PROGX	  ends

prog	  segment byte public 'PROG'
	  assume  CS:PGROUP
;************************************************************************
;*			 Linkage to %getpath				*
;************************************************************************
	  public  get_path
get_path  proc	  near
	  call	  %getpath
	  ret
get_path  endp

prog	  ends
	  end
