;							=====> ALINK.ASM
;***************************************
;*   TIPC Scheme '84 Runtime Support   *
;*	     Misc Utilities	       *
;*				       *
;*    (C) Copyright 1985 by Texas      *
;*     Instruments Incorporated.       *
;*	  All rights reserved.	       *
;*				       *
;* Date Written:  23 June 1985	       *
;* Last Modification:  29 May 1986     *
;***************************************
	  page	  60,132

MSDOS	  equ	  021h

DGROUP	  group   data
data	  segment word public 'DATA'
	  assume  DS:DGROUP
	  extrn   _psp:dword
ret_area  db	  20 dup (0)	   ; filename return area
dir_fnd   db	  ' <DIR>'

data	  ends

PGROUP	  group   prog
prog	  segment byte public 'PROG'
	  assume  CS:PGROUP

;************************************************************************
;*				Find Match File 			*
;*									*
;* Purpose:  Given a pathname specification, which may contain wildcard *
;*		characters, this routine returns the first filename in	*
;*		the current directory which matches the specification.	*
;************************************************************************

find_arg  struc
	  dw	  ?		   ; caller's ES
	  dw	  ?		   ; caller's BP
	  dw	  ?		   ; return address
filespec  dw	  ?		   ; pointer to file spec (ASCIZ string)
find_arg  ends

	  public  dir1
dir1	  proc	  near
	  push	  BP		   ; save the caller's BP
	  push	  ES
	  mov	  BP,SP 	   ; establish local addressability

	  mov	  AX,word ptr _psp+2
	  mov	  ES,AX 	   ; set ES to point to the psp

	  push	  DS		   ; save DS
	  push	  ES
	  pop	  DS		   ; set DS to point to the psp

; set Disk Transfer Address (DTA) to 80h in the psp
	  mov	  AH,1ah	   ; load "set DTA" function code
	  mov	  DX,80h	   ; load DTA offset
	  int	  MSDOS
	  pop	  DS		   ; restore DS

;     issue service call to find the first file match
	  mov	  DX,[BP].filespec ; load address of filespec in DS:DX
	  mov	  CX,10h	   ; set attributes to search for,
				   ; directories and all files except for
				   ; hidden and system files.
	  mov	  AH,04Eh	   ; load "find match file" function code
	  int	  MSDOS 	   ; perform the service call
;     if no file found, return a null string ("")
	  jnc	  dir1_ok	   ; if filename returned, jump
dir1_nf:  xor	  AX,AX 	   ; return a null pointer
	  jmp	  short dir1_ret
;     copy filename found from DTA to local storage
dir1_ok:  mov	  SI,09eh	      ; load offset of DTA filename area
	  mov	  DI,offset ret_area  ; load address of local filename storage
	  cmp	  byte ptr ES:[SI],2eh ; don't bother with . and ..
	  je	  dir2_nxt

dir1_x:   mov	  AL,ES:[SI]	   ; load next character of filename
	  cmp	  AL,00H	   ; character a null string?
	  je	  dir1_y
	  mov	  [DI],AL	   ;  and store it into return area
	  inc	  DI		   ; increment return area pointer
	  inc	  SI
	  jmp	  dir1_x	   ; if more characters, loop (jump)
dir1_y:   and	  byte ptr ES:[95h],10h  ; check for directory bit
	  cmp	  byte ptr ES:[95h],10h
	  jne	  dir_done
	  mov	  SI,offset dir_fnd ; load offset of directory message
	  mov	  CX,6
dir1_z:   mov	  AL,[SI]
	  mov	  [DI],AL
	  inc	  DI
	  inc	  SI
	  loop	  dir1_z
dir_done: mov	  byte ptr [DI],00h  ; add in null byte to terminate string
	  mov	  AX,offset ret_area ; load offset of filename copy
;     return to caller
dir1_ret: pop	  ES
	  pop	  BP		   ; restore caller's BP
	  ret			   ; return to caller
dir1	  endp

	  public  dir2
dir2	  proc	  near
	  push	  BP		   ; save the caller's BP
	  push	  ES

	  mov	  AX,word ptr _psp+2
	  mov	  ES,AX 	   ; set ES to point to the psp

;     issue service call to find the next file match
dir2_nxt: mov	  AH,04Fh	   ; load "step, matching files" function code
	  int	  MSDOS 	   ; perform the service call
;     if no file found, return a null string ("")
	  jnc	  dir1_ok	   ; if filename returned, jump
	  jmp	  short dir1_nf    ; else, return filename found
dir2	  endp

prog	  ends
	  end
