;
;***************************************
;*     TIPC Scheme  Runtime Support    *
;*				       *
;*    (C) Copyright 1985 by Texas      *
;*     Instruments Incorporated.       *
;*	  All rights reserved.	       *
;*				       *
;* Date Written:  5 June 1985	       *
;* Last Modification:  15 May 1986     *
;***************************************
	  page	  60,132

MSDOS	  equ	  021h		   ; MS-DOS service call interrupt
FREEMEM   equ	  049h		   ; Free memory function identifier
MODIFMEM  equ	  04Ah		   ; Modify allocated memory function id
BIDTASK   equ	  04Bh		   ; Load and execute program function id
PRSTRING  equ	  09h
CREATE_FL equ	  3Ch		   ; Create file function
OPEN_FL   equ	  3Dh		   ; Open file function
CLOSE_FL  equ	  3Eh		   ; Close file function
READ_FL   equ	  3Fh		   ; Read file function
WRITE_FL  equ	  40h		   ; Write file function
DELETE_FL equ	  41h		   ; Delete file function
GET_DRIVE equ	  19h		   ; Current disk function
SET_DRIVE equ	  0Eh		   ; Select disk function
GET_DIR   equ	  47h		   ; Return text of current directory function
SET_DIR   equ	  3Bh		   ; Change the current directory function
TI_CRTINT  equ	  49h*4 	   ; CRT dsr interrupt - TI
IBM_CRTINT equ	  10h*4 	   ; CRT dsr interrupt - IBM

DGROUP	  group   data
data	  segment word public 'DATA'
	  assume  DS:DGROUP
	  extrn   _paras:word	   ; total number of paragraphs available
	  extrn   _psp:dword	   ; program segment prefix paragraph address
;	  extrn   first_pa:word    ; seg addr of 1st page in Scheme heap
	  extrn   first_dos:word   ; seg addr of memory allocated to Scheme heap
	  extrn   PC_MAKE:word	   ; type of machine
drive	  db	  ?		   ; place holder for current drive number
dir_path  db	  ?		   ; Drive Letter (as part of the path name)
	  db	  ":\"             ; GET_DIR function doesn't prepend "root"
path	  db	  80 dup(?)	   ; dir path buffer, excluding drive
sav_file  db	  "pc__s.sav",00   ; ASCIZ save file pathname
len_sav_name equ  $-sav_file
cmd_      db	  "COMSPEC="
cmd_1     equ	  $
ENVPTR	  dw	  0		   ; DOS EXEC parameter block
CMDOFF	  dw	  0		   ; 		"
CMDSEG	  dw	  0		   ; 		"
FCB1OFF   dw	  5Ch		   ; 		"
FCB1SEG   dw	  0		   ; 		"
FCB2OFF   dw	  6Ch		   ; 		"
FCB2SEG   dw	  0		   ; 		"
data	  ends

XGROUP	  group   PROGX
PROGX	  segment byte public 'PROGX'
	  assume  CS:XGROUP
	  public  install
	  public  uninstall

;************************************************************************
;*			     Bid another Task				*
;************************************************************************
bid_args  struc
	  dw	  ?		   ; caller's BP
	  dw	  ?		   ; caller's ES
	  dd	  ?		   ; far return address  to caller of bid_task
	  dw	  ?		   ; near return address to caller of bid
bid_file  dw	  ?		   ; program's file name
bid_parm  dw	  ?		   ; parameters
free_req  dw	  ?		   ; requested # of free paragraphs
bid_args  ends

sav_SP	  dw	  0		   ; save area for current stack pointer
sav_SS	  dw	  0		   ; save area for stack segment register

;
;  Paragraph Addresses
;
;	   _paras  --> +--------------------+  <----
;		       |   /|\		    |	 :  Freed for bidded task,
;		       |    |		    |	 :  Saved to disk save file
;		       |    | -- free_req   |	 :   start:   _paras - free_req
;		       |    |		    |	 :   length:  free_req
;		       |   \|/		    |	 :    (free_req >= _paras - first_dos)
;		       |~~~~~~~~~~~~~~~~~~~~|  <----
;		       |		    |	 :
;		       |      (heap)	    |	 :  Allocated to stay resident
;		       |		    |	 :   # paras:  _paras -
;	 first_pa  --> +--------------------+	 :		 _psp -
;		       |   (unused area)    |	 :	      free_req
;	 first_dos --> +--------------------+	 :
;		       |		    |	 :
;		       |      (PCS)	    |	 :
;		       |		    |	 :
;		       |		    |	 :
;	     _psp  --> +--------------------+  <----
;		       |		    |
;

close	  proc	  near		   ; Closes the file whose handle is in BX
	  mov	  AH,CLOSE_FL
	  int	  MSDOS
	  ret
close	  endp

delete	  proc	  near		   ; Deletes the save file
	  assume  DS:DGROUP
	  mov	  DX,offset dir_path
	  mov	  AH,DELETE_FL
	  int	  MSDOS
	  ret
delete	  endp


bid_task  proc	  far
	  push	  ES
	  push	  BP
	  mov	  BP,SP 	   ; establish local addressability

;     Check if requested # of free paragraphs within bounds
	  cmp	  [BP].free_req,0  ; default to free max?
	  je	  free_all	   ; yes, branch
	  mov	  AX,_paras	   ; compute requested base of free area
	  sub	  AX,[BP].free_req ;
	  jb	  free_all	   ; request greater than all memory? branch
	  cmp	  AX,first_dos	   ; below base of free-able area?
	  jnb	  req_ok	   ; no, ok -- jump
free_all: mov	  AX,_paras	   ; compute max # of free-able paras
	  sub	  AX,first_dos	   ;
	  mov	  [BP].free_req,AX ; update # of paras to free
req_ok:

;     Save Scheme's user memory
;     First create save file
;	Save current drive and directory path
	  mov	  AH,GET_DRIVE	   ; get current drive number (0=A,1=B,...,4=E)
	  int	  MSDOS
	  mov	  drive,AL	   ; and save it
	  inc	  AL		   ; "correct" current drive number
	  mov	  DL,AL 	   ; put current drive into DL
	  add	  AL,40h	   ;	(make it a capital letter)
	  mov	  dir_path,AL	   ; put the drive letter into dir_path
	  mov	  SI,offset path   ; point DS:SI to path buffer
	  mov	  AH,GET_DIR	   ; get current path
	  int	  MSDOS
;	Append save file's name to end of directory path
find_end: mov	  BX,offset path   ; point to beginning of path name
	  mov	  CX,64 	   ; maximum length of path name
findloop: cmp	  byte ptr [BX],0
	  je	  name_end
	  inc	  BX
	  loop	  findloop

name_end: cmp	  byte ptr [BX-1],'\' ; was last character a backslash?
	  je	  add_save	   ; if so then don't append another one (jump!)
	  mov	  byte ptr [BX],'\' ; else append a backslash then the filename
	  inc	  BX
add_save: push	  SI		   ; Now add concat'nate filename  (PC__S.SAV)
	  mov	  AX,DS
	  mov	  ES,AX
	  mov	  DI,BX 	   ; load destination address
	  mov	  SI,offset sav_file
	  mov	  CX,len_sav_name
      rep movsb 		   ; appending the save file name + NULL
	  pop	  SI

; Now open the save file...
	  mov	  DX,offset dir_path  ; point DS:DX to ASCIZ save file path
	  mov	  CX,20h	   ; file attribute
	  mov	  AH,CREATE_FL
	  int	  MSDOS 	   ; do it
	  jnb	  crt_ok	   ; branch if create ok
	  jmp	  exit		   ; quit now if unable to create save file
crt_ok:

;     Now dump memory to the file (file handle in AX)
	  mov	  BX,AX 	   ; put file handle into BX
	  mov	  DI,[BP].free_req ; DI = number of paras to write
	  mov	  AX,_paras	   ; compute base of area to free
	  sub	  AX,[BP].free_req ;
	  push	  DS		   ; save DS
	  mov	  DS,AX 	   ; init DS:DX to base of area to save
	  xor	  DX,DX 	   ;
wrt_para: cmp	  DI,0FFFh	   ; can write all paras in one shot?
	  jbe	  wrt_last	   ; yes, jump
	  sub	  DI,0FFFh	   ; dec paras-to-write count
	  mov	  CX,0FFF0h	   ; write FFF0 bytes
	  mov	  AH,WRITE_FL
	  int	  MSDOS 	   ; do it
	  jb	  wrt_err	   ; branch if error
	  cmp	  AX,CX 	   ; wrote all bytes?
	  je	  wrt_ok1	   ; yes, branch
	  mov	  AX,20 	   ; indicate write count error
	  jmp	  short wrt_err
wrt_ok1:  mov	  AX,DS 	   ; inc buffer pointer
	  add	  AX,0FFFh
	  mov	  DS,AX
	  jmp	  wrt_para	   ; write out next FFF paras
wrt_last: mov	  CL,4		   ; shift para count to byte count
	  shl	  DI,CL
	  mov	  CX,DI 	   ; put byte count into CX
	  mov	  AH,WRITE_FL
	  int	  MSDOS 	   ; do it
	  jb	  wrt_err	   ; branch if error
	  cmp	  AX,CX 	   ; wrote all bytes?
	  je	  wrt_ok2
	  mov	  AX,20 	   ; indicate write count error
wrt_err:  pop	  DS		   ; restore DS
	  push	  AX		   ; save error code
	  call	  close 	   ; close and delete save file
	  call	  delete
	  pop	  AX		   ; restore error code
	  jmp	  exit		   ;  and quit
wrt_ok2:  pop	  DS		   ; restore DS
	  call	  close 	   ; close up file for safe keeping
	  jnb	  wrt_ok3	   ; branch if all ok
	  jmp	  exit		   ; quit if can't close file
wrt_ok3:

;     Free up Scheme's user memory
	  mov	  ES,first_dos	   ; point ES to base of allocated area
	  mov	  BX,_paras	   ; compute # paras to remain allocated
	  sub	  BX,first_dos	   ;
	  sub	  BX,[BP].free_req ;
	  mov	  AH,MODIFMEM	   ; load modify memory function id
	  int	  MSDOS 	   ; change PCS memory allocation
	  jnc	  mem_ok
memerr:   push	  AX		   ; save error code
	  call	  delete	   ; delete save file
	  pop	  AX		   ; restore error code
	  jmp	  exit		   ;  and quit
mem_ok:

;     Bid up specified program
;     Set up parameter block
	  mov	  AX,[BP].bid_parm ; Set up dword pointer to command line
	  mov	  CMDOFF,AX
	  mov	  CMDSEG,DS

	  mov	  AX,word ptr _psp+2 ; Point to FCBs in program segment prefix
	  mov	  FCB1SEG,AX
	  mov	  FCB2SEG,AX

	  mov	  ES,AX
	  mov	  AX,ES:[02Ch]	   ; copy current environment ptr to
	  mov	  ENVPTR,AX	   ;  parameter area

;     Set ES:BX to address of parameter block
	  mov	  AX,DS
	  mov	  ES,AX
	  mov	  BX,offset ENVPTR

;     Set DS:DX to address of ASCIZ pathname (of file to be loaded)
	  push	  DS		   ; save DS segment register
	  mov	  DX,[BP].bid_file
	  mov	  DI,DX
	  cmp	  byte ptr [di],0    ; check if pt'ed to string is empty
	  jne	  bid_it

;     No filename-- bid up a new command interpreter;
;     have to search environment for COMSPEC= string
	  mov	  ES,ENVPTR	   ; ES:DI points to 1st string in environment
	  xor	  DI,DI

;     Test for end of environment
get_plop: cmp	  byte ptr ES:[DI],0 ; last entry in environment?
	  je	  cmd_err 	   ; if so, COMSPEC= not found
	  mov	  SI,offset cmd_   ; load address of comparison string
	  mov	  CX,cmd_1-cmd_    ;  and length of same
     repe cmps	  cmd_,ES:[DI]     ; does this entry begin "COMSPEC="?
	  je	  found 	   ; if so, found it! (jump)
	  xor	  AX,AX 	   ; clear AX for search
	  mov	  CX,-1 	   ; set CX for maximum length
    repne scas	  byte ptr ES:[DI] ; find \0 which terminates string
	  jmp	  get_plop	   ; loop

;     No command interpreter found
cmd_err:  mov	  AX,10		   ; treat as bad-environment error
	  jmp	  short bid_err

;     Found COMSPEC=
found:	  mov	  DX,DI		   ; DS:DX is ptr to command interpreter
	  push	  DS		   ; (swap DS and ES)
	  push	  ES
	  pop	  DS
	  pop	  ES

;     issue load task function call
bid_it:   push	  BP		   ; Old IBM-PCs & XTs destroy BP on func 4B.
	  mov	  CS:sav_SP,SP	   ; save current stack pointer
	  mov	  CS:sav_SS,SS	   ; save stack segment register
	  xor	  AL,AL 	   ; load and execute condition
	  mov	  AH,BIDTASK	   ; load "load and execute" ftn id
	  int	  MSDOS 	   ; perform service call
	  cli			   ; disable all interrupts
	  mov	  SS,CS:sav_SS	   ; restore stack base pointer
	  mov	  SP,CS:sav_SP	   ; restore stack pointer
	  sti			   ; enable interrupts
	  pop	  BP		   ; restore BP (Thanks IBM) :-(
	  pop	  DS		   ; restore DS segment register
	  jb	  bid_err	   ; branch if error in bidding task
	  xor	  AX,AX 	   ; indicate no error
bid_err:  push	  AX		   ; save error code

;     ReAllocate Scheme's user memory
	  mov	  ES,first_dos	   ; point ES to base of allocated area
	  mov	  BX,_paras	   ; compute # of all available paras
	  sub	  BX,first_dos	   ;
	  mov	  AH,MODIFMEM	   ; load modify memory function id
	  int	  MSDOS 	   ; change PCS memory allocation
	  jnc	  read_mem
fatal:	  pop	  AX		   ; throw away bid error code
	  call	  delete	   ; delete save file
	  mov	  AX,0FFFFh	   ; indicate cannot continue, -1
	  jmp	  exit

;     Restore Scheme's user memory
;     First open   save file
read_mem: mov	  DX,offset dir_path  ; point DS:DX to ASCIZ save file path
	  mov	  AL,00 	   ; access code for reading
	  mov	  AH,OPEN_FL
	  int	  MSDOS 	   ; do it
	  jb	  fatal 	   ; abort if cannot open save file

;     Now read memory from the file (file handle in AX)
	  mov	  BX,AX 	   ; put file handle into BX
	  mov	  DI,[BP].free_req ; DI = number of paras to read
	  mov	  AX,_paras	   ; compute base of area to restore from disk
	  sub	  AX,[BP].free_req ;
	  push	  DS		   ; save DS
	  mov	  DS,AX 	   ; init DS:DX to base of area to restore
	  xor	  DX,DX
rd_para:  cmp	  DI,0FFFh	   ; can read all paras in one shot?
	  jbe	  rd_last	   ; yes, jump
	  sub	  DI,0FFFh	   ; dec paras-to-read count
	  mov	  CX,0FFF0h	   ; read FFF0 bytes
	  mov	  AH,READ_FL
	  int	  MSDOS 	   ; do it
	  jb	  read_err	   ; branch if read error
	  cmp	  AX,CX 	   ; read all bytes?
	  jne	  read_err	   ; no, branch
read_ok1: mov	  AX,DS 	   ; inc buffer pointer
	  add	  AX,0FFFh
	  mov	  DS,AX
	  jmp	  rd_para	   ; read in next FFF paras
rd_last:  mov	  CL,4		   ; shift para count to byte count
	  shl	  DI,CL
	  mov	  CX,DI 	   ; put byte count into CX
	  mov	  AH,READ_FL
	  int	  MSDOS 	   ; do it
	  jb	  read_err	   ; branch if error reading file
	  cmp	  AX,CX 	   ; read all bytes?
	  je	  read_ok2	   ; yes, branch
read_err: pop	  DS		   ; restore DS
	  call	  close 	   ; close save file
	  jmp	  fatal 	   ;  and abort
read_ok2: pop	  DS		   ; restore DS
	  call	  close 	   ; close save file
	  call	  delete	   ;  and delete it
	  pop	  AX		   ; restore bid error code

exit:	  pop	  BP		   ; restore caller's BP
	  pop	  ES		   ; restore ES segment register
	  ret			   ; return to caller
bid_task  endp

;------------------------------------------------------------------------
; The following routines will inhibit text display to the screen for
;  the duration of the dos-call.
;
;  Note:  Programs such as Lotus 1-2-3 which write directly to the
;	   screen memory will still be visible.
;
;------------------------------------------------------------------------

exec_args  struc
	  dw	  ?		   ; caller's BP
	  dd	  ?		   ; far return address  to caller of install
	  dw	  ?		   ; near return address to caller of exec
file	  dw	  ?		   ; program's file name
parm	  dw	  ?		   ; parameters
fre_req   dw	  ?		   ; requested # of free paragraphs
display   dw	  ?		   ; Indicates if screen should be disturbed
exec_args  ends

CRTSAV	  dd	  ?
CRTINT	  dw	  ?
DSSAV	  dw	  ?
INSTALLED dw	  ?

install   proc	  far
    ;  This routine installs a routine at the CRT DSR interrupt
    ;
	  push	  bp
	  mov	  bp,sp
	  push	  bx
	  mov	  cs:INSTALLED,0	; Assume routine won't be installed
	  mov	  bx,[BP].display	; Indicates if commands will be sent
	  cmp	  bx,0			; Screen can be disturbed?
	  pop	  bx
	  jne	  non_null		; Install new interrupt routine
	  jmp	  xinstall		; exit
non_null:
	  mov	  cs:INSTALLED,1
	  push	  ds
	  push	  es
	  push	  ax
	  push	  bx
	  push	  dx
	  push	  si
	  push	  di
	  mov	  ax,ds
	  mov	  cs:DSSAV,ax
;
;  Install new routine at the CRT DSR interrupt
;
	  mov	  ax,0			; Save off routine adr of CRT DSR
	  mov	  ds,ax
	  mov	  si,offset xgroup:CRTSAV
	  mov	  word ptr cs:[CRTINT],IBM_CRTINT	; Assume its IBM
	  mov	  es,cs:DSSAV
	  cmp	  word ptr es:PC_MAKE,1 	; Is it a TI?
	  jne	  is_IBM
	  mov	  word ptr cs:[CRTINT],TI_CRTINT
is_IBM:
	  mov	  di,cs:CRTINT
	  mov	  ax,ds:[di]
	  mov	  cs:[si],ax
	  mov	  ax,ds:[di+2]
	  mov	  cs:[si+2],ax
	  cli				; Clear interrupts
	  mov	  ax,offset xgroup:crtdsr
	  mov	  ds:[di],ax
	  mov	  ds:[di+2],cs
	  sti				; Enable interrupts
	  pop	  di
	  pop	  si
	  pop	  dx
	  pop	  bx
	  pop	  ax
	  pop	  es
	  pop	  ds
xinstall:
	  pop	  bp
	  ret
install   endp
; **************************************************************************
;  This routine restores the original routine for the CRT DSR interrupt
;
uninstall proc	  far
	  cmp	  cs:INSTALLED,1	; Was an int routine installed?
	  je	  non_null2
	  jmp	  xuninstall
non_null2:
	  push	  ds
	  push	  ax
	  push	  si
	  push	  di
	  mov	  ax,0
	  mov	  ds,ax
	  mov	  si,offset xgroup:CRTSAV ; Restore CRT DSR routine
	  mov	  ax,cs:[si]
	  mov	  di,cs:CRTINT
	  mov	  ds:[di],ax
	  mov	  ax,cs:[si+2]
	  mov	  ds:[di+2],ax
	  pop	  di
	  pop	  si
	  pop	  ax
	  pop	  ds
xuninstall:
	  ret
uninstall endp
;
;  This is the do-nothing routine installed at the CRT DSR interrupt
;
crtproc   proc	  far
crtdsr:
	  sti
	  mov	 ax,0
	  iret
crtproc   endp


PROGX	  ends

PGROUP	  group   prog
prog	  segment byte public 'PROG'
	  assume  CS:PGROUP
	  extrn   unfixint:near
	  extrn   zcuron:near
	  extrn   zcuroff:near
	  extrn   fix_intr:near
	  public  bid
bid	  proc	  near
	  call	  unfixint	   ; reset shift-break vector
	  call	  zcuron	   ; turn the cursor back on
	  call	  install
	  call	  bid_task
	  push	  AX		   ; save error code
	  call	  uninstall
	  call	  zcuroff	   ; turn the cursor back off
	  call	  fix_intr	   ; set shift-break vector
	  pop	  AX		   ; restore error code
	  ret
bid	  endp
prog	  ends
	  end
