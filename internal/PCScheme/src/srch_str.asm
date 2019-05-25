;							=====> SRCH_STR.ASM
;***************************************
;*     TIPC Scheme  Runtime Support    *
;*      String Search Capabilities     *
;*				       *
;*    (C) Copyright 1985 by Texas      *
;*     Instruments Incorporated.       *
;*	  All rights reserved.	       *
;*				       *
;* Date Written:  21 July 1985	       *
;* Last Modification:  17 October 1985 *
;***************************************
	  include scheme.equ
	  include pcmake.equ
	  include sinterp.arg
IFDEF PROMEM
	  include rpc.equ
	  include xli_pro.mac
	  include realio.equ
	  .286c
ENDIF

DGROUP	  group   data
XGROUP	  group   PROGX
PGROUP	  group   prog

MSDOS	  equ	  021h
TI_CRT	  equ	  049h
IBM_CRT   equ	  010h

;     Definitions for control characters
CTL_A	  equ	  1
CTL_I	  equ	  9
CTL_Z	  equ	  26

data	  segment word public 'DATA'
	  assume  DS:DGROUP
IFDEF PROMEM
;from pro2real.asm
	  extrn   REAL_BUF_SELECTOR:word,REAL_BUF_TOP:word,RPC_HANDLE:byte
ENDIF
ret_sav1  dw	  0		   ; return address save area
ret_sav2  dw	  0		   ; return address save area
m_srch_f  db	  "SUBSTRING-FIND-NEXT-CHAR-IN-SET",0
m_srch_b  db	  "SUBSTRING-FIND-PREVIOUS-CHAR-IN-SET",0
m_st_ln	  db	  "STRING-LENGTH",0
m_mk_str  db	  "MAKE-STRING",0
m_stapnd  db	  "%STRING-APPEND",0
m_reifs	  db	  "%REIFY-STACK",0
m_reifsb  db	  "%REIFY-STACK!",0
m_st_dsp  db	  "%SUBSTRING-DISPLAY",0
m_opnd	  dw	  INVALID_OPERAND_ERROR ; numeric error code
m_one	  dw	  1		   ; a constant "one" (1)
data	  ends

prog	  segment byte public 'PROG'
	  assume  CS:PGROUP
	  extrn	  %allocbl:far	   ; "alloc_block" linkage routine
	  extrn	  next:far	   ; Interpreter's "next instruction" point
	  extrn	  next_PC:far	   ; Interpreter's "next instruction" point
	  extrn	  sch_err:far	   ; Linkage to Scheme debugger
;************************************************************************
;*		      Far Linkage to "set_src_err"			*
;************************************************************************
	  public  %set_src
%set_src  proc	  far
	  pop	  ret_sav1
	  pop	  ret_sav2
	  push	  DS		   ; make ES point to the current data segment
	  pop	  ES
	  extrn	  set_src_:near
	  call	  set_src_
	  push	  ret_sav2
	  push	  ret_sav1
	  ret
%set_src  endp

;************************************************************************
;*		     Far Linkage to "set_numeric_error"			*
;************************************************************************
	  public  %set_num
%set_num  proc	  far
	  pop	  ret_sav1
	  pop	  ret_sav2
	  push	  DS		   ; make ES point to the current data segment
	  pop	  ES
	  extrn	  set_nume:near
	  call	  set_nume
	  push	  ret_sav2
	  push	  ret_sav1
	  ret
%set_num  endp

;************************************************************************
;*		      Far Linkage to "dissamble"			*
;************************************************************************
	  public  %disasse
%disasse  proc	  far
	  pop	  ret_sav1
	  pop	  ret_sav2
	  push	  DS		   ; make ES point to the current data segment
	  pop	  ES
	  extrn	  disassem:near
	  call	  disassem
	  push	  ret_sav2
	  push	  ret_sav1
	  ret
%disasse  endp

;************************************************************************
;*		      Far Linkage to "get_port" 			*
;************************************************************************
	  public  %getport
%getport  proc	  far
	  pop	  ret_sav1
	  pop	  ret_sav2
	  push	  DS		   ; make ES point to the current data segment
	  pop	  ES
	  extrn	  get_port:near
	  call	  get_port
	  push	  ret_sav2
	  push	  ret_sav1
	  ret
%getport  endp

prog	  ends

PROGX	  segment byte public 'PROGX'
	  assume  CS:XGROUP

	  extrn	  CRT_DSR:far

;************************************************************************
;*		      Substring-Find-Next-Char-in-Set			*
;************************************************************************
srch_arg  struc
strt_off  dw	  ?		   ; starting offset (16 bit positive integer)
end_off	  dw	  ?		   ; ending offset (16 bit positive integer)
lngth	  dw	  ?		   ; number of characters in source string
result	  dw	  ?		   ; index of character matched

;     Note:  the following two entries are order dependent
str_beg	  dw	  ?		   ; beginning offset of string
str_DS	  dw	  ?		   ; segment register value for string

srch_BP	  dw	  ?		   ; caller's BP
	  dw	  ?		   ; caller's ES
	  dd	  ?		   ; return address (far)
	  dw	  ?		   ; return address (near)
str_reg	  dw	  ?		   ; register containing string
strt_reg  dw	  ?		   ; register containing substr starting offset
end_reg	  dw	  ?		   ; register containing substr ending offset
cs_reg	  dw	  ?		   ; register containing charset string
srch_arg  ends

srch_str  proc	  far

%srchprv  label	  far
	  mov	  CX,1		   ; set search direction = backward
	  jmp	  short srch_go    ; go to common processing code

;     Long Branch to Source Error Support
srch_er1: jmp	  srch_err

%srchnxt  label	  far
	  xor	  CX,CX		   ; set search direction = forward

srch_go:  push	  ES		   ; save caller's ES register
	  push	  BP		   ; save caller's BP register
	  sub	  SP,offset srch_BP ; allocate local storage
	  mov	  BP,SP		   ; establish addressability of local data

;     Validate Source String Argument
	  mov	  BX,[BP].str_reg  ; load address of string register
	  mov	  SI,[BX].C_page   ;  and load string's page number
	  cmp	  byte ptr ptype+[SI],STRTYPE*2 ; is source string a string?
	  jne	  srch_er1	   ; if not a string, error (jump)
	  %LoadPage ES,SI
;;;	  mov	  ES,pagetabl+[SI] ; load string's paragraph address
	  mov	  SI,[BX].C_disp   ; load displacement of string
	  mov	  AX,ES:[SI].str_len ; load length of string
	  cmp	  AX,0             ;;; check length of string
	  jge	  srch_01
	  add	  AX,PTRSIZE
	  jmp	  srch_02
srch_01:  sub	  AX,BLK_OVHD	   ;  and compute number of characters in it
srch_02:  mov	  [BP].lngth,AX	   ; save string length for further testing
	  add	  SI,BLK_OVHD	   ; advance string start to first character
	  mov	  [BP].str_beg,SI  ; save starting character offset
	  mov	  [BP].str_DS,ES   ;  and segment register pointer

;     Validate Starting Offset Argument
	  mov	  BX,[BP].strt_reg ; load address of starting offset regsiter
	  call	  get_num	   ; obtain the integer value
	  jc	  srch_er1	   ; if carry set, error
	  mov	  [BP].strt_off,AX ;  and save it for future use

;     Validate Ending Offset Argument
	  mov	  BX,[BP].end_reg  ; load address of ending offset register
	  call	  get_num	   ; obtain the integer value
	  jc	  srch_er1	   ; if carry set, error
	  cmp	  [BP].strt_off,AX ; is starting offset greater than ending?
	  ja	  srch_er1	   ; if so, invalid substring range (jump)
	  cmp	  AX,[BP].lngth	   ; test ending offset against string length
	  ja	  srch_er1	   ; if ending offset too big, error (jump)
	  mov	  [BP].end_off,AX  ; save ending offset for future use

;     Validate Charset String Argument
	  mov	  BX,[BP].cs_reg   ; load number of register holding charset
	  mov	  DI,[BX].C_page   ; load page number of charset pointer
	  cmp	  byte ptr ptype+[DI],STRTYPE*2 ; this is a sting, isn't it?
	  jne	  char_p	   ; if not a string, error (jump)
	  %LoadPage ES,DI
;;;	  mov	  ES,pagetabl+[DI] ; load paragraph address of string
	  mov	  DI,[BX].C_disp   ; load displacement of string in page
	  mov	  DX,ES:[DI].str_len ; load length of string object
	  cmp	  DX,0             ;;; check length of string
	  jge	  srch_03
	  add	  DX,PTRSIZE
	  jmp	  srch_04
srch_03:  sub	  DX,BLK_OVHD	   ; compute number of characters in charset
srch_04:  add	  DI,BLK_OVHD	   ; advance string pointer past block header
	  jmp	  short go
char_p:	  cmp	  DI,SPECCHAR*2	   ; is charset argument a single character?
	  je      char_p0	   ; Yes, continue
	  jmp	  srch_er1	   ; No,  error (jump)

;     Single character search-- optimize it
char_p0:  mov	  AL,byte ptr [BX].C_disp
	  les	  DI,dword ptr [BP].str_beg
	  mov	  DX,CX		   ; save direction indicator in DX
	  mov	  CX,[BP].end_off  ; compute length of search string
	  sub	  CX,[BP].strt_off
	  je	  not_fnd1	   ; if search length is zero, return 'nil
	  cmp	  DX,0
	  jne	  b_ward	   ; if backward, jump
;     search for single character in forward direction
	  add	  DI,[BP].strt_off ; compute address of start of substring
repne	  scasb			   ; search for single character
	  jne	  not_fnd1	   ; character found?  If so, jump
	  dec	  DI		   ; fix up ending index
	  jmp	  short over
;     search for single character in backward direction
b_ward:   add	  DI,[BP].end_off  ; compute address of end of substring
	  dec	  DI
	  std			   ; set search direction to be backwards
repne	  scasb			   ; search for single character
	  cld			   ; reset "direction" flag to go forwards
	  jne	  not_fnd1	   ; if search length is zero, return 'nil
	  inc	  DI		   ; fix up ending index
over:	  mov	  SI,DI		   ; copy character address to SI
	  sub	  SI,[BP].str_beg  ;  and compute found character's address
	  jmp	  short found	   ; return index to found character

;     Determine whether string search is forward or backward
go:	  push	  DS		   ; save the data segment address
	  cmp	  CX,0		   ; in which direction are we to search?
	  je	  forward	   ; if CX=0, forward; else backward

;   Register Usage in Innermost Loop:
;	DS:SI - pointer to next character in source string
;	ES:DI - pointer to charset string
;	AL    - search character
;	BX    - ending offset (source string)
;	CX    - length of charset string
;	DX    - length of charset string (used to refresh CX)

;     Search Source String in a Backwards Direction
	  mov	  BX,[BP].str_beg  ; compute ending offset for string
	  add	  BX,[BP].strt_off
	  lds	  SI,dword ptr [BP].str_beg  ; load addr of string's beginning
	  add	  SI,SS:[BP].end_off ;  and compute end of substring address
	  jmp	  short startb	   ; jump to initial entry point in loop

loopb:    sub	  DI,DX		   ; reset starting offset of charset string
startb:   cmp	  SI,BX		   ; at beginning of substring?
	  jbe	  not_fnd	   ; if at end, jump
	  mov	  CX,DX		   ; reload charset string length
	  dec	  SI		   ; decrement source string index
	  mov	  AL,[SI]	   ;  and load next character to test
repne	  scasb			   ; search charset for current character
	  jne	  loopb
	  pop	  DS		   ; restore DS to point to data segment
	  sub	  SI,[BP].str_beg  ; compute index of current character
	  jmp	  short found	   ; current character found in charset

;     no characters found which appear in the charset
not_fnd:  pop	  DS		   ; restore DS to current data segment
not_fnd1: xor	  AX,AX		   ; store #!false in the
	  mov	  BX,[BP].str_reg  ;  destination register
	  mov	  byte ptr [BX].C_page,AL
	  mov	  [BX].C_disp,AX
	  jmp	  short ret	   ; return to caller

;     Search Source String in a Forward Direction
forward:  mov	  BX,[BP].str_beg  ; compute ending offset for string
	  add	  BX,[BP].end_off
	  lds	  SI,dword ptr [BP].str_beg  ; load addr of string's beginning
	  add	  SI,SS:[BP].strt_off ;  and compute beginning of substring
	  jmp	  short start	   ; jump to initial entry point in loop

loop:     sub	  DI,DX		   ; reset starting offset of charset string
start:	  cmp	  SI,BX		   ; at end of source string?
	  jae	  not_fnd	   ; if at end, jump
	  mov	  CX,DX		   ; reload charset string length
	  lodsb			   ; load next character to test
repne	  scasb			   ; search charset for current character
	  jne	  loop

;     current character found in charset-- return offset of current character
	  pop	  DS		   ; restore DS to current data segment
	  sub	  SI,[BP].str_beg  ; adjust offset of character found
	  dec	  SI
found:	  mov	  BX,[BP].str_reg  ; load address of destination register
	  call	  ret_num	   ; convert offset to Scheme integer

;     return to caller
ret:	  xor	  AX,AX		   ; set completion code for normal return
ret1:	  add	  SP,offset srch_BP ; release local storage
	  pop	  BP		   ; restore the caller's BP register
	  pop	  ES		   ; restore the caller's ES register
	  ret			   ; return

;     error-- invalid operand to string search primitive
srch_err: pushm	  <[BP].cs_reg,[BP].end_reg,[BP].strt_reg,[BP].str_reg>
	  cmp	  CX,0		   ; search forward or backward?
	  jne	  backward	   ; if backward search, jump
	  mov	  AX,offset m_srch_f
	  jmp	  short common
backward: mov	  AX,offset m_srch_b 
common:   mov	  BX,4		   ; load VM argument count
	  pushm	  <BX,AX>	   ; push args=4, name of instruction
	  call	  %set_src	   ; call set_src_err(...);
	  mov	  AX,-1		   ; load "invalid operand" flag
	  mov	  SP,BP		   ; drop arguments off the TIPC's stack
	  jmp	  ret1		   ; return to interpreter
	  
srch_str  endp

dumy_arg  struc
	  dw	  ?		   ; caller's BP
	  dd	  ?		   ; return address (far linkage)
	  dw	  ?		   ; return address (near linkage)
arg1	  dw	  ?		   ; register address for argument 1
arg2	  dw	  ?		   ; register address for argument 2
arg3	  dw	  ?		   ; register address for argument 3
arg4	  dw	  ?		   ; register address for argument 4
dumy_arg  ends

;************************************************************************
;*								   AL	*
;* (string-length string)			string-length      d=s1	*
;*									*
;* Purpose:  Scheme Interpreter support for the "string-lengt" function.*
;************************************************************************
%st_len	  proc	  far
	  push	  BP		   ; save the caller's BP register
	  mov	  BP,SP		   ; establish addressability for arguments
;     validate the string argument
	  mov	  BX,[BP].arg1	   ; load address of argument register
	  mov	  SI,[BX].C_page   ; load the string's page number
	  cmp	  byte ptr ptype+[SI],STRTYPE*2 ; it is a string, isn't it?
	  jne	  st_l_err	   ; if not a string, error (jump)
;     compute string length
	  %LoadPage ES,SI
;;;	  mov	  ES,pagetabl+[SI] ; load paragraph address of string's page
	  mov	  SI,[BX].C_disp   ; load string's displacement
	  mov	  SI,ES:[SI].str_len ; load length field from string object
	  cmp	  SI,0             ;;; check length of string
	  jge	  str_010
	  add	  SI,PTRSIZE
	  jmp	  str_020
str_010:  sub	  SI,BLK_OVHD	   ;  and compute number of characters in it
;     return string length as an integer
str_020:  call	  ret_num	   ; create Scheme representation for integer
;     return
	  xor	  AX,AX		   ; set error code for normal return
str_ret:  pop	  BP		   ; restore the caller's BP register
	  ret			   ; return to caller
;     ***error-- operand was not a string***
st_l_err: mov	  AX,offset m_st_ln ; load text address for "STRING-LENGTH"
	  mov	  CX,1		   ; indicate one operand
	  pushm	  <BX,CX,AX>	   ; push arguments for call
	  call	  %set_src	   ; call:  set_src_err("STRING-LENGTH",1,arg1)
	  mov	  SP,BP		   ; drop arguments off stack
	  mov	  AX,-1		   ; indicate error return
	  jmp	  str_ret	   ; return
%st_len	  endp

;************************************************************************
;* MAKE-STRING				(MAKE-STRING  LENGTH  INIT-VAL)	*
;*									*
;* Purpose:  Scheme Interpreter support for the "MAKE-STRING" function.	*
;*									*
;* Note:  The maximum length of a PCS string is 2^16 - 3 (65,532)	*
;*	characters.							*
;************************************************************************
%makestr  proc	  far
	  push	  BP		   ; save the caller's BP register
	  mov	  BP,SP		   ; establish addressability for arguments
;     validate the length operand
	  mov	  BX,[BP].arg1	   ; load address of reg containing length
	  call	  get_num	   ; get the value of the integer
	  jc	  mk_s_err	   ; error?  if so, jump
;     allocate the string object
	  mov	  CX,STRTYPE	   ; load the type code for a string
	  pushm	  <AX,CX,BX>	   ; push arguments for the call
	  call	  %allocbl	   ; call:  alloc_block(arg1, STRTYPE, length)
	  mov	  SP,BP		   ; drop the arguments off the stack
;     validate the initialization value
	  mov	  BX,[BP].arg2	   ; load address of register with init value
	  mov	  SI,[BX].C_page   ; load initialization value's page number
	  cmp	  SI,0		   ; default initialization value (nil)?
	  jne	  mk_s_ch	   ; if not nil, check for character (jump)
	  mov	  AL," "	   ; use blank (" ") as default fill value
	  jmp	  short mk_s_in	   ; initialize the string
mk_s_ch:  cmp	  SI,SPECCHAR*2	   ; is initialization value a character?
	  jne	  mk_s_err	   ; if not a character or nil, error (jump)
	  mov	  AL,byte ptr [BX].C_disp ; load the value of the character
;     initialize the string
mk_s_in:  mov	  BX,[BP].arg1	   ; load a pointer to the newly allocated
	  mov	  DI,[BX].C_disp
	  mov	  BX,[BX].C_page
	  %LoadPage ES,BX
;;;	  mov	  ES,pagetabl+[BX]
	  mov	  CX,[BP].arg3     ; load length of string object
	  add	  DI,BLK_OVHD	   ; advance string ptr to 1st char position
rep	  stosb			   ; propagate initval throughout string
;     return to caller
	  xor	  AX,AX		   ; set the return code for a normal return
mk_s_ret: pop	  BP		   ; restore the caller's BP register
	  ret			   ; return
;     ***error-- invalid operand to MAKE-STRING***
mk_s_err: mov	  AX,offset m_mk_str ; load addr of "MAKE-STRING" text
	  mov	  BX,2		   ; load argument count = 2
	  pushm	  <[BP].arg2,[BP].arg1,BX,AX>
	  call	  %set_src	   ; set_src_err("MAKE-STRING",2,arg1,arg2)
	  mov	  SP,BP		   ; drop arguments off TIPC stack
	  mov	  AX,-1		   ; indicate error return
	  jmp	  short mk_s_ret   ; return
%makestr  endp

;************************************************************************
;* (%str-append str1 start1 end1 {nil,char,str2} str3 start3 end3)	*
;************************************************************************
str_arg   struc
start1	  dw	  ?		   ; starting offset of first string
start3	  dw	  ?		   ; starting offset of third string
len1	  dw	  ?		   ; length of first string
len2	  dw	  ?		   ; length of second string
len3	  dw	  ?		   ; length of third string
str_BP    dw	  ?		   ; caller's BP
	  dw	  ?		   ; caller's SI
	  dw	  ?		   ; caller's ES
reg7	  dw	  ?		   ; address of 7th operand register
reg6	  dw	  ?		   ; address of 6th operand register
reg5	  dw	  ?		   ; address of 5th operand register
reg4	  dw	  ?		   ; address of 4th operand register
reg3	  dw	  ?		   ; address of 3rd operand register
reg2	  dw	  ?		   ; address of 2nd operand register
reg_1	  dw	  ?		   ; address of 1st operand register
str_arg   ends

	  public  str_apnd
%strapnd  proc	  far
str_err1: jmp	  str_err	   ; indirect jump to error code
;     Load operands of this here instruction and compute register addresses
str_apnd: mov	  CX,7		   ; load count of number of operands
str_ld:   xor	  AX,AX		   ; clear AH
	  lods	  byte ptr ES:[SI] ; load register number of this operand
	  add	  AX,offset reg0   ;  and compute the register's address
	  push	  AX		   ; save the register's address on the stack
	  loop	  str_ld	   ; continue until all operands processed
;     Save registers and establish addressability of local storage
	  push	  ES		   ; save caller's ES register
	  push	  SI		   ; save caller's SI register
	  push	  BP		   ; save caller's BP register
	  sub	  SP,offset str_BP ; allocate local storage
	  mov	  BP,SP		   ;  and establish addressability

;     Validate the First String's Starting Offset
	  mov	  BX,[BP].reg2	   ; load address of start1 register
	  call	  get_num	   ; fetch value for start1
	  jc	  str_err1	   ; if error, jump
	  add	  AX,BLK_OVHD	   ; advance starting offset past block header
	  mov	  [BP].start1,AX   ; save start1 offset
;     Validate the First String's Ending Offset
	  mov	  BX,[BP].reg3	   ; load address of end1 register
	  call	  get_num	   ; fetch value for end1
	  jc	  str_err1	   ; if error, jump
	  add	  AX,BLK_OVHD	   ; advance ending offset past block header
;     Validate the First String Operand
	  mov	  BX,[BP].reg_1	   ; load address of string1 register
	  mov	  SI,[BX].C_page   ; load string's page number
	  cmp	  byte ptr ptype+[SI],STRTYPE*2 ; it is a string, isn't it?
	  jne	  str_err1	   ; if not a string, error (jump)
	  %LoadPage ES,SI
;;;	  mov	  ES,pagetabl+[SI] ; load a pointer to the string
	  mov	  SI,[BX].C_disp
	  mov	  CX,ES:[SI].str_len ; ending offset past string end?
	  cmp	  CX,0             ;;; check length of string
	  jge	  str_01
	  add	  CX,BLK_OVHD+PTRSIZE  ;;; adjust the string length
str_01:	  cmp	  AX,CX
	  ja	  str_err1	   ; if ending offset too big, error (jump)
	  sub	  AX,[BP].start1   ; is ending offset too small?
	  jb	  str_err1	   ; if ending offset smaller than start, jump
	  mov	  [BP].len1,AX	   ; save length of substring1

;     Validate the Third String's Starting Offset
	  mov	  BX,[BP].reg6	   ; load address of start3 register
	  call	  get_num	   ; fetch value for start3
	  jc	  str_err	   ; if error, jump
	  add	  AX,BLK_OVHD	   ; advance starting offset past block header
	  mov	  [BP].start3,AX   ; save start3 offset
;     Validate the Third String's Ending Offset
	  mov	  BX,[BP].reg7	   ; load address of end3 register
	  call	  get_num	   ; fetch value for end3
	  jc	  str_err	   ; if error, jump
	  add	  AX,BLK_OVHD	   ; advance ending offset past block header
;     Validate the Third String Operand
	  mov	  BX,[BP].reg5	   ; load address of string3 register
	  mov	  SI,[BX].C_page   ; load string's page number
	  cmp	  byte ptr ptype+[SI],STRTYPE*2 ; it is a string, isn't it?
	  jne	  str_err	   ; if not a string, error (jump)
	  %LoadPage ES,SI
;;;	  mov	  ES,pagetabl+[SI] ; load a pointer to the string
	  mov	  SI,[BX].C_disp
	  mov	  CX,ES:[SI].str_len ; ending offset past string end?
	  cmp	  CX,0             ;;; check length of string
	  jge	  str_02
	  add	  CX,BLK_OVHD+PTRSIZE  ;;; adjust the string length
str_02:   cmp	  AX,CX
       	  ja	  str_err	   ; if ending offset too big, error (jump)
	  sub	  AX,[BP].start3   ; is ending offset too small?
	  jb	  str_err	   ; if ending offset smaller than start, jump
	  mov	  [BP].len3,AX	   ; save length of substring3

;     Validate the "thing" to be inserted between string1 and string3
	  mov	  BX,[BP].reg4	   ; load register with said "thing"
	  mov	  SI,[BX].C_page   ; load page number
	  cmp	  SI,NIL_PAGE*2	   ; is object nil?
	  jne	  str_10	   ; if not nil, jump
;     The "thing" is nil-- indicate nothing to insert
	  mov	  [BP].len2,0	   ; indicate zero length "thing"
	  jmp	  short str_30	   ; continue processing

;     ***We interrupt this routine for some error support code***
str_err:  mov	  AX,offset m_stapnd
	  mov	  CX,7
	  pushm	  <[BP].reg7,[BP].reg6,[BP].reg5,[BP].reg4,[BP].reg3>
	  pushm	  <[BP].reg2,[BP].reg_1,CX,AX>
	  call	  %set_src
	  mov	  SP,BP
	  add	  SP,offset str_BP
	  pop	  BP
	  pop	  SI
	  pop	  ES	  
	  jmp	  sch_err

str_10:   cmp	  SI,SPECCHAR*2	   ; is "thing" a character?
	  jne	  str_20	   ; if not a character, jump
;     The "thing" is a character
	  mov	  [BP].len2,1      ; indicate length = 1 character
	  jmp	  short str_30
str_20:   cmp	  byte ptr ptype+[SI],STRTYPE*2 ; is "thing" a string?
	  jne	  str_err	   ; if not a string, error (jump)
;     The "thing" is a string-- establish string length
	  %LoadPage ES,SI
;;;	  mov	  ES,pagetabl+[SI] ; load string page's paragraph address
	  mov	  SI,[BX].C_disp
	  mov	  AX,ES:[SI].str_len
	  cmp	  AX,0             ;;; check length of string
	  jge	  str_03
	  add	  AX,PTRSIZE
	  jmp	  str_04
str_03:   sub	  AX,BLK_OVHD	   ;  and compute number of characters in it
str_04:   mov	  [BP].len2,AX 	   ; save string length for further testing

;     All arguments OK, allocate the new string
str_30:	  mov	  AX,[BP].len1	   ; compute the length of the new string
	  add	  AX,[BP].len2
	  add	  AX,[BP].len3
	  cmp	  AX,16383	   ; is new string greater than max string size?
	  jg	  str_err 	   ; Yes ... error
	  mov	  BX,STRTYPE	   ; load tag=string
	  mov	  CX,offset tmp_reg
	  pushm	  <AX,BX,CX>	   ; push arguments to call
	  call	  %allocbl
	  mov	  SP,BP		   ; drop arguments off the stack
	  mov	  DI,tmp_page	   ; load pointer to newly allocated string
	  %LoadPage0 ES,DI
;;;	  mov	  ES,pagetabl+[DI]
	  mov	  DI,tmp_disp	   ; pointer is now in ES:[DI]
	  add	  DI,BLK_OVHD	   ; advance pointer to 1st character location
;     Move in data from all substrings
	  mov	  CX,[BP].len1	   ; load length of string1
	  mov	  BX,[BP].reg_1	   ; load addr of register containing string 1
	  mov	  SI,[BX].C_disp   ; load string 1's offset
	  add	  SI,[BP].start1   ; add in offset of starting character
	  mov	  BX,[BX].C_page   ; load page number
	  push	  DS		   ; save the data segment register
	  %LoadPage1 DS,BX
;;;	  mov	  DS,pagetabl+[BX]
;**********************************************************************
;* * * Warning:  The data segment register (DS) does not point to * * *
;* * *		 the data segment in the code which follows	  * * *
;**********************************************************************
rep	  movsb			   ; copy string1 into new string
	  pop	  DS		   ; restore data segment register

	  mov	  CX,[BP].len2	   ; load length of string2
	  cmp	  CX,0		   ; any characters to move?
	  je	  str_60	   ; if no characters, jump
	  mov	  BX,[BP].reg4	   ; load addr of register with "thing"
	  mov	  SI,SS:[BX].C_disp ; load a pointer to "thing"
	  mov	  BX,SS:[BX].C_page
	  push	  DS		   ; Save data segment register
	  cmp	  BL,SPECCHAR*2	   ; is "thing" a character?
	  jne	  str_40	   ; if not a character, then a string (jump)
	  mov	  SI,[BP].reg4	   ; load addr of register containing character
	  jmp	  short str_50
str_40:	  %LoadPage1 DS,BX
;;;    	  mov	  DS,SS:pagetabl+[BX] ; "thing" is a string-- load pointer
	  add	  SI,BLK_OVHD	   ;  to it and advance to 1st character
str_50:
rep	  movsb			   ; copy string2 into new string
	  pop	  DS		   ; restore data segment register

str_60:	  mov	  CX,[BP].len3	   ; load length of string3
	  mov	  BX,[BP].reg5	   ; load addr of register containing string 3
	  mov	  SI,SS:[BX].C_disp ; load string offset
	  add	  SI,[BP].start3   ; advance starting offset past block header
	  mov	  BX,SS:[BX].C_page ; load the string's page number and
	  push	  DS
	  %LoadPage1 DS,BX
;;;	  mov	  DS,SS:pagetabl+[BX] ;  paragraph address
rep	  movsb			   ; copy string3 into new string
	  pop	  DS		   ; restore the data segment register
;**********************************************************************
;* * * Warning:  The data segment register (DS) does not point to * * *
;* * *		 the data segment in the code above		  * * *
;**********************************************************************

;     Place pointer to new string into the destination register
	  mov	  DI,[BP].reg_1	   ; load destination register address
	  mov	  AL,byte ptr tmp_page
	  mov	  byte ptr [DI].C_page,AL
	  mov	  AX,tmp_disp
	  mov	  [DI].C_disp,AX

;     Return
	  add	  SP,offset str_BP ; deallocate local storage
	  pop	  BP		   ; restore caller's BP
	  pop	  SI		   ; restore caller's SI
	  pop	  ES		   ; restore caller's ES
	  jmp	  next		   ; return to Scheme interpreter

%strapnd  endp

;************************************************************************
;*			     Reify(!)-Stack				*
;*									*
;* Purpose:  To provide the ability to manipulate items on the Scheme	*
;*		runtime stack from Scheme.				*
;*									*
;* Description:  The elements of the stack are referenced by providing	*
;*		the byte offset of the desired element as an index	*
;*		to the REIFY-STACK or REIFY-STACK! instruction.	 An	*
;*		index of -1 to REIFY-STACK is a request that the current*
;*		stack frame pointer be returned.			*
;************************************************************************
r_stk	  struc
bang	  dw	  ?		   ; fetch/store indicator
r_stk_BP  dw	  ?		   ; caller's BP
	  dd	  ?		   ; return address (far call)
	  dw	  ?		   ; return address (near call)
r_index   dw	  ?		   ; register containing index; destination reg
r_value	  dw	  ?		   ; register containing value (for stores)
r_stk	  ends

%reifyst  proc	  far
;     ***Error-- Invalid Index for REIFY-STACK(!) Instruction***
reif_err: cmp	  CX,0		   ; is this a fetch or store?
	  jne	  reif_e10	   ; if store, jump
	  mov	  AX,offset m_reifs ; load text address for "REIFY-STACK"
	  mov	  BX,1		   ; indicate 1 operand to this instruction
	  jmp	  short reif_e20   ; jump to common error code
reif_e10: mov	  AX,offset m_reifsb ; load text address for "REIFY-STACK!"
	  mov	  BX,2		   ; indicate 2 operands to this instruction
	  push	  [BP].r_value	   ;  and push second reigster operand
reif_e20: pushm	  <[BP].r_index,BX,AX> ; push arguments
	  call	  %set_src	   ; indicate source operand error
	  mov	  SP,BP		   ; drop arguments off the stack
	  mov	  AX,-1		   ; load an error flag
	  jmp	  reif_rt1	   ; return with error flag in AX

;     (REIFY-STACK! index value)   ; entry point
%reifstb  label   far
	  mov	  CX,1		   ; indicate a store operation
	  jmp	  short reif_go	   ; jump to common entry code

;     (REIFY-STACK index)	   ; entry point
%reifstk  label	  far
	  xor	  CX,CX		   ; indicate a fetch operation

reif_go:  push	  BP		   ; save the caller's BP register
	  sub	  SP,offset r_stk_BP ; allocate local storage
	  mov	  BP,SP		   ; establish addressability for operands/data

;     Validate index
	  mov	  BX,[BP].r_index  ; load address of register containing index
	  cmp	  CX,0		   ; is this a REIFY-STACK operation?
	  jne	  reif_no	   ; if not, skip special check for -1
;     Check for an index of -1 indicating we need to return FP
	  cmp	  byte ptr [BX].C_page,SPECFIX*2 ; is index a fixnum?
	  jne	  reif_no	   ; if not a fixnum index, jump
	  cmp	  [BX].C_disp,07FFFh ; is index a -1?
	  jne	  reif_no	   ; if not -1, jump
	  mov	  AX,FP		   ; load FP's offset in stack buffer
	  add	  AX,BASE	   ;  and add BASE to compute absolute offset
	  mov	  SI,AX		   ; copy quotient to SI
	  call	  ret_num	   ; convert element index to a Scheme integer
	  jmp	  reif_ret	   ; return the element index of FP
;     Fetch the index value
reif_no:  call	  get_num	   ; fetch the integer value
	  jc	  reif_err	   ; if not a valid index, jump
	  push	  AX		   ; save the byte offset
	  xor	  DX,DX		   ; convert to a double word (w/out sign ext)
	  mov	  BX,PTRSIZE	   ; load divisor
	  div	  BX		   ; divide by number of bytes/pointer
	  pop	  AX		   ; restore the byte index
	  cmp	  DX,0		   ; is remainder zero?
	  jne	  reif_err	   ; if not a multiple of PTRSIZE, error (jump)
	  mov	  DX,BASE	   ; compute the current top of stack (TOS)
	  add	  DX,TOS	   ;  offset
	  cmp	  AX,DX		   ; is index larger than TOS?
	  ja	  reif_err	   ; if so, error (jump)

;     Attempt to find the desired element in the stack buffer
	  cmp	  AX,BASE	   ; is BASE < element index?
	  jb	  reif_cnt	   ; if so, element is in previous stack segment
	  sub	  AX,BASE	   ; compute byte offset of desired element
	  add	  AX,offset S_stack ; compute offset in stack buffer
	  mov	  SI,AX		   ;  and move offset into SI
	  mov	  AX,DS		   ; put data segment address into ES so that
	  mov	  ES,AX		   ; desired element is pointed to by ES:[SI]
	  jmp	  reif_do	   ; fetch/store the element

;     Find the element in a previous stack segment (a continuation object)
reif_cnt: mov	  BX,PREV_pag	   ; make ES:[SI] point to the previous
	  mov	  SI,PREV_dis	   ;  stack segment continuation object
	  %LoadPage ES,BX
;;;	  mov	  ES,pagetabl+[BX]
;     Follow stack segment chain until desired offset found
reif_lop: cmp	  AX,ES:[SI].con_base ; compare element index:continuation base
	  jae	  reif_fnd	   ; if offset > base, element in this segment
	  mov	  BL,ES:[SI].con_spag ; load pointer to previous stack segment
	  mov	  SI,ES:[SI].con_sdis ;  into ES:[SI]
	  %LoadPage ES,BX
;;;	  mov	  ES,pagetabl+[BX] ; 
	  jmp	  short reif_lop   ; loop until desired segment found
;     Element found in stack segment (continuation) object
reif_fnd: sub	  AX,ES:[SI].con_base ; subtrace off continuation's base
	  add	  SI,AX		   ; add entry's byte offset
	  add	  SI,offset con_data ; adjust for continuation header

;     Desired stack element address by ES:[SI]-- is this a fetch or store?
reif_do:  cmp	  CX,0		   ; test fetch/store flag
	  jne	  reif_st	   ; if a store, jump
;     Fetch desired stack element
	  mov	  BX,[BP].r_index  ; load address of destination register
	  mov	  AL,ES:[SI].car_page ; load page number of stack entry
	  mov	  byte ptr [BX].C_page,AL ; and store into destination register
	  mov	  AX,ES:[SI].car   ; load displacement of stack entry
	  mov	  [BX].C_disp,AX   ;  and store into destination register
	  jmp	  short reif_ret
;     Re-define desired stack element
reif_st:  mov	  BX,[BP].r_value  ; load add of register containing new value
	  mov	  AL,byte ptr [BX].C_page
	  mov	  ES:[SI].car_page,AL
	  mov	  AX,[BX].C_disp
	  mov	  ES:[SI].car,AX

;     return to caller
reif_ret: xor	  AX,AX		   ; indicate no error encountered
reif_rt1: add	  SP,offset r_stk_BP ; deallocate local storage
	  pop	  BP		   ; restore the caller's BP register
	  ret			   ; return to caller
%reifyst  endp

;************************************************************************
;*			    AL     AL    AH  AL       AH		*
;*	(%SUBSTRING-DISPLAY string start end row-bias window)		*
;*									*
;* Purpose:  Special support for displaying strings to the CRT for	*
;*		applications such as text editors.			*
;************************************************************************

IFDEF PROMEM

SD_BSIZE  equ	  100		   	; buffer size
sd_args	  struc
;     Warning: the following five (5) items are order dependent
sd_dummy  dw      ?			; extra for realio
sd_len    dw	  ?		   	; #chars in following buffer
sd_buff	  db	  SD_BSIZE dup (?)	; string buffer
sd_text	  dw	  ?		   	; text attributes for window
sd_cursv  dw	  ?		   	; cursor coordinate save area
;
sd_char	  db	  ?		   	; "saved" character
sd_streg  dw	  ?		   	; string register address
sd_start  dw	  ?		   	; substring's starting offset
sd_end	  dw	  ?		   	; substring's ending offset
sd_bias	  dw	  ?		   	; row bias
sd_cline  dw	  ?		   	; cursor line number
sd_ccol	  dw	  ?		   	; cursor column number
sd_nline  dw	  ?		   	; number of lines in the window
sd_ncols  dw	  ?		   	; number of columns in the window
sd_ullin  dw	  ?		   	; upper left corner line number
sd_ulcol  dw	  ?		   	; upper left corner column number
sd_arg45  dw	  ?		   	; arguments 4,5 save area
sd_last	  dw	  ?		   	; last write flag
sd_linum  db	  ?		   	; line number
;     Warning: the following two (2) items are order dependent
sd_wn_SI  dw	  ?		   	; pointer to window object, part 1
sd_wn_ES  dw	  ?		   	; pointer to window object, part 2
;
sd_BP	  dw	  ?		   	; caller's BP
sd_args	  ends

ELSE 

sd_args	  struc
sd_buff	  db	  100 dup (?)	   ; string buffer
sd_char	  db	  ?		   ; "saved" character
sd_streg  dw	  ?		   ; string register address
sd_start  dw	  ?		   ; substring's starting offset
sd_end	  dw	  ?		   ; substring's ending offset
sd_bias	  dw	  ?		   ; row bias
sd_cline  dw	  ?		   ; cursor line number
sd_ccol	  dw	  ?		   ; cursor column number
sd_nline  dw	  ?		   ; number of lines in the window
sd_ncols  dw	  ?		   ; number of columns in the window
sd_ullin  dw	  ?		   ; upper left corner line number
sd_ulcol  dw	  ?		   ; upper left corner column number
sd_text	  dw	  ?		   ; text attributes for window
sd_arg45  dw	  ?		   ; arguments 4,5 save area
sd_cursv  dw	  ?		   ; cursor coordinate save area
sd_last	  dw	  ?		   ; last write flag
sd_linum  db	  ?		   ; line number
;     Warning: the following two (2) items are order dependent
sd_wn_SI  dw	  ?		   ; pointer to window object, part 1
sd_wn_ES  dw	  ?		   ; pointer to window object, part 2
;
sd_BP	  dw	  ?		   ; caller's BP
sd_args	  ends
SD_BSIZE  equ	  sd_char-sd_buff  ; buffer size

ENDIF
	  public  str_disp
strdisp   proc	  far
sd_err1:  jmp	  sd_err	   ; indirect branch to error code

;     load all five (5) of this instruction's operands
str_disp: lods	  byte ptr ES:[SI]
	  add	  AX,offset reg0
	  mov	  BX,AX		   ; save address of string register
	  lods	  word ptr ES:[SI]
	  mov	  DX,AX
	  lods	  word ptr ES:[SI]
	  save	  <SI>		   ; save location pointer

;     allocate local storage
	  push	  BP
	  sub	  SP,offset sd_BP
	  mov	  BP,SP
	  mov	  [BP].sd_last,0   ; initialize "last write?" flag
	  mov	  [BP].sd_linum,0  ; line number

;     save off argument information
	  mov	  [BP].sd_streg,BX
	  mov	  [BP].sd_arg45,AX

;     validate the string offsets
	  xor	  BX,BX		   ; clear register BX
	  mov	  BL,DL		   ; copy starting offset register number
	  add	  BX,offset reg0   ;  and compute register's address
	  call	  get_num	   ; obtain starting offset
	  jc	  sd_err1	   ; valid offset?  if not, error (jump)
	  add	  AX,BLK_OVHD	   ; adjust offset for block header
	  mov	  [BP].sd_start,AX ; save starting offset
	  xor	  BX,BX
	  mov	  BL,DH		   ; copy ending offset register number
	  add	  BX,offset reg0   ;  and compute register's address
	  call	  get_num	   ; obtain ending offset
	  jc	  sd_err1	   ; valid offset?  if not, error (jump)
	  add	  AX,BLK_OVHD	   ; adjust offset for block header
	  cmp	  AX,[BP].sd_start ; is ending offset greater than starting?
	  jb	  sd_err1	   ; if ending offset smaller, error (jump)
	  mov	  [BP].sd_end,AX   ; save ending offset

;     validate the row-bias
	  xor	  BX,BX
	  mov	  BL,byte ptr [BP].sd_arg45
	  cmp	  byte ptr reg0_pag+[BX],SPECFIX*2
	  je	  next$0
	  jmp	  sd_err
next$0:	  mov	  AX,reg0_dis+[BX]
	  shl	  AX,1
	  sar	  AX,1
	  mov	  [BP].sd_bias,AX

;     Validate the window operand
	  xor	  AX,AX
	  mov	  AL,byte ptr [BP].sd_arg45+1
	  add	  AX,offset reg0
	  pushm	  <m_one,AX>	   ; push mode=output, reg address
	  call	  %getport	   ; map port operand (result in tmp_reg)
	  cmp	  AX,0		   ; valid port operand?
	  jne	  sd_err	   ; if not a port, error (jump)
	  mov	  SI,tmp_page	   ; load a pointer to the port object
	  %LoadPage ES,SI
;;;	  mov	  ES,pagetabl+[SI]
	  mov	  SI,tmp_disp
	  mov	  AX,ES:[SI].pt_pflgs ; load the port attributes
	  test	  AX,WINDOW	   ; is this port a window?
	  jz	  sd_err	   ; if not a window, error (jump)
	  test	  AX,OPEN	   ; window open for output?
	  jnz	  sd_open	   ; if open, jump
	  jmp	  sd_done	   ; if closed, ignore I/O request (jump)
;     Move parameters from the window object to local storage
sd_open:  mov	  AX,ES:[SI].pt_cline ; get cursor line number
	  mov	  [BP].sd_cline,AX
	  mov	  AX,ES:[SI].pt_ccol ; get cursor column number
	  mov	  [BP].sd_ccol,AX
	  mov	  AX,ES:[SI].pt_nline ; get number of lines in window
	  mov	  [BP].sd_nline,AX
	  mov	  AX,ES:[SI].pt_ncols ; get number of columns in window
	  mov	  [BP].sd_ncols,AX
	  mov	  AX,ES:[SI].pt_ullin ; get upper left corner's line number
	  mov	  [BP].sd_ullin,AX
	  mov	  AX,ES:[SI].pt_ulcol ; get upper left corner's column number
	  mov	  [BP].sd_ulcol,AX
	  mov	  AX,ES:[SI].pt_text ; get window's text attributes
	  mov	  [BP].sd_text,AX
	  mov	  [BP].sd_wn_ES,ES ; save pointer to window object
	  mov	  [BP].sd_wn_SI,SI
	  jmp	  short sd_more	   ; branch over error code

;     ***error-- invalid operand***
sd_err:   mov	  SP,BP		   ; clean up stack
	  add	  SP,offset sd_BP
	  pop	  BP
	  restore <SI>		   ; load address of next instruction and
	  sub	  SI,6		   ;  adjust for 5 operands + opcode
	  mov	  AX,offset m_st_dsp ; load address of "SUBSTRING-DISPLAY"
	  pushm	  <SI,AX>	   ; push arguments to "disassemble"
	  call	  %disasse	   ; create *irritant* (pointer in tmp_reg)
	  pushm	  <tmp_adr,m_opnd,m_one> ; push operands
	  call	  %set_num	   ; indicate source operand error
	  jmp	  sch_err	   ; Link to Scheme debugger

;     validate the string operand
sd_more:  mov	  BX,[BP].sd_streg ; load string register's address
	  mov	  SI,[BX].C_page   ; load string's page number
	  cmp	  byte ptr ptype+[SI],STRTYPE*2 ; type = string?
	  jne	  sd_err	   ; if not a string, error (jump)
	  %LoadPage ES,SI
;;;	  mov	  ES,pagetabl+[SI] ; load pointer to string
	  mov	  SI,[BX].C_disp
	  mov	  AX,ES:[SI].str_len ; load string's length
	  cmp	  AX,0             ;;; check length of string
	  jge	  sd_010
	  add	  AX,BLK_OVHD+PTRSIZE  ;;; adjust for small string
sd_010:	  cmp	  AX,[BP].sd_end   ; is ending offset too big?
	  jb	  sd_err	   ; if too big, error (jump)
;     Note:  ES:[SI] points to the source string
	  mov	  DX,[BP].sd_end   ; load ending displacement and
	  add	  DX,SI		   ;  compute ending address
	  add	  SI,[BP].sd_start ; compute starting address
	
;     translate the string into the local buffer
	  mov	  CX,[BP].sd_ccol  ; load current cursor position
	  mov	  BX,[BP].sd_ncols ; load line length
	  mov	  DI,BP		   ; load pointer to local data
	  add	  DI,sd_buff	   ; and address buffer
	  push	  DS		   ; save the data segment register
	  mov	  AX,ES		   ; make DS point to the page containing
	  mov	  DS,AX		   ;  the source string
;**********************************************************************
;* * * Warning:  The data segment register (DS) does not point to * * *
;* * *		 the data segment in the code which follows	  * * *
;**********************************************************************
	  pop	  ES		   ; make ES point to the data segment
	  push	  ES

;     Register usage:  ES:[DI] - next character in output buffer
;		       DS:[SI] - next character in source string
;		       BX - number of columns in window
;		       CX - current column (cursor position) relative to window
;		       DX - end of source string address

sd_next:  cmp	  SI,DX		   ; end of input string?
	  jae	  sd_final	   ; if end of string, jump
	  lodsb			   ; fetch next character from string
	  cmp	  AL,CTL_Z	   ; possible control character?
	  ja	  sd_norml	   ; if not control character, jump
	  cmp	  AL,CTL_A	   ; nul character?
	  jb	  sd_next	   ; if nul character, ignore it (jump)
	  cmp	  AL,CTL_I	   ; tab character?
	  jne	  sd_notab	   ; if not a tab, jump
;     TAB character-- output a series of blanks
	  mov	  AL," "	   ; load a blank to store one or more times
	  mov	  AH,CL		   ; copy cursor position
	  sub	  AH,[BP].sd_linum ;  and adjust for line number
sd_tloop: stosb			   ; store a blank to the output buffer
	  inc	  CX		   ; increment the current column number
	  inc	  AH
	  test	  AH,07h	   ; is next column a multiple of eight?
	  jnz	  sd_tloop	   ; if not, loop
	  jmp	  sd_test
;     "normal" control character-- prefix with "^"
sd_notab: mov	  AH,AL		   ; save control character
	  mov	  AL,"^"	   ; load a "^" character and output to buffer
	  stosb
	  inc	  CX
	  mov	  AL,AH		   ; copy control character to AL and
	  add	  AL,"A"-CTL_A	   ;  compute alphabetic for said
;     non- control character-- just copy to output buffer
sd_norml: stosb			   ; store character into output buffer
	  inc	  CX		   ; increment the current column number
sd_test:  cmp	  CX,BX		   ; line full?
	  jb	  sd_next	   ; if more room on current line, loop

;     Full line buffered-- display it on the screen
	  call	  flush		   ; display line
	  mov	  AX,[BP].sd_cline ; load the current line number
	  cmp	  AX,[BP].sd_nline ; are we at the end of the screen?
	  jl	  sd_next	   ; if more lines in window, jump
;     Window full-- set cursor position to last line + 1, column 0
	  les	  SI,dword ptr [BP].sd_wn_SI ; load pointer to window object
	  mov	  ES:[SI].pt_ccol,0 ; set next column number to zero
	  mov	  CX,[BP].sd_cline ; store next line number into window
	  mov	  ES:[SI].pt_cline,CX ; object, too
	  jmp	  sd_fin	   ; window full, jump

;     end of string-- output final line
sd_final: push	  ES		   ; save pointer to data segment
	  les	  SI,dword ptr [BP].sd_wn_SI ; load pointer to window object
	  mov	  AX,CX		   ; save current column
	  mov	  ES:[SI].pt_ccol,CX ; store next column into window object
	  mov	  CX,[BP].sd_cline ; store current line number into window
	  mov	  ES:[SI].pt_cline,CX ; object, too
	  pop	  ES		   ; restore pointer to data segment
	  mov	  CX,SD_BSIZE-1    ; load buffer length
	  sub	  CX,AX		   ; subtract number of columns in buffer
	  mov	  AL," "	   ; load a blank
rep	  stosb			   ; blank the remainder of output buffer
	  mov	  [BP].sd_last,1   ; indicate last line
	  call	  flush		   ; display to screen
sd_fin:	  pop	  DS		   ; restore DS

;**********************************************************************
;* * * Warning:  The data segment register (DS) does not point to * * *
;* * *		 the data segment in the code above		  * * *
;**********************************************************************

;     Operation complete-- return to Scheme interpreter
sd_done:  mov	  SP,BP		   ; clean up anything pushed on stack
	  add	  SP,offset sd_BP  ; deallocate local storage
	  pop	  BP		   ; restore Scheme interpreter's BP
	  jmp	  next_PC	   ; return to Schemem interpreter

strdisp   endp

;************************************************************************
;*	      Local Support:  Flush Output Buffer to Screen		*
;*									*
;* Input Parameters:  ES - points to data segment			*
;************************************************************************
	  public  flush	
flush	  proc	  near
	  pushm	  <DS,SI,DI,CX,DX> ; save valuable registers
;     Make DS register point to data segment
	  mov	  AX,ES
	  mov	  DS,AX

;     Test for negative bias
	  inc	  [BP].sd_bias	   ; increment and test "bias" value
	  jg	  fl_no_bs	   ; if zero or positive, no bias (jump)
	  jmp	  fl_bias	   ; if negative, don't display current line
;     Position the cursor in the current column position
fl_no_bs: mov	  DL,byte ptr [BP].sd_cline ; load the current cursor
	  mov	  DH,byte ptr [BP].sd_ccol  ;  position
	  add	  DL,byte ptr [BP].sd_ullin ; adjust cursor positon by
	  add	  DH,byte ptr [BP].sd_ulcol ;  coordinates of upper left corner
	  mov	  [BP].sd_cursv,DX ; save the cursor coordinates
IFNDEF PROMEM
	  xor	  BH,BH		   ; IBMism (page 0 for text-mode)
	  mov	  AH,02h	   ; load "put cursor" code
	  call	  CRT_DSR	   ; put cursor at current position
ENDIF
;     Display the line
	  mov	  CX,[BP].sd_ncols ; load line length
	  sub	  CX,[BP].sd_ccol  ; subtract starting column offset
;     Replace the "last" character in line with an exclamation mark
	  cmp	  [BP].sd_last,0   ; last line to be output?
	  jnz	  fl_last	   ; if last line, leave character alone (jump)
	  mov	  SI,CX		   ; copy character count
	  mov	  AL,"!"	   ; load an exclamation mark
	  xchg	  AL,[BP]+sd_buff+[SI]-1   ; swap with final character in line
	  mov	  [BP].sd_char,AL  ; save character to later viewing

	  public  fl_last
fl_last   label   near
IFDEF PROMEM
	  buffer_is_stack 	   ;treat comm buffer as stack
	  mov	  [BP].sd_len,cx   ;save character count
	  REALIO  REAL_WRTBLOCK,sd_len,sd_cursv,continue
	  buffer_is_buffer	   ;treat comm buffer as buffer
ELSE
;     Determine PC make
	  cmp	  PC_MAKE,TIPC	   ; on what flavor PC are we running?
	  jne	  fl_ibm	   ; if an IBM, jump
;     Write line to TIPC's screen
	  mov	  AL,byte ptr [BP].sd_text ; load text attributes
	  mov	  AH,010h	   ; load "write block w/ attr" code
	  mov	  DX,DS		   ; load segment address
	  mov	  BX,BP
	  add	  BX,sd_buff	   ; load buffer offset in segment	   
	  int	  TI_CRT	   ; write the buffer
	  jmp	  fl_back

;     Write line to IBM's screen
fl_ibm:	  mov	  DI,BP
	  add	  DI,sd_buff	   ; load buffer offset
	  mov	  DX,[BP].sd_cursv ; reverse row/column coordinates
	  xchg	  DL,DH
	  mov	  [BP].sd_cursv,DX
	  push	  CX		   ; save the character counter
	  jmp	  short fl_imidl   ; jump into middle of loop

fl_iloop: push	  CX		   ; save the character counter
	  mov	  DX,[BP].sd_cursv ; load the previous cursor coordinates,
	  inc	  DL		   ;  increment the column number
	  mov	  [BP].sd_cursv,DX ;  and save new coordinates
	  xor	  BH,BH		   ; page number (0 for graphics mode) IBMism
	  mov	  AH,02h	   ; load "put cursor" code
	  push	  DI
	  int	  IBM_CRT
	  pop	  DI
fl_imidl: mov	  AH,09h	   ; Load "write char w/ attributes" code
	  mov	  AL,byte ptr [DI] ; load character from buffer
	  mov	  BL,byte ptr [BP].sd_text ; load attribute bits
	  xor	  BH,BH		   ; page # for alpha mode
	  mov	  CX,1		   ; load repeat count = 1
;     test to see if we buy anything by using a repeat count
	  pop	  DX		   ; restore character count
fl_imore: cmp	  DX,1		   ; more characters to display?
	  jle	  fl_ibotm	   ; if no more characters, jump
	  cmp	  AL,byte ptr [DI]+1 ; is next character the same as previous?
	  jne	  fl_ibotm	   ; if not same character, jump
	  inc	  CX		   ; increment the repeat count
	  inc	  DI		   ; increment the output buffer index
	  inc	  byte ptr [BP].sd_cursv ; increment the cursor position
	  dec	  DX		   ; decrement the character count
	  jmp	  fl_imore	   ; try for another
fl_ibotm: push	  DX		   ; save the adjusted character count
;     output the character(s)
	  push	  DI		   ; save the output buffer index
	  int	  IBM_CRT	   ; write character with attributes
	  pop	  DI		   ; restore the output buffer index
	  pop	  CX		   ; restore character counter
	  inc	  DI		   ; increment buffer pointer
	  loop	  fl_iloop	   ; continue 'til all characters output
ENDIF

;     Restore last character in line to its rightful value
fl_back:  mov	  SI,[BP].sd_ncols
	  sub	  SI,[BP].sd_ccol
	  mov	  AL,[BP].sd_char
	  mov	  [BP]+sd_buff+[SI]-1,AL

;     Shift buffer to remove the line just displayed
	  inc	  [BP].sd_cline	   ; increment the line number
fl_bias:  mov	  SI,[BP].sd_ncols ; compute number of characters just output
	  sub	  SI,[BP].sd_ccol  ;  (unless bias < 0, in which case we just
	  dec	  SI		   ;   branched here)
	  push	  SI		   ; save character count
	  mov	  CX,10		   ; make up a character count for move
	  mov	  DI,BP		   ; load address of buffer start
	  add	  DI,sd_buff
	  add	  SI,BP		   ; load address of leftover characters
	  add	  si,sd_buff
rep	  movsb			   ; shift any characters left over
          mov     BX,[BP].sd_ccol  ;;; new code for fix
                                   ;;; save the current column for adjust
	  mov	  [BP].sd_ccol,0   ; set current column to zero
	  inc	  [BP].sd_linum	   ; increment formatting line number
	  
;     Reset Active Registers to reflect shifted buffer
	  pop	  AX		   ; restore output character count
	  popm	  <DX,CX,DI,SI>	   ; restore control registers
	  sub	  DI,AX		   ; adjust buffer index
	  sub	  CX,AX		   ; adjust current column
          sub     CX,BX            ;;; new code for fix
                                   ;;; adjust current column
	  mov	  BX,[BP].sd_ncols ; reload line length
	  pop	  DS		   ; restore DS register
	  ret			   ; return
flush	  endp

;************************************************************************
;* Local Support:  Fetch and Validate Integer Argument			*
;*									*
;* Input Parameters:  BX - address of register containing the integer	*
;*			  argument					*
;*									*
;* Output Parameters:  If CARRY off, normal return:			*
;*			  AX - the 16 bit positive integer value	*
;*		       If CARRY on, error:				*
;*			  AX - the error condition; 0=operand not an	*
;*				integer; 1=integer operand was negative	*
;*				or larger than 16 bits.			*
;************************************************************************
	  public  get_num
get_num	  proc	  near
;     test for a fixnum argument
	  cmp	  byte ptr [BX].C_page,SPECFIX*2 ; fixnum?
	  jne	  big_p		   ; if not a fixnum, test for bignum (jump)
	  mov	  AX,[BX].C_disp   ; load immediate value of fixnum
	  test	  AX,04000h	   ; negative?
	  jnz	  get_val	   ; if negative, error (jump)
	  ret			   ; if positive, return with value in AX

;     test for a bignum argument
big_p:	  mov	  SI,[BX].C_page   ; load page number of argument
	  cmp	  byte ptr ptype+[SI],BIGTYPE*2 ; is argument a bignum?
	  jne	  get_type	   ; if not a bignum, invalid type (jump)
	  %LoadPage ES,SI
;;;	  mov	  ES,pagetabl+[SI] ; load paragraph address of bignum's page
	  mov	  SI,[BX].C_disp   ; load displacement of bignum
	  cmp	  ES:[SI].big_sign,0 ; test sign of bignum
	  jne	  get_val	   ; if negative, error (jump)
	  cmp	  ES:[SI].big_len,BLK_OVHD+WORDINCR+1 ; test size of bignum
	  jne	  get_val	   ; if too large, error (jump)
	  mov	  AX,ES:[SI].big_data ; load 16 bit value of bignum
	  clc
	  ret			   ; return with value in AX
;     ***error-- operand is not an integer***
get_type: mov	  AX,0		   ; indicate operand wrong type
	  jmp	  short get_err
;     ***error-- integer operand is negative, or too large***
get_val:  mov	  AX,1		
get_err:  stc
	  ret
get_num	  endp

;************************************************************************
;* Local Support:  Return a 16 bit positive integer value		*
;*									*
;* Input Parameters:  BX - address of destination register		*
;*		      SI - 16 bit unsigned integer value to be returned *
;*									*
;* Output Parameters:  The Scheme representation of the 16 bit unsigned	*
;*			value is placed into the destination register.	*
;************************************************************************
	  public  ret_num
ret_num	  proc	  near
	  cmp	  SI,03fffh	   ; can result be represented as a fixnum?
	  ja	  make_big	   ; if not, create a bignum
;     return a fixnum result
	  mov	  byte ptr [BX].C_page,SPECFIX*2 ; set tag=fixnum
	  mov	  [BX].C_disp,SI   ; store value
	  ret			   ; return
;     return a bignum result
make_big: 
	  push    SI	   	   ; save value around call
	  push	  BX		   ; save destination reg also

	  mov	  CX,WORDINCR+1    ; load size of bignum desired
	  mov	  AX,BIGTYPE	   ; load type = bignum
	  pushm	  <CX,AX,BX>	   ; push arguments to allocate block
	  call	  %allocbl	   ; allocate the bignum
	  add	  SP,WORDINCR*3	   ; drop arguments off stack

	  pop	  BX		      ; restore destination reg
	  mov	  SI,[BX].C_page      ; get page number of new bignum
	  %LoadPage ES,SI	      ; and fetch its segment address
	  mov	  SI,[BX].C_disp      ; load the bignum's displacement
	  mov	  ES:[SI].big_sign,0  ; set bignum's sign to '+'
	  pop	  AX		      ; restore value and
 	  mov	  ES:[SI].big_data,AX ; store it into the bignum
	  ret			      ; return
ret_num	  endp


PROGX	  ends

prog	  segment byte public 'PROG'
	  assume  CS:PGROUP
;************************************************************************
;* Long Linkage to SUBSTRING-FIND-NEXT-CHAR-IN-SET			*
;************************************************************************
	  public  srch_nxt
srch_nxt  proc	  near
	  call	  %srchnxt
	  ret
srch_nxt  endp

;************************************************************************
;* Long Linkage to SUBSTRING-FIND-PREVIOUS-CHAR-IN-SET			*
;************************************************************************
	  public  srch_prv
srch_prv  proc	  near
	  call	  %srchprv
	  ret
srch_prv  endp

;************************************************************************
;* Long Linkage to STRING-LENGTH					*
;************************************************************************
	  public  st_len
st_len	  proc	  near
	  call	  %st_len
	  ret
st_len	  endp

;************************************************************************
;* Long Linkage to MAKE-STRING						*
;************************************************************************
;;;	  public  make_str
make_str  proc	  near
	  call	  %makestr
	  ret
make_str  endp

;************************************************************************
;* Long Linkage to REIFY_STACK						*
;************************************************************************
	  public  reif_stk
reif_stk  proc	  near
	  call	  %reifstk
	  ret
reif_stk  endp

;************************************************************************
;* Long Linkage to REIFY_STACK!						*
;************************************************************************
	  public  reif_stb
reif_stb  proc	  near
	  call	  %reifstb
	  ret
reif_stb  endp

prog	  ends
	  end
