;							=====> PROSREAD.ASM
;***************************************
;*     TIPC Scheme  Runtime Support    *
;*	  S-Expression reading	       *
;*				       *
;*    (C) Copyright 1985 by Texas      *
;*     Instruments Incorporated.       *
;*	  All rights reserved.	       *
;*				       *
;* Date Written:  24 March 1986        *
;* Last Modification: 10 Feb 1987      *
;*				       *
;*  tc	2/10/87 fix to convert first   *
;*	char after # to upper case     *
;*  tc	2/10/87 added support to do    *
;*	readline		       *
;***************************************
	  page	  60,132
	  include scheme.equ
	  include sinterp.arg

SPACE	  equ	  20h
CTRL_Z	  equ	  1Ah
LINEFEED  equ	  0Ah
RETURN	  equ	  0Dh
COM	  equ	  3Bh
BK_SLASH  equ	  5Ch
BUFSIZE   equ	  256
TEST_NUM  equ	  8
EOFERR	  equ	  1
SHARPERR  equ	  7
PORTERR   equ	  -2
HEAPERR   equ	  -3

DGROUP	  group   data
data	  segment word public 'DATA'
	  assume  DS:DGROUP
	  public  test_ch, t_array
	  extrn   locases:word
	  extrn   hicases:word
	  extrn   CXFERR_s:word
	  extrn   port_r:word
srd_str   db	  "READ-ATOM",0
sln_str   db	  "READ-LINE",0
inv_char  db	  "Invalid character constant",0
limit	  dw	  ?			    ; current size of atom buffer
main_reg  dw	  ?			    ; main register
flg_eof   dw	  ?			    ; whether to flag end-of-file
atomb	  dw	  ?			    ; atom buffer
test_ch   db	  0Ah,20h,7Fh,0Ch,09h,08h,0Dh,1Bh ; special characters
char	  db	  20h			    ; most recently received char
t_str1	  db	  "NEWLINE",0
t_str2	  db	  "SPACE",0
t_str3	  db	  "RUBOUT",0
t_str4	  db	  "PAGE",0
t_str5	  db	  "TAB",0
t_str6	  db	  "BACKSPACE",0
t_str7	  db	  "RETURN",0
t_str8	  db	  "ESCAPE",0
t_array   dw	  t_str1
	  dw	  t_str2
	  dw	  t_str3
	  dw	  t_str4
	  dw	  t_str5
	  dw	  t_str6
	  dw	  t_str7
	  dw	  t_str8
data	  ends

PGROUP	  group   prog
prog	  segment byte public 'PROG'
	  assume  CS:PGROUP

;;;***************************************************************************
;;;			 Support for read-line
;;;***************************************************************************
rln_proc  proc
	  extrn   next_SP:near
	  extrn   src_err:near

	  public  srd_line
srd_line: lods	  byte ptr ES:[SI]
	  save	  <SI>
	  add	  AX,offset reg0	    ; compute register address
	  mov	  main_reg,AX
	  xor	  BX,BX
	  push	  BX
	  push	  AX
	  C_call  get_port,,Load_ES	    ; get the port object
	  mov	  SP,BP 		    ; get the return status
	  test	  AX,AX 		    ; error returned?
	  jnz	  srd_lerr
	  pushm   <tmp_disp,tmp_page,main_reg>
	  call	  sread_ln		    ; get a line
	  mov	  SP,BP
	  jmp	  next_SP		    ; return to interpreter
;
srd_lerr:  lea	   BX,sln_str
	   jmp	   src_err		     ; link to error handler
rln_proc   endp

;;;***************************************************************************
;;;   Set up for the operation of reading a single line from the given port.
;;;***************************************************************************
	  extrn   setabort:near
	  extrn   abort:near
	  extrn   ssetadr:near
srdlnarg  struc
temp_r	  dw	  ?			   ; temporary storage
srdln_BP  dw	  ?			   ; caller's BP
	  dw	  ?			   ; caller's return address
rp_reg	  dw	  ?			   ; port register
rpg	  dw	  ?			   ; adjusted page number
rdisp	  dw	  ?			   ; displacement
srdlnarg  ends
;
	  public  sread_at
sread_ln  proc	  near
	  push	  BP
	  sub	  SP, offset srdln_BP	   ; allocate local storage
	  mov	  BP,SP
	  call	  setabort		   ; save stack pointer
	  pushm   <[BP].rdisp,[BP].rpg>
	  call	  ssetadr		   ; set port address
	  mov	  SP,BP
	  test	  AX,AX 		   ; check return status
	  jz	  srdl_010
	  mov	  AX,PORTERR		   ; port error
	  push	  AX
	  call	  abort
;
	  mov	 flg_eof,1		  ; flag eof
srdl_010:
	  call	 rcvchar		  ; get char, eof won't return here
	  cmp	 AL,LINEFEED		  ; is char linefeed?
	  je	 srdl_010		  ; if so, ignore

	  mov	 [BP].temp_r,AX 	  ; save character read

	  mov	 AX,BUFSIZE		  ; Get buffer size
	  mov	 limit,AX
	  push	 AX
	  C_call getmem 		  ; allocate buffer
	  mov	 SP,BP
	  cmp	 AX,0			  ; memory available?
	  jne	 srdl_020
;error allocate C heap space
	  mov	 AX,HEAPERR		  ; no, error
	  push	 AX
	  call	 abortrea
	  mov	 SP,BP
	  jmp	 srdln_ret

srdl_020: mov	 SI,AX
	  mov	 atomb,AX		  ; address of buffer
	  mov	 flg_eof,0		  ; don't flag error on EOF
	  xor	 BX,BX			  ; index into buffer
	  mov	 AX,[BP].temp_r 	  ; restore saved character
; read characters
srdln_cha:
	  cmp	 AL,RETURN		  ; Return character?
	  je	 srdln_ret		  ;   yes, return
	  cmp	 AL,CTRL_Z		  ; EOF character?
	  je	 srdln_ret		  ;   yes, return
	  cmp	 AL,LINEFEED		  ; Linefeed character?
	  je	 srdln_ret		  ;   yes, don't put in atomb

	  pushm  <AX,BX>
	  call	 addchar		  ; Add character to buffer
	  mov	 SP,BP
	  inc	 BX
srdln_nxt:
	  call	 rcvchar		  ; Get next character
	  jmp	 srdln_cha		  ; Go get next character

srdln_ret:
	  mov	 CX,STRTYPE		; Allocate string data type
	  mov	 [BP].temp_r,BX
	  pushm  <BX,CX,main_reg>
	  c_call alloc_bl,,Load_ES
	  mov	 SP,BP
	  mov	 CX,3			; Copy buffer to Scheme string
	  mov	 SI,atomb
	  pushm  <[BP].temp_r,SI,CX,main_reg>
	  call	 toblock
	  mov	 AX,limit		; Release buffer
	  pushm  <AX,atomb>
	  C_call rlsmem
	  mov	 SP,BP
	  mov	 flg_eof,1		; Reset flags
	  mov	 limit,0
	  add	 SP,offset srdln_BP	; Deallocate local storage
	  pop	 BP
	  ret				; Return
sread_ln  endp

;;;***************************************************************************
;;;			 Support for read-atom
;;;***************************************************************************
rds_proc  proc
	  extrn   next_SP:near
	  extrn   src_err:near

	  public  srd_atom
srd_atom: lods	  byte ptr ES:[SI]
	  save	  <SI>
	  add	  AX,offset reg0	    ; compute register address
	  mov	  main_reg,AX
	  xor	  BX,BX
	  push	  BX
	  push	  AX
	  C_call  get_port,,Load_ES	    ; get the port object
	  mov	  SP,BP 		    ; get the return status
	  test	  AX,AX 		    ; error returned?
	  jnz	  srd_err
	  pushm   <tmp_disp,tmp_page,main_reg>
	  call	  sread_at		    ; sread_atom()
	  mov	  SP,BP
	  jmp	  next_SP		    ; return to interpreter
;
srd_err:  lea	  BX,srd_str
	  jmp	  src_err		    ; link to error handler
rds_proc  endp

;;;***************************************************************************
;;;   Set up for the operation of reading a single atom from the given port.
;;;   Special characters such as ')' are parsed as lists(!) to tell them from
;;;   ordianry atoms.
;;;***************************************************************************
	  extrn   setabort:near
	  extrn   abort:near
	  extrn   ssetadr:near
sreadarg  struc
	  dw	  ?			   ; caller's BP
	  dw	  ?			   ; caller's return address
p_reg	 dw	 ?			  ; port register
pg	 dw	 ?			  ; adjusted page number
disp	 dw	 ?			  ; displacement
sreadarg  ends
;
	  public  sread_at
sread_at  proc	  near
	  push	  BP
	  mov	  BP,SP
	  call	  setabort		   ; save stack pointer
	  mov	  BX,[BP].p_reg     	   ;be certain main_reg gets set if
	  				   ;sread_at gets called directly from C
	  mov	  main_reg,BX
	  pushm   <[BP].disp,[BP].pg>
	  call	  ssetadr		   ; set port address
	  mov	  SP,BP
	  test	  AX,AX 		   ; check return status
	  jz	  srd_010
	  mov	  AX,PORTERR		   ; port error
	  push	  AX
	  call	  abort
;
srd_010:  mov	  flg_eof,1		   ; initialization
	  mov	  limit,0
; skip spaces
srd_spa:  call	  rcvchar
	  call	  ck_space		   ; check for space
	  test	  CX,CX
	  jz	  srd_spa		   ; yes, skip
; skip comments
srd_com:  cmp	  AL,COM		   ; check for comment
	  jne	  srd_at
srd_c10:  call	  rcvchar
	  cmp	  AL,RETURN
	  jne	  srd_c10		   ; yes, ignore the whole line
	  jmp	  srd_spa
;
srd_at:   test	  AL,AL 		   ; null character?
	  jz	  srd_spa
	  call	  read_ato
	  pop	  BP
	  ret
sread_at  endp

;;;***************************************************************************
;;;	       Fetch one character from the input stream
;;;***************************************************************************
	  extrn  take_ch:near
rcvchar   proc	 near
	  pop	 DX			    ; fetch return address
;
	  push	 DX			    ; save registers
	  push	 SI
	  push	 DI
	  push	 CX
	  push	 BX
	  call	 take_ch		    ; takechar()
	  pop	 BX			    ; restore registers
	  pop	 CX
	  pop	 DI
	  pop	 SI
	  pop	 DX
; Check the character
	  cmp	 AX,256
	  jge	 rcv_10
	  cmp	 AL,CTRL_Z		    ; EOF character?
	  je	 rcv_10 		    ; yes, jump
	  mov	 char,AL
	  jmp	 DX			    ; return to caller
; EOF character is fetched
rcv_10:   cmp	 flg_eof,0		    ; EOF flag set?
	  jne	 rcv_20 		    ; yes, error
	  mov	 AX,CTRL_Z
	  mov	 char,AL
	  jmp	 DX			    ; return to caller
;
rcv_20:   mov	 AX,EOFERR
	  push	 AX
	  call	 abortrea		    ; abortread(EOFERR)
rcvchar   endp

;;;***************************************************************************
;;;		 Read in an atom (symbol, string, number)
;;;  Store the pointer to the atom in REG.
;;;  Special characters such as ')' or ',' are read as atoms themselves.
;;;  Normal atoms will end in a whitespace or a terminating macro character;
;;;  strings end with the closing  '"'.
;;;  Numbers in the requested base are interpreted as such.
;;;  On exit, the next character in the buffer is the one following the last
;;;  character of the atom.
;;;***************************************************************************
	  extrn   toblock:near
	  extrn   cons:near
	  extrn   buildint:near
	  extrn   alloc_st:near
	  extrn   scannum:near
	  extrn   pushchar:near

readarg   struc
num_base  dw	  ?			   ; base of number
tmpreg	  dw	  ?
inputch   dw	  ?			   ; whether the #\ macro is in effect
escaped   dw	  ?			   ; whether an escape char is used
inflo	  dq	  ?			   ; for floating point value
bignum	  dw	  ?
biglimit  dw	  ?
read_BP   dw	  ?			   ; caller's BP
	  dw	  ?			   ; caller's ES
	  dw	  ?			   ; caller's return address
readarg   ends
;
read_ato  proc	 near
	  push	 ES
	  push	 BP
	  sub	 SP,offset read_BP	   ; allocate local storage
	  mov	 BP,SP
	  xor	 CX,CX
	  mov	 [BP].tmpreg,AX
;;;	  cmp	 AL,SPACE		   ; check for space?
;;;	  jne	 read_at
;;;	  mov	 [DI].C_page,CX 	   ; yes, form NIL and return
;;;	  mov	 [DI].C_disp,CX
;;;	  jmp	 read_end
read_at:  mov	 flg_eof,CX		   ; initialization
	  mov	 [BP].inputch,CX
	  mov	 [BP].escaped,CX
	  mov	 CXFERR_s,CX
	  mov	 AX,BUFSIZE
	  mov	 limit,AX
	  mov	 [BP].num_base,10
	  push	 AX
	  C_call getmem 		   ; allocate memory
	  mov	 SP,BP
	  cmp	 AX,0			   ; memory available?
	  jne	 read_01
memerr:   mov	 AX,HEAPERR		   ; no, error
	  push	 AX
	  call	 abortrea
	  mov	 SP,BP
	  jmp	 read_ret
read_01:  mov	 SI,AX
	  mov	 atomb,AX		   ; save the address of atom buffer
	  mov	 DI,main_reg
	  xor	 BX,BX
	  mov	 AX,[BP].tmpreg
; check for the special character first
	  cmp	 AL,5Bh 		   ; [
	  je	 read_10
	  cmp	 AL,5Dh 		   ; ]
	  je	 read_10
	  cmp	 AL,7Bh 		   ; {
	  je	 read_10
	  cmp	 AL,7Dh 		   ; }
	  je	 read_10
	  cmp	 AL,28h 		   ; (
	  je	 read_10
	  cmp	 AL,29h 		   ; )
	  je	 read_10
	  cmp	 AL,27h 		   ; '
	  je	 read_10
	  cmp	 AL,60h 		   ; `
	  jne	 read_st
; special character case
read_10:  mov	 [SI],AL		   ; *atomb = ch
	  inc	 BX
	  jmp	 read_sp
;
read_st:  cmp	 AL,22h 		   ; "
	  jne	 read_co
; string case
	  push	 AX
	  call	 delimby		   ; get the string
	  mov	 SP,BP
	  mov	 [BP].tmpreg,BX 	   ; save BX register
	  mov	 CX,STRTYPE
	  pushm  <BX,CX,main_reg>
	  C_call alloc_bl,,Load_ES	   ; allocate string object
	  mov	 SP,BP
	  mov	 CX,3
	  mov	 SI,atomb
	  pushm  <[BP].tmpreg,SI,CX,main_reg>
	  call	 toblock		   ; copy string to string object
	  jmp	 read_bye
;
read_co:  cmp	 AL,2Ch 		   ; ,
	  jne	 read_mac
; comma case
	  mov	 [SI],AL
	  inc	 BX
	  call	 rcvchar		   ; get the next character
	  cmp	 AL,40h 		   ; check for @
	  je	 read_20
	  cmp	 AL,2Eh 		   ; check for .
	  je	 read_20
	  jmp	 read_nor
read_20:  mov	 [SI+BX],AL
	  inc	 BX
	  jmp	 read_sp
;
read_mac: cmp	 AL,23h 		   ; #
	  je	 read_25
	  jmp	 read_sym
; macro case
read_25:  mov	 flg_eof,1
read_30:  test	 BX,BX			   ; first character?
	  jz	 read_34
read_32:  jmp	 read_200		   ; no, jump
;
read_34:  cmp	 AL,23h 		   ; #
	  jne	 read_32		   ; no, jump
	  call	 rcvchar		   ; get the next character
	  call	 ck_space		   ; check for space
	  test	 CX,CX
	  jnz	 read_40
read_35:  mov	 AX,SHARPERR		   ; yes, error
	  push	 AX
	  call	 abortrea
;
read_40:  mov	 byte ptr [SI+1],AL	   ; save the character
	  push	 BX
	  mov	 BX,offset locases	   ; address of lower-case characters
	  xlat
	  pop	 BX			   ; restore registers
	  cmp	 AL,62h 		   ; b?
	  jne	 read_d
	  mov	 [BP].num_base,2
	  jmp	 read_100
;
read_d:   cmp	 AL,64h 		   ; d?
	  jne	 read_x
	  mov	 [BP].num_base,10
	  jmp	 read_100
;
read_x:   cmp	 AL,78h 		   ; x?
	  je	 read_50
	  cmp	 AL,68h 		   ; h?
	  jne	 read_o
read_50:  mov	 [BP].num_base,16
	  jmp	 read_100
;
read_o:   cmp	 AL,6Fh 		   ; o?
	  jne	 read_ba
	  mov	 [BP].num_base,8
	  jmp	 read_100
;
read_ba:  cmp	 AL,BK_SLASH		   ; \?
	  jne	 read_i
	  call	 rcvchar
	  pushm  <AX,BX>
	  call	 addchar
	  mov	 SP,BP
	  inc	 BX
	  mov	 [BP].inputch,1
	  mov	 [BP].escaped,1
	  jmp	 read_100
;
read_i:   cmp	 AL,69h 		   ; i?
	  je	 read_100
	  cmp	 AL,65h 		   ; e?
	  je	 read_100
	  cmp	 AL,73h 		   ; s?
	  je	 read_100
	  cmp	 AL,6Ch 		   ; l?
	  je	 read_100
	  cmp	 AL,3Ch 		   ; <?
	  je	 read_60		   ; yes, error
	  cmp	 AL,29h 		   ; )?
	  jne	 read_70
read_60:  jmp	 read_35		   ; yes, error
;
read_70:  mov	 byte ptr [SI],23h	   ; default
	  mov	 BX,offset hicases	   ; address of higher-case characters
	  xlat
	  mov	 byte ptr [SI+1],AL	   ; Change letter past # to upper case
	  mov	 BX,2
	  cmp	 AL,28h 		   ; check for (
	  jne	 read_100
	  jmp	 read_sp		   ; yes, special case
;
read_100: call	 rcvchar		   ; get the next character
	  jmp	 read_30
;
read_200: mov	 flg_eof,0
; handle for symbol
read_sym:				   ; default
	  call	 ck_space		   ; check for space
	  test	 CX,CX
	  jz	 read_en		   ; yes, jump
	  cmp	 AL,CTRL_Z		   ; eof character?
	  je	 read_en
	  cmp	 AL,28h 		   ; (
	  je	 read_en
	  cmp	 AL,29h 		   ; )
	  je	 read_en
	  cmp	 AL,27h 		   ; '
	  je	 read_en
	  cmp	 AL,60h 		   ; `
	  je	 read_en
	  cmp	 AL,COM 		   ; comment?
	  je	 read_en
	  cmp	 AL,2Ch 		   ; ,
	  je	 read_en
	  cmp	 AL,22h 		   ; "
	  je	 read_en
	  cmp	 AL,5Bh 		   ; [
	  je	 read_en
	  cmp	 AL,5Dh 		   ; ]
	  je	 read_en
	  cmp	 AL,7Bh 		   ; {
	  je	 read_en
	  cmp	 AL,7Dh 		   ; }
	  je	 read_en
	  push	 BX
	  mov	 BX,offset hicases	   ; address of higher-case characters
	  xlat
	  pop	 BX
	  cmp	 AL,7Ch 		   ; |?
	  jne	 read_210
	  mov	 [BP].escaped,1
	  push	 AX
	  call	 delimby		   ; read the whole symbol
	  mov	 SP,BP
	  jmp	 read_250
;
read_210: cmp	 AL,BK_SLASH		   ; \?
	  jne	 read_220
	  mov	 [BP].escaped,1
	  mov	 flg_eof,1
	  call	 rcvchar
	  mov	 flg_eof,0
read_220: pushm  <AX,BX>
	  call	 addchar
	  mov	 SP,BP
	  inc	 BX
read_250: call	 rcvchar		   ; get the next character
	  jmp	 read_sym
;
read_en:  xor	 AL,AL			   ; put null at end of token
	  pushm  <AX,BX>
	  call	 addchar
	  mov	 SP,BP
; Check for single, unescaped dot
	  cmp	 BX,1
	  jne	 read_num
	  cmp	 byte ptr [SI],2Eh	   ; check for .
	  jne	 read_num
	  cmp	 [BP].escaped,1
	  je	 read_num
	  jmp	 read_nor
; At this point a token has been accumulated, check for number
read_num: mov	 [BP].tmpreg,BX 	   ; save BX register
	  push	 [BP].num_base
	  push	 SI
	  call	 scannum		   ; scan number
	  mov	 SP,BP
	  mov	 SI,atomb		   ; restore SI register
	  mov	 BX,[BP].tmpreg 	   ; restore BX register
	  test	 AX,AX			   ; number or not?
	  jnz	 read_n05
	  jmp	 read_500
read_n05: cmp	 [BP].escaped,1
	  jne	 read_n07
	  jmp	 read_500
read_n07: cmp	 AX,0
	  jle	 read_300		   ; negative for floating point number
; integer of some size
	  add	 AX,9			   ; (AX + 9) / 2
	  shr	 AX,1			   ; AX = bytes needed for integer
	  mov	 [BP].biglimit,AX	   ; save for later
	  push	 AX
	  C_call getmem 		   ; allocate memory for bignum
	  mov	 SP,BP
	  cmp	 AX,0			   ; memory available?
	  jne	 read_n10
	  jmp	 memerr 		   ; no, error
read_n10: mov	 BX,AX
	  mov	 [BP].bignum,AX
	  mov	 byte ptr [BX+3],0
	  mov	 byte ptr [BX+4],0
	  pushm  <[BP].num_base, atomb, BX>
	  call	 buildint		   ; form integer
	  mov	 SP,BP
	  mov	 DI,main_reg
	  mov	 BX,[BP].bignum
	  pushm  <BX,DI>
	  C_call alloc_in,,Load_ES	   ; alloc_int
	  mov	 SP,BP
	  pushm  <[BP].biglimit,[BP].bignum>
	  C_call rlsmem 		   ; release memory for bignum
	  mov	 SP,BP
	  jmp	 read_rls
; Floating point number
read_300: lea	 DX,[BP].inflo
	  pushm  <[BP].num_base, DX, SI>
	  C_call scanflo,,Load_ES	   ; scan the flonum
	  mov	 SP,BP
	  mov	 DI,main_reg
	  lea	 BX,[BP].inflo
	  pushm  <[BX+6],[BX+4],[BX+2],[BX]> ; push flonum value
	  push	 DI
	  C_call alloc_fl,,Load_ES	   ; alloc_flonum
	  mov	 SP,BP
	  jmp	 read_rls
; Allocate character or interned symbol
read_500: cmp	 [BP].inputch,0 	   ; #\ macro?
	  mov	 DI,main_reg
	  jne	 read_510
	  jmp	 read_600		   ; no, symbol
read_510: mov	 [DI].C_page,SPECCHAR*2
	  cmp	 BX,1			   ; only one character?
	  jne	 read_mul		   ; no, jump
	  xor	 AH,AH
	  mov	 AL,byte ptr [SI]
	  mov	 [DI].C_disp,AX 	   ; return the character
	  jmp	 read_rls
; Check for a multichar character constant
read_mul: mov	 AL,byte ptr [SI]
	  mov	 BX,offset hicases	   ; address of higher-case characters
	  xlat
	  mov	 byte ptr [SI],AL
	  xor	 BX,BX
read_515: cmp	 BL,TEST_NUM		   ; finish the comparison?
	  je	 read_580		   ; yes, jump
	  lea	 DI,t_array		   ; save BX register
	  mov	 CX,BX
	  shl	 BX,1			   ; get the word offset
	  mov	 DI,word ptr [DI+BX]	   ; address of special string
	  xor	 BX,BX
read_520: mov	 AL,byte ptr [DI+BX]	   ; get the character in string
	  cmp	 AL,0			   ; end of string
	  je	 read_530		   ; match
	  cmp	 byte ptr [SI+BX],AL
	  jne	 read_540
	  inc	 BX
	  jmp	 read_520
read_530: mov	 BX,CX
	  lea	 SI,test_ch		   ; address of special characters
	  mov	 AL,byte ptr [SI+BX]
	  mov	 DI,main_reg
	  mov	 [DI].C_disp,AX 	   ; return the special character
	  jmp	 read_rls
;
read_540: mov	 BX,CX
	  inc	 BX
	  jmp	 read_515
; For the unrecognized multi-char character constant, return #\?
read_580: mov	 DI,main_reg
	  mov	 [DI].C_disp,3Fh	   ; return '?' character
;;;	  push	 SI
;;;	  lea	 BX,tmp_reg
;;;	  push	 BX
;;;	  C_call alloc_st,,Load_ES	   ; alloc_string for error message
;;;	  mov	 SP,BP
;;;	  lea	 BX,tmp_reg
;;;	  push	 BX
;;;	  lea	 BX,inv_char
;;;	  push	 BX
;;;	  xor	 BX,BX
;;;	  push	 BX
;;;	  C_call set_erro,,Load_ES	   ; set_error
;;;	  mov	 SP,BP
	  mov	 CXFERR_s,-1		   ; error status
	  jmp	 read_rls
; Not a character, but a symbol
read_600: push	 BX			   ; length of symbol
	  push	 SI			   ; address of symbol
	  push	 DI			   ; register
	  C_call intern,,Load_ES	   ; intern the symbol
	  mov	 SP,BP
	  jmp	 read_rls
;
read_sp:  pushm  <BX, SI, DI>
	  C_call intern,,Load_ES	   ; intern the symbol
	  mov	 SP,BP
	  lea	 BX,nil_reg
	  mov	 DI,main_reg
	  pushm  <BX, DI, DI>
	  call	 cons			   ; encase in a list
	  mov	 SP,BP
	  jmp	 read_bye
;
read_nor: pushm  <BX, SI, DI>
	  C_call intern,,Load_ES	   ; intern the symbol
	  mov	 SP,BP
	  lea	 BX,nil_reg
	  mov	 DI,main_reg
	  pushm  <BX, DI, DI>
	  call	 cons			   ; encase in a list
	  mov	 SP,BP
read_rls: cmp	 char,CTRL_Z		   ; EOF character?
	  je	 read_bye
	  call	 pushchar		   ; put post-atom char back to buffer
;
read_bye: mov	 AX,limit
	  pushm  <AX, atomb>
	  C_call rlsmem 		   ; release memory
	  mov	 SP,BP
	  mov	 flg_eof,1		   ; reset flags
	  mov	 limit,0
;
read_end: mov	 AX,CXFERR_s		   ; return status
read_ret: add	 SP,offset read_BP	   ; release local storage
	  pop	 BP
	  pop	 ES
	  ret
read_ato  endp

;;;************************************************************************
;;;			  DELIMBY(c)
;;;	DELIMBY takes characters from the input stream and places them
;;; in the buffer ATOMB, starting at offset stored in BX register, and
;;; ending when the delimiting character C is reached.
;;; Note:   SI = address of atomb
;;;	    BX = number of characters in atomb
;;;************************************************************************
deliarg   struc
	  dw	 ?			   ; caller's BP
	  dw	 ?			   ; caller's return address
cha	  dw	 ?			   ; character
deliarg   ends

delimby   proc	 near
	  push	 BP			   ; get the return address
	  mov	 BP,SP
	  mov	 flg_eof,1		   ; signal the EOF error
	  call	 rcvchar
deli_10:  mov	 CX,[BP].cha
	  cmp	 AL,CL			   ; reach the end?
	  je	 deli_50		   ; yes, return
	  cmp	 AL,RETURN		   ; carriage return?
	  je	 deli_40		   ; yes, ignore
	  cmp	 AL,BK_SLASH		   ; check for \
	  jne	 deli_30
	  call	 rcvchar		   ; yes, ignore
deli_30:  pushm  <AX,BX>
	  call	 addchar
	  mov	 SP,BP
	  inc	 BX
deli_40:  call	 rcvchar		   ; get the next character
	  jmp	 deli_10
deli_50:  mov	 flg_eof,0
	  pop	 BP
	  ret
delimby   endp

;;;************************************************************************
;;;			 ADDCHAR (i, c)
;;;	      ADDCHAR takes the character c and places it in the dynamic
;;;	 atom buffer atomb, at offset i. If the buffer can not contain
;;;	 any more characters, additional space is allocated, and limit
;;;	 is adjusted accordingly.
;;;************************************************************************
addarg	  struc
add_tmp   dw	 ?
add_BP	  dw	 ?			   ; caller's BP
	  dw	 ?			   ; caller's return address
index	  dw	 ?
chara	  dw	 ?
addarg	  ends

addchar   proc	 near
	  push	 BP
	  sub	 SP,offset add_BP	   ; allocate local storage
	  mov	 BP,SP
	  mov	 BX,[BP].index
	  cmp	 BX,limit		   ; room for character?
	  jge	 add_10 		   ; no, jump
add_01:   mov	 AX,[BP].chara
	  mov	 byte ptr [SI+BX],AL
add_ret:  add	 SP,offset add_BP
	  pop	 BP
	  ret
add_10:   mov	 AX,limit
	  add	 AX,BUFSIZE
	  push	 AX
	  C_call getmem 		   ; allocate memory
	  mov	 SP,BP
	  cmp	 AX,0			   ; memory available?
	  jne	 add_20
	  mov	 AX,HEAPERR		   ; no, error
	  push	 AX
	  call	 abortrea
	  mov	 SP,BP
	  jmp	 add_ret
add_20:   mov	 DI,AX			   ; address of new buffer
	  mov	 SI,atomb
	  mov	 CX,limit
rep	  movsb 			   ; copy characters
	  mov	 [BP].add_tmp,AX	   ; save buffer pointer
	  pushm  <limit, atomb>
	  C_call rlsmem 		   ; discard the old buffer
	  mov	 SP,BP
	  mov	 SI,[BP].add_tmp
	  mov	 atomb,SI
	  mov	 CX,limit
	  add	 CX,BUFSIZE		   ; increase the limit
	  mov	 limit,CX
	  mov	 BX,[BP].index
	  jmp	 add_01
addchar   endp

;;;************************************************************************
;;;			ABORTREAD(code)
;;;    Cancels the entire read operation via ABORT, after
;;; resetting some vital registers.
;;; Note:   DI = address of main register
;;;************************************************************************
abortarg  struc
	  dw	 ?			   ; caller's BP
	  dw	 ?			   ; caller's return address
errcode   dw	 ?			   ; error code
abortarg  ends

abortrea  proc	 near
	  push	 BP
	  mov	 BP,SP
	  mov	 DI,main_reg		   ; main register
	  cmp	 [BP].errcode,EOFERR	   ; EOF error?
	  jne	 ab_010
	  mov	 [DI].C_page,EOF_PAGE*2    ; return eof indicator
	  mov	 [DI].C_disp,EOF_DISP
	  jmp	 ab_020
;
ab_010:   xor	 AX,AX
	  mov	 [DI].C_page,AX 	   ; NUL main register
	  mov	 [DI].C_disp,AX
;
ab_020:   push	 [BP].errcode
	  call	 abort
	  pop	 BP
	  ret
abortrea  endp

;;;**********************************************************************
;;;	 Local support to check the character in AX is space or not
;;;  Note:   CX = 0 iff the character is whitespace
;;;**********************************************************************
ck_space  proc	 near
	  pop	 DX			   ; get the return address
	  xor	 CX,CX
	  cmp	 AL,SPACE		   ; space?
	  je	 is
	  cmp	 AL,9
	  jb	 isnot
	  cmp	 AL,0Dh
	  jbe	 is
isnot:	  inc	 CX
is:	  jmp	 DX			   ; return to caller
ck_space  endp
prog	  ends
	  end

