;							=====> STIMER.ASM
;***************************************
;*  TIPC Scheme '84 Things That Could  *
;*  Have Been Done in C but Why Waste  *
;*    Execution Time and Codespace?    *
;*				       *
;*  (C) Copyright 1984,1985 by Texas   *
;*     Instruments Incorporated.       *
;*	  All rights reserved.	       *
;*				       *
;* Date Written:  July 1985	       *
;* Last Modification:  8 October 1985  *
;***************************************
	  include scheme.equ

DGROUP	  group   data
data	  segment word public 'DATA'
	  assume  DS:DGROUP
;Table of strange characters
stranges  db	  " ,'"
	  db	  ';":()`'
	  db	  13,12,11,10,9,0

;Random number registers
krala	  dw	  22425
kralb	  dw	  30029 	   ;RANDOMIZE puts seed value here
;Random number table
kraltbl   dw	  4053,32361,7773,17385,11177,20413,27513,16501
	  dw	  5953,17673,20725,12247,28429,30861,16849,22375
;Copy of random number registers and table.
krala1	  dw	  22425
kralb2	  dw	  30029
kraltbl1  dw	  4053,32361,7773,17385,11177,20413,27513,16501
	  dw	  5953,17673,20725,12247,28429,30861,16849,22375
kral_len  equ	  krala1-krala

data	  ends

PGROUP	  group   prog
prog	  segment byte public 'PROG'
	  assume  CS:PGROUP

;For space and performance reasons, some procedures have been written in the
;  following style: the arguments are popped off the stack, and the
;  procedure may end in an indirect JMP instead of a RET.  In this source file,
;  the following are such procedures:
;      toblock, gvchars, blk2pbuf, putlong, thefix, ldlong, msubstr,
;       mcmpstr, ldreg, pt_flds4, pt_flds6, str2str, adj4bord

;     Convert flonum to bignum
;	Calling sequence: flotobig(flo,bigbuf)
;	      Where ---- flo: double-length flonum such that abs(flo)>=1
;			 bigbuf: pointer to buffer for bignum formation
fbargs	  struc
	  dw	  ?		   ;Return address
flo	  dw	  ?,?,?,?	   ;Flonum
bigbuf	  dw	  ?		   ;Pointer to bignum buffer
fbargs	  ends
	  public  flotobig
flotobig  proc	  near
	  mov	  BX,SP
	  lea	  SI,[BX].flo	   ;Fetch pointer to flonum
	  mov	  DI,[BX].bigbuf   ;Fetch buffer pointer
	  inc	  DI		   ;Point DI to sign byte
	  inc	  DI
	  cld			   ;Direction forward
	  mov	  AX,[BX+6].flo    ;Fetch exponent word to CX
	  mov	  CX,AX
	  and	  AX,0fh	   ;Save mantissa part back
	  or	  AL,10h
	  mov	  [BX+6].flo,AX
	  mov	  AL,AH 	   ;Zero AL
	  test	  CH,80h	   ;Negative flonum?
	  jz	  ftb1		   ;Jump if not
	  inc	  AL		   ;Otherwise, set AL to 1
ftb1:	  stosb 		   ;Store sign byte
	  mov	  BX,DI 	   ;Save address of first word in BX
	  mov	  AL,AH 	   ;Zero AL again
	  and	  CX,7ff0h	   ;Discard sign byte and mantissa
	  sub	  CX,3ff0h	   ;Remove exponent bias
	  shl	  CX,1
;At this stage, CH+1==number of bytes for bignum, CL shows how much to
;  shift mantissa left (once per 20h)
	  mov	  DX,CX 	   ;Use DX to count the shifts
	  xor	  DH,DH 	   ;Set up shift count
	  add	  DX,80h	   ;Account for placing leading 1 in high byte
ftb2:	  shl	  word ptr[SI],1   ;Shift mantissa left
	  rcl	  word ptr[SI+2],1
	  rcl	  word ptr[SI+4],1
	  rcl	  word ptr[SI+6],1
	  sub	  DX,20h	   ;Repeat until done
	  jnz	  ftb2
	  mov	  CL,CH 	   ;Set CX to number of bignum bytes
	  xor	  CH,CH
	  inc	  CX
	  sub	  CX,8		   ;Check for leading zeros
	  js	  ftb3		   ;Jump if not all the mantissa will be done
	  jz	  ftb3		   ;Jump if no trailing zeros exist
	  rep	  stosb 	   ;Else store as many zeros as necessary
ftb3:	  sub	  SI,CX 	   ;Point SI to eligible part of mantissa
	  add	  CX,8		   ;Set mantissa byte count
	  rep	  movsb 	   ;Copy flonum mantissa to bignum
	  mov	  CX,DI 	   ;Find number of bytes in bignum proper
	  sub	  CX,BX
	  shr	  CX,1		   ;Find number of words
	  jnc	  ftb4		   ;If a whole number of words, do nothing
	  mov	  byte ptr[DI],0   ;Otherwise, pad with a 0
	  inc	  CX		   ;Adjust word count
ftb4:	  mov	  [BX-3],CX	   ;Save size of bignum
	  ret
flotobig  endp

;     Find the size of a flonum
;	Calling sequence: flosiz(flo);
;	      Where ---- flo: double-length flonum
;     Returns the number of bytes needed for a working flonum formed from
;	trunc(flonum)
fsargs	  struc
	  dw	  ?		   ;Return address
fl	  dw	  ?,?,?,?	   ;Double-length flonum
fsargs	  ends
	  public  flosiz
flosiz	  proc	  near
	  mov	  SI,SP
	  mov	  AX,[SI+6].fl	   ;Fetch word containing exponent
	  and	  AX,7ff0h	   ;Drop sign and mantissa
	  sub	  AX,3ff0h	   ;Is abs(flo) < 1?
	  jc	  small 	   ;Jump if small
	  mov	  AL,AH 	   ;Otherwise, return number of bytes
	  xor	  AH,AH
	  shl	  AL,1
	  add	  AL,5
	  ret
small:	  xor	  AX,AX 	   ;Return 0 for smallness
	  ret
flosiz	  endp

;     Move bytes from buffer to allocated Scheme block
;	Calling sequence: toblock(reg,offs,buf,q)
;	      Where ---- reg:  Scheme register pointing to block
;			 offs: Offset into block to begin transfer
;			 buf:  Buffer pointer
;			 q:    Number of bytes to move
;Stack elements in order of popping:
;  Return address, register, offset, buffer address, number of bytes
	  public  toblock
toblock   proc	  near
	  pop	  DX		   ;Save return address in DX
	  pop	  BX		   ;Get register address
	  mov	  DI,[BX].C_disp   ;Put 8088 address in ES:DI
	  mov	  BX,[BX].C_page
	  mov	  AX,ES
	  LoadPage ES,BX
;;;	  mov	  ES,pagetabl+[BX]
	  pop	  CX		   ;Get offset
	  add	  DI,CX 	   ;Add to DI
	  pop	  SI		   ;Get source address (buffer ptr)
	  pop	  CX		   ;Get number of bytes
	  jcxz	  tbskip	   ;If no bytes, don't move
	  cld			   ;Direction forward
	  rep	  movsb 	   ;Move bytes
tbskip:   mov	  ES,AX 	   ;Restore ES
	  jmp	  DX		   ;Return
toblock   endp

IFNDEF PROMEM
;     Give characters from a C string
;	Calling sequence: gvchars(str,len)
;	      Where ---- str:  C string address
;			 len:  Number of characters to give
;Stack elements in order of popping:
;  Return address, string address, number of chars
	  extrn   givechar:near
	  public  gvchars
gvchars   proc	  near
	  pop	  DI		   ;Get return address
	  pop	  SI		   ;Get string address
	  pop	  CX		   ;Get number of chars
	  push	  DI		   ;Put return address back
	  jcxz	  given 	   ;If no chars, stop
	  cld			   ;Direction forward
gvlp:	  push	  CX		   ;Save count
	  lodsb 		   ;Fetch string character
	  push	  SI		   ;Save pointer to next char
	  push	  AX
	  call	  givechar	   ;Give it
	  inc	  SP		   ;Restore stack
	  inc	  SP
	  pop	  SI		   ;Restore address and count
	  pop	  CX
	  loop	  gvlp		   ;Give 'til done
given:	  ret			   ;Return
gvchars   endp

; Move characters from block (symbol or string) to print buffer
;     Calling sequence: blk2pbuf(pg,ds,buf,len,ch,display)
;	 Where ---- pg:      logical page of the block
;		    ds:      block displacement
;		    buf:     address of print buffer
;		    len:     number of chars in the block
;		    ch:      character to escape (| for syms, " for strs)
;		    display: whether to use escape characters
; Returns the number 2n+s, where n is the number of characters in the
;   print buffer, and s=1 if strange chars were encountered, 0 otherwise.
; Popping order:  return address, pg, ds, buf, len, ch, display
	  public  blk2pbuf
	  extrn   hicases:byte
blk2pbuf  proc	  near
	  pop	  DX		   ;Pop return address
	  pop	  BX		   ;Pop page
	  shl	  BX,1		   ;Put segment of block in DS
	  LoadPage DS,BX
;;;	  mov	  DS,pagetabl+[BX]
	  pop	  SI		   ;Pop block displacement
	  pop	  DI		   ;Pop print buffer
	  pop	  CX		   ;Pop character count
	  pop	  BX		   ;Pop must-be-escaped character
	  pop	  AX		   ;Pop whether to use escapes
	  mov	  BH,AL 	   ;Save escape boolean in BX
	  and	  BH,7fh	   ;Save bit in BH for strangeness
	  push	  DX		   ;Push return address
	  push	  ES		   ; Save caller's ES register
	  mov	  DX,DI 	   ;Save start address of print buffer in DX
	  jcxz	  zstrng	   ;If len=0, mark strangeness
	  cmp	  BL,'"'           ;Are we looking at a string?
	  jne	  b2plp 	   ;Skip if not
zstrng:   or	  BH,80h	   ;Otherwise, mark as strange
	  jcxz	  done		   ;If len=0, forget everything else
b2plp:	  lodsb 		   ;Fetch char from block
	  test	  BH,7fh	   ;Are we displaying escape chars?
	  jz	  storit	   ;Jump if not
	  cmp	  AL,BL 	   ;Does the char need escaping?
	  je	  escit 	   ;If needed, do so
	  cmp	  AL,'\'
	  jne	  storit	   ;If not, just store it
escit:	  mov	  AH,AL 	   ;Save char in AH
	  mov	  AL,'\'           ;Store escape character
	  stosb
	  mov	  AL,AH 	   ;Restore char
storit:   stosb 		   ;Store it
	  test	  BH,80h	   ;Do we already know that atom's strange?
	  jnz	  skptest	   ;If so, don't bother testing
	  push	  SI		   ;Else save SI
	  mov	  SI,offset DGROUP:hicases  ;Point SI to table of upper cases
	  xchg	  BX,SI
	  mov	  AH,AL 	   ;Save char in AH
	  xlat	  ES:hicases	   ;Fetch upper-case equivalent
	  xchg	  BX,SI 	   ;Restore BX
	  cmp	  AH,AL
	  jne	  mrkstrng	   ;If chars different, mark as strange
	  mov	  SI,offset stranges  ;Point SI to strange-character string
strnglp:  lods	  byte ptr ES:[SI] ;Fetch strange char
	  or	  AL,AL 	   ;End of string?
	  jz	  notstrng	   ;Jump if so
	  cmp	  AH,AL 	   ;Is AH strange?
	  jne	  strnglp	   ;If not, try again
mrkstrng: or	  BH,80h	   ;Mark strange bit
notstrng: pop	  SI		   ;Restore SI
skptest:  loop	  b2plp 	   ;Repeat until done
done:	  push	  ES		   ;Restore DS
	  pop	  DS
	  pop	  ES		   ; Restore caller's ES register
	  mov	  byte ptr[DI],0   ;Put null at end of string
	  mov	  AX,DI 	   ;Return 2*(# of chars in string)+strangeness
	  sub	  AX,DX
	  shl	  BH,1
	  rcl	  AX,1
	  ret			   ;Return
blk2pbuf  endp
ENDIF

; Load bignum block with long integer
;     Calling sequence:  putlong(reg,longi)
;	Where ----- reg:   register pointing to a bignum block
;		    longi: 32-bit integer to store
; Popping order: return address, register address, low & high integer words
	  public  putlong
putlong   proc	  near
	  pop	  DX		   ;Fetch return address
	  pop	  DI		   ;Fetch register address
	  mov	  BX,[DI].C_page   ;Point ES:DI to bignum block
	  LoadPage ES,BX
;;;	  mov	  ES,pagetabl+[BX]
	  mov	  DI,[DI].C_disp
	  add	  DI,3		   ;Point ES:DI to block data area
	  pop	  BX		   ;Put long integer in CX:BX
	  pop	  CX
	  xor	  AL,AL 	   ;Sign byte - default positive
	  test	  CH,80h	   ;Integer negative?
	  jz	  poslong	   ;Jump if not
	  inc	  AL		   ;Otherwise, set sign negative
	  xor	  BX,-1 	   ;Negate long integer
	  xor	  CX,-1
	  add	  BX,1
	  adc	  CX,0
poslong:  cld			   ;Direction forward
	  stosb 		   ;Store sign byte
	  mov	  AX,BX 	   ;Store least significant word
	  stosw
	  jcxz	  notlong	   ;If most signif. word=0, don't store it
	  mov	  AX,CX
	  stosw
notlong:  push	  DS		   ;Restore ES
	  pop	  ES
	  jmp	  DX		   ;Return
putlong   endp


; Add word of zeros, if necessary, to bignum buffer
;     Calling sequence: thefix(buf)
;	Where ----- buf: address of bignum buffer
; THEFIX is intended to alleviate a problem in the bignum division package.
; Popping order: return address, buf
	  public  thefix
thefix	  proc	  near
	  pop	  DI		   ;Return address in DX
	  pop	  SI		   ;Fetch bignum buffer address
	  mov	  BX,[SI]	   ;Get bignum size in words
	  inc	  BX		   ;Point BX+SI to last bignum byte
	  shl	  BX,1
	  test	  byte ptr[BX+SI],80h  ;Is most signif. bit set?
	  jz	  fixed 	   ;If not, nothing to fix
	  inc	  word ptr[SI]	   ;Otherwise, increase bignum size
	  inc	  BX		   ;Add word of 0 to most significant end
	  mov	  word ptr[BX+SI],0
fixed:	  jmp	  DI		   ;Return
thefix	  endp


; Load a long integer value with a bignum
;     Calling sequence: ldlong(v, reg)
;	Where ----- v:	 pointer to a long integer
;		    reg: register pointing to a bignum
; Returns 0 if the load was successful, 1 otherwise
; Popping order: return address, v, reg
	  public  ldlong
ldlong	  proc	  near
	  pop	  DX		   ;Pop return address
	  pop	  DI		   ;Pop longint destination
	  pop	  BX		   ;Pop register address
	  push	  DS		   ;Save DS
	  mov	  SI,[BX].C_disp   ;Point DS:SI to bignum object
	  mov	  BX,[BX].C_page
	  LoadPage DS,BX
;;;	  mov	  DS,pagetabl+[BX]
	  cld			   ;Direction forward
	  inc	  SI		   ;Put bignum length in CX
	  lodsw
	  mov	  CX,AX
	  lodsb 		   ;Put bignum sign in BL
	  mov	  BL,AL
	  cmp	  CX,6		   ;Check size
	  je	  big6
	  cmp	  CX,8
	  je	  big8
	  mov	  AX,1		   ;If here, bignum wrong size: error
	  pop	  DS		   ;Restore DS
	  jmp	  DX		   ;Return
big6:	  lodsw 		   ;Put bignum in CX:AX
	  xor	  CX,CX
	  jmp	  short havenum
big8:	  lodsw 		   ;Put bignum in CX:AX
	  mov	  CX,AX
	  lodsw
	  xchg	  CX,AX
havenum:  test	  BL,1		   ;Was bignum negative?
	  jz	  storenum	   ;No, skip
	  xor	  CX,-1 	   ;Otherwise, negate
	  xor	  AX,-1
	  add	  AX,1
	  adc	  CX,0
storenum: stosw 		   ;Store to long integer
	  mov	  AX,CX
	  stosw
	  xor	  AX,AX 	   ;All's well
	  pop	  DS		   ;Restore DS
	  jmp	  DX		   ;Return
ldlong	  endp


; Move string bytes from one part of PCS memory to another
;     Calling sequence: msubstr(to_reg, from_reg, start, end)
;	Where ----- to_reg:   register pointing to destination string
;		    from_reg: register pointing to source string
;		    start:    offset at which to start copying
;		    end:      byte after the last to be copied
; Popping order: return address, from_reg, to_reg, start, end
	  public  msubstr
msubstr   proc	  near
	  pop	  DX		   ;Pop return address (temporarily)
	  pop	  DI		   ;Pop destination register address
	  pop	  SI		   ;Pop source register address
	  pop	  AX		   ;Pop start index
	  pop	  CX		   ;Pop end index
	  push	  DS		   ;Save caller's DS & ES
	  push	  ES
	  mov	  BX,[DI].C_page   ;Point ES:DI to destination object
	  mov	  DI,[DI].C_disp
	  LoadPage ES,BX
;;;	  mov	  ES,pagetabl+[BX]
	  add	  DI,BLK_OVHD	   ;Adjust DI past string overhead
	  mov	  BX,[SI].C_page   ;Point DS:SI to source object
	  mov	  SI,[SI].C_disp
	  LoadPage DS,BX
;;;	  mov	  DS,pagetabl+[BX]
	  add	  SI,BLK_OVHD	   ;Adjust SI past string overhead
	  add	  SI,AX 	   ;Point SI to start of substring
	  sub	  CX,AX 	   ;Set number of bytes to move
	  cld			   ;Direction forward
	  rep	  movsb
	  pop	  ES		   ;Restore caller's DS & ES
	  pop	  DS
	  jmp	  DX		   ;Return
msubstr   endp

; Compare two Scheme bignums or strings for equal?-ness
;     Calling sequence: mcmpstr(reg1,reg2)
;	Where ----- reg1,reg2: registers pointing to objects to be compared
; Returns 1 if the objects are equal?, 0 otherwise
	  public  mcmpstr
mcmpstr   proc	  near
	  pop	  DX		   ;Pop return address
	  pop	  SI		   ;Pop register addresses
	  pop	  DI
	  push	  DS		   ;Save caller's DS and ES
	  push	  ES
	  mov	  BX,[DI].C_page   ;Point ES:DI to second object
	  mov	  DI,[DI].C_disp
	  LoadPage ES,BX
;;;	  mov	  ES,pagetabl+[BX]
	  mov	  BX,[SI].C_page   ;Point DS:SI to the first object
	  mov	  SI,[SI].C_disp
	  LoadPage DS,BX
;;;	  mov	  DS,pagetabl+[BX]
	  mov	  CX,[SI].str_len  ;Fetch byte count from source's length
	  cmp	  CX,0		 ;;; check for small string
	  jge	  mcm_010
	  add	  CX,BLK_OVHD+PTRSIZE
mcm_010:  xor	  AX,AX 	   ;Default AX to FALSE
	  cld			   ;Direction forward
	  repe	  cmpsb 	   ;Compare
	  jne	  cmpskp	   ;If not equal, return FALSE
	  inc	  AX		   ;Otherwise return TRUE
cmpskp:   pop	  ES		   ;Restore caller's ES and DS
	  pop	  DS
	  jmp	  DX		   ;Return
mcmpstr   endp


; Load a register with a pointer from Scheme memory
;     Calling sequence: ldreg(reg,pg,ds)
;	Where ----- reg:   register to be loaded
;		    pg,ds: page and displacement of Scheme pointer
; Popping order: return address, reg, pg, ds
	  public  ldreg
ldreg	  proc	  near
	  pop	  DX		   ;Pop return address
	  pop	  DI		   ;Pop destination register
	  pop	  BX		   ;Pop page and displacement
	  pop	  SI
	  mov	  CX,DS 	   ;Save caller's DS
	  shl	  BX,1		   ;Point DS:SI to Scheme pointer
	  LoadPage DS,BX
;;;	  mov	  DS,pagetabl+[BX]
	  cld			   ;Direction forward
	  lodsb 		   ;Load register's page field
	  xor	  AH,AH
	  mov	  ES:[DI].C_page,AX
	  lodsw 		   ;Load displacement field
	  mov	  ES:[DI].C_disp,AX
	  mov	  DS,CX 	   ;Restore caller's DS
	  jmp	  DX		   ;Return
ldreg	  endp


; Generate pseudorandom numbers in the range 0-16,383
;
; Author:  John C. Jensen (converted to assembly lang. by Mark Meyer)
; Date Written:  9 January 1985
; Last Modification:  9 July 1985
;
; Calling Sequence:  krandom()
;
; Note:  the following random number generator is due to Jaroslav
;      Kral.  It was adapted to 16 bit words and proven both efficient
;      and statistically satisfactory by Overstreet and Nance of SMU.
;      See Karl's paper for initialization values for other word
;      lengths.
;
;      -- Kral, Jaroslav.  "A New Additive Pseudorandom Number
;	  Generator for Extremely Short Word-Lengths," Information
;	  Processing Letters, 1 (1972), 164-167 (erratum noted in 1
;	  (1972), 216).
;
;      -- Overstreet, C. and Nance, R.E., "A Random Number Generator
;	  for Small Word-Length Computers," Proceedings of the ACM '73
;	  Conference, p. 219-223.
;
	  public  krandom
krandom   proc	  near
	  mov	  AX,krala	   ;Put old KRALA in AX, old KRALB in BX
	  mov	  BX,kralb
	  mov	  CX,BX 	   ;KRALC = KRALB
	  add	  BX,AX 	   ;KRALB = (KRALA+KRALB) mod 2^n
	  and	  BH,3fh	   ;  (Currently, n=14)
	  mov	  kralb,BX
	  mov	  BL,BH 	   ;J = KRALB / 2^(n-4)
	  shr	  BL,1
	  and	  BX,01eh
	  mov	  AX,[BX]+offset kraltbl  ;KRALA = KRALTBL[J]
	  mov	  krala,AX
	  add	  AX,CX 	   ;KRALTBL[J] = (KRALA+KRALC) mod 2^n
	  and	  AH,3fh
	  mov	  [BX]+offset kraltbl,AX
	  ret			   ;Return KRALTBL[J]
krandom   endp

; RANDOMIZE - Reset the random number registers and table back to their
;	      original values, then put the seed value into "kralb".
; Calling sequence:  randomize(seed)	;seed = normal C int
	  public  randomiz
randz_args struc
	  dw	  ?		   ;caller's ES
	  dw	  ?		   ;caller's BP
	  dw	  ?		   ;return address
rseed	  dw	  ?		   ;argument 1 (seed)
randz_args ends
randomiz  proc	  near
	  push	  BP		   ;save caller's BP
	  push	  ES		   ;save ES
	  mov	  BP,SP 	   ;establish local addressability
	  mov	  AX,DS 	   ;copy DS to ES
	  mov	  ES,AX
	  mov	  CX,kral_len/2    ;restore random state to its original state
	  lea	  SI,krala1
	  lea	  DI,krala
      rep movsw
	  mov	  BX,[BP].rseed    ;get seed
	  cmp	  BX,0		   ;is it zero?
	  jnz	  randz_1	   ;no, jump; use the seed directly
	  mov	  AX,2C00h	   ;get the time from DOS
	  int	  21h
	  push	  DX		   ;tempsave DX (seconds, hundredths)
	  xor	  AX,AX
	  mov	  AL,CH 	   ;determine #sec-in-hours
	  mov	  DX,3600
	  mul	  DX
	  mov	  BX,AX
	  xor	  AX,AX
	  mov	  AL,CL 	   ;determine #sec-in-minutes
	  mov	  DX,60
	  mul	  DX
	  add	  BX,AX 	   ;#sec-in-hours + #sec-in-minutes
	  pop	  DX		   ;restore seconds (and hundredths, but ignore it)
	  xchg	  DH,DL
	  mov	  DH,0
	  add	  BX,DX 	   ;add in seconds
randz_1:  mov	  kralb,BX	   ;set seed
	  pop	  ES		   ;wrap up
	  pop	  BP
	  ret
randomiz  endp


; Set the cdr field of a list cell
;     Calling sequence:  asetcdr(creg, preg)
;	Where ----  creg: register pointing to cell
;		    preg: register holding new pointer
; Popping order: Return address, destination register, pointer register
	  public  asetcdr
asetcdr   proc	  near
	  pop	  DX		   ;Pop return address
	  pop	  DI		   ;Pop address of register
	  mov	  CX,ES 	   ;Save caller's ES
	  mov	  BX,[DI].C_page   ;Point ES:DI to list cell
	  mov	  DI,[DI].C_disp
	  LoadPage ES,BX
;;;	  mov	  ES,pagetabl+[BX]
	  add	  DI,PTRSIZE	   ;Adjust for cdr field
	  pop	  SI		   ;Pop address of pointer
	  cld			   ;Direction forward
	  mov	  AX,[SI].C_page   ;Store into cdr field
	  stosb
	  mov	  AX,[SI].C_disp
	  stosw
	  mov	  ES,CX 	   ;Restore ES
	  jmp	  DX		   ;Return
asetcdr   endp


;  Get field values from a port object
;     Calling sequence: pt_flds4(reg, &ull, &ulc, &nl, &nc)
;			pt_flds6(reg, &cl, &cc, &ull, &ulc, &nl, &nc)
;	Where ----- reg: register pointing to port
;		    cl:  variable to receive CUR_LINE value
;		    cc:  ... CUR_COL value
;		    ull: ... UL_LINE value
;		    ulc: ... UL_COL value
;		    nl:  ... N_LINES value
;		    nc:  ... N_COLS value
;  Warning: This routine expects these six fields to be contiguous
;  Popping order: return address, reg, (&cl, &cc,) &ull, &ulc, &nl, &nc
	  public  pt_flds4,pt_flds6
pt_flds   proc	  near
pt_flds6: mov	  CX,pt_cline	   ;Set CX to offset of first field
	  jmp	  fldsmrg
pt_flds4: mov	  CX,pt_ullin	   ;Set CX to offset of first field
fldsmrg:  pop	  DX		   ;Pop return address
	  mov	  AX,DS 	   ;Save caller's DS
	  pop	  BX		   ;Pop register address
	  mov	  SI,[BX].C_disp   ;Point DS:SI to first field
	  mov	  BX,[BX].C_page
	  LoadPage DS,BX
;;;	  mov	  DS,pagetabl+[BX]
	  add	  SI,CX
	  cld			   ;Direction forward
	  sub	  CX,pt_cline	   ;Set CX to number of fields to do
	  shr	  CX,1		   ;  (6 - (1/2)(CX - pt_cline))
	  neg	  CX
	  add	  CX,6
fldslp:   pop	  DI		   ;Pop destination variable address
	  movsw 		   ;Transfer value
	  loop	  fldslp	   ;Repeat until done
	  mov	  DS,AX 	   ;Restore DS
	  jmp	  DX		   ;Return
pt_flds   endp

; Copy bytes from one C location to another
;     Calling sequence: str2str(dest_adr, src_adr, n)
;	Where ----- dest_adr: destination address
;		    src_adr:  source address
;		    n:	      number of bytes to copy
; Popping order: return address, dest_adr, src_adr, n
	  public  str2str
str2str   proc	  near
	  pop	  DX		   ;Pop return address
	  pop	  DI
	  pop	  SI
	  pop	  CX
	  cld			   ;Direction forward
	  rep	  movsb 	   ;Copy bytes
	  jmp	  DX		   ;Return
str2str   endp

; Adjust window region variables for presence of a border
;     Calling sequence: adj4bord(&ull, &nl, &ulc, &nc)
;	Where ----- ull: Upper-left-line variable
;		    nl:  Number-of-lines variable
;		    ulc: Upper-left-column variable
;		    nc:  Number-of-columns variable
; Popping order: return address, &ull, &nl, &ulc, &nc
	  public  adj4bord
max_lines equ	  25
max_cols  equ	  80
adj4bord  proc	  near
	  pop	  DX		   ;Pop return address
	  mov	  BX,max_lines	   ;Expand HEIGHT of window region
expand:   pop	  SI		   ;Pop upper-left parameter
	  pop	  DI		   ;Pop extent parameter
	  mov	  AX,[SI]	   ;Get value of upper-left parm
	  or	  AX,AX 	   ;If zero,
	  jz	  expand1	   ;  skip next two instructions
	  dec	  word ptr[SI]	   ;Else, expand backward
	  inc	  word ptr[DI]
	  dec	  AX		   ;Adjust AX to match upper-left parm
expand1:  add	  AX,[DI]	   ;Find opposite edge
	  cmp	  AX,BX 	   ;If edge too far,
	  jae	  expand2	   ;  skip next instruction
	  inc	  word ptr[DI]	   ;Else, expand forward
expand2:  cmp	  BX,max_cols	   ;If we're finished,
	  je	  adjex 	   ;  jump out
	  mov	  BX,max_cols	   ;Else, expand WIDTH of window region
	  jmp	  expand
adjex:	  jmp	  DX		   ;Return
adj4bord  endp

prog	  ends
	  end
