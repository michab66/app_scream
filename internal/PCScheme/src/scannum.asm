;							   =====> SCANNUM.ASM
;****************************************
;*    TIPC Scheme '84 Runtime Support   *
;*	    Numeric I/O Support 	*
;*					*
;*  (C) Copyright 1984,1985 by Texas	*
;*     Instruments Incorporated.	*
;*	  All rights reserved.		*
;*					*
;* Date Written:  12 June 1985		*
;* Last Modification:  22 July 1985	*
;****************************************

DGROUP	  group   data
data	  segment word public 'DATA'
	  assume  DS:DGROUP
	  public  decpoint
decpoint  db	  '.'
data	  ends

PGROUP	  group   prog
prog	  segment byte public 'PROG'
	  assume  CS:PGROUP

;     Classify numeric string ending with a control character
;	  Calling sequence: scannum(s,base)
;		  Where ---- s:    pointer to start of character string
;			     base: default base
;     This function returns 0 if not a number, -1 if a flonum, and n>0
;	  if an integer, where n is the number of digits in the integer.
;
; NOTE : DS is not guaranteed to point to the local data segment
;
scanargs  struc
	  dw	  ?		    ;Caller's BP
	  dw	  ?		    ;Return address
sptr	  dw	  ?		    ;Pointer to string
scanbase  dw	  ?		    ;Default base
scanargs  ends
	  public  scannum
scannum   proc	  near
	  push	  BP
	  mov	  BP,SP
	  cld			    ;Direction forward
	  mov	  SI,[BP].sptr	    ;Point DS:SI to characters
	  mov	  BX,[BP].scanbase  ;Set default base
	  xor	  CX,CX 	    ;Initialize digit count
baselp:   lodsb 		    ;Fetch first char
	  cmp	  AL,'#'            ;Skip over the base macros
	  jne	  nomac
	  lodsb 		    ;Get base argument
	  sub	  AL,40h
	  js	  nonnum	    ;If not a base designator, not a number
	  and	  AL,0dfh	    ;Shift to upper case
	  xor	  BL,BL 	    ;Zero current base
	  cmp	  AL,5		    ;Check for #E,#I,#L,#S macros
	  je	  baselp	    ; (legal, but don't affect base)
	  cmp	  AL,9
	  je	  baselp
	  cmp	  AL,12
	  je	  baselp
	  cmp	  AL,19
	  je	  baselp
	  cmp	  AL,2		    ;Jump if binary (#B)
	  je	  bbin
	  cmp	  AL,4		    ;Jump if decimal (#D)
	  je	  bdec
	  cmp	  AL,15 	    ;Jump if octal (#O)
	  je	  boct
	  cmp	  AL,24 	    ;Jump if hexadecimal (#X)
	  je	  bhex
	  cmp	  AL,8		    ;Jump if not #H (the only legal one left)
	  jne	  nonnum
bhex:	  mov	  BL,6
bdec:	  add	  BL,2
boct:	  add	  BL,6
bbin:	  add	  BL,2
	  jmp	  baselp	    ;Check for another switch
nomac:	  cmp	  AL,'+'            ;If +, note its presence
	  je	  wassign
	  cmp	  AL,'-'            ;If not -, skip next char fetch
	  jne	  notsign
wassign:  lodsb 		    ;Fetch next char
notsign:  cmp	  AL,ss:decpoint    ;Decimal point already?
	  je	  point1	    ;Jump if so... must be a flonum
	  call	  isdg		    ;Otherwise, there must be a digit
	  jnc	  nonnum	    ;If not, not a number
wholelp:  lodsb 		    ;Else get next character
	  call	  isdg
	  jc	  wholelp	    ;Keep reading digits in whole part
	  cmp	  AL,32 	    ;End of string?
	  jb	  intnum	    ;Yes, we have an integer
	  cmp	  AL,ss:decpoint    ;Jump on decimal point
	  je	  point
	  call	  ismarker	    ;Jump if exponent marker (E or L valid)
	  je	  expon
nonnum:   xor	  AX,AX 	    ;Return 0, forget all else
	  pop	  BP
	  ret
intnum:   mov	  AX,CX 	    ;Return digit count
	  pop	  BP
	  ret
point1:   lodsb 		    ;We must have digit here
	  call	  isdg
	  jnc	  nonnum
point:	  lodsb 		    ;Get characters up to non-digit
	  call	  isdg
	  jc	  point
	  cmp	  AL,32 	    ;If end of string, we have flonum
	  jb	  flonum
	  call	  ismarker	    ;Otherwise, check for exponent marker
	  je	  expon
	  jne	  nonnum
expon:	  mov	  BL,10 	    ;Exponents are in base 10
	  lodsb 		    ;Get next char
	  cmp	  AL,'-'            ;Valid exponent sign
	  jne	  edig		    ;Jump if not signed
	  lodsb 		    ;Else get next char
edig:	  call	  isdg		    ;We must end with a nonempty string
	  jnc	  nonnum	    ;  of base 10 digits
exponlp:  lodsb
	  call	  isdg
	  jc	  exponlp
	  cmp	  AL,32 	    ;If not end of string, nonnum
	  jae	  nonnum
flonum:   mov	  AX,-1 	    ;Return -1 (flonum code)
	  pop	  BP
	  ret
;ISDG: CF is set iff the char in AL is a digit in base BX
;  Also, if a digit, the digit count in CX is incremented
isdg:	  cmp	  AL,'0'            ;Not if below 0
	  jl	  nodig
	  cmp	  AL,'1'            ;0 or 1 anytime
	  jbe	  yesdig
	  cmp	  BL,2		    ;Nothing else for base 2
	  je	  nodig
	  cmp	  AL,'7'            ;2-7 for base 8,10,16
	  jbe	  yesdig
	  cmp	  BL,8		    ;Nothing else for base 8
	  je	  nodig
	  cmp	  AL,'9'            ;8 or 9 for bases 10 or 16
	  jbe	  yesdig
	  cmp	  BL,10 	    ;Nothing else for base 10
	  je	  nodig
	  and	  AL,0dfh	    ;Convert to upper case
	  cmp	  AL,'A'            ;Base 16... Check for A-F
	  jb	  nodig
	  cmp	  AL,'F'
	  jbe	  yesdig
nodig:	  clc
	  ret
yesdig:   inc	  CX		    ;Increment digit count
	  stc
	  ret
;ISMARKER: ZF is set iff the character in AL is an exponent marker
ismarker: cmp	  AL,'e'
	  je	  mark
	  cmp	  AL,'l'
	  je	  mark
	  cmp	  AL,'E'
	  je	  mark
	  cmp	  AL,'L'
	  je	  mark
mark:	  ret
scannum   endp

;     Check character for digit status in a given base
;	Calling sequence: isdig(c,base)
;		Where ---- c:	 character to check
;			   base: base in which to check
isdargs   struc
	  dw	  ?		    ;Caller's BP
	  dw	  ?		    ;Return address
charg	  dw	  ?		    ;Character
barg	  dw	  ?		    ;Base
isdargs   ends
	  public  isdig
isdig	  proc	  near
	  push	  BP
	  mov	  BP,SP
	  mov	  AL,byte ptr[BP].charg     ;Fetch character
	  mov	  BX,[BP].barg	    ;Fetch base
	  call	  isdg		    ;Determine digitness
	  jc	  wasdg 	    ;Was a digit...don't zero AX
	  xor	  AX,AX 	    ;Otherwise return 0
wasdg:	  pop	  BP
	  ret
isdig	  endp

;     Convert digit character to its value
;	Calling sequence: digval(c)
;		Where ---- c:  assumed to be a digit character
digargs   struc
	  dw	  ?		    ;Caller's BP
	  dw	  ?		    ;Return address
carg	  dw	  ?		    ;Character
digargs   ends
	  public  digval
digval	  proc	  near
	  push	  BP
	  mov	  BP,SP
	  mov	  AL,byte ptr[BP].carg	    ;Fetch character
	  xor	  AH,AH
	  and	  AL,01fh	    ;Reduce bits
	  cmp	  AL,16 	    ;Number or letter?
	  jb	  hexdig	    ;Jump if letter
	  and	  AL,0fh	    ;Zero the high nibble
	  pop	  BP
	  ret
hexdig:   add	  AL,9		    ;Raise the lower nibble
	  pop	  BP
	  ret
digval	  endp

;     Convert flonum in interval [1.0e15,1.0e16) to bignum
;	Calling sequence: flo2big(flo,buf)
;		Where ---- flo: flonum in interval [1e15,1e16)
;			   buf: bignum math buffer, minimum size 11 bytes
flo2args  struc
	  dw	  ?		    ;Caller's BP
	  dw	  ?		    ;Return address
num	  dw	  ?,?,?,?	    ;Flonum (4 words)
big	  dw	  ?		    ;Pointer to math buffer
flo2args  ends
	  public  flo2big
flo2big   proc	  near
	  push	  BP
	  mov	  BP,SP
	  mov	  DI,[BP].big	    ;Point DI to math buffer
	  cld			    ;Direction forward
	  mov	  AX,4		    ;Store bignum size (words) in buffer
	  stosw
	  mov	  AX,[BP+6].num     ;Fetch exponent
	  mov	  CX,AX 	    ;Save exponent in CX
	  rol	  AX,1		    ;Store sign in buffer
	  and	  AL,1
	  stosb
	  mov	  AX,CX 	    ;Restore exponent to AX
	  xor	  CH,CH 	    ;Put (433h-exponent) in CX
	  shr	  CL,1
	  shr	  CL,1
	  shr	  CL,1
	  shr	  CL,1
	  sub	  CL,3
	  neg	  CL
	  and	  AX,0fh	    ;Remove exponent from word in AX
	  or	  AL,10h
	  lea	  SI,[BP].num	    ;Point SI to flonum
	  movsw
	  movsw
	  movsw
	  stosw 		    ;Word that used to have exponent
	  sub	  DI,8		    ;Point DI back to start of bignum
	  cmp	  CL,-1 	    ;Branch if mantissa to be shifted left
	  je	  manleft
	  or	  CL,CL 	    ;Branch if not to be shifted right
	  jz	  shifted
manright: shr	  word ptr[DI+6],1  ;Shift bignum right
	  rcr	  word ptr[DI+4],1
	  rcr	  word ptr[DI+2],1
	  rcr	  word ptr[DI],1
	  loop	  manright	    ;Loop until done
	  jmp	  short shifted
manleft:  shl	  word ptr[DI],1    ;Shift bignum left
	  rcl	  word ptr[DI+2],1
	  rcl	  word ptr[DI+4],1
	  rcl	  word ptr[DI+6],1
shifted:  pop	  BP
	  ret
flo2big   endp

;     Form floating-point ASCII representation from 16 digits and scale
;	Calling sequence: formflo(digs,chars,scale,prec,exp)
;		Where ---- digs:  the digit characters of the flonum
;			   chars: buffer to store the formed flonum
;			   scale: flonum exponent part
;			   prec:  desired precision
;			   exp:   whether to use exponential format
;	Returns the length of the formed flonum string
formargs  struc
	  dw	  ?		    ;Caller's BP
	  dw	  ?		    ;Return address
digptr	  dw	  ?		    ;Pointer to digits
chrptr	  dw	  ?		    ;Pointer to result string
scale	  dw	  ?		    ;Exponent part
fprec	  dw	  ?		    ;Precision
fexp	  dw	  ?		    ;Exponential format specifier
formargs  ends
	  public  formflo
formflo   proc	  near
	  push	  BP
	  mov	  BP,SP
	  mov	  SI,[BP].digptr    ;Point SI to digit string
	  mov	  DI,[BP].chrptr    ;Point DI to destination
	  cld			    ;Direction forward
	  mov	  DX,[BP].fexp	    ;Fetch form specifier
	  mov	  AL,[SI]	    ;Fetch first digit
	  cmp	  AL,'0'
	  je	  toosmall	    ;Jump if zero
	  cmp	  AL,'-'            ;Negative sign?
	  jne	  nonsign	    ;Jump if not signed
signed:   stosb 		    ;Put sign in return buffer
	  inc	  [BP].digptr	    ;Adjust pointer to first digit
	  inc	  SI
nonsign:  mov	  BX,14 	    ;Round off the last digit
	  call	  round
	  mov	  BX,[BP].fprec     ;Fetch precision
	  or	  BX,BX
	  jz	  putspace	    ;Jump if arbitrary precision
;Determine location at which to begin rounding
	  cmp	  BX,14 	    ;If precision out of range, replace
	  jbe	  precok	    ;  with highest possible
	  mov	  BX,14
precok:   or	  DX,DX
	  jnz	  doround	    ;If exponential, round now
	  add	  BX,[BP].scale     ;Add scale to precision
	  jns	  notsmall	    ;Jump unless number rounds to 0
	  cmp	  BX,-1
	  jne	  toosmall	    ;Jump if num definitely rounds to 0
	  cmp	  byte ptr[SI],'5'  ;Check sigfig
	  jb	  toosmall	    ;Jump if too small
	  mov	  word ptr[SI],2031h   ;Else round up and adjust scale
	  inc	  [BP].scale
	  jmp	  short spaced
toosmall: mov	  AL,'0'            ;Put (prec+1) 0's at start of input
	  mov	  BX,[BP].fprec     ;  buffer
toosmlp:  mov	  [SI],AL
	  inc	  SI
	  dec	  BL
	  jns	  toosmlp
	  mov	  byte ptr[SI],' '  ;Follow by space
	  mov	  DI,[BP].chrptr    ;Start output over (wipe out any sign)
	  jmp	  short spaced
notsmall: cmp	  BX,16
	  jae	  spaced	    ;Jump if no sense in rounding
doround:  call	  round 	    ;Round the digits
	  jmp	  short spaced
;For arbitrary precision, change all trailing zeros to spaces
;  (there exists at least one nonzero digit)
putspace: add	  SI,14 	    ;Point SI to last digit
spacelp:  cmp	  byte ptr[SI],'0'
	  jne	  spaced
	  and	  byte ptr[SI],0efh
	  dec	  SI
	  jmp	  spacelp
;Now the spaces are in - start formatting
spaced:   mov	  SI,[BP].digptr    ;Point SI to digit string
	  mov	  BX,[BP].scale     ;Fetch scale
	  mov	  CX,[BP].fprec     ;Fetch precision
	  or	  DX,DX 	    ;If exponent form desired
	  jnz	  exform	    ;  supply it
	  cmp	  BX,-14	    ;If scale>-15, check precision
	  jge	  midscale
	  or	  CL,CL 	    ;If precision arbitrary, force expo-form
	  jz	  exform
midscale: cmp	  BX,0
	  jl	  smallfix	    ;Branch if explicit form called for
	  cmp	  BX,14
	  jle	  largefix	    ;Branch if explicit, but >1
;Form an exponential-format flonum
exform:   movsb 		    ;Transfer first digit
	  mov	  AL,decpoint	    ;Place decimal point
placex:   stosb 		    ;Store character
	  lodsb 		    ;Transfer digits up to first space
	  cmp	  AL,' '
	  jne	  placex
	  mov	  AL,'e'            ;Place exponent marker
	  stosb
	  cmp	  BH,0		    ;If scale negative, negate & store sign
	  jge	  posscale
	  neg	  BX
	  mov	  AL,'-'
	  stosb
posscale: mov	  AX,BX 	    ;Move scale to AX
	  mov	  BH,10 	    ;Put divisor in BH
	  mov	  DX,SP 	    ;Save current stack pointer
divlpf:   div	  BH		    ;Divide
	  mov	  BL,AH 	    ;Push digit
	  add	  BL,'0'
	  push	  BX
	  xor	  AH,AH 	    ;Remove the remainder
	  or	  AL,AL 	    ;Loop until the quotient is zero
	  jnz	  divlpf
storelp:  pop	  AX		    ;Restore exponent digit
	  stosb 		    ;Place it
	  cmp	  SP,DX 	    ;Loop until no more digits left
	  jne	  storelp
	  jmp	  short retlen
;Form a fixed-decimal flonum magnitude greater than 1
largefix: lodsb 		    ;Fetch digit
	  or	  AL,10h	    ;Turn ' ' to '0'
	  stosb 		    ;Store digit
	  dec	  BL		    ;Loop until all pre-point digs done
	  jns	  largefix
	  mov	  AL,decpoint	    ;Place decimal point
	  stosb
digmrg:   or	  CL,CL
	  jnz	  preclp	    ;Jump if precision set
arblp:	  lodsb 		    ;Otherwise, arbitrary; do until space
	  cmp	  AL,' '
	  je	  retlen
	  stosb
	  jmp	  arblp
llp:	  stosb
preclp:   dec	  CL		    ;Last digit done?
	  js	  retlen	    ;Jump if so
dodigs:   lodsb 		    ;Now do digits until precision reached
	  cmp	  AL,' '            ;Space?
	  jne	  llp		    ;If not, store it
	  dec	  SI		    ;Restore SI
	  mov	  AL,'0'            ;Prepare to place 0
	  jmp	  llp
;Form a fixed-decimal flonum magnitude less than 1
smallfix: mov	  CH,CL 	    ;Copy precision to CH
	  mov	  AL,'0'            ;Place "0."
	  stosb
	  mov	  AL,decpoint
slp:	  stosb
	  inc	  BX
	  jz	  digmrg	    ;If 0's done, do significant figures
	  or	  CH,CH 	    ;If precision was zero
	  jz	  skpprec	    ;  don't bother checking it
	  dec	  CL
	  js	  retlen	    ;If the precision is reached, stop
skpprec:  mov	  AL,'0'            ;Otherwise, place 0's until scale=0
	  jmp	  slp
;Formation complete
retlen:   mov	  AX,DI 	    ;Return length of string
	  sub	  AX,[BP].chrptr
	  pop	  BP
	  ret
;ROUND: Round the ASCII digits of a flonum, starting at [BX+SI]
;  SI->start of digits and is unchanged; BX destroyed
round:	  mov	  AL,' '            ;Get digit after least-rounded and
	  xchg	  AL,[BX+SI+1]	    ;  replace it with a space
	  cmp	  AL,'5'
	  jb	  rounded	    ;Jump if rounded down
roundlp:  mov	  AL,[BX+SI]	    ;Otherwise, increment digit
	  inc	  AL
	  mov	  [BX+SI],AL	    ;Replace incremented digit
	  cmp	  AL,'9'
	  jbe	  rounded	    ;Jump if no carryover
	  mov	  byte ptr[BX+SI],'0'   ;Else replace digit
	  dec	  BX		    ;Go to next digit
	  jns	  roundlp
	  mov	  byte ptr[BX+SI+1],'1'  ;There are no more digits, place
	  inc	  [BP].scale	    ;  leading 1 and adjust scale
rounded:  ret
formflo   endp

prog	  ends
end
