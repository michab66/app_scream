;							=====> SBIGMATH.ASM
;***************************************
;*   TIPC Scheme '84 Runtime Support   *
;*	 Bignum Math Utilities	       *
;*				       *
;*  (C) Copyright 1984,1985 by Texas   *
;*     Instruments Incorporated.       *
;*	  All rights reserved.	       *
;*				       *
;* Date Written:  June 1984	       *
;* Last Modification:  27 May 1986     *
;***************************************
	  include scheme.equ

DGROUP	  group   data
data	  segment word public 'DATA'
	  assume  DS:DGROUP
data	  ends

XGROUP	  GROUP   PROGX
PROGX	  segment byte public 'PROGX'
	  assume  CS:XGROUP,DS:DGROUP

;     Convert a bignum to a flonum
;	Calling sequence: big2flo(bigptr,floptr)
;		Where: bigptr ---- pointer to bignum workspace
;		       floptr ---- pointer to flonum
b2fargs   struc
	  dw	  ?		   ;Caller's BP
	  dd	  ?		   ;Return address
	  dw	  ?		   ;Another return address
big	  dw	  ?		   ;Pointer to bignum
flo	  dw	  ?		   ;Pointer to flonum
b2fargs   ends

%big2flo  proc	  far
	  push	  BP
	  mov	  BP,SP
	  cld			   ;Direction forward
	  mov	  SI,[BP].big	   ;Point DS:SI to working bignum
	  mov	  CX,[SI]	   ;Get size
	  cmp	  CX,3
	  ja	  all64 	   ;Jump if at least 64 bits
	  add	  SI,3		   ;Point to bignum proper
	  xor	  BX,BX
	  xor	  DI,DI
	  lodsw 		   ;Fetch least sig. word
	  mov	  DX,AX 	   ;Store in DX
	  dec	  CX
	  jcxz	  smljust	   ;Jump if no more bignum words
	  lodsw 		   ;Else get next least sig. word
	  mov	  DI,AX
	  dec	  CX
	  jcxz	  smljust	   ;Jump if no more bignum words
	  lodsw 		   ;Get 3rd least sig. word
	  mov	  BX,AX
smljust:  xor	  AX,AX 	   ;Clear most sig. word
	  jmp	  short justify    ;Left-justify the number
all64:	  shl	  CX,1		   ;Point SI to 4th most sig. word
	  add	  SI,CX
	  sub	  SI,5
	  lodsw 		   ;Load bignum into registers
	  mov	  DX,AX
	  lodsw
	  mov	  DI,AX
	  lodsw
	  mov	  BX,AX
	  lodsw
;JUSTIFY: At this stage, the 64 most significant bignum bits are in
;   AX:BX:DI:DX respectively, AX most significant
justify:  mov	  SI,[BP].big	   ;Fetch pointer to bignum again
	  mov	  CX,[SI]	   ;Get size (words)
	  cmp	  CX,40h
	  ja	  toobig	   ;Jump if bignum too big
	  cmp	  CX,4		   ;Skip if not a small bignum
	  jae	  enough
	  mov	  CX,4		   ;Otherwise, start with constant
enough:   shl	  CX,1		   ;Multiply by 16 (size in bits)
	  shl	  CX,1
	  shl	  CX,1
	  shl	  CX,1
justflp:  dec	  CX		   ;Reduce exponent
	  shl	  DX,1		   ;Shift bignum left
	  rcl	  DI,1
	  rcl	  BX,1
	  rcl	  AX,1
	  jnc	  justflp	   ;Until most significant 1 vanishes
	  add	  CX,3ffh	   ;Add flonum exponent constant
	  mov	  SI,DI 	   ;Now use SI for num, DI for address
shftrt:   shr	  CX,1		   ;Shift CX:AX:BX:SI:DX as one
	  rcr	  AX,1
	  rcr	  BX,1
	  rcr	  SI,1
	  rcr	  DX,1
	  cmp	  CX,80h	   ;Until most sig. exponent bit is in 2nd
	  jae	  shftrt	   ;  most sig. bit of CL
	  mov	  DI,[BP].big	   ;Get pointer to bignum again
	  test	  byte ptr[DI]+2,1  ;Negative?
	  jz	  posskip	   ;No, skip
	  or	  CL,80h	   ;Set sign bit
posskip:  mov	  DI,[BP].flo	   ;Point ES:DI to flonum
	  push	  AX		   ;Save part of new flonum
	  mov	  AL,DH 	   ;Write to flonum space
	  stosb
	  mov	  AX,SI
	  stosw
	  mov	  AX,BX
	  stosw
	  pop	  AX
	  stosw
	  mov	  AL,CL
	  stosb
	  xor	  AX,AX 	   ;Return 0 if all well
	  pop	  BP		   ;Restore BP
	  ret
toobig:   mov	  AX,1		   ;Return 1 if conversion impossible
	  pop	  BP
	  ret
%big2flo  endp

;     Convert fixnum to bignum
;	Calling sequence: fix2big(fixnum,bigptr)
;		Where: fixnum ---- Integer of small absolute value
;		       bigptr ---- Pointer to bignum space
f2bargs   struc
	  dw	  ?		   ;Caller's BP
	  dd	  ?		   ;Return address
	  dw	  ?		   ;Another return address
fix	  dw	  ?		   ;Fixnum
bigp	  dw	  ?		   ;Pointer to bignum
f2bargs   ends

%fix2big  proc	  far
	  push	  BP
	  mov	  BP,SP
	  mov	  DI,[BP].bigp	   ;Point ES:DI to bignum
	  mov	  AX,1		   ;Fill size field
	  stosw
	  xor	  AL,AL 	   ;Clear AL
	  mov	  BX,[BP].fix	   ;Fetch fixnum value
	  shl	  BX,1		   ;Put sign bit in AL
	  rcl	  AL,1
	  stosb 		   ;Fill sign field
	  sar	  BX,1		   ;Restore fixnum
	  or	  AL,AL 	   ;Negative fixnum?
	  jz	  posfx 	   ;Skip if positive
	  neg	  BX		   ;Otherwise, find absolute value
posfx:	  mov	  [DI],BX	   ;Store magnitude of fixnum
	  pop	  BP		   ;Restore BP
	  ret
%fix2big  endp

;;;;	 Decrement bignum
;;;;	Calling sequence: sub1big(buf)
;;;;		Where: buf ---- pointer to bignum
;;;unibig	  struc
;;;	  dw	  ?		   ;Return address
;;;bbuf   dw	  ?		   ;Pointer to working bignum
;;;unibig	  ends
;;;	  public  sub1big
;;;sub1big	  proc	  far
;;;	  mov	  BX,SP 	   ;Get bignum pointer
;;;	  mov	  SI,SS:[BX].bbuf
;;;	  test	  byte ptr[SI]+2,1  ;Is bignum negative?
;;;	  jnz	  incbig	   ;If so, increase magnitude
;;;	  jmp	  short decbig	   ;Else decrease magnitude
;;;sub1big	  endp

;;;;	 Increment bignum
;;;;	Calling sequence: add1big(buf)
;;;	  public  add1big
;;;add1big	  proc	  far
;;;	  mov	  BX,SP 	   ;Get bignum pointer
;;;	  mov	  SI,SS:[BX].bbuf
;;;	  test	  byte ptr[SI]+2,1  ;Is bignum negative?
;;;	  jnz	  decbig	   ;Yes, decrease magnitude
;;;;INCBIG increments magnitude of working bignum at DS:SI
;;;incbig:	  mov	  DI,SI 	   ;Save bignum pointer
;;;	  mov	  CX,[SI]	   ;Get length (words)
;;;	  add	  SI,3		   ;Point to bignum proper
;;;carrylp:  inc	  word ptr[SI]	   ;Increment bignum
;;;	  jnz	  done		   ;If no carry, finished
;;;	  inc	  SI		   ;Else go to next word
;;;	  inc	  SI
;;;	  loop	  carrylp	   ;Loop while not end of bignum
;;;	  mov	  word ptr[SI],1   ;Else place final 1
;;;	  inc	  word ptr[DI]	   ;Lengthen bignum
;;;done:	  ret
;;;;DECBIG decrements magnitude of working bignum at DS:SI
;;;decbig:	  mov	  DI,SI 	   ;Save pointer
;;;	  mov	  CX,[SI]	   ;Get length
;;;	  add	  SI,3		   ;Point to bignum proper
;;;	  dec	  CX		   ;CX = (length - 1)
;;;borrowlp: lodsw			   ;Load current word
;;;	  sub	  AX,1		   ;Decrement and store
;;;	  mov	  [SI-2],AX
;;;	  jnc	  done		   ;Jump if no borrow
;;;	  loop	  borrowlp	   ;Loop if not on last word
;;;	  dec	  word ptr[SI]	   ;Else decrement last word
;;;	  jnz	  done		   ;Jump if bignum not to be shortened
;;;	  dec	  word ptr[DI]	   ;Else shorten
;;;	  ret
;;;add1big	  endp

; set up big1's index for comparison, used with %magcomp
%pbig1	 proc near
	 shl	CX,1
	 dec	SI
	 add	SI,CX
	 shr	CX,1
	 ret
%pbig1	 endp

; set up big1's index for comparison, used with %magcomp
%pbig2	 proc near
	 shl	CX,1
	 dec	DI
	 add	DI,CX
	 shr	CX,1
	 ret
%pbig2	 endp

;     Compare magnitudes of two bignums
;	Calling sequence: data = magcomp(big1,big2)
;		Where: big1,big2 -- pointers to bignum buffers
;		       data ------- a positive integer as follows:
;			 Bit 0 set iff |BIG1| < |BIG2|
;			 Bit 1 set iff |BIG1| > |BIG2|
;			 Bit 2 set iff BIG1 < BIG2
;			 Bit 3 set iff BIG1 > BIG2
;			 Bit 4 set iff BIG1,BIG2 have same sign
twobigs   struc
	  dd	  ?		   ;Return address
	  dw	  ?		   ;Another return address
big1	  dw	  ?		   ;First bignum
big2	  dw	  ?		   ;Second bignum
twobigs   ends

%magcomp  proc	  far
	  xor	  AL,AL 	   ;Clear AL
	  xor	  DX,DX 	   ; clear DX
	  mov	  BX,SP 	   ;Fetch bignum pointers
	  mov	  SI,[BX].big1
	  mov	  DI,[BX].big2
	  mov	  AH,[SI]+2	   ;Get sign bits
	  mov	  DH,[DI]+2
	  xor	  DH,AH 	   ;Put XOR of signs into carry
	  shr	  DH,1
	  jc	  sgnskp	   ;Jump if different signs
	  or	  AL,16 	   ;Else set proper bit in AL
sgnskp:   rcl	  AH,1
	  mov	  CX,[SI]	   ;Get BIG1's length
	  mov	  DX,[DI]	   ; get BIG2's length
	  cld			   ;Direction forward
	  cmpsw 		   ;Compare lengths
	  jb	  bigr2 	   ;Jump if BIG2 longer
	  ja	  bigr1 	   ;Jump if BIG1 longer
same_ln:  call	  %pbig1	   ;If same size, point SI,DI to last words
	  call	  %pbig2	   ;  (most significant)
	  std			   ;Direction backward
	  repe	  cmpsw 	   ;Repeat until unequal
	  jb	  rbig2
	  ja	  rbig1
	  test	  AH,1		   ;Signs same?
	  jz	  compend	   ;Yes, exit
difsign:  test	  AH,2		   ;Is BIG1 positive?
	  jnz	  grtr2 	   ;No, BIG2 is greater
	  jz	  grtr1 	   ;Else BIG1 is greater
bigr1:	  call	  %pbig1
	  cmp	  word ptr [SI],0  ; check high word,
	  jne	  rbig1 	   ; big1 is really bigger
	  mov	  SI,[BX].big1	   ; restore SI
	  inc	  SI
	  inc	  SI
	  dec	  CX		   ; high order word is empty
	  cmp	  CX,DX 	   ; compare length's again
	  je	  same_ln	   ; same length
	  jmp	  bigr1 	   ; repeat until unequal or same lengths

rbig1:	  or	  AL,2		   ;Set the |BIG1|>|BIG2| bit
	  test	  AH,1		   ;Signs same?
	  jnz	  difsign	   ;No, different signs
	  test	  AH,2		   ;Both positive?
	  jnz	  grtr2 	   ;No, so BIG2 is greater
grtr1:	  or	  AL,8		   ;Set the BIG1>BIG2 bit
	  cld			   ; Set direction forward (JCJ-12/6/84)
	  ret
bigr2:	  push	  CX
	  mov	  CX,DX 	   ; swap CX and DX
	  pop	  DX
	  call	  %pbig2	   ; Set up big2's pointers
	  cmp	  word ptr [DI],0  ; check high word
	  jne	  rbig2 	   ; big2 really is bigger
	  mov	  DI,[BX].big2	   ; restore DI
	  inc	  DI
	  inc	  DI
	  dec	  CX		   ; high order word is empty
	  cmp	  DX,CX 	   ; compare length's again
	  je	  same_ln	   ;
	  jmp	  bigr2 	   ; repeat until unequal or same lengths

rbig2:	  or	  AL,1		   ;Set the |BIG1|<|BIG2| bit
	  test	  AH,1		   ;Signs same?
	  jnz	  difsign	   ;No, different signs
	  test	  AH,2		   ;Both positive?
	  jnz	  grtr1 	   ;No, BIG1 is greater
grtr2:	  or	  AL,4		   ;Set the BIG1<BIG2 bit
compend:  cld			   ; Set direction forward (JCJ-12/6/84)
	  ret
%magcomp  endp

;     Add magnitudes of bignums
;	Calling sequence: bigadd(big1,big2)
;		Where: big1 ---- bignum of lesser magnitude
;		       big2 ---- bignum of greater magnitude
;		When done, BIG2 will hold the sum

%bigadd   proc	  far
	  mov	  BX,SP 	   ;Fetch bignum pointers
	  mov	  SI,[BX].big1
	  mov	  DI,[BX].big2
	  cld			   ;Direction forward
	  lodsw 		   ;Get length of smaller bignum
	  mov	  CX,AX 	   ;Save length
	  sub	  AX,[DI]	   ;Find and push difference in lengths
	  neg	  AX
	  push	  AX
	  inc	  SI		   ;Point SI,DI to bignums proper
	  add	  DI,3
	  clc			   ;Prepare to add
addlp:	  lodsw 		   ;Fetch source addend
	  adc	  [DI],AX	   ;Add to destination addend
	  inc	  DI		   ;Point DI to next word
	  inc	  DI
	  loop	  addlp 	   ;Do until smaller bignum exhausted
	  pop	  CX		   ;Fetch length difference (CF unchanged)
	  jnc	  doneadd	   ;Stop if no carry
	  mov	  SI,[BX].big2	   ;Point SI to destination bignum
	  jcxz	  samlen	   ;Jump if bignums the same length
adclp:	  inc	  word ptr[DI]	   ;Otherwise, add carry
	  jnz	  doneadd	   ;Jump if no resultant carry
	  add	  DI,2		   ;Point DI to next word
	  loop	  adclp 	   ;Do until whole number is done or no carry
samlen:   mov	  word ptr[DI],1   ;Store last carry
	  inc	  word ptr[SI]	   ;Note bignum's size increase
doneadd:  ret
%bigadd   endp

;     Subtract magnitudes of bignums
;	Calling sequence: bigsub(big1,big2)
;		Where: big1 ---- bignum of lesser magnitude
;		       big2 ---- bignum of greater magnitude
;		When done, BIG2 will hold the difference
;		When done, BIG2 will hold the sum

%bigsub   proc	  far
	  mov	  BX,SP 	   ;Fetch pointers to bignums
	  mov	  SI,[BX].big1
	  mov	  DI,[BX].big2
	  cld			   ;Direction forward
	  lodsw 		   ;Get length of smaller bignum
	  mov	  CX,AX
	  inc	  SI		   ;Point SI,DI to bignums proper
	  add	  DI,3
	  clc			   ;Prepare to subtract
sublp:	  lodsw 		   ;Fetch subtrahend
	  sbb	  [DI],AX	   ;Subtract
	  inc	  DI		   ;Point DI to next word
	  inc	  DI
	  loop	  sublp 	   ;Do until smaller bignum exhausted
	  jnc	  pack		   ;Jump if no borrow
borlp:	  mov	  AX,[DI]	   ;Fetch word
	  sub	  AX,1		   ;Decrement and store
	  stosw
	  jc	  borlp 	   ;Jump if further borrowing needed
pack:	  mov	  DI,[BX].big2	   ;Fetch pointer to 2nd bignum
	  mov	  SI,DI 	   ;Save pointer in SI
	  mov	  AX,[SI]	   ;Fetch bignum length
	  mov	  CX,AX 	   ;Save (length-1) in CX
	  dec	  CX
	  shl	  AX,1		   ;Point DI to last word of bignum
	  inc	  AX
	  add	  DI,AX
	  std			   ;Direction backward
	  xor	  AX,AX 	   ;Find number of leading 0-words
	  repe	  scasw 	   ;  (not counting least sig. word)
	  jz	  smlskp	   ;Jump if only one non-0 word
	  inc	  CX		   ;Else, at least 2 non-0 words
smlskp:   inc	  CX		   ;Form (length - # of leading 0-words)
	  mov	  [SI],CX	   ;Save in bignum size field
	  cld			   ;Clear the direction flag
	  ret
%bigsub   endp

;     Multiply two bignums
;	Calling sequence: bigmul(big1,big2,big3)
;		Where: big1,big2 -- factors
;		       big3 ------- destination of product
mulargs   struc
carry	  dw	  ?		   ;Multiplication carry
	  dw	  ?		   ;Caller's BP
	  dd	  ?		   ;Return address
	  dw	  ?		   ;Another return address
mbig1	  dw	  ?		   ;Factor of greater magnitude
mbig2	  dw	  ?		   ;Factor of lesser magnitude
mbig3	  dw	  ?		   ;Product destination
mulargs   ends
;		When done, BIG2 will hold the sum

%bigmul   proc	  far
	  push	  BP		   ;Save BP
	  dec	  SP		   ;Create space for multiplication carry
	  dec	  SP
	  mov	  BP,SP 	   ;Point BP to args
	  cld			   ;Direction forward
	  mov	  SI,[BP].mbig1    ;Fetch factor pointers
	  mov	  DI,[BP].mbig2
	  lodsw 		   ;Fetch BIG1's length
	  mov	  CX,AX 	   ;Put sum of lengths in CX
	  add	  CX,[DI]
	  scasw 		   ;Which has greater magnitude?
	  jae	  xchgskp	   ;Jump if BIG1 is not smaller
	  xchg	  DI,SI
xchgskp:  lodsb 		   ;Fetch one factor's sign
	  xor	  AL,[DI]	   ;XOR with the other factor's sign
	  inc	  DI		   ;Point DI to bignum proper
	  mov	  BX,DI 	   ;And store in BX
	  mov	  DI,[BP].mbig3    ;Store length into product
	  xchg	  AX,CX
	  stosw
	  push	  AX		   ;Save total length of product
	  xchg	  AX,CX 	   ;Store sign byte into product
	  stosb
	  push	  DI		   ;Set product to 0 over whole length
	  xor	  AX,AX
	  rep	  stosw
	  pop	  DI
	  xchg	  DI,BX 	   ;Restore BX and DI
	  mov	  CX,[DI]-3	   ;Fetch length of BIG2
	  sub	  BX,SI 	   ;Point [BX+SI-2] to product
	  dec	  BX
	  dec	  BX
	  mov	  [BP].mbig1,SI    ;Store pointer to data of BIG1
mullp2:   push	  CX		   ;Save counter of BIG2 words
;Add (BIG1*part of BIG2) to current product
	  mov	  word ptr[BP].carry,0	;Clear carry in
	  mov	  SI,[BP].mbig1    ;Fetch bignum pointer
	  mov	  CX,[SI]-3	   ;Get number of words in bignum
mullp:	  lodsw 		   ;Get factor part from BIG1
	  mul	  word ptr[DI]	   ;Multiply by factor part from BIG2
	  add	  AX,[BP].carry    ;Add carry in
	  adc	  DX,0
	  add	  [BX+SI],AX	   ;Add product part into BIG3
	  adc	  DX,0		   ;Adjust and store carry
	  mov	  [BP].carry,DX
	  loop	  mullp 	   ;Continue for all BIG1
	  mov	  [BX+SI+2],DX	   ;Store carry remaining
;
	  pop	  CX		   ;Restore BIG2 counter
	  inc	  DI		   ;Point DI to next word in BIG2
	  inc	  DI
	  inc	  BX		   ;Point BX to next word in BIG3
	  inc	  BX
	  loop	  mullp2	   ;Continue for all BIG2
	  mov	  BX,[BP].mbig3    ;Fetch pointer to BIG3 (beginning)
	  pop	  SI		   ;Point SI to last word of product
	  shl	  SI,1
	  inc	  SI
	  add	  SI,BX
	  cmp	  word ptr[SI],0   ;Test last word for zero
	  jnz	  muldone	   ;Done if not zero
	  dec	  word ptr[BX]	   ;Decrement bignum length
muldone:  inc	  SP		   ;Discard temporary carry variable
	  inc	  SP
	  pop	  BP		   ;Restore BP
	  ret
%bigmul   endp

;     Divide one bignum by another
;	Calling sequence: bigdiv(dvdnd,dvsr,quot)
;		Where: dvdnd ----- dividend
;		       dvsr ------ divisor
;		       quot ------ quotient
divargs   struc
	  dw	  ?		   ;Caller's BP
dvsrsz	  dw	  ?		   ;Size of divisor (words)
bitcount  dw	  ?		   ;Estimated bits in quotient
align	  dw	  ?		   ;Alignment of dividend to divisor
ldvsr	  dw	  ?		   ;Pointer to last word of divisor
	  dd	  ?		   ;Return address
	  dw	  ?		   ;Another return address
dvdnd	  dw	  ?		   ;Dividend
dvsr	  dw	  ?		   ;Divisor
quot	  dw	  ?		   ;Quotient
divargs   ends
;		When done, BIG2 will hold the sum

%bigdiv   proc	  far
	  sub	  SP,8		   ;Room for local variables
	  push	  BP
	  mov	  BP,SP
	  mov	  DI,[BP].quot	   ;Get pointers to arguments
	  mov	  SI,[BP].dvdnd
	  mov	  BX,[BP].dvsr
	  cld			   ;Direction forward
	  lodsw 		   ;Get dividend length
	  mov	  CX,[BX]	   ;Fetch divisor length
	  cmp	  CX,1		   ;Check divisor for 0
	  jne	  dvsrok
	  cmp	  word ptr[BX]+3,0  ;Check divisor data word
	  jnz	  dvsrok
	  mov	  AX,CX 	   ;Put nonzero value in AX
	  pop	  BP
	  add	  SP,8		   ;Restore stack
	  ret			   ;Exit
dvsrok:   inc	  BX		   ;Point BX+1 to divisor sign
	  mov	  DX,CX 	   ;Find & store pointer to last divisor word
	  shl	  DX,1
	  add	  DX,BX
	  mov	  [BP].ldvsr,DX
	  sub	  AX,CX 	   ;Find dividend-divisor length difference
	  mov	  DX,AX 	   ;Save in DX for now
	  inc	  AX		   ;Store maximum quotient length (words)
	  stosw
	  inc	  CX		   ;Save length of working divisor
	  mov	  [BP].dvsrsz,CX
	  dec	  AX		   ;Find and store quotient bit count
	  shl	  AX,1
	  shl	  AX,1
	  shl	  AX,1
	  shl	  AX,1
	  inc	  AX
	  mov	  [BP].bitcount,AX
	  lodsb 		   ;Get dividend sign
	  xor	  AL,[BX]+1	   ;Find and store quotient sign
	  stosb
	  mov	  [BP].dvdnd,SI    ;Save pointer to dividend proper
	  mov	  [BP].quot,DI	   ;Save pointer to quotient proper
	  xor	  AX,AX 	   ;Zero first two words of quotient
	  stosw
	  std
	  stosw
	  dec	  DX		   ;Account for extra divisor word
	  shl	  DX,1		   ;Store divisor-dividend alignment
	  add	  DX,SI
	  mov	  [BP].align,DX
	  mov	  word ptr[BX],0   ;Put 0-word at start of divisor
	  mov	  [BP].dvsr,BX	   ;Save pointer to working divisor
bigdivlp: call	  divcmp	   ;Dividend less than aligned divisor?
	  jb	  divbit0	   ;Yes, perform division
	  test	  word ptr[BX],8000h  ;Can divisor be shifted left?
	  jnz	  divbit1	   ;No, perform division
	  mov	  SI,[BP].dvsr	   ;Otherwise, shift entire divisor left
	  mov	  CX,[BP].dvsrsz
	  clc			   ;Start by shifting in 0
shllp:	  rcl	  word ptr[SI],1   ;Shift through divisor word
	  inc	  SI		   ;Point SI to next word
	  inc	  SI
	  loop	  shllp 	   ;Do for entire divisor
	  inc	  [BP].bitcount    ;Increase bit count
	  jmp	  bigdivlp	   ;See if divisor is big enough yet
divlp:	  call	  divcmp	   ;Dividend less than aligned divisor?
	  cld			   ;  (Direction forward)
	  jb	  divbit0	   ;Yes, rotate 0 into quotient
	  mov	  SI,[BP].align    ;Otherwise, subtract divisor
	  mov	  DI,SI
	  mov	  BX,[BP].dvsr
	  sub	  BX,SI
	  dec	  BX
	  dec	  BX
	  mov	  CX,[BP].dvsrsz
	  clc			   ;No carry in
divsublp: lodsw
	  sbb	  AX,[SI+BX]
	  stosw
	  loop	  divsublp
divbit1:  clc			   ;Clear carry (to rotate 1 in)
divbit0:  cmc
	  mov	  SI,[BP].quot	   ;Fetch pointer to quotient
	  mov	  CX,[SI]-3	   ;Fetch quotient length
quotlp:   rcl	  word ptr[SI],1   ;Rotate bit in
	  inc	  SI
	  inc	  SI
	  loop	  quotlp	   ;Rotate bits through whole quotient
	  dec	  [BP].bitcount    ;Last quotient bit rotated in?
	  jz	  divdone	   ;Yes, stop
	  mov	  SI,[BP].ldvsr    ;Otherwise realign divisor (shr)
	  mov	  CX,[BP].dvsrsz
	  std			   ;Direction backward
	  cmp	  word ptr[SI],0   ;Time to shift divisor words?
	  jnz	  wshftskp	   ;No, don't bother
	  mov	  BX,SI 	   ;Save last word pointer
	  mov	  DX,CX 	   ;Save word count
	  mov	  DI,SI 	   ;Destination = source+2
	  dec	  SI
	  dec	  SI
	  dec	  CX		   ;Shift significant divisor words
	  rep	  movsw
	  xor	  AX,AX 	   ;Clear least significant word
	  stosw
	  mov	  SI,BX 	   ;Restore last word pointer
	  mov	  CX,DX 	   ;Restore count
	  sub	  [BP].align,2	   ;Reset divisor alignment
wshftskp: clc			   ;Shift 0 in
shrlp:	  rcr	  word ptr[SI],1   ;Shift
	  dec	  SI
	  dec	  SI
	  loop	  shrlp 	   ;Shift entire divisor
	  jmp	  divlp 	   ;After all this, loop 'til division done
divdone:  mov	  BX,[BP].dvdnd    ;Fetch dividend pointer
	  mov	  DI,[BX]-3	   ;Fetch former length of dividend
	  dec	  DI		   ;Put length-1 in CX
	  mov	  CX,DI
	  shl	  DI,1		   ;Point DI to last dividend word
	  add	  DI,BX
	  std			   ;Direction backward
	  xor	  AX,AX 	   ;Pack as in BIGSUB
	  repe	  scasw
	  jz	  smlskp2
	  inc	  CX
smlskp2:  inc	  CX
	  mov	  [BX]-3,CX	   ;Save in bignum size field
	  mov	  BX,[BP].quot	   ;Fetch quotient pointer
	  mov	  DI,[BX]-3	   ;Point BX+DI to last quotient word
	  dec	  DI
	  shl	  DI,1
	  cmp	  word ptr[BX+DI],0  ;If last word is 0, decrease length
	  jnz	  divex
	  dec	  word ptr[BX]-3
divex:	  pop	  BP		   ;Restore stack
	  add	  SP,8
	  xor	  AX,AX 	   ;Return 0
	  cld			   ;Clear direction flag
	  ret
%bigdiv   endp

;Compare working divisor to dividend
divcmp	  proc	  near
	  mov	  DI,[BP].ldvsr    ;Get pointer to last divisor word
	  mov	  CX,[BP].dvsrsz   ;Fetch number of compares to do
	  mov	  SI,[BP].align    ;Get dividend pointer
	  mov	  AX,CX 	   ;Save # of wrods for pointer adjust
	  cmp	  SI,[BP].dvdnd    ;Dividend longer than divisor?
	  jae	  adjskp	   ;Yes, jump
	  dec	  CX		   ;Don't compare first divisor word
adjskp:   dec	  AX		   ;Adjust pointer into dividend
	  shl	  AX,1
	  add	  SI,AX
	  mov	  BX,DI 	   ;Save pointer to last divisor byte
	  std			   ;Direction backward
	  repz	  cmpsw 	   ;Compare until unequal
	  ret
divcmp	  endp

PROGX	  ends



PGROUP	  group   prog
prog	  segment byte public 'PROG'
	  assume  CS:PGROUP

	  public  big2flo
big2flo   proc	  near
	  call	  %big2flo
	  ret
big2flo   endp

	  public  fix2big
fix2big   proc	  near
	  call	  %fix2big
	  ret
fix2big   endp

	  public  magcomp
magcomp   proc	  near
	  call	  %magcomp
	  ret
magcomp   endp

	  public  bigadd
bigadd	  proc	  near
	  call	  %bigadd
	  ret
bigadd	  endp

	  public  bigsub
bigsub	  proc	  near
	  call	  %bigsub
	  ret
bigsub	  endp

	  public  bigmul
bigmul	  proc	  near
	  call	  %bigmul
	  ret
bigmul	  endp

	  public  bigdiv
bigdiv	  proc	  near
	  call	  %bigdiv
	  ret
bigdiv	  endp

prog	  ends
	  end
