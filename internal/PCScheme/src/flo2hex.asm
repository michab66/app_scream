		name	flo2hex
		title	Convert Floating Point Number to Hex Ascii
		page	62,132
;							=====> FLO2HEX.ASM
;****************************************************************
;*	  	  TIPC Scheme Runtime Support			*
;*								*
;*    (C) Copyright 1987 by Texas Instruments Incorporated.	*
;*		      All rights reserved.			*
;*								*
;* Author:		Terry Caudill				*
;* Date written:	10 March 1987				*
;****************************************************************

DGROUP    group   data
data      segment word public 'DATA'
          assume  DS:DGROUP
hexbuf	  db	030h,031h,032h,033h,034h,035h,036h,037h,038h,039h
	  db    041h,042h,043h,044h,045h,046h
data      ends

XGROUP    group   PROGX
PROGX     segment byte public 'PROGX'
          assume  CS:XGROUP,DS:DGROUP

;************************************************************************
;*									*
;*   Routine Name: FLO2HEX						*
;*									*
;*   Description: FLO2HEX is a %escape routine provided for PC+ and     *
;*		  is used to return the Hex Ascii value of a floating	*	
;*		  point number when outputting kb's in fsl format.	*
;*									*
;*   Calling Sequence: (FLO2HEX float string #words)			*
;*		where: float  = the floating point number               *
;*		       string = a return string to place result         *
;*		       #words = size of the float to be converted       *
;*				will be 4 for floating point		*	
;*									*
;*   Note: Actually, this routine can be called with integers, etc.     *
;*									*
;************************************************************************

INARGS	  struc
OLDBP	  DW	?
FRETN	  DD	?	; Far return to 'prog' segment
NRETN	  DW    ?	; Return from flo2hex
_STRING	  DW	?
_FLOAT	  DW	?
_WORDS	  DW	?
INARGS	  ENDS

%flo2hex  proc	far
          push  BP
          mov   BP,SP
	  mov	si,[bp]._FLOAT		; floating point value
	  mov	di,[bp]._STRING		; string for result
	  mov	dx,[bp]._WORDS		; #words to convert
	  mov	bx,offset hexbuf	
	  mov	cl,4			; shift count
	  cld
movdigits:
	  lodsw				; get word to convert
	  push	ax			; save word
	  shr	ax,cl			; work on high byte
	  shr   ax,cl
	  call	cvthex			; convert lower byte
	  stosw				; store into string
	  pop	ax			; restore word
	  xor	ah,ah			; now work lower byte
	  call  cvthex			; convert it
	  stosw				; store into string
	  dec	dx			; any more words?
	  jne	movdigits		; yes, jump

	  xor	al,al			; 0 terminate the string
	  stosb

	  mov	ax,[bp]._STRING		; return string
	  pop	BP
	  ret			        ; return to caller.
%flo2hex  endp
	  
cvthex    proc  near
	  shl	ax,cl			; seperate digits
	  shr	al,cl			; work on lower nibble
	  xlat				; convert to hex
       	  xchg	ah,al			; work on upper nibble
	  xlat				; convert to hex
	  ret				; return with hex ascii value
cvthex	  endp
PROGX     ends


PGROUP    group   prog
prog      segment byte public 'PROG'
          assume  CS:PGROUP
	  public  flo2hex

flo2hex	  proc	near
	  call  %flo2hex
	  ret
flo2hex	  endp
prog	  ends
          end
