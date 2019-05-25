	  name	  sound
	  title   PC Scheme XLI interface to sound
	  page	  84,120


	  comment ~

   This program provides access to the PC's sound-generating devices.
   It demonstrates an XLI interface written in assembly language.

   User documentation is available under XLI\SOUND.DOC.  SOUND.EXE is
   already provided and can be used immediately by inserting its pathname
   in your .XLI control file.

   To generate SOUND.EXE yourself, do the following (substituting
   directory names and setting the path as needed; Microsoft's
   Macro Assembler version 4.0 was used):

        masm sound;
	link sound;

   	  ~


DATA	  segment byte public 'DATA'
	  assume  DS:DATA
datastart =	  $

;-----------------------------------------------------------------------------
;	Equates
;-----------------------------------------------------------------------------
ppi_port   equ	61h		;Programmable Peripheral Interface port#
timer_port equ  42h		;timer chip port#
				;reset timer is port# + 1
timer_mask equ  00000001b	;mask to extract timer bit	1=on
spkr_mask  equ  00000010b	;mask to extract speaker bit	1=on

;-----------------------------------------------------------------------------
;	XLI
;-----------------------------------------------------------------------------
;;; ----- Equates -----
; offsets into the PSP
term_addr equ	0Ah
fb_addr	  equ	5Ch

;;; ----- Data structures -----

; file block
file_block label word
	dw	4252h
	dw	0011b		;flags = 0,0,16-bit,near
	dw	offset lookup_table, seg lookup_table
	dw	offset parm_block, seg parm_block
	dw	8 dup (0)	;reserved

; parameter block
parm_block label word
	dw	0		;selector
	dw	0		;ssr
	dw	8 dup (0)	;ssr args
	dw	8 dup (0)	;reserved
	dw	0		;return value type
	dw	4 dup (0)	;return value
	; begin arguments
over	dw	?		;overlay the 2 sound sources? (you and timer)
				;  0 - enable/disable sound commands
				;  1 - timer only 
				;	 (processor-speed independent)
				;  2 - manual control only
				;	 (processor-speed dependent)
				;  3 - overlay manual control with timer
				;	 (processor-speed dependent)
				;  4 - speaker off
freq	dw	?		;timer chip set to this frequency
dura	dw	?		;duration
pitch	dw	?		;pitch (silent section)
pitch2  dw	?		;pitch (voiced section)

; lookup table
lookup_table label word
	db	'sound//'

; other needed values
psp	dw	?		;PSP segment address
psize   dw	?		;size of program in paragraphs
xwait   dw	2 dup (?)	;XLI wait address
xbye	dw	2 dup (?)	;XLI bye address

;-----------------------------------------------------------------------------
;	Local data
;-----------------------------------------------------------------------------
;;; ----- Constants -----
clock   dd	1193180		;main clock frequency (Hz)
;;; ----- Variables -----
tmask	db	?		;reflects state of timer mask
enable  dw	1		;enabled flag; 0=no, 1=yes

datasize =	$-datastart
DATA	ends


STACK   segment word stack 'STACK'
stackstart =	$
	dw	16 dup (?)
stacksize  =	$ - stackstart
STACK	ends


PROG	  segment byte public 'PROG'
	  assume  CS:PROG,DS:DATA
progstart =	  $

;-----------------------------------------------------------------------------
;	The XLI interface.
;-----------------------------------------------------------------------------

main    proc    far			;this file's initial entry point

; Initialization

	mov	AX,data
	mov	DS,AX
	mov	psp,ES			;save PSP@
	mov	word ptr ES:fb_addr,offset file_block	;poke file block@
	mov	word ptr ES:fb_addr+2,seg file_block    ;into PSP
	mov	AX,ES:term_addr		;calc ptrs in PCS to jump to
	add	AX,3
	mov	xwait,AX
	add	AX,3
	mov	xbye,AX
	mov	AX,ES:term_addr+2
	mov	xwait+2,AX
	mov	xbye+2,AX
	mov	psize,plen		;calc program size

; Suspend this program until an XCALL comes in, or until PCS terminates.

hloop:	push	psp
	push	psize
	call	dword ptr [xwait]	;connect to PCS
	pop	ax
	pop	ax
	cmp	ax,0
	jnz	case0
	call	dword ptr [xbye]	;disconnect from PCS

;-----------------------------------------------------------------------------
; 	The individual cases (just one, here).
;-----------------------------------------------------------------------------

case0:
	cmp	over,0			;enable/disable sound?
	jnz	check			;no, jump
	mov	ax,freq			;set flag appropriately
	mov	enable,ax
	mov	dx,0
	jmp	short exit		;turn off sound before exiting

check:  cmp	enable,0		;is sound enabled?
	jz	hloop			;no, exit
;	
	cmp	over,4			;silence?
	jnz	s1			;no, jump
	mov	dx,freq
exit:	in	al,ppi_port		;turn off speaker bit
	and	al,not spkr_mask
	out	ppi_port,al
	cmp	dx,0			;delay before returning?
	jne	timed			;yes, jump
	jmp	hloop			;no, return immediately to PC Scheme

s1:	cmp	over,1			;timer only?
	jnz	s2			;no, jump
	call	init_timer
	in	al,ppi_port
	or	al,spkr_mask OR timer_mask
	out	ppi_port,al
	cmp	dura,0			;if duration=0,
	jz	hloop			;exit without turning sound off
timed:	mov	tmask,0			;x (time filler)
	mov	bx,dura
again1:	mov	cx,pitch
	nop				;x
	nop				;x
	nop				;x
here1a:	loop	here1a
	nop				;x
	nop				;x
	nop				;x
	or	al,tmask		;x
	mov	cx,pitch2
here1b: loop	here1b
	dec	bx
	jnz	again1
	xor	dx,dx			;clear DX for exiting
	jmp	exit

s2:	cmp	over,2			;manual control only?
	jnz	s3			;no, jump
	mov	tmask,0			;reset timer-bit mask
merge:	mov	bx,dura			;BX is duration
	in	al,ppi_port
again2:	and	al,not (spkr_mask OR timer_mask)  ;turn off speaker
	out	ppi_port,al
	mov	cx,pitch		;CX is first half of pitch half-cycle
here2a:	loop	here2a
	or	al,spkr_mask		;turn on speaker
	or	al,tmask		;include timer bit state
	out	ppi_port,al
	mov	cx,pitch2		;CX is second half of pitch half-cycle
here2b: loop	here2b
	dec	bx
	jnz	again2
	xor	dx,dx			;clear DX for exiting
	jmp	exit

s3:	cmp	over,3			;both?
	jnz	error			;no, jump; error
	call	init_timer
	mov	tmask,timer_mask	;set timer-bit mask
	jmp	merge

error:	jmp	exit	

main    endp

init_timer proc
	mov	al,182			;reset timer chip
	out	timer_port+1,al
	mov	ax,word ptr clock	;calc number to give to timer chip
	mov	dx,word ptr clock+2	;  = 1193180 / freq
	mov	bx,freq
	mov	cx,20			;avoid underflow 
	cmp	bx,cx			;(occurs for divisors <= 18)
	jge	it_10
	mov	bx,cx
it_10:	div	bx
	out	timer_port,al		;send number to timer chip
	mov	al,ah
	out	timer_port,al
	ret
init_timer endp

progsize =	$-progstart
plen	equ	(progsize+datasize+stacksize+100h+20h)/16

PROG    ends
	end	main

