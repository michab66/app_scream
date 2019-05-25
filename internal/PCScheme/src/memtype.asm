	page	60,132
	title	MEMTYPE - MEMORY TYPE CHECKER
	.286c			;; Utilize the expanded 80286 instruction set

CODE	segment byte public
	assume  CS:CODE
	org	100h
begin:
	jmp	start

EmmName db "EMMXXXX0"

start:
	push	CS
	pop	DS		;; Set up data segment
	mov	DX,0		;; Default to conventional memory

;; See if this is a 286 machine	

	mov	BX,SP		;; Set up BX with current stack pointer
	pusha			;; 286 instruction, ignored on 808x
	nop			;; Must be after pusha
	cmp	BX,SP		;; Were regs pushed?
	je	CHECKEXP	;;   No...return
	popa			;; Restore regs

	mov	AH,88h		;; Get number of contiguous 1k
	int	15h		;;    blocks starting at 1MByte
	cmp	AX,0		;; If none available
	je	CHECKEXP	;;    then jump
	inc	DX		;;    else note extended memory available

;; Check to see if expanded memory available

CHECKEXP:
	mov	AH,35H		;; Get Interrupt Vector
	mov	AL,67H		;; "Vector"
	int	21H
	mov	DI,000AH	;; ES:DI points to device name field
	lea	SI,EmmName	;; DS:SI points to device name
	mov	CX,8
	cld
	repe	CMPSB		;; Compare the two strings
	jne	MEMRET		;; If not equal jump
	or	DX,0002h	;; Note EMM Present
MEMRET:
	mov	AX,DX
	mov	AH,4ch
 	int	21h
CODE	ENDS
	END	begin
