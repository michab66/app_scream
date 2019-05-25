	page	60,132
	title	MACHTYPE - MACHINE TYPE CHECKER
	.286c			;; Utilize the expanded 80286 instruction set

;
; This routine determines the type of machine we are running on by using the
; System Services Bios call (INT 15h), Return System Configuration Parms
; function (AH = C0h). A return code which specifies the machine will be
; returned via the DOS Terminate function (INT 21h, Func 4ch) as follows:
;
;		return type		machine		 	bios Date
;		-----------		-------			---------
;
;		    -1		  Not a 286/386 machine 	   ----
;		     0		  Unknown machine		   ----
;		     1		  IBM PC AT			  1/10/84
;		     2		  IBM PC AT			> 6/10/85
;		     3		  IBM PS2			   ----
;
; The information is used to determine shutdown parameters when switching
; between protected and real mode by AI Architects OS286 operating environ-
; ment.
;


CODE	segment byte public
	assume  CS:CODE
	org	100h
begin:
	jmp	start


start:
	push	CS
	pop	DS		;; Set up data segment
	mov	DX,-1		;; Default to error condition

;; See if this is a 286 machine	

	mov	BX,SP		;; Set up BX with current stack pointer
	pusha			;; 286 instruction, ignored on 808x
	nop			;; Must be after pusha
	cmp	BX,SP		;; Were regs pushed?
	je	MEMRET  	;;   No...return with error
	popa			;; Restore regs

;; Determine machine

	mov	dx,0		;; Default to unknown

	mov	ah,0C0h		;; Return system config parameters
	int	15h		;; System services call
	jnc	CHK286		;; jump if carry not set
	mov	dx,1		;; indicate older AT, bios dated 1/10/84
	jmp	MEMRET		;; return

CHK286:
	cmp	byte ptr ES:[BX+2],0FCh ;; AT or PS2 model 50 or 60?
	jne	CHK386		       	;;   no, jump
	cmp	byte ptr ES:[BX+3],04h  ;; Regular AT or PC XT model 286?
	jge	GOTPS2			;;   no, see if PS2 Model 80
	mov	dx,2			;; Indicate newer AT, bios dated > 6/10/85
	jmp	MEMRET			;;   and return
GOTPS2: mov	dx,3			;; Indicate PS2 model 50 or 60
	jmp	MEMRET			;;   and return
CHK386:
	cmp	byte ptr ES:[BX+2],0F8h ;; PS2 Model 80?
	jne	MEMRET			;;   No, return
	mov	dx,3			;; Indicate PS2 model 80
MEMRET:
	mov	AX,DX			;; Return return code
	mov	AH,4ch
 	int	21h
CODE	ENDS
	END	begin
