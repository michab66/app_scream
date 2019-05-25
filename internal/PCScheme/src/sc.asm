	TITLE	CXINIT - Initiate execution of a Lattice C program
	SUBTTL	Copyright 1982 by Lattice, Inc.
	NAME	CXINIT
	INCLUDE DOS.MAC
	.8087

;**** modified to add EGA support 10-20-86 dbs

;**** modified for PROTECTED MODE 7-29-87 tc 

; ***** Lattice C v3.0 C.asm file modified for PCS *****
; (None of the original file is altered.  Mods done thru conditional assembly.)

scheme	equ	1

;**
;
; name		CXINIT -- initiate execution of C program
;
; description	This is the main module for a C program on the
;		MS-DOS implementation.	It performs various low-level
;		initializations and then calls _main.
;
;		This routine is stored as C.OBJ and is included as
;		the first object module when linking.  It is important
;		to include it first so that the segment structure will
;		be defined properly.
;
;		NOTE:  In order for the segment structure to work correctly,
;		this module must be assembled with Version 3 or later of the
;		Microsoft assembler.  Specifically, older assemblers emit
;		segments in alphabetical order, while Version 3 and later
;		emit them in the order that they appear.   The latter is
;		required for this module.
;
;**

;**
;
; Assembly parameters
;
TAB	EQU	09H			; tab character

;**
;
; External program references (P and L models)
;
	IF	LPROG
	EXTRN	_MAIN:FAR
	EXTRN	RBRK:FAR
	ENDIF

;**
;
; Define data group
;
DGROUP	GROUP	DATA,UDATA,XSTACK
	ASSUME	DS:DGROUP
;**
;
; Define program segment
;
	if	scheme		       ;extra segment for Scheme
XGROUP	GROUP	PROGX
PROGX	SEGMENT BYTE PUBLIC 'PROGX'
PROGX	ENDS
	endif

	IF	S8086
	IF	COM
PGROUP	GROUP	PROG,TAIL
	ENDIF
	IFDEF	PROMEM				;;; Protected Mode
PROG	SEGMENT PARA PUBLIC 'PROG'
        ELSE
PROG	SEGMENT BYTE PUBLIC 'PROG'
	ENDIF
	ASSUME	CS:PROG
	IF	COM
	ORG	100H
	ENDIF
	ENDIF

	IF	D8086
CODE	SEGMENT BYTE PUBLIC 'CODE'
	ASSUME	CS:CODE
	ENDIF

	IF	P8086
_CODE	SEGMENT BYTE PUBLIC 'CODE'
	ASSUME	CS:_CODE
	ENDIF

	IF	L8086
_PROG	SEGMENT BYTE PUBLIC 'PROG'
	ASSUME	CS:_PROG
	ENDIF

;**
;
; External program references (S and D models)
;
	IF	LPROG EQ 0
	EXTRN	_MAIN:NEAR
	IFNDEF	PROMEM				;;; Protected Mode - IGNORE
	EXTRN	RBRK:NEAR			      ; C library function
	ENDIF
	if	scheme
	extrn	allmem:near			      ; C library function
	extrn	rsttimer:near,unfixint:near	      ; PCS routines
	extrn	zcuron:near,zclear:near,zputcur:near  ;     "
	extrn	xli_term:near			      ; PCS routine	
	IFDEF	EXPMEM
	extrn	rlsexp:near			      ; PCS routine
	ENDIF
	endif
	ENDIF

;**
;
; Execution begins here
;
	PUBLIC	CXINIT

CXINIT	PROC	FAR
	CLI
	IF	COM
	MOV	AX,OFFSET PGROUP:TAIL	; set up DS/SS for .COM file
	ADD	AX,16
	MOV	CL,4
	SHR	AX,CL
	MOV	BX,CS
	ADD	AX,BX
	MOV	DS,AX
	MOV	SS,AX
	MOV	SP,OFFSET DGROUP:SBASE+STKRSV
	ELSE
	MOV	AX,DGROUP		; set up DS/SS for .EXE file
	MOV	DS,AX
	IFDEF   PROMEM				;;; Protected Mode
	mov	ss,ax			; ss=ds
	mov	_ss,ax			;   and save in _ss 
	mov	bx, _base		; bx = base of stack (relative to DS)
	add	bx, _stack		; add # bytes of stack (default = 2048)
	mov	sp, bx			; and save in sp 
	mov	_top, bx		; top of stack (relative to DS)
	mov	_sp,bx			; sp reset value
	ELSE
	MOV	AX,XSTACK
	MOV	SS,AX
	MOV	SP,STKRSV
	ENDIF
	ENDIF
	STI
	if	scheme
;
; Save address of our exit routine where C can use it
;
	mov	ax,offset xcexit
	mov	_onexit,ax
	endif
;
; Compute program size in bytes and save in _PSIZE
;
	IFDEF   PROMEM				;;; Protected Mode
; _PSIZE actually has limit, should increment....
	mov	ax, 0ffffh		; Program size will always be 64kb
	MOV	WORD PTR _PSIZE,AX
	xor	ax, ax	
	MOV	WORD PTR _PSIZE+2,AX
	ELSE
	MOV	AX,DS
	MOV	CX,CS
	SUB	AX,CX
	MOV	CL,4
	ROL	AX,CL
	MOV	WORD PTR _PSIZE,AX
	AND	WORD PTR _PSIZE,0FFF0H
	AND	AX,15
	MOV	WORD PTR _PSIZE+2,AX
	ENDIF
;
; Clear the uninitialized data area
;
	XOR	AX,AX
	MOV	DI,OFFSET DGROUP:UDATA
	MOV	CX,OFFSET DGROUP:SBASE
	SUB	CX,DI
	SHR	CX,1
	JZ	M0
	PUSH	ES
	PUSH	DS
	POP	ES
	CLD
	REP	STOSW
	POP	ES
;
; Get DOS version number
;
M0:	MOV	AH,30H			; get DOS version number
	INT	21H
	OR	AL,AL
	JNZ	M00
	MOV	AX,1			; assume 1.00 if null return
M00:	MOV	WORD PTR _DOS,AX
	MOV	_PSP+2,ES		; set up pointer to prog seg prefix
;
; Examine the environment string area
;
	ife	scheme			; PCS ignores DOS environment strings
	IF	MSDOS EQ 1
	CMP	_DOS,2
	JL	M01
	ENDIF
	MOV	AX,ES:[2CH]
	MOV	WORD PTR _ENV+2,AX	; set up pointer to environment
	OR	AX,AX
	JZ	M01			; branch if null
	PUSH	ES			; determine number of bytes
	CLD
	LES	DI,_ENV
	XOR	AX,AX
	XOR	BX,BX
	MOV	CX,7FFFH
M0A:	REPNZ	SCASB
	JNZ	M0C
	INC	BX
	SCASB
	JNZ	M0A
	MOV	_ESIZE,DI		; save environment size
	MOV	_ENVC,BX		; save environment string count
	CMP	_DOS,3			; include command name for DOS 3
	JL	M0B
	ADD	DI,2
	REPNZ	SCASB
	JNZ	M0C
M0B:	ADD	DI,2			; make size an even number
	AND	DI,0FFFEH
	MOV	_XSIZE,DI
M0C:	POP	ES
	endif
;
; Examine command line
;
M01:	MOV	SI,80H			; check command line
	MOV	CL,ES:[SI]
	XOR	CH,CH			; CX contains length of cmd line

	JCXZ	M12			; branch if null
M1:	INC	SI			; scan for first arg
	MOV	AL,ES:[SI]
	ife	scheme			; PCS doesn't scan for special args
	IF	MSDOS EQ 1
	CMP	_DOS,2
	JGE	M10
	CMP	AL,'<'
	JE	M2			; branch if input file name
	CMP	AL,'>'
	JE	M3			; branch if output file name
	ENDIF
M10:	CMP	AL,'='
	JE	M4			; branch if stack size
	endif
	CMP	AL,' '
	JE	M11			; branch if white space
	CMP	AL,TAB
	JNE	M12			; branch if normal arg
M11:	DEC	CX
	JG	M1
	XOR	CX,CX
M12:	JMP	M5			; branch if no args found
	ife	scheme			; PCS doesn't scan for special args
	IF	MSDOS EQ 1
;
; Get input file name
;
M2:	MOV	DI,OFFSET DGROUP:_INAME
	JMP	M31
;
; Get output file name
;
M3:	MOV	DI,OFFSET DGROUP:_ONAME
;
; Save file name in data area
;
M31:	XOR	AH,AH
M32:	DEC	CX
	JZ	M33
	INC	SI
	MOV	AL,ES:[SI]
	CMP	AL,' '
	JZ	M33
	CMP	AL,TAB
	JZ	M33
	MOV	DS:[DI],AL
	INC	DI
	INC	AH
	CMP	AH,32
	JE	M34
	JMP	M32
M33:	MOV	BYTE PTR DS:[DI],0
	JMP	M11
M34:	MOV	DX,OFFSET DGROUP:NAMERR
	JMP	NEAR PTR XCABT
	ENDIF
;
; Get stack and heap sizes from command line
;
M4:	CALL	GETNUM			; get stack size
	JC	M47			; branch if error
	TEST	BX,BX
	JZ	M42A			; bypass if size is 0
	CMP	AL,'K'
	JE	M41
	CMP	AL,'k'
	JNE	M42			; branch if not kilobytes
M41:	TEST	BX,0FFC0H
	JNZ	M47			; error if size > 63
	XCHG	BH,BL			; multiply by 1024
	SHL	BH,1
	SHL	BH,1
	DEC	CX			; advance to next character
	JZ	M42
	INC	SI
	MOV	AL,ES:[SI]
M42:	MOV	_STACK,BX		; save stack size
M42A:	TEST	CX,CX
	JZ	M5			; branch if end of command line
	CMP	AL,' '
	JE	M46			; loop if blank
	CMP	AL,TAB
	JE	M46			; loop if tab
	CMP	AL,'/'
	JNE	M47			; branch if not slash
	CALL	GETNUM			; get heap size
	JC	M47			; branch if error
	XOR	DX,DX			; long result is in DX,BX
	TEST	BX,BX
	JZ	M45			; branch if size is 0
	CMP	AL,'K'
	JE	M43
	CMP	AL,'k'
	JNE	M44			; branch if not kilobytes
M43:	XCHG	BH,BL			; multiply by 1024
	ROL	BX,1
	ROL	BX,1
	MOV	DX,BX
	AND	BX,0FC00H
	AND	DX,3FFH
	DEC	CX			; advance to next character
	JZ	M44
	INC	SI
	MOV	AL,ES:[SI]
M44:	MOV	WORD PTR _MNEED,BX	; save heap size
	MOV	WORD PTR _MNEED+2,DX
M45:	TEST	CX,CX
	JZ	M5			; branch if end of command line
	CMP	AL,' '
	JE	M46			; loop if blank
	CMP	AL,TAB
	JNE	M47			; error if not tab
M46:	JMP	M11
M47:	MOV	DX,OFFSET DGROUP:STKERR ; abort if stack/heap size error
	JMP	NEAR PTR XCABT
	endif				;matches IFE near M12:
;
; Set up the stack
;
	ife	scheme			; PCS ignores DOS environment strings
	IF	LDATA EQ 0
M5:	MOV	AX,_XSIZE		; reserve space for environment
	ADD	AX,15
	AND	AX,0FFFEH
	ELSE
M5:	XOR	AX,AX
	ENDIF
	else
M5:	xor	ax,ax
	endif

	IFDEF	PROMEM				;;; Protected Mode
	mov	bx,_top			 ; bx = top of stack
	jmp	M54a
M54:	MOV	DX,OFFSET DGROUP:MEMERR2 ; abort if error in releasing memory
	JMP	NEAR PTR XCABT
M54a:
	ELSE
	MOV	BX,_STACK		; get stack size
	SHR	BX,1			; make size even
	ADD	BX,BX
	CMP	BX,STKMIN
	JA	M51
	MOV	BX,STKMIN		; use default if too small
	MOV	_STACK,BX
M51:	ADD	BX,AX			; add environment size
	JC	M54			; abort if overflow
	MOV	DX,ES:2 		; compute available paragraphs
	if	scheme
	mov	_paras,dx		; save no. paragraphs for Scheme
	endif
	MOV	AX,SS
	SUB	DX,AX
	TEST	DX,0F000H
	JNZ	M52			; branch if greater than 64Kbytes
	SHL	DX,1			; convert to bytes
	SHL	DX,1
	SHL	DX,1
	SHL	DX,1
	JMP	M53
M52:	MOV	DX,0FFF0H		; use largest value
	IF	LDATA
M53:	CMP	DX,BX			; check if stack will fit
	JA	M55
	ELSE
M53:	CMP	DX,BX			; check if stack will fit
	JB	M54
	ADD	BX,_BASE		; adjust S/P model for statics
	JNC	M55
	ENDIF
M54:	MOV	DX,OFFSET DGROUP:MEMERR2 ; abort if error in releasing memory
	JMP	NEAR PTR XCABT
M55:	CLI
	MOV	_TOP,BX 		; set top-of-stack
	MOV	SP,BX			; set stack pointer
	IF	LDATA EQ 0
	MOV	AX,DS
	MOV	SS,AX
	ENDIF
	MOV	_SS,SS
	STI
	ENDIF				; IFDEF PROMEM

;
; Set up memory allocation pointers
;
	PUSH	CX			; save command byte count

	IFDEF	PROMEM				;;; Protected Mode
					; Assume small data model
	MOV	AX,SS			; ax=stack segment
	MOV	_MBASE+2,AX		; _mbase = top of stack
	mov	_mbase, bx		;
	MOV	_MNEXT+2,AX		; _mnext = _mbase 
	mov	_mnext, bx		; 
	mov	bx, 0fff0h		; Protected mode has a full 64K
	sub	bx, _top 		; subtract out top of stack
	MOV	_MSIZE,BX		; _msize = size of heap pool
	xor	ax, ax
	ELSE
	ADD	BX,15			; compute mem pool base segment number
	JC	M54
	MOV	CL,4
	SHR	BX,CL
	MOV	AX,SS
	ADD	AX,BX
	MOV	_MBASE+2,AX
	MOV	_MNEXT+2,AX
	MOV	BX,ES:2 		; get top segment number
	SUB	BX,AX			; compute memory pool size
	JBE	M54			; branch if insufficient memory
	MOV	CL,4			; compute number of bytes
	ROL	BX,CL
	MOV	AX,BX
	AND	AX,15
	AND	BX,0FFF0H
	MOV	_MSIZE,BX
	ENDIF

	MOV	_MSIZE+2,AX

	PUSH	ES			; reset memory pool
	PUSH	SI
	CALL	RBRK
	POP	SI
	POP	ES
	OR	AX,AX
	JNZ	M54			; branch if not enough memory

	if	scheme
	IFNDEF	PROMEM				;;; Protected Mode - IGNORE
	push	si
	call	allmem			; allocate the rest of the data segment
					; to C's heap (small model only);
					; later, in "initmem", PCS will get the
					; rest of MSDOS's memory for itself
	pop	si
	or	ax,ax
	jnz	m54
	ENDIF
	endif

	POP	DX			; restore command line length
;
; Put return address at top of stack
;
	IF	MSDOS EQ 1
	PUSH	ES			; return is to 1st word of prog prefix
	XOR	AX,AX
	PUSH	AX
	MOV	BP,SP			; BP contains stack linkage
	ENDIF
;
; copy command line to stack
;
	MOV	CX,DX			; get residual command line length
	SUB	SP,CX			; allocate stack space
	DEC	SP
	MOV	DI,SP
	MOV	DX,SP			; save pointer to end of command
	ADD	DX,CX
	JCXZ	M71			; skip if no bytes to move
	PUSH	DS			; move them
	PUSH	ES
	POP	DS
	PUSH	SS
	POP	ES
	CLD
	REP	MOVSB
	POP	DS
M71:	MOV	BYTE PTR SS:[DI],0	; append null byte
	MOV	WORD PTR _ARG,SP	; save arg array pointer
	MOV	WORD PTR _ARG+2,SS
;
; Move environment to stack for small data models
;
	ife	scheme			; PCS ignores DOS environment strings
	IF	LDATA EQ 0
	MOV	CX,_XSIZE		; get extended env size
	JCXZ	M72			; branch if no environment
	SUB	SP,CX			; allocate stack space
	MOV	DI,SP
	PUSH	DS			; save important regs
	PUSH	ES
	PUSH	SI
	PUSH	SS			; set stack as destination seg
	POP	ES
	MOV	SI,WORD PTR _ENV+2
	MOV	WORD PTR _ENV,DI	; save relocated env pointer
	MOV	WORD PTR _ENV+2,SS
	MOV	DS,SI			; set env as source
	XOR	SI,SI
	CLD				; move environment
	REP	MOVSB
	POP	SI			; restore regs and continue
	POP	ES
	POP	DS
	ENDIF
	endif
;
; Build argument vector
;
	ife	scheme			; PCS parses its own command line
M72:	AND	SP,0FFFEH		; make stack pointer even
	MOV	SI,DX			; prepare to scan command backwards
	XOR	CX,CX
	PUSH	CX			; push null terminator for argv
	IF	LDATA
	PUSH	CX
	ENDIF
	MOV	BX,SP			; save this null byte address
M8:	CMP	SI,WORD PTR _ARG	; skip trailing white space
	JNE	M80
	JMP	M83
M80:	DEC	SI
	CMP	BYTE PTR SS:[SI],' '
	JE	M8
	CMP	BYTE PTR SS:[SI],TAB
	JE	M8
	MOV	BYTE PTR SS:[SI+1],0	; install null terminator byte
	MOV	AL,0			; indicate white space scan
	CMP	BYTE PTR SS:[SI],'"'
	JNE	M81			; branch if not quoted arg
	CMP	SI,WORD PTR _ARG
	JE	M81
	CMP	BYTE PTR SS:[SI-1],'\'
	JE	M810			; branch if \"
	MOV	AL,'"'                  ; indicate quote scan
	MOV	BYTE PTR SS:[SI],0	; put null in place of quote
M81:	CMP	SI,WORD PTR _ARG
	JE	M82			; branch if no more chars
	DEC	SI			; position to next char
	CMP	BYTE PTR SS:[SI],'"'
	JNE	M81C			; branch if not quote
	CMP	SI,WORD PTR _ARG
	JE	M81C			; branch if no more chars
	CMP	BYTE PTR SS:[SI-1],'\'
	JNE	M81C			; branch if not \"
M810:	PUSH	SI			; squish args to remove \
M81A:	DEC	SI
	CMP	SI,WORD PTR _ARG
	JE	M81B
	MOV	AH,SS:[SI-1]
	MOV	SS:[SI],AH
	JMP	M81A
M81B:	INC	WORD PTR _ARG
	POP	SI
	JMP	M81D
M81C:	OR	AL,AL
	JNZ	M82A			; branch if not white space scan
M81D:	CMP	BYTE PTR SS:[SI],' '    ; find next white space
	JE	M81E
	CMP	BYTE PTR SS:[SI],TAB
	JNE	M81
M81E:	INC	SI			; position to start of arg
	IF	LDATA
M82:	PUSH	SS			; add arg pointer to vector
	PUSH	SI
	ELSE
M82:	PUSH	SI			; put arg pointer into argv
	ENDIF
	INC	CX			; bump arg count
	JMP	M8			; loop till all args done
M82A:	CMP	BYTE PTR SS:[SI],'"'    ; come here to find starting quote
	JNE	M81			; branch if not quote
	MOV	BYTE PTR SS:[SI],' '    ; replace it with white space
	JMP	M81E			; go save arg pointer
;
; Construct argv[0]
;
M83:	CMP	_DOS,2
	JLE	M84
	MOV	AX,WORD PTR _ENV	; use env trailer for DOS 3
	ADD	AX,_ESIZE
	INC	AX
	INC	AX
	IF	LDATA
	PUSH	[WORD PTR _ENV+2]
	ENDIF
	PUSH	AX
	JMP	M85
	IF	LDATA
M84:	PUSH	SS			; use null byte for DOS 2
	PUSH	BX
	ELSE
M84:	PUSH	BX			; use null byte for DOS 2
	ENDIF
;
; Save argv information
;
M85:	INC	CX			; save arg count
	MOV	_ARGC,CX
	IF	LDATA
	MOV	WORD PTR _ARGV,SP	; save arg vector pointer
	MOV	WORD PTR _ARGV+2,SS
	ELSE
	MOV	_ARGV,SP		; save arg vector pointer
	ENDIF

	  else				; PCS parses its own command line

; -------------------------------------------------
; The PCS command line parser
; -------------------------------------------------

; The PCS command line can look as follows:
;
;    PCS (... ...) atom atom (... ...) "string" atom ...
;
; which parses into PCS-INITIAL-ARGUMENTS as:
;
;    ( "(... ...)" "atom" "atom" "(... ...)" "\"string\"" "atom" ... )
;
; Each command line argument is either an atom, list, or string.
; Each is treated as one argument for the argv vector, and each is
; converted to a string which becomes an element of PCS-INITIAL-ARGUMENTS.
;
; The command line parser is not a Scheme reader.  It looks for blank-separated
; tokens, where a token can start with a ( and end with the matching ),
; start and end with a ", or just be a sequence of nonblanks.  Backslashed
; delimiters are skipped over as you'd expect.  We don't bother with | since
; that is a special character to DOS.  The blanks between tokens are important
; since the parser replaces them with nulls to get things set up for C's
; argv vector, so situations like ...)(... won't be parsed correctly.
; The parser is implemented as a finite state automaton (FSA).
;
; The first command line argument has special meaning but that is
; handled in "smain.c".
;

; On entry to this section:
;	  ES = SS (but we don't care)
;	  SP = ptr to start of cmdline
;	  DX = ptr to cmdline eos (cmdline on stack with eos at its end)

M72:
	  push	  DS			; ES <- DS
	  pop	  ES
	  xor	  AX,AX 		; AH is state, AL is current char
	  push	  AX			; push 0 at front of cmdline
					; (we're going to parse backwards)
	  and	  SP,0FFFEH		; make SP even
	  mov	  SI,DX 		; SI is index into cmdline
	  xor	  DX,DX 		; DH is paren counter, DL is argc
	  cld
M72loop:  dec	  SI			; get the next char from cmdline
	  cmp	  byte ptr SS:[SI-1],'\' ; is it singly escaped?
	  jne	  M72a			; no, jump
	  dec	  SI			; yes, back up to escape char
M72a:	  mov	  AL,SS:[SI]
	  mov	  CX,scan_size		; look it up in char table
	  mov	  DI,offset scan_table
    repne scasb 			; put into CX the "char class" for
					; indexing into state table
	  mov	  AL,bytes_per_state	; do 2-D subscript into state table
	  mul	  AH			; ... row
	  shl	  CX,1			; ... col
	  add	  AX,CX
	  mov	  BX,AX 		; (BH=0 since subscript small enough)
	  mov	  AH,state[BX]		; get next state
	  mov	  BL,state+1[BX]	; do action routine
	  add	  BX,offset actions
	  jmp	  BX

actions   label   near			; start action routines ---------->
ar_out2:  inc	  SI			; 1 char past token, back up
	  push	  SI			; push next argv
	  dec	  SI
	  inc	  DL			; incr argc
ar_skip:  mov	  byte ptr SS:[SI],0	; output a null char
ar_decr:  jmp	  short M72loop
ar_lpar:  dec	  DH			; decr paren count
	  js	  ar_err		; wrong paren to start with
	  jnz	  short M72loop 	; on an inner paren, keep looking
	  mov	  AH,0			; override state in table
ar_out1:  push	  SI			; matched delimiter, push next argv
	  inc	  DL			; incr argc
	  jmp	  short M72loop
ar_rpar:  inc	  DH			; incr paren count
	  jmp	  short M72loop
ar_err:   mov	  DX,offset dgroup:parserr  ; abort on error in cmdline parsing
	  jmp	  near ptr XCABT
ar_out_end: inc   SI			; 1 char past token, back up
	  push	  SI			; push next argv
	  dec	  SI
	  inc	  DL			; incr argc
ar_end: 				; end action routines <----------

continue: xor	  AX,AX 		; argv[0] = \0
	  push	  AX
	  inc	  DL			; incr argc
	  mov	  _argv,SP		; save address of argv
	  mov	  DH,0
	  push	  DX
	  mov	  _argc,DX		; save argc

	  endif

;
; Build environment vector
;
	ife	scheme			; PCS ignores DOS environment strings
	XOR	AX,AX			; push null terminating pointer
	PUSH	AX
	IF	LDATA
	PUSH	AX
	ENDIF
	MOV	AX,_ENVC		; allocate stack space for vector
	ADD	AX,AX
	JZ	M87			; branch if null environment
	SUB	SP,AX
	IF	LDATA
	SUB	SP,AX
	ENDIF
	MOV	SI,SP			; scan environment
	LES	DI,_ENV
	XOR	AX,AX
	MOV	CX,7FFFH
	MOV	DX,_ENVC
	CLD
M86:	MOV	SS:[SI],DI
	ADD	SI,2
	IF	LDATA
	MOV	SS:[SI],ES
	ADD	SI,2
	ENDIF
	DEC	DX
	JLE	M87
	REPNE	SCASB
	JMP	M86
M87:	MOV	WORD PTR environ,sp
	IF	LDATA
	MOV	WORD PTR environ+2,SS
	ENDIF
	endif
;
; initialize 8087 numeric data processor
;
	IFNDEF	PROMEM				;;; Protected Mode - IGNORE
	FNINIT				; reset
	FNSTSW	_NDPSW			; get status
	MOV	AX,100			; this is just for delay
	MOV	DX,AX
	IMUL	DX
	TEST	_NDPSW,0B8BFH		; 8087 will reset all these
	JNZ	M9
	INC	_NDP			; indicate ndp present
	ENDIF
;
; set up args for _main and call it
;
M9:	MOV	_SP,SP			; save stack pointer reset value
	PUSH	DS			; make ES same as DS
	POP	ES
	CALL	_MAIN			; call C main
	IF	MSDOS EQ 1
	CMP	_DOS,2
	JL	M91			; branch if DOS 1
	ENDIF
	MOV	AX,4C00H		; exit with return code of 0
	INT	21H
	IF	MSDOS EQ 1
M91:	MOV	SP,BP			;restore ptr to far return
	RET				;return to MS-DOS
	ENDIF
;**
;
; name		GETNUM -- get a number from the command line
;
; description	This function is used internally by the start-up routine
;		while processing the command line.
;
;**
	ife	scheme
GETNUM	PROC	NEAR
	XOR	BX,BX			; reset accumulator
NUM1:	DEC	CX			; advance to next character
	JZ	NUM3			; branch if end of command line
	INC	SI
	MOV	AL,ES:[SI]
	CMP	AL,'0'
	JL	NUM3			; return if not decimal digit
	CMP	AL,'9'
	JG	NUM3			; return if not decimal digit
	SUB	AL,'0'                  ; multiply accumulator by 10
	ADD	BX,BX
	JC	NUM2
	MOV	DX,BX
	ADD	BX,BX
	JC	NUM2
	ADD	BX,BX
	JC	NUM2
	ADD	BX,DX
	JC	NUM2
	XOR	AH,AH			; add this digit
	ADD	BX,AX
	JNC	NUM1			; loop till done
NUM2:	RET
NUM3:	CLC				; clear carry to indicate no error
	RET
GETNUM	ENDP
	endif

;**
;
; name		XCABT -- Ignominious abort
;
; description	This area is entered by direct jump with a message
;		pointer in DS:DX.  It sends the message to the
;		console via DOS function 9 and then aborts.
;
	ENTRY	XCABT
	MOV	AH,9			; print error message
	INT	21H
	MOV	ES,_PSP+2
	IF	MSDOS EQ 1
	CMP	_DOS,2
	JL	A1
	ENDIF
	MOV	AX,4C01H
	INT	21H
	IF	MSDOS EQ 1
A1:	PUSH	ES
	XOR	AX,AX
	PUSH	AX
	RET
	ENDIF

	if	scheme
;
; Scheme wrapup - the C fn "exit" calls "_exit" which calls this hook routine
;
 	public	xcexit
DOS	equ	21h		; MSDOS function request interrupt
xcexit	proc	near

	mov	bp,sp		; don't lose our return address to _exit

	IFDEF   EXPMEM
	call	rlsexp		; Release Expanded Memory (if any)
	ENDIF

	call	xli_term	; release all external programs
				; allocated under XLI
	IFNDEF  PROMEM
	push	ES		; return Scheme heap to DOS
	mov	AH,49h
	mov	ES,first_dos
	int	DOS
	pop	ES
	jnc	MA
	MOV	DX,OFFSET DGROUP:MEMERR2 ; abort if error in releasing memory
	JMP	NEAR PTR XCABT
	ENDIF
MA:	call	rsttimer	; Reset the timer interrupt, if necessary
	call	unfixint	; Restore the keyboard "patch" (MWH2)
	call	zcuron		; Turn cursor back on
	mov	AX,15		; Load character attribute = white,enable
	push	AX
	mov	AX,80		; 80 Columns
	push	AX
	mov	AX,25		; 25 Rows
	push	AX
	xor	AX,AX		; Origin at 0,0
	push	AX
	push	AX
	call	zclear		; Clear the screen
	xor	AX,AX
	push	AX
	push	AX
	inc	cur_off 	; keep cursor off for EGA modes
	call	zputcur 	; Put cursor at 0,0
	mov	sp,bp		; reestablish the return address to _exit
	ret
xcexit	endp

	endif

CXINIT	ENDP
	

	IFDEF	PROMEM				;;; Protected Mode

;;;	Reset heap break point (unix lingo)
;;;
;;;	Set _mbase = _mnext = DS:_top
;;;	Set _pool = 0:0, _melt = 0:0
public	rbrk
RBRK	PROC	NEAR
        push	bp 
        mov	bp,sp 
	;
	; set up memory allocation pointers
	;
	mov	bx, _top
	add	bx, 0fh		; round up to para
	and	bx, 0fff0h
	mov	_tsize, bx	; total size in paras

	mov	ax,ds
	mov	_mbase+2,ax	; long ptr to heap base
	mov	_mbase, bx
	mov	_mnext+2,ax	; long ptr to heap top
	mov	_mnext, bx


        xor	ax,ax 
	mov	[_msize+2],ax   ; number of bytes left in pool
	mov	_msize, ax

; Clear the 2nd level pool
        mov	[_melt],ax 
        mov	[_pool],ax 
        mov	[_melt+02],ax 
        mov	[_pool+02],ax 
        pop	bp 
        ret	 
RBRK	ENDP

;	

public	lsbrk
lsbrk	PROC	NEAR
	push	bp 
	mov	bp,sp 
	mov	cx,[bp+6]       ; requested size (high)
	mov	dx,[bp+4]       ; requested size (low)
	or	cx, cx
	jnz	lsbrk_err1
	mov	cx, 0FFF0h
	sub	cx, _mnext	; available bytes
	cmp	cx, dx
	jb	lsbrk_err1
	mov	ax, _mnext	; next location in pool
	add	_mnext, dx	; bump by requested amount
	mov	bx, ax
	pop	bp 
	ret

lsbrk_err1:
	xor	ax,ax 
	mov	bx, ax
	pop	bp 
	ret
lsbrk	ENDP


COMMENT	%
;; _RBRK (int desire)
;;   desire is initial heap in paras
;;   return 0 if OK, ffff otherwise
;;
public	_rbrk
;;extrn	_model:word
_rbrk	proc	near
	push	bp 
	mov	bp,sp 
	push	es 
;mov	word ptr [_oserr],_model 

	mov	bx, 0		;; what is really needed here? 128K?
	mov	[_tsize],bx 	;; really total size with heap

	mov	ax,[_mbase] 	;; reset heap allocator ptrs
	mov	[_mnext],ax 
	mov	ax,[_mbase+02]
	mov	[_mnext+02] ,ax
	xor	ax,ax 
	pop	es 
	pop	bp 
	ret	
_rbrk_error1: 
	mov	[_oserr],ax 
_rbrk_error:
	mov	ax,0ffffh
	pop	es 
	pop	bp 
	ret	 
_rbrk	endp
%
        ENDIF
;**
;
; Dummy segment to establish top of program for small program models
;
	IF	S8086
	IF	COM
TAIL	SEGMENT WORD 'PROG'
	DW	-1
TAIL	ENDS
	ENDIF
	ENDIF

	IF	S8086
PROG	ENDS
	ENDIF
	IF	D8086
CODE	ENDS
	ENDIF
	IF	P8086
_CODE	ENDS
OLDCODE SEGMENT BYTE			; This catches Version 2 code
	DW	0CACAH
OLDCODE ENDS
	ENDIF
	IF	L8086
_PROG	ENDS
OLDPROG SEGMENT BYTE			; This catches Version 2 code
	DW	0CACAH
OLDPROG ENDS
	ENDIF
	PAGE
;**
;
;  DGROUP includes the segments named DATA, UDATA, and XSTACK.	The startup
;  routine initializes DS to point to this group, and DS must then be pre-
;  served throughout the program.  The segments within DGROUP are defined
;  as follows:
;
;	DATA   => Contains all static (local and global) initialized items.
;	UDATA  => Contains all static (local and global) uninitialized items.
;	XSTACK => Stack for the startup routine.
;
;  During the startup routine, the initial stack (XSTACK) is replaced with
;  one that has the correct size for program execution.  This size is
;  determined by examining the command line and the _STACK global item. Then
;  for the S and P memory models, the stack is set up relative to DGROUP (i.e.
;  stack items can addressed via DS).  For the D and L models, the stack
;  segment stands alone and can be up to 64K bytes.
;
;  The heap (i.e. the space used by the memory allocator) resides above the
;  stack and is also initialized by the startup routine.  Any space not
;  immediately needed for the heap (as defined by _MNEED) is returned to DOS.
;
;  At the end of the startup routine, memory is organized in the following
;  sequence:
;
;	-- code --
;	-- DATA  --
;	-- UDATA --
;	-- stack --
;	-- heap --
;
;  FOR PROPER OPERATION OF THE STANDARD MEMORY ALLOCATOR, THIS SEQUENCE IS
;  EXTREMELY IMPORTANT.  IF YOU TAMPER WITH THE STARTUP ROUTINE OR INTRODUCE
;  SEGMENTS AND CLASSES THAT DO NOT FOLLOW LATTICE CONVENTIONS, CHECK THE
;  LOAD MAP CAREFULLY.
;
;**


;**
;
; Initialized data
;
DATA	SEGMENT PARA PUBLIC 'DATA'
	EXTRN	_STACK:WORD
	EXTRN	_MNEED:DWORD

	IFDEF	PROMEM				;;; Protected Mode
	EXTRN   _POOL:WORD
	EXTRN   _MELT:WORD
	ENDIF

	PUBLIC	_MODEL,_VER,_TOP,_BASE,_PSP,_MBASE,_MNEXT,_MSIZE,_DSIZE,_PSIZE
	PUBLIC	_ENV,_DOS,_TSIZE,_ESIZE,_XSIZE,_SS,_SP,_NDP,_NDPSW,_NDPCW
	PUBLIC	_FPA,_FPERR,_OSERR,_SIGFPE,_ARGV,_ARGC,_ENVC,environ
	if	scheme
	extrn	_onexit:word			      ; our hook into C exit fn
	extrn	cur_off:byte
	public	_paras,first_pa,first_dos
	endif
	IF	S8086
_MODEL	DW	0
	ENDIF
	IF	P8086
_MODEL	DW	1
	ENDIF
	IF	D8086
_MODEL	DW	2
	ENDIF
	IF	L8086
_MODEL	DW	3
	ENDIF
_VER	DB	"LC 3.00",0
_DOS	DB	0			; DOS major version number
	DB	0			; DOS minor version number
_SS	DW	0			; stack segment number
_SP	DW	0			; SP reset value
_TOP	DW	0			; top of stack (relative to SS)
_BASE	DW	OFFSET DGROUP:SBASE	; base of stack (relative to DS)
_PSP	DW	0			; program segment prefix pointer
	DW	0
_MBASE	DW	0			; base of memory pool
	DW	0
_MNEXT	DW	0			; next available memory location
	DW	0
_MSIZE	DW	0			; number of bytes left in pool
	DW	0
_TSIZE	DW	0			; total size in paragraphs
_PSIZE	DD	0			; size of program in bytes
_DSIZE	DW	OFFSET DGROUP:SBASE	; size of static data in bytes
	DW	0
	IF	LDATA
_ARGV	DD	0			; argument vector pointer
environ DD	0			; environment vector pointer
	ELSE
	DD	0			; *** DOS 2.00 trashing bug ***
_ARGV	DW	0			; argument vector pointer
environ DW	0			; environment vector pointer
	ENDIF
_ARGC	DW	0			; argument count
_ENVC	DW	0			; environment count
_ARG	DD	0			; far pointer to original arg array
_ENV	DD	0			; far pointer to original env array
_ESIZE	DW	0			; environment size in bytes
_XSIZE	DW	0			; extended env size in bytes
_FPA	DQ	0			; floating point accumulator
_FPERR	DW	0			; floating point error code
_NDP	DB	0			; non-zero if 8087 is installed
_NDPSW	DW	0FFFFH			; 8087 status word
_NDPCW	DW	0			; 8087 control word
_OSERR	DW	0			; DOS error code
	IF	LPROG
_SIGFPE DD	0			; Floating point error signal
	ELSE
_SIGFPE DW	0			; Floating point error signal
	ENDIF

	if	scheme

_PARAS	DW	0			; # of paragraphs of memory available
FIRST_PA DW	0			; seg# of first para. actually used for
					;   Scheme heap
FIRST_DOS DW	0			; seg# of first para. from DOS for
					;   Scheme heap

; the characters sought by PCS's scanner
scan_table db	  0,' ()"',0
scan_size equ	  $-scan_table

; the FSA transition table used to parse PCS's command line
; (This once included handling for vertical-bar delimited symbols, but
; DOS's use of | rendered it useless, so it was removed.)

start_state equ   0
list_state  equ   1
atom_state  equ   2
strg_state  equ   3
end_state   equ   4
err_state   equ   5

state	  label   byte
					; initial state
	  db	  atom_state,ar_decr-actions	  ; any char
	  db	  strg_state,ar_decr-actions	  ; "
	  db	  list_state,ar_rpar-actions	  ; )
	  db	  err_state,ar_err-actions	  ; (
	  db	  start_state,ar_skip-actions	  ; blank
	  db	  end_state,ar_end-actions	  ; null
bytes_per_state equ $-state
					; in list
	  db	  list_state,ar_decr-actions
	  db	  list_state,ar_decr-actions
	  db	  list_state,ar_rpar-actions
	  db	  list_state,ar_lpar-actions	 ; start_state also possible, see ar_lpar
	  db	  list_state,ar_decr-actions
	  db	  err_state,ar_err-actions
					; in atom
	  db	  atom_state,ar_decr-actions
	  db	  atom_state,ar_decr-actions
	  db	  atom_state,ar_decr-actions
	  db	  atom_state,ar_decr-actions
	  db	  start_state,ar_out2-actions
	  db	  end_state,ar_out_end-actions
					; in string
	  db	  strg_state,ar_decr-actions
	  db	  start_state,ar_out1-actions
	  db	  strg_state,ar_decr-actions
	  db	  strg_state,ar_decr-actions
	  db	  strg_state,ar_decr-actions
	  db	  err_state,ar_err-actions

; The exit and error states are not explicitly represented
; in the table, action routines deal with them.

	endif

STKERR	DB	"Invalid stack size",0DH,0AH,"$"
MEMERR	DB	"Insufficient memory",0DH,0AH,"$"
	if	scheme
memerr2 db	"Error in returning memory to DOS",0dh,0ah,"$"
parserr db	"Error in parsing command line",0dh,0ah,"$"
	endif

	IF	MSDOS EQ 1
	PUBLIC	_INAME,_ONAME
_INAME	DB	32 DUP(0)		; input file name
_ONAME	DB	32 DUP(0)		; output file name
NAMERR	DB	"Invalid I/O redirection",0DH,0AH,"$"
	ENDIF
DATA	ENDS


;**
;
; Uninitialized data
;
UDATA	SEGMENT PUBLIC 'DATA'
UDATA	ENDS

;**
;
; The stack segment is included to prevent the warning from the
; linker, and also to define the base (lowest address) of the stack.
;
STKRSV	EQU	128			; reserved stack size
STKMIN	EQU	512			; minimum run-time stack size
	IF	COM
XSTACK	SEGMENT 'DATA'
	ELSE
XSTACK	SEGMENT STACK 'DATA'
	ENDIF
SBASE	DB	STKRSV DUP (?)
XSTACK	ENDS

	END	CXINIT
