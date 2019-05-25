;							=====> SUTIL.ASM
;***************************************
;*	PC Scheme  Runtime Support     *
;*	     Misc Utilities	       *
;*				       *
;*   (C) Copyright 1984.1985,1986 by   *
;*   Texas Instruments Incorporated.   *
;*	  All rights reserved.	       *
;*				       *
;* Date Written:  April 1984	       *
;* Last Modification:  26 February 1986*
;***************************************
	  include scheme.equ
	  include pcmake.equ

;* Modification History:
;*   27 Jan 86 - Changed the code which looks for the TI Copyright notice
;*     (JCJ)	 (when determining machine type) to search two areas instead
;*		 of just one.  Now, checks are made at segment (paragraph)
;*		 offsets FC00 and FE00.
;*
;*   25 Feb 86 - Added the routine "put_ptr" to combine the "put_byte/put_word"
;*     (JCJ)	 operations when a pointer is being stored into memory.
;*
;*   17 Feb 88 - Conditionally assemble XPCTYPE and PC_TYPE for Protected Memory
;*     (TC)      Scheme. These routines can be found in PRO2REAL.ASM and
;*		 REALIO.ASM for PROMEM

DGROUP	  group   data
data	  segment word public 'DATA'
	  assume  DS:DGROUP
	  extrn   _base:word
data	  ends


IFNDEF PROMEM
      
      ; See PRO2REAL.ASM for protected mode scheme

XGROUP	  group   PROGX
PROGX	  segment para public 'PROGX'
	  assume CS:XGROUP,DS:DGROUP
;************************************************************************
;*			Determine PC's Manufacturer                     *
;*									*
;* Purpose:  To determine whether or not we're running on a TIPC or     *
;*	     another brand and set the "PC_MAKE" variable accordingly.  *
;* Returns:  PC_MAKE will contain  1 for TIPC or Business Pro in TI mode*
;*				  FF for IBM-PC 			*
;*				  FE for IBM-PC/XT			*
;*				  FD for IBM-PC/jr			*
;*				  FC for IBM-PC/AT or B-P in IBM mode	*
;*				   0 for undeterminable 		*
;************************************************************************
	  public  pc_type
XPCTYPE   proc	  far
	  push	  ES		   ; save caller's ES register
	  push	  DI
	  mov	  AX,0FC00h	   ; move paragraph address of copyright
pc_002:   mov	  ES,AX 	   ;  notice into ES
	  xor	  DI,DI 	   ;  Clear DI; 0 is lowest address in ROM @ES:
	  xor	  BX,BX 	   ;  Flag for "PC_MAKE" variable
	  mov	  CX,40h	   ;  This'll be as far as I go...
	  mov	  AL,'T'           ;  look for beginning of "Texas Instruments"
	  cli			   ;  Stop interrupts - bug in old 8088's
again:
    repne scas	  byte ptr es:[di] ; SEARCH
	  or	  CX,CX 	   ; Reach my limit?
	  jz	  short pc_005	   ; quit if we've exhausted search
	  cmp	  byte ptr ES:[di],'e'     ; make sure this is it
	  jne	  again 		   ; use defaults if not found
	  cmp	  byte ptr ES:[di]+1,'x'   ; really make sure this is it
	  jne	  again
	  push	  DS
	  mov	  DS,BX 	   ; 0->DS for addressing low mem.
	  inc	  BX		   ; BX==1 => TIPC
	  mov	  AX,DS:word ptr [01A2h]   ; If TIPC then what kind?
	  pop	  DS		   ; get DS back
	  add	  AL,AH 	   ; checkout vector 68 bytes 2 & 3
	  cmp	  AL,0F0h	   ; if AL==F0 then TIPC=Business Pro
	  jne	  pc_010	   ; jump if not a B-P
	  in	  AL,068h	   ; Read from port
	  push	  AX		   ; Save for later
	  and	  AL,0FBh	   ; Enable CMOS
	  out	  068h,AL	   ; Write back out
	  mov	  DX,8296h	   ; I/O address for B-P's mode byte
	  in	  AL,DX 	   ; TI or IBM Mode on the B-P?
	  cmp	  AL,0		   ; if not zero then B-P emulates a TIPC
	  pop	  AX		   ; Restore original port value
	  out	  068h,AL	   ;   and write back out
	  jne	  pc_010	   ; jump if TIPC else IBM machine code is
				   ; where it should be.
	  jmp	  short pc_007
pc_005:
	  mov	  AX,ES
	  cmp	  AH,0FEh	   ; test for segment offset FE00
	  jae	  pc_007	   ; two checks made? if so, jump
	  add	  AH,2		   ; go back and check segment offset
	  jmp	  pc_002	   ;  FE00
pc_007:   mov	  AX,0F000h
	  mov	  ES,AX
	  mov	  al,byte ptr ES:0FFFEh ; IBM's machine code is @F000:FFFE
	  cmp	  AL,IBMTYPE	   ; Is this suckah an IBM?
	  jb	  pc_010	   ; Jump if AL is below F0 (BX will be 0)
	  mov	  BL,AL
pc_010:   sti			   ; Turn interrups back on
	  mov	  PC_MAKE,BX	   ; set variable PC_MAKE
	  pop	  DI
	  pop	  ES		   ; restore caller's ES register
	  ret			   ; return to caller
XPCTYPE   endp
PROGX	  ends

      ; See PRO2REAL.ASM for above definition

ENDIF

PGROUP	  group   prog
prog	  segment byte public 'PROG'
	  assume  CS:PGROUP

;For space and performance reasons, some procedures have been written in the
;  following style: the arguments are popped off the stack, and the
;  procedure ends in an indirect JMP instead of a RET.	In this source file,
;  the following are such procedures:
;	zero_pag, zero_blk, get_byte, get_word, put_byte, put_word,
;	   get_flo, put_flo, get_str, put_str, get_sym, put_sym,
;		   make_ptr, alloc_fi, take_car, take_cdr

;     Return Value of Stack Segment Register (SS:)
;;;	  public  _SS
;;;_SS	     proc    near
;;;	     mov     AX,SS
;;;	     ret
;;;_SS	     endp

;;;;	 Return Value of Extra Segment Register (ES:)
;;;	  public  _ES
;;;_ES	  proc	  near
;;;	  mov	  AX,ES
;;;	  ret
;;;_ES	  endp

;;;;	 Return Value of Code Segment Register (CS:)
;;;	  public  _CS
;;;_CS	  proc	  near
;;;	  mov	  AX,CS
;;;	  ret
;;;_CS	  endp

;     Return Value of Data Segment Register (DS:)
	  public  _DS
_DS	  proc	  near
	  mov	  AX,DS
	  ret
_DS	  endp

;     Zero a page in memory - Calling sequence:  zero_page(page_no)
	  public  zero_pag
zero_arg  struc
	  dw	  ?		   ; Return address
zero_pg   dw	  ?		   ; Page number
zero_arg  ends
zero_pag  proc	  near
	  pop	  DX		   ;Pop return address
	  pop	  BX		   ;Pop page number
	  push	  ES		   ;Save ES
	  sal	  BX,1
	  LoadPage ES,BX
;;;	  mov	  ES,DGROUP:pagetabl+[BX]
	  xor	  AX,AX
	  xor	  DI,DI
	  mov	  CX,psize+[BX]
	  shr	  CX,1
	  cld
	  rep	  stosw
	  pop	  ES		   ;Restore ES
	  jmp	  DX
zero_pag  endp

;************************************************************************
;* Zero a block of memory						*
;*									*
;* Purpose:  To initialize a variable length block of memory to zero.	*
;*									*
;* Description:  The block is zeroed using the 8088's "store string"    *
;*			instruction using a repeat count.  For		*
;*			efficiency reasons, the zeroing is done by	*
;*			words, with a fixup to account for blocks with	*
;*			an odd number of bytes. 			*
;*									*
;* Calling sequence:  zero_blk(page_no, disp)				*
;*			where page_no = page number (C's unshifted      *
;*					 page number)			*
;*			disp	      = displacement of block within	*
;*					 the page			*
;************************************************************************
	  public  zero_blk
zb_args   struc
	  dw	  ?		   ; Return address
zb_page   dw	  ?		   ; Page number
zb_disp   dw	  ?		   ; Displacement
zb_args   ends

zero_blk  proc	  near
	  pop	  SI		   ;Pop return address
	  pop	  BX		   ; Pop the page number for the block
	  shl	  BX,1		   ;  and adjust for use as index
	  pop	  DI		   ; Pop the displacement of the block
	  push	  ES		   ; save the caller's ES register
	  LoadPage ES,BX
;;;	  mov	  ES,DGROUP:pagetabl+[BX] ; load page's paragraph address
	  mov	  CX,ES:[DI].vec_len ;	and the block's length
	  add	  DI,BLK_OVHD	   ;  and advance pointer past block header
	  cmp	  CX,0		   ;;; check for small string
	  jge	  zero_010
	  add	  CX,PTRSIZE
	  jmp	  zero_020
zero_010: sub	  CX,BLK_OVHD	   ; subtract block overhead from the length
zero_020: mov	  DX,CX 	   ; copy the length in bytes, and
	  and	  DX,1		   ;  isolate the least significant bit
	  shr	  CX,1		   ; convert number of bytes to number of words
	  xor	  AX,AX 	   ; load a value of zero into AX
	  cld			   ; set forward direction
	  rep	  stosw 	   ; zero the block
	  mov	  CX,DX 	   ; copy the fixup byte count
	  rep	  stosb 	   ; zero the last byte, if odd number of bytes
	  pop	  ES		   ; restore ES register
	  jmp	  SI		   ; return to caller
zero_blk  endp

;     Fetch/Store byte/word
get_args  struc  ; Arguments Template
	  dw	  ?		   ; return address
get_page  dw	  ?		   ; page number
get_disp  dw	  ?		   ; displacement into page
get_val   dw	  ?		   ; value (if a store operation)
get_args  ends

;     Get a byte of data
;	Calling sequence:  data = get_byte(page, disp)
;		where:	page ----- page number
;			disp ----- (byte) displacement within page
	  public  get_byte
get_byte  proc	  near
	  mov	  CX,ES 	   ; save caller's ES in CX
	  pop	  SI		   ; get return address
	  pop	  BX		   ; get page argument
	  shl	  BX,1		   ; adjust it for segment lookup
	  LoadPage ES,BX
;;;	  mov	  ES,pagetabl+[BX] ; get page segment
	  pop	  BX		   ; get displacement
	  mov	  AL,ES:[BX]	   ; get byte
	  xor	  AH,AH 	   ; and only a byte
	  mov	  ES,CX 	   ; restore ES
	  jmp	  SI		   ; return
get_byte  endp

;     Get a word of data
;	Calling sequence:  data = get_word(page, disp)
;		where:	page ----- page number
;			disp ----- (byte) displacement within page
	  public  get_word
get_word  proc	  near
	  mov	  CX,ES 	   ; save caller's ES in CX
	  pop	  SI		   ; get return address
	  pop	  BX		   ; get page argument
	  shl	  BX,1		   ; adjust it for segment lookup
	  LoadPage ES,BX
;;;	  mov	  ES,pagetabl+[BX] ; get page segment
	  pop	  BX		   ; get displacement
	  mov	  AX,ES:[BX]	   ; get word
	  mov	  ES,CX 	   ; restore ES
	  jmp	  SI		   ; return
get_word  endp

;     Put a byte of data
;	Calling sequence:  put_byte(page, disp, value)
;		where:	page ----- page number
;			disp ----- (byte) displacement within page
;			value ---- value to be stored (low order 8 bits)
	  public  put_byte
put_byte  proc	  near
	  mov	  CX,ES 	   ; save caller's ES in CX
	  pop	  SI		   ; get return address
	  pop	  BX		   ; get page
	  sal	  BX,1		   ; double page number for use as index
	  LoadPage ES,BX
;;;	  mov	  ES,pagetabl+[BX] ; load page's paragraph address
	  pop	  BX		   ; get displacement
	  pop	  AX		   ; load byte to store
	  mov	  byte ptr ES:[BX],AL ; store new data
	  mov	  ES,CX 	   ; restore segment register ES
	  jmp	  SI		   ; return
put_byte  endp

;     Put a word of data
;	Calling sequence:  put_word(page, disp, value)
;		where:	page ----- page number
;			disp ----- (byte) displacement within page
;			value ---- value to be stored (16 bits)
	  public  put_word
put_word  proc	  near
	  mov	  CX,ES 	   ; save caller's ES in CX
	  pop	  SI		   ; get return address
	  pop	  BX		   ; load the page number
	  sal	  BX,1		   ; double page number for use as index
	  LoadPage ES,BX
;;;	  mov	  ES,pagetabl+[BX] ; load page's paragraph address
	  pop	  BX		   ; load displacement
	  pop	  AX		   ; load word to store
	  mov	  word ptr ES:[BX],AX ; store new data
	  mov	  ES,CX 	   ; restore segment register ES
	  jmp	  SI		   ; return
put_word  endp

;     Exchange a byte of data
;	Calling sequence:  old_data = xch_byte(page, disp, value)
;		where:	old_data - original data (overwritten)
;			page ----- page number
;			disp ----- (byte) displacement within page
;			value ---- value to be stored (low order 8 bits)
;	  public  xch_byte
;xch_byte  proc    near
;	  mov	  CX,ES 	   ; save caller's ES in CX
;	  pop	  SI		   ; get return address
;	  pop	  BX		   ; get page
;	  sal	  BX,1		   ; double page number for use as index
;	  mov	  ES,pagetabl+[BX] ; load page's paragraph address
;	  pop	  BX		   ; get displacement
;	  pop	  AX		   ; load byte to store
;	  xchg	  AL,byte ptr ES:[BX] ; swap old and new data
;	  xor	  AH,AH 	   ; clear high order byte of AX
;	  mov	  ES,CX 	   ; restore segment register ES
;	  jmp	  SI		   ; return
;xch_byte  endp

;     Exchange a word of data
;	Calling sequence:  old_data = xch_word(page, disp, value)
;		where:	old_data - original data (overwritten)
;			page ----- page number
;			disp ----- (byte) displacement within page
;			value ---- value to be stored (16 bits)
;	  public  xch_word
;xch_word  proc    near
;	  mov	  CX,ES 	   ; save caller's ES in CX
;	  pop	  SI		   ; get return address
;	  pop	  BX		   ; load the page number
;	  sal	  BX,1		   ; double page number for use as index
;	  mov	  ES,pagetabl+[BX] ; load page's paragraph address
;	  pop	  BX		   ; load displacement
;	  pop	  AX		   ; load word to store
;	  xchg	  AX,word ptr ES:[BX] ; swap old and new data
;	  mov	  ES,CX 	   ; restore segment register ES
;	  jmp	  SI		   ; return
;xch_word  endp

;     Put a pointer
;	Calling sequence:  put_word(page, disp, pg_value, ds_value)
;		where:	old_data - original data (overwritten)
;			page ----- page number
;			disp ----- (byte) displacement within page
;			pg_value ---- value of page number to store (16 bits)
;			ds_value ---- value of displacement to store (16 bits)
	  public  put_ptr
put_ptr   proc	  near
	  mov	  CX,ES 	   ; save caller's ES in CX
	  pop	  SI		   ; get return address
	  pop	  BX		   ; load the page number
	  sal	  BX,1		   ; double page number for use as index
	  LoadPage ES,BX
;;;	  mov	  ES,pagetabl+[BX] ; load page's paragraph address
	  pop	  BX		   ; load displacement
	  pop	  AX		   ; load page number value to store
	  mov	  byte ptr ES:[BX],AL ; store page number
	  pop	  AX		   ; load displacement value to store
	  mov	  word ptr ES:[BX]+1,AX ; store page number
	  mov	  ES,CX 	   ; restore segment register ES
	  jmp	  SI		   ; return
put_ptr   endp

;     Fetch/Store Flonum
getf_arg  struc  ; Arguments Template
	  dw	  ?		   ; caller's BP
	  dw	  ?		   ; return address
getf_pag  dw	  ?		   ; page number
getf_dis  dw	  ?		   ; displacement into page
getf_val  dw	  ?		   ; value (if a store operation)
getf_arg  ends

;     Get a floating point value
;	Calling sequence:  fdata = get_flo(page, disp)
;		where:	page ----- page number
;			disp ----- (byte) displacement within page
	  public  get_flo
get_flo   proc	  near
	  pop	  DI		   ;Pop return address
	  pop	  BX		   ; load the page number
	  sal	  BX,1		   ; double page number for use as index
	  pop	  SI		   ; load displacement
	  inc	  SI		   ;  and advance page flonum's tag
	  push	  DS		   ; save the caller's DS segment register
	  LoadPage DS,BX
;;;	  mov	  DS,pagetabl+[BX] ; load page's paragraph address
	  cld			   ;Direction forward
	  lodsw 		   ;Put the flonum in AX:BX:CX:DX
	  mov	  DX,AX
	  lodsw
	  mov	  CX,AX
	  lodsw
	  mov	  BX,AX
	  lodsw
	  pop	  DS		   ; restore caller's DS segment register
	  jmp	  DI		   ; return
get_flo   endp

;     Put a flonum value into Scheme's memory
;	Calling sequence:  put_flo(page, disp, value)
;		where:	page ----- page number
;			disp ----- (byte) displacement within page
;			value ---- flonum value to be stored (4 words)
	  public  put_flo
put_flo   proc	  near
	  pop	  DX		   ;Pop return address
	  pop	  BX		   ; load the page number
	  sal	  BX,1		   ; double page number for use as index
	  pop	  DI		   ; load displacement
	  inc	  DI		   ;  and advance offset past flonum's tag
	  mov	  SI,SP 	   ;SP points to flonum - point SI to it too
	  push	  ES		   ; save the caller's ES segment register
	  LoadPage ES,BX
;;;	  mov	  ES,pagetabl+[BX] ; load page's paragraph address
	  mov	  CX,FLOSIZE/WORDINCR ; load number of words to store
	  cld			   ; clear direction flag
	  rep	  movsw 		   ; move the words of the flonum
	  pop	  ES		   ; restore the ES segment register
	  jmp	  DX		   ; return to caller
put_flo   endp

;     Transfer string to/from Scheme's memory
s_args	  struc
	  dw	  ?		   ; Caller's BP
	  dw	  ?		   ; Return address
sptr	  dw	  ?		   ; Pointer to string in C's memory
spage	  dw	  ?		   ; page number
sdisp	  dw	  ?		   ; displacement in page
lpage	  dw	  ?		   ; link field page number (for symbols)
ldisp	  dw	  ?		   ; link field displacement (for symbols)
hash_key  dw	  ?		   ; hash value (for symbols)
s_args	  ends

	  public  get_str,get_sym
get_str   proc	  near
	  pop	  DX		   ;Pop return address
	  pop	  DI		   ; Fetch destination string's displacement
	  pop	  BX		   ; Fetch source page number
	  shl	  BX,1		   ; Adjust page number for use as index
	  pop	  SI		   ; Fetch source string's displacement
	  push	  DS		   ;Save caller's DS
	  LoadPage DS,BX
;;;	  mov	  DS,pagetabl+[BX] ; Get source page's paragraph address
	  mov	  CX,[SI].vec_len  ; Fetch length of string/symbol
	  add	  SI,offset vec_data ; Adjust for string header
	  cmp	  CX,0		   ;;; check for small string
	  jge	  get_010
	  add	  CX,PTRSIZE
	  jmp	  get_mrg
get_010:  sub	  CX,offset vec_data ; Adjust length for string header
get_mrg:  cld			   ; clear string direction
	  rep	  movsb 	   ; move 'em out
	  pop	  DS		   ; Restore DS segment register
	  jmp	  DX		   ;Return
get_str   endp

get_sym   proc	  near
	  pop	  DX		   ;Pop return address
	  pop	  DI		   ; Fetch destination string's displacement
	  pop	  BX		   ; Fetch source page number
	  shl	  BX,1		   ; Adjust page number for use as index
	  pop	  SI		   ; Fetch source string's displacement
	  push	  DS		   ;Save caller's DS
	  LoadPage DS,BX
;;;	  mov	  DS,pagetabl+[BX] ; Get source page's paragraph address
	  mov	  CX,[SI].sym_len  ; Fetch length of string/symbol
	  add	  SI,offset sym_data ; Adjust offset for symbol header
	  sub	  CX,offset sym_data ; Adjust length for symbol header
	  jmp	  get_mrg	   ;Get pname bytes
get_sym   endp

	  public  put_str,put_sym
put_str   proc	  near
	  pop	  DX		   ;Pop return address
	  pop	  SI		   ; Load source string offset
	  pop	  BX		   ; Load destination page number,
	  pop	  DI		   ;  and displacement
	  shl	  BX,1		   ; Adjust page number for use as index
	  push	  ES		   ; Save caller's ES segment register
	  LoadPage ES,BX
;;;	  mov	  ES,pagetabl+[BX] ; Load destination page paragraph address
	  mov	  CX,ES:[DI].vec_len ; Load string length
	  add	  DI,offset vec_data ; Adjust pointer for string header
	  cmp	  CX,0		   ;;; check for small string
	  jge	  put_010
	  add	  CX,PTRSIZE	   ;;; get the right string length
	  jmp	  putmrg
put_010:  sub	  CX,offset vec_data ; Adjust length for string header
putmrg:   cld			   ; Clear direction flag
	  rep	  movsb 	   ; Move 'em in
	  pop	  ES		   ; Restore caller's ES
	  jmp	  DX		   ; Return
put_str   endp

put_sym   proc	  near
	  pop	  DX		   ;Pop return address
	  pop	  SI		   ; Load source string offset
	  pop	  BX		   ; Load destination page number,
	  pop	  DI		   ;  and displacement
	  shl	  BX,1		   ; Adjust page number for use as index
	  mov	  CX,ES 	   ;Save caller's ES in CX
	  LoadPage ES,BX
;;;	  mov	  ES,pagetabl+[BX] ; Load destination page paragraph address
	  pop	  AX		   ; Load link field page number and
	  mov	  ES:[DI].sym_page,AL ;  and move into symbol structure
	  pop	  ES:[DI].sym_disp ; Store link field displacement
	  pop	  AX		   ; move hash value into symbol data object
	  mov	  ES:[DI].sym_hkey,AL
	  push	  CX		   ;Now move caller's ES to stack
	  mov	  CX,ES:[DI].sym_len ; Load string length
	  add	  DI,offset sym_data ; Adjust displacement for symbol header
	  sub	  CX,offset sym_data ; Adjust length for symbol header
	  jmp	  putmrg	   ; Move 'em in
put_sym   endp

;     Convert page, displacement values to a long integer
	  public  make_ptr
make_args struc
	  dw	  ?		   ; return address
mak_page  dw	  ?		   ; page number
mak_disp  dw	  ?		   ; pointer displacement
make_args ends

make_ptr  proc	  near
	  pop	  DI
	  pop	  AX
	  adjpage AX
	  pop	  BX
	  jmp	  DI
make_ptr  endp

;     Allocate a cell for a fixnum (actually, return an immediate value)
;	Calling sequence:  alloc_fixnum(&reg, value)
a_fix_arg struc
	  dw	  ?		   ; Return address
a_reg	  dw	  ?		   ; Address of register to hold pointer
a_val	  dw	  ?		   ; Fixnum value
a_fix_arg ends

	  public  alloc_fi
alloc_fi  proc	  near
	  pop	  DI		   ;Pop return address
	  pop	  SI		   ; Pop address of return register
	  pop	  DX		   ; Pop fixnum value
	  sal	  DX,1		   ; Shift out high order bit
	  jo	  a_fix_ov
a_fix_ov:		; Ignore overflow for now (create a bignum later)
	  shr	  DX,1		   ; Position 15 bit quantity
	  mov	  [SI].C_disp,DX   ; Store immediate value into register
	  mov	  [SI].C_page,SPECFIX*2 ; Store immediate tag
	  jmp	  DI		   ;Return
alloc_fi  endp

;************************************************************************
;*		    Copy Variable Length Data Object			*
;*									*
;* Purpose:  To create a copy of a variable length Scheme data object.	*
;*									*
;* Calling Sequence:  copy_blk(&dest, &src)				*
;*			where &dest - address of VM register into which *
;*					pointer to new copy is to be	*
;*					placed				*
;*			      &src  - address of VM register containing *
;*					block to be copied		*
;************************************************************************
cpy_args  struc
	  dw	  ?		   ; caller's BP
	  dw	  ?		   ; caller's ES
	  dw	  ?		   ; return address
cpy_dest  dw	  ?		   ; address of destination register
cpy_src   dw	  ?		   ; address of source register
cpy_args  ends

	  public  copy_blk
copy_blk  proc	  near
	  push	  ES		   ; save caller's ES
	  push	  BP		   ; save caller's BP
	  mov	  BP,SP

;     allocate new block
	  mov	  SI,[BP].cpy_src  ; load address of source register
	  mov	  BX,[SI].C_page   ; load pointer to object to be copied
	  mov	  DI,[SI].C_disp
	  LoadPage ES,BX
;;;	  mov	  ES,pagetabl+[BX]

	  mov	  AX,ES:[DI].vec_len ; load length of object
	  cmp	  AX,0		   ;;; check for small string
	  jge	  copy_010
	  add	  AX,PTRSIZE	   ;;; adjust for small string
	  jmp	  copy_011
copy_010: sub	  AX,BLK_OVHD	   ; adjust size for block header
copy_011: push	  AX		   ; push length of "data" in block

	  xor	  AX,AX 	   ; load type field from source block
	  mov	  AL,ES:[DI].vec_type
	  push	  AX

	  push	  [BP].cpy_dest    ; push address of destination register
	  mov	  AX,DS 	   ; make ES point to the current data
	  mov	  ES,AX 	   ;  segment
	  C_call  alloc_bl	   ; allocate new block
	  mov	  SP,BP 	   ; drop arguments off stack

;     copy contents of source block into newly created block
	  mov	  BX,[BP].cpy_dest ; make ES:[DI] point to newly created
	  mov	  DI,[BX].C_disp   ;  block
	  mov	  BX,[BX].C_page
	  LoadPage ES,BX
;;;	  mov	  ES,pagetabl+[BX]

	  mov	  BX,[BP].cpy_src  ; make DS:[SI] point to source block
	  mov	  SI,[BX].C_disp
	  mov	  BX,[BX].C_page
	  push	  DS
	  LoadPage DS,BX
;;;	  mov	  DS,pagetabl+[BX]

	  mov	  CX,[SI].vec_len  ; load length of source block
	  cmp	  CX,0		   ;;; check for small string
	  jge	  copy_020
	  add	  CX,PTRSIZE
	  jmp	  copy_021
copy_020: sub	  CX,BLK_OVHD	   ;  and subtract off size of block header
copy_021: mov	  DX,CX 	   ; copy length (in bytes) into DX
	  and	  DX,1		   ;  and isolate the lsb
	  shr	  CX,1		   ; convert size from bytes to words

	  add	  SI,BLK_OVHD	   ; advance source/destination pointers
	  add	  DI,BLK_OVHD	   ;  past block header
rep	  movsw 		   ; move contents of source to destination
	  mov	  CX,DX 	   ; copy fixup (in case odd number of bytes)
rep	  movsb 		   ; copy odd byte, if necessary
	  pop	  DS		   ; restore DS

;     return to calling procedure
	  pop	  BP		   ; restore caller's BP
	  pop	  ES		   ; restore caller's ES
	  ret			   ; return
copy_blk  endp

;;;;	 Make sure we haven't overflowed C's runtime stack
;;;	  public  chk_stk
;;;chk_stk	  proc	  near
;;;	  mov	  AX,SP
;;;	  cmp	  AX,_base
;;;	  ja	  chk_ret
;;;	  C_call  gc_on
;;;	  C_call  exit
;;;chk_ret:  ret
;;;chk_stk	  endp

;************************************************************************
;*	      C callable Routine to Take car/cdr of a List		*
;************************************************************************
take_arg  struc
	  dw	  ?		   ; caller's BP
	  dw	  ?		   ; return address
take_reg  dw	  ?		   ; argument register address
take_arg  ends

	  public  take_car
take_car  proc	  near
	  pop	  DX		   ;Pop return address
	  pop	  SI		   ; load argument register address
	  mov	  BX,[SI].C_page   ; load list's page number
	  cmp	  byte ptr ptype+[BX],LISTTYPE*2 ; it is a list, isn't it?
	  jne	  take_err	   ; if not a list, error (jump)
	  mov	  CX,ES 	   ; save caller's ES
	  LoadPage ES,BX
;;;	  mov	  ES,pagetabl+[BX] ; load list's page's paragraph address
	  mov	  BX,[SI].C_disp   ; load list's offset
	  mov	  AL,ES:[BX].car_page ; copy car field of list cell
	  mov	  BX,ES:[BX].car
	  jmp	  short tkmrg
;     ***error-- argument register doesn't contain list-- return nil***
take_err: mov	  [SI].C_page,NIL_PAGE*2
	  mov	  [SI].C_disp,NIL_DISP
	  jmp	  DX		   ; return
take_car  endp

	  public  take_cdr
take_cdr  proc	  near
	  pop	  DX		   ;Pop return address
	  pop	  SI		   ; load argument register address
	  mov	  BX,[SI].C_page   ; load list's page number
	  cmp	  byte ptr ptype+[BX],LISTTYPE*2 ; it is a list, isn't it?
	  jne	  take_err	   ; if not a list, error (jump)
	  mov	  CX,ES 	   ; save caller's ES
	  LoadPage ES,BX
;;;	  mov	  ES,pagetabl+[BX] ; load list's page's paragraph address
	  mov	  BX,[SI].C_disp   ; load list's offset
	  mov	  AL,ES:[BX].cdr_page ; Get cdr field of list cell
	  mov	  BX,ES:[BX].cdr
tkmrg:	  mov	  byte ptr [SI].C_page,AL ; Copy into argument register
	  mov	  [SI].C_disp,BX
	  mov	  ES,CX 	   ; restore caller's ES
	  jmp	  DX		   ; return to caller
take_cdr  endp

			 
IFNDEF PROMEM
      
      ; See PRO2REAL.ASM for protected mode scheme

	  public  pc_type
pc_type   proc	  near
	  push	  BP
	  call	  XPCTYPE	   ; XPCTYPE is located at beginning of this
				   ; program in XPROG, it determines PC type
	  pop	  BP
	  ret
pc_type   endp

	  public  pcinit
	  extrn   XPCINIT:FAR
pcinit	  proc	  near
	  push	  BP
	  call	  XGROUP:XPCINIT   ; XPCINIT is in GRAPHCMD.ASM - in XPROG
				   ; it does special initialization per PC type
				   ; also, it is called from main()
	  pop	  BP
	  ret
pcinit	  endp

      ; See PRO2REAL.ASM for above definitions

ENDIF

;************************************************************************
;*	      Symbol Hashing Routine					*
;*									*
;* Calling Seguence: hash_value = hash(symbol, len);			*
;************************************************************************
	  public  hash
hash	  proc	  near
	  pop	  DI		   ; unload return address
	  pop	  SI		   ; fetch symbol "string" pointer
	  pop	  CX		   ; fetch length
	  xor	  BX,BX 	   ; zero accumulator
	  xor	  AH,AH
hash_1:   lodsb 		   ; fetch next character in symbol name
	  add	  BX,AX 	   ; sum them up
	  loop	  hash_1	   ; iterate 'til symbol used up
	  mov	  AX,BX 	   ; copy sum of chars to AX
	  xor	  DX,DX
	  mov	  BX,HT_SIZE	   ; load divisor with hash table size
	  div	  BX		   ; divide sum
	  mov	  AX,DX
	  jmp	  DI		   ; return to caller
hash	  endp

;************************************************************************
;*	      Symbol Equality Routine					*
;*									*
;*  Calling Sequence:  equal? = sym_eq(page, disp, symbol, len);	*
;************************************************************************
	  public  sym_eq
sym_eq	  proc	  near
	  pop	  DX		   ; unload return address
	  pop	  BX		   ; fetch page number
	  shl	  BX,1		   ;  and adjust for word indexing
	  pop	  DI		   ; fetch displacement
	  pop	  SI		   ; fetch pointer to symbol name
	  pop	  CX		   ; fetch length
	  mov	  AX,ES 	   ; save value of ES
	  LoadPage ES,BX
;;;	  mov	  ES,pagetabl+[BX] ; laod symbol page's paragraph address
	  mov	  BX,ES:[DI].sym_len ; fetch length of symbol
	  sub	  BX,offset sym_data ;	and compute character count
	  cmp	  CX,BX 	   ; length of symbol match?
	  jne	  not_eq	   ; if not same length, jump
	  add	  DI,offset sym_data ; advance symbol pointer to print name
repe	  cmpsb 		   ; compare symbol to name
	  jne	  not_eq	   ; symbols the same? if not, jump
	  mov	  ES,AX 	   ; restore caller's ES register
	  jmp	  DX		   ; return (non-zero value in AX => true)
not_eq:   mov	  ES,AX 	   ; restore caller's ES register
	  xor	  AX,AX 	   ; zero AX (return false value)
	  jmp	  DX		   ; return
sym_eq	  endp
prog	  ends
	  end
