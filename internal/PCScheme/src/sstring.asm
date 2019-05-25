;							=====> SSTRING.ASM
;************************************************************************
;*     		     TIPC Scheme  Runtime Support    			*
;*   		   Interpreter -- String Operations  			*
;*				       					*
;*    	(C) Copyright 1985 by Texas Instruments Incorporated.       	*
;*	  		All rights reserved.	       			*
;*				       					*
;* Date Written:  18 January 1985      					*
;* Last Modification:		       					*
;*  4/27/88 (tc) - removed case conversion from characters in the range *
;*		   of 128 through 167 (see locases, hicases, collate).  *
;*		   Our previous assumptions did not work for some inter-*
;*		   national character sets.				*
;************************************************************************
	  include scheme.equ
	  include sinterp.mac

	  include sinterp.arg

DGROUP	  group   data
data	  segment word public 'DATA'
	  assume  DS:DGROUP

;     Local data definitions
m_ch_eq   db	  "CHAR=?",0
m_ceq_ci  db	  "CHAR-CI=?",0
m_ch_lt   db	  "CHAR<?",0
m_chl_ci  db	  "CHAR-CI<?",0
m_ch_up   db	  "CHAR-UPCASE",0
m_ch_dwn  db	  "CHAR-DOWNCASE",0
m_mk_str  db	  "MAKE-STRING",0
m_st_fl   db	  "FILL-STRING!",0
m_st_ref  db	  "STRING-REF",0
m_st_set  db	  "STRING-SET!",0
m_one	  dw	  1		   ; a constant "one" (1)
m_two	  dw	  2		   ; a constant "two" (2)
m_soff	  dw	  STRING_OFFSET_ERROR ; error code

;     Case tables (for characters between 40h and 0bfh)
	  public  locases,hicases,collate

locases   db	  000,001,002,003,004,005,006,007
	  db	  008,009,010,011,012,013,014,015
	  db	  016,017,018,019,020,021,022,023
	  db	  024,025,026,027,028,029,030,031
	  db	  " ","!",'"',"#","$","%","&","'"
	  db	  "(",")","*","+",",","-",".","/"
	  db	  "0","1","2","3","4","5","6","7"
	  db	  "8","9",":",";","<","=",">","?"

	  db	  "@","a","b","c","d","e","f","g"
	  db	  "h","i","j","k","l","m","n","o"
	  db	  "p","q","r","s","t","u","v","w"
	  db	  "x","y","z","[","\","]","^","_"
	  db	  "`","a","b","c","d","e","f","g"
	  db	  "h","i","j","k","l","m","n","o"
	  db	  "p","q","r","s","t","u","v","w"
	  db	  "x","y","z","{","|","}","~",127
						  ; C   ..  '   ^   ..  `   o   c
						  ; '   u   e   a   a   a   a   '
	  db	  128,129,130,131,132,133,134,135 ;135,129,130,131,132,133,134,135
						  ; ^   ..  `   ..  ^   `   ..  o
						  ; e   e   e   i   i   i   A   A
	  db	  136,137,138,139,140,141,142,143 ;136,137,138,139,140,141,132,134
						  ; '           ^   ..  `   ^   `
						  ; E   ae  AE  o   o   o   u   u
	  db	  144,145,146,147,148,149,150,151 ;130,145,145,147,148,149,150,151
						  ; ..  ..  ..
						  ; y   O   U   (currency symbols)
	  db	  152,153,154,155,156,157,158,159 ;152,148,129,155,156,157,158,159
						  ; '   '   '   '   ~   ~
						  ; a   i   o   u   n   N
	  db	  160,161,162,163,164,165,166,167 ;160,161,162,163,164,164,166,167
	  db	  168,169,170,171,172,173,174,175
	  db	  176,177,178,179,180,181,182,183
	  db	  184,185,186,187,188,189,190,191

	  db	  192,193,194,195,196,197,198,199
	  db	  200,201,202,203,204,205,206,207
	  db	  208,209,210,211,212,213,214,215
	  db	  216,217,218,219,220,221,222,223
;		      beta
	  db	  224,225,226,227,228,229,230,231
	  db	  232,233,234,235,236,237,238,239
	  db	  240,241,242,243,244,245,246,247
	  db	  248,249,250,251,252,253,254,255

hicases   db	  000,001,002,003,004,005,006,007
	  db	  008,009,010,011,012,013,014,015
	  db	  016,017,018,019,020,021,022,023
	  db	  024,025,026,027,028,029,030,031
	  db	  " ","!",'"',"#","$","%","&","'"
	  db	  "(",")","*","+",",","-",".","/"
	  db	  "0","1","2","3","4","5","6","7"
	  db	  "8","9",":",";","<","=",">","?"

	  db	  "@","A","B","C","D","E","F","G"
	  db	  "H","I","J","K","L","M","N","O"
	  db	  "P","Q","R","S","T","U","V","W"
	  db	  "X","Y","Z","[","\","]","^","_"
	  db	  "`","A","B","C","D","E","F","G"
	  db	  "H","I","J","K","L","M","N","O"
	  db	  "P","Q","R","S","T","U","V","W"
	  db	  "X","Y","Z","{","|","}","~",127
						  ; C   ..  '   ^   ..  `   o   c
						  ; '   u   e   a   a   a   a   '
	  db	  128,129,130,131,132,133,134,135 ;128,154,144,"A",142,"A",143,128
						  ; ^   ..  `   ..  ^   `   ..  o
						  ; e   e   e   i   i   i   A   A
	  db	  136,137,138,139,140,141,142,143 ;"E","E","E","I","I","I",142,143
						  ; '           ^   ..  `   ^   `
						  ; E   ae  AE  o   o   o   u   u
	  db	  144,145,146,147,148,149,150,151 ;144,146,146,"O",153,"O","U","U"
		  				  ;..  ..  ..
		   				  ; y   O   U   (currency symbols)
	  db	  152,153,154,155,156,157,158,159 ;"Y",153,154,155,156,157,158,159
		   				  ; '   '   '   '   ~   ~
		   				  ; a   i   o   u   n   N
	  db	  160,161,162,163,164,165,166,167 ;"A","I","O","U",165,165,166,167
	  db	  168,169,170,171,172,173,174,175
	  db	  176,177,178,179,180,181,182,183
	  db	  184,185,186,187,188,189,190,191

	  db	  192,193,194,195,196,197,198,199
	  db	  200,201,202,203,204,205,206,207
	  db	  208,209,210,211,212,213,214,215
	  db	  216,217,218,219,220,221,222,223
;		      beta
	  db	  224,225,226,227,228,229,230,231
	  db	  232,233,234,235,236,237,238,239
	  db	  240,241,242,243,244,245,246,247
	  db	  248,249,250,251,252,253,254,255

collate   db	  000,001,002,003,004,005,006,007
	  db	  008,009,010,011,012,013,014,015
	  db	  016,017,018,019,020,021,022,023
	  db	  024,025,026,027,028,029,030,031
	  db	  " ","!",'"',"#","$","%","&","'"
	  db	  "(",")","*","+",",","-",".","/"
	  db	  "0","1","2","3","4","5","6","7"
	  db	  "8","9",":",";","<","=",">","?"

	  db	  "@","A","B","C","D","E","F","G"
	  db	  "H","I","J","K","L","M","N","O"
	  db	  "P","Q","R","S","T","U","V","W"
	  db	  "X","Y","Z","[","\","]","^","_"
	  db	  "`","a","b","c","d","e","f","g"
	  db	  "h","i","j","k","l","m","n","o"
	  db	  "p","q","r","s","t","u","v","w"
	  db	  "x","y","z","{","|","}","~",127
		   				  ; C   ..  '   ^   ..  `   o   c
		   				  ; '   u   e   a   a   a   a   '
	  db	  128,129,130,131,132,133,134,135 ;"C","u","e","a","a","a","a","c"
		   				  ; ^   ..  `   ..  ^   `   ..  o
		   				  ; e   e   e   i   i   i   A   A
	  db	  136,137,138,139,140,141,142,143 ;"e","e","e","i","i","i","A","A"
		   				  ; '           ^   ..  `   ^   `
		   				  ; E   ae  AE  o   o   o   u   u
	  db	  144,145,146,147,148,149,150,151 ;"E","a","A","o","o","o","u","u"
		   				  ; ..  ..  ..
		   				  ;y   O   U   (currency symbols)
	  db	  152,153,154,155,156,157,158,159 ;"y","O","U","$","$","$","$","$"
				   		  ; '   '   '   '   ~   ~
		   				  ; a   i   o   u   n   N
	  db	  160,161,162,163,164,165,166,177 ;"a","i","o","u","n","N",166,167
	  db	  168,169,170,171,172,173,174,175 ;"?",169,170,171,172,"!",'"','"'
	  db	  176,177,178,179,180,181,182,183
	  db	  184,185,186,187,188,189,190,191

	  db	  192,193,194,195,196,197,198,199
	  db	  200,201,202,203,204,205,206,207
	  db	  208,209,210,211,212,213,214,215
	  db	  216,217,218,219,220,221,222,223
;		      beta
	  db	  224,"s",226,227,228,229,230,231
	  db	  232,233,234,235,236,237,238,239
	  db	  240,241,242,243,244,245,246,247
	  db	  248,249,250,251,252,253,254,255

data	  ends


PGROUP	  group   prog
prog	  segment byte public 'PROG'
	  assume  CS:PGROUP

str_int   proc	  near

;     Entry points defined in "sinterp.asm"
	  extrn   next:near	   ; Top of interpreter
	  extrn   next_PC:near	   ; Reload ES,SI at top of interpreter
	  extrn   next_SP:near	   ; mov SP,BP before next_PC
	  extrn   src_err:near	   ; Source (operand) error print routine
	  extrn   sch_err:near	   ; link to Scheme debugger

char_cmp  macro   comparison,case,operation
	  local   w,x,y,z
	  lods	  word ptr ES:[SI] ; load operands
	  xor	  BX,BX
	  mov	  BL,AL 	   ; copy the destination=source1 register
	  mov	  DI,BX 	   ;  number, copy into DI, and compute
	  add	  DI,offset reg0   ;  the register's address
	  mov	  BL,AH 	   ; copy the source2 register number
	  mov	  AL,byte ptr reg0_pag+[BX] ; load tag of src2 operand
	  cmp	  AL,SPECCHAR*2    ; is source2 a character?
	  jne	  z		   ; if not, error (jump)
	  cmp	  AL,byte ptr [DI].C_page ; is source1 a character?
	  jne	  z		   ; if not, error (jump)
IFIDN	  <case>,<CI>
	  mov	  AL,byte ptr reg0_dis+[BX] ; move character value of source2
	  mov	  BX,offset locases  ; Fetch lower-case equivalents
	  xlat
	  mov	  AH,AL 	   ; Save equivalent in AH
	  mov	  AL,byte ptr[DI].C_disp    ; move char value of source1
	  xlat			   ; Fetch lower-case eqivalent
ELSE
	  mov	  AL,byte ptr [DI].C_disp   ; Fetch characters
	  mov	  AH,byte ptr reg0_dis+[BX]
ENDIF
	  mov	  BX,offset collate ; Get collation values of chars
	  xlat
	  xchg	  AL,AH
	  xlat
	  cmp	  AH,AL 	   ; Compare
	  j&comparison	  y	   ; jump, if test is satisfied
	  xor	  AX,AX 	   ; place 'nil in destination
	  mov	  byte ptr [DI].C_page,AL ;  register
	  mov	  [DI].C_disp,AX
	  jmp	  next		   ; return to interpreter
y:	  mov	  byte ptr [DI].C_page,T_PAGE*2 ; place 't in
	  mov	  [DI].C_disp,T_DISP ;	destination register
	  jmp	  next		   ; return to interpreter
;     ***error-- one (or both) operands aren't characters***
z:	  mov	  AX,offset operation
IFIDN	  <operation>,<m_ch_eq>
error_2:  add	  BX,offset reg0   ; compute address of source 2
	  pushm	  <BX,DI,m_two,AX> ; push source 2, source 1, operation name
	  C_call  set_src_,<SI>,Load_ES ; call:  set_source_error
	  jmp	  sch_err	   ; link to Scheme debugger
ELSE
	  jmp	  error_2
ENDIF
	  endm


;************************************************************************
;*							       AL   AH	*
;* (char-= char1 char2) 			  char-=       dest,src *
;*									*
;* Purpose:  Scheme interpreter support for comparing the equality of	*
;*		character data objects. 				*
;*									*
;* Description:  The tags (page numbers) or the objects are compared	*
;*		for equality.  If they are not equal, at least one of	*
;*		the operands is not a character, and an error is	*
;*		signaled.  If they are equal, a second check to make	*
;*		sure both are characters is performed.			*
;*									*
;*		After validating the tag fields, the displacement fields*
;*		are compared for equality.  If they are identical, the	*
;*		characters are equal and 't is returned in the destina- *
;*		tion register.	If not equal, 'nil is returned in the   *
;*		destination register.					*
;************************************************************************
	  public  ch_eq_p
ch_eq_p:  char_cmp e,CS,m_ch_eq

;************************************************************************
;*							       AL   AH	*
;* (char-equal? char1 char2)			char-eq?       dest,src *
;*									*
;* Purpose:  Scheme interpreter support for comparing the equality of	*
;*		character data objects ignoring case.			*
;*									*
;* Description:  The tags (page numbers) or the objects are compared	*
;*		for equality.  If they are not equal, at least one of	*
;*		the operands is not a character, and an error is	*
;*		signaled.  If they are equal, a second check to make	*
;*		sure both are characters is performed.			*
;*									*
;*		The displacements of both operands are loaded and	*
;*		mapped to uppercase.  They are then compared for	*
;*		equality.  If equal, 't is returned in the destination  *
;*		registers.  Otherwise, 'nil is returned.                *
;************************************************************************
	  public  ch_eq_ci
ch_eq_ci: char_cmp e,CI,m_ceq_ci

;************************************************************************
;*							       AL   AH	*
;* (char-<char1 char2)				char-<	       dest,src *
;************************************************************************
	  public  ch_lt_p
ch_lt_p:  char_cmp b,CS,m_ch_lt

;************************************************************************
;*							       AL   AH	*
;* (char-less? char1 char2)			char-less?     dest,src *
;************************************************************************
	  public  ch_lt_ci
ch_lt_ci: char_cmp b,CI,m_chl_ci

	  purge   char_cmp

ch_case   macro   direction,name
	  local   y
	  lods	  byte ptr ES:[SI]
	  mov	  DI,AX
	  add	  DI,offset reg0
	  cmp	  byte ptr [DI].C_page,SPECCHAR*2 ; is input char?
	  jne	  y		   ; if not a character, error (jump)
	  mov	  AL,byte ptr [DI].C_disp  ; Put char in AL
IFIDN	  <direction>,<UP>
	  mov	  BX,offset hicases
ELSE
IFIDN	  <direction>,<DOWN>
	  mov	  BX,offset locases
ELSE
	  ***error***	Invalid: direction
ENDIF
ENDIF
	  xlat			    ; Fetch alternate case
	  mov	  byte ptr [DI].C_disp,AL ;  and store into dest register
	  jmp	  next
;     ***error-- invalid source operand***
y:	  mov	  AX,offset name   ; load the instruction's name text
IFIDN	  <direction>,<UP>
error_1:  pushm	  <DI,m_one,AX>    ; push operand, operand count, instr. name
	  C_call  set_src_,<SI>,Load_ES ; call set_source_error
	  jmp	  sch_err	   ; link to Scheme debugger
ELSE
	  jmp	  error_1	   ; jump to error routine
ENDIF
	  endm

;************************************************************************
;*							       AL	*
;* (char-upcase char)				char-upcase    dest	*
;*									*
;* Purpose:  Scheme interpreter support for conversion of characters	*
;*		to uppercase						*
;************************************************************************
	  public  ch_up
ch_up:	  ch_case UP,m_ch_up

;************************************************************************
;*							       AL	*
;* (char-downcase char) 			char-downcase  dest	*
;*									*
;* Purpose:  Scheme interpreter support for conversion of characters	*
;*		to lowercase						*
;************************************************************************
	  public  ch_down
ch_down:  ch_case DOWN,m_ch_dwn

	  purge   ch_case

;************************************************************************
;*								AL  AH	*
;* (make-string len init)			make-string	len,init*
;************************************************************************
	  public  make_str
make_str: lods	  word ptr ES:[SI] ; load the operands of the instruction
	  save	  <AX,SI>	   ; save the operands and location pointer
	  xor	  BX,BX
	  mov	  BL,AL 	   ; copy the destination register number
	  add	  BX,offset reg0   ;  into BX and compute its address
	  cmp	  byte ptr [BX].C_page,SPECFIX*2 ; is length a fixnum?
	  jne	  mk_st_er	   ; if not, error (jump)
	  mov	  AX,[BX].C_disp   ; load the immediate value for the size
	  shl	  AX,1		   ;  and sign extend it
	  sar	  AX,1
	  jl	  mk_st_er	   ; if size is negative, error
	  mov	  CX,STRTYPE	   ; load the tag value for the string object
	  pushm   <AX,CX,BX>	   ; push arguments to "alloc_block"
	  C_call  alloc_bl,,Load_ES ; call: alloc_block(reg, STRTYPE, len)
	  pop	  BX		   ; restore destination register address
	  mov	  DI,[BX].C_disp   ; load a pointer to the newly allocated
	  mov	  BX,[BX].C_page   ;  string object
	  LoadPage ES,BX
;;;	  mov	  ES,pagetabl+[BX]
	  restore <AX>		   ; reload operands to instruction
	  mov	  BL,AH 	   ; copy initial value register number
	  mov	  AL,byte ptr reg0_pag+[BX] ; load page number of init value
	  cmp	  AL,SPECCHAR*2    ; init value a character?
	  je	  st_fl_3	   ; if a character, jump
	  cmp	  AL,NIL_PAGE*2    ; use default value? (nil?)
	  jne	  mk_st_er	   ; if not nil, error (jump)
	  mov	  AL," "           ; load default string fill character
	  jmp	  short st_fl_4
mk_st_er: lea	  BX,m_mk_str	   ; load address of "make-string" text
	  jmp	  src_err	   ; display "source operand error" message

;************************************************************************
;*							      AL  AH	*
;* (string-fill! string char)			string-fill!  str,char	*
;************************************************************************
	  public  str_fill
str_fill: lods	  word ptr ES:[SI] ; load string-fill operands
	  save	  <SI>		   ; save current location pointer
	  xor	  BX,BX
	  mov	  BL,AL 	   ; copy string register number
	  mov	  DI,BX
	  mov	  BL,byte ptr reg0_pag+[DI]
	  cmp	  byte ptr ptype+[BX],STRTYPE*2 ; is 1st operand a string?
	  jne	  st_fl_er	   ; if not, error (jump)
	  LoadPage ES,BX
;;;	  mov	  ES,pagetabl+[BX] ; load a pointer to the string object
	  mov	  DI,reg0_dis+[DI]
	  mov	  BL,AH 	   ; copy initialization value register number
	  cmp	  reg0_pag+[BX],SPECCHAR*2 ; is it a char?
	  jne	  st_fl_er	   ; if not, error
st_fl_3:  mov	  AL,byte ptr reg0_dis+[BX] ; load initialization character
st_fl_4:  mov	  CX,ES:[DI].str_len ; load length of string object
	  cmp	  CX,0		   ;;; check for small string
	  jge	  st_010
	  add	  CX,BLK_OVHD+PTRSIZE  ;;; adjust for small string
st_010:	  sub	  CX,offset str_data ; compute number of characters
	  add	  DI,offset str_data ; advance index to 1st character position
rep	  stosb 		   ; fill string object with init character
	  jmp	  next_SP	   ; return to interpreter
st_fl_er: lea	  BX,m_st_fl	   ; load address of "fill-string" text
	  jmp	  src_err	   ; display "source operand error" message

str_int   endp

;************************************************************************
;*		Macro Support for String ref/set			*
;************************************************************************
st_thing  macro   ref_or_set,message
	  local   w,x,y,z
	  lods	  word ptr ES:[SI] ; load string pointer and index regs
	  xor	  BX,BX
	  mov	  BL,AL 	   ; copy string/dest reg number into DI
	  mov	  DI,BX
IFIDN	  <ref_or_set>,<SET>
	  lods	  byte ptr ES:[SI] ; load source operand register number
	  mov	  DL,AL 	   ;  and save it in TIPC register DL
ENDIF
	  save	  <SI>		   ; save the location pointer
	  mov	  BL,byte ptr reg0_pag+[DI] ; load string page number
	  cmp	  byte ptr ptype+[BX],STRTYPE*2 ; is it a string?
	  jne	  y		   ; if not a string, error (jump)
	  LoadPage ES,BX
;;;	  mov	  ES,pagetabl+[BX]
	  mov	  BL,AH 	   ; copy index register number
	  cmp	  byte ptr reg0_pag+[BX],SPECFIX*2 ; is index a fixnum?
	  jne	  y		   ; if not a fixnum, error (jump)
	  mov	  AX,reg0_dis+[BX] ; load immediate value and
	  shl	  AX,1		   ;  sign extend to 16 bits
	  sar	  AX,1
	  jl	  z		   ; if index negative, error (jump)
	  add	  AX,offset str_data ; advance pointer past string header
	  mov	  SI,reg0_dis+[DI] ; load pointer to string object
	  mov	  CX,ES:[SI].str_len ; compare index with string length
	  cmp	  CX,0		   ;;; check for small string
	  jge	  w
	  add	  CX,BLK_OVHD+PTRSIZE  ;;; adjust for small string
w:	  cmp	  AX,CX
	  jge	  z		   ; if index too large, error (jump)
	  add	  SI,AX 	   ; add index to string pointer
IFIDN	  <ref_or_set>,<REF>
	  mov	  byte ptr reg0_pag+[DI],SPECCHAR*2 ; set tag=character
	  mov	  BL,ES:[SI]	   ; fetch desired character
	  mov	  reg0_dis+[DI],BX ;  and store into destination register
ELSE
IFIDN	  <ref_or_set>,<SET>
	  mov	  BL,DL 	   ; copy source value register number
	  cmp	  byte ptr reg0_pag+[BX],SPECCHAR*2 ; is source a character?
	  jne	  y		   ; if not a character, error (jump)
	  mov	  AL,byte ptr reg0_dis+[BX] ; store source character into
	  mov	  ES:[SI],AL	   ;  string at desired offset
ELSE
	  ***error***  Invalid:  ref_or_set
ENDIF
ENDIF
	  jmp	  next_PC	   ; return to interpreter
;     ***error-- invalid source operand***
y:	  lea	  BX,message	   ; load address of operation name text
	  jmp	  src_err	   ; display "source operand error" message
;     ***error-- invalid string offset***
z:	  mov	  BX,offset message ; load address of instruction name
IFIDN	  <ref_or_set>,<REF>
	  mov	  DX,3		   ; STRING-REF is 3 bytes long
s_out_bn: restore <SI>		   ; load location pointer and
	  sub	  SI,DX		   ;  back up to start of instruction in error
	  pushm	  <SI,BX>	   ; push instruction's offset, name
	  C_call  disassem,,Load_ES ; disassemble instruction for *irritant*
	  pushm	  <tmp_adr,m_soff,m_one> ; push args to "set_numeric_error"
	  C_call  set_nume	   ; set_numeric_error(1,ST_OFF_ERR,tmp_reg);
	  restore <SI>		   ; load offset of next instruction
	  jmp	  sch_err	   ; Link to Scheme debugger
ELSE
	  mov	  DX,4		   ; STRING-SET! is 4 bytes long
	  jmp	  s_out_bn	   ; continue error processing
ENDIF
	  endm

;************************************************************************
;*							      AL  AH	*
;* (string-ref string index)			string-ref    str,index *
;************************************************************************
	  public  st_ref
st_ref:   st_thing REF,m_st_ref

;************************************************************************
;*							AL  AH	  AL	*
;* (string-set! string index char)	string-set!	str,index,char	*
;************************************************************************
	  public  st_set
st_set:   st_thing SET,m_st_set

	  purge   st_thing

prog	  ends
	  end
