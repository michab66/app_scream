;                                                       =====> SCHEMED.ASM
;***************************************
;*   TIPC Scheme '84 Runtime Support   *
;*  (C) Copyright 1984,1985,1986 by    *
;*   Texas Instruments Incorporated.   *
;*        All rights reserved.         *
;*                                     *
;* Date Written:  April 1984           *
;* Last Modification:  10 Feb 1987     *
;***************************************
          include schemed.equ
          include screen.equ
; Modification History:
;   26 Feb 86 - Modified the initial value of the global variable "listpage"
;    (JCJ)      so that it points to page zero (0) instead of END_LIST.  This
;               causes it to always point to a valid page, thus eliminating
;               one check for each CONS operation.
; rb 5/22/86 - changed debug flag in R2 used as VM starts up;
;              if none, R2=0 (nil), else R2=Scheme 0 (i.e. tagged fixnum zero)
; tc 2/10/87 - Changed page 5 special symbols to for #T instead of #!TRUE
;	       for the R^3 Report.	

;************************************************************************
;*                      Segment Alignment Macro                         *
;*                                                                      *
;* Purpose:  This macro causes "define bytes" to be inserted in the     *
;*              current data section to force the data item which       *
;*              follows it to be aligned on a paragraph boundary.       *
;*                                                                      *
;* Note:  For this macro to work, the current data segment must be      *
;*              aligned on a paragraph boundary.  This is accomplished  *
;*              through the "para" option of the "segment" assembler    *
;*              directive, e.g.,                                        *
;*                                                                      *
;*                      data    segment para public 'DATA'              *
;*                                                                      *
;************************************************************************
align     macro
AL_TMP    =       $ - AL_start     ; get current location
AL_TMP    =       AL_TMP MOD 16    ; isolate low order 4 bits
AL_TMP    =       16 - AL_TMP      ; determine "correction"
AL_TMP    =       AL_TMP MOD 16    ; adjust if already aligned
IF        AL_TMP
          db      AL_TMP dup (0)
ENDIF
          endm

DGROUP    group   data
data      segment para public 'DATA'
          assume  DS:DGROUP
AL_start  equ     $                ; Start of data segment for align macro
 
;;; Page Table - This area of memory holds the table of base
;;;              (paragraph) addresses for each of the page
;;;              frames in Scheme's memory system.

          public  pagetabl
pagetabl  label   word
          dw      page0            ; page 0 - 'nil or cdr nil
          dw      0                ; page 1 - characters (immediates)
          dw      0                ; page 2 - forwarded pointer
          dw      0                ; page 3 - 15-bit fixnums (immediates)
          dw      page4            ; page 4 - special 32-bit flonums
          dw      page5            ; page 5 - special symbols
          dw      page6            ; page 6 - standard port page
          dw      page7            ; page 7 - code for test programs
          dw      page8            ; page 8 - initial environments
				   ; remainder of page table
	  dw	NUMPAGES-PreAlloc dup (0)
 
;     Page Attribute Table - The bits in the following table are
;                  used to indicate the state of each of the pages
;                  in the Scheme memory system.  Only one kind of data
;                  object can be stored in a given page, so a single bit
;                  can be used to classify all references to a page.

          public  attrib,w_attrib
w_attrib  equ     $                ; Special redefinition for C to use as int
attrib    dw      ATOM+READONLY    ; page 0 - 'nil
          dw      ATOM+CHARS+READONLY+NOMEMORY
          dw      NOMEMORY
          dw      ATOM+FIXNUMS+READONLY+NOMEMORY
          dw      ATOM+FLONUMS+READONLY
          dw      ATOM+SYMBOLS+READONLY
          dw      ATOM+PORTS+READONLY
          dw      ATOM+CODE
          dw      ATOM             ; Initial Environments
          dw      NUMPAGES-9 dup (NOMEMORY)

;     Next available location table - The following table contains
;                  the offsets of the next available location which
;                  may be allocated in each page.  A negative value
;                  indicates that the page is full and that no further
;                  allocation is possible within a page.
          public  nextcell
nextcell  dw      8 dup (END_LIST)
          dw      env_nxt-page8    ; Environments page
          dw      NUMPAGES-9 dup (END_LIST)

;     Page link table - Pages which contain data objects of the same
;                  type are linked together via the following table.
          public  pagelink
pagelink  dw      NUMPAGES dup (END_LIST)

;     Page type table - This table holds the "type" of each page for
;                   pointer classification purposes.  The values in
;                   this table may be used as indicies into branch
;                   tables.
          public  ptype
ptype     dw      LISTTYPE*2       ; Page 0 contains list cells
          dw      CHARTYPE*2       ; Page 1 is for character immediates
          dw      FREETYPE*2       ; Page 2 is for "forwarded pointers"
          dw      FIXTYPE*2        ; Page 3 is for fixnum immediates
          dw      FLOTYPE*2        ; Page 4 contains pre-defined flonums
          dw      SYMTYPE*2        ; Page 5 contains pre-defined symbols
          dw      PORTTYPE*2       ; Page 6 contains standard I/O ports
          dw      CODETYPE*2       ; Page 7 contains test programs
          dw      ENVTYPE*2        ; Page 8 contains environments
          dw      NUMPAGES-9 dup (FREETYPE*2) ; Rest of pages not pre-allocated

          public  psize
psize     dw      page0_end-page0  ; Page 0 contains special list cells
          dw      0                ; Page 1 is a tag for immediate characters
          dw      0                ; Page 2 reserved for "forwarded pointers"
          dw      0                ; Page 3 is a tag used for immediate fixnums
          dw      page4_end-page4  ; Page 4 contains pre-defined flonums
          dw      page5_end-page5  ; Page 5 contains pre-defined symbols
          dw      page6_end-page6  ; Page 6 contains standard I/O ports
          dw      page7_end-page7  ; Page 7 contains test programs
          dw      page8_end-page8  ; Page 8 contains environments
          dw      NUMPAGES-9 dup (MIN_PAGESIZE) ; Initialize default page size

;     Table of pages for allocation by type
          public  pagelist,listpage,fixpage,flopage,bigpage,sympage,strpage
          public  vectpage,contpage,clospage,freepage,codepage,refpage,portpage
          public  envpage
pagelist  equ     $
listpage  dw      0                ; [0] Page number for list cell allocation
fixpage   dw      END_LIST         ; [1] Page number for fixnum allocation
flopage   dw      END_LIST         ; [2] Page number for flonum allocation
bigpage   dw      END_LIST         ; [3] Page number for bignum allocation
sympage   dw      END_LIST         ; [4] Page number for symbol allocation
strpage   dw      END_LIST         ; [5] Page number for string allocation
vectpage  dw      END_LIST         ; [6] Page number for vector allocation
contpage  dw      END_LIST         ; [7] Page number for continuation allocation
clospage  dw      END_LIST         ; [8] Page number for closure allocation
freepage  dw      END_LIST         ; [9] Free page list header
codepage  dw      END_LIST         ; [10] Page number for code block allocation
refpage   dw      END_LIST         ; [11] Page number for ref cell allocation
portpage  dw      END_LIST         ; [12] Page number for port allocation
charpage  dw      END_LIST         ; [13] Page number for characters
envpage   dw      ENV_PAGE         ; [14] Page for environments

;     Table of page attributes by data object type
          public  pageattr
pageattr  dw      LISTCELL         ; [0] List cell attributes
          dw      ATOM+FIXNUMS     ; [1] Fixnum attributes
          dw      ATOM+FLONUMS     ; [2] Flonum attributes
          dw      ATOM+BIGNUMS     ; [3] Bignum attributes
          dw      ATOM+SYMBOLS     ; [4] Symbol attributes
          dw      ATOM+STRINGS     ; [5] String attributes
          dw      ATOM+VECTORS     ; [6] Vector (array) attributes
          dw      ATOM+CONTINU     ; [7] Continuation attributes
          dw      ATOM+CLOSURE     ; [8] Closure attributes
          dw      0                ; [9] Free page has no attributes
          dw      ATOM+CODE        ; [10] Code block attributes
          dw      ATOM+REFS        ; [11] Ref cell attributes
          dw      ATOM+PORTS       ; [12] Port attributes
          dw      ATOM+CHARS       ; [13] Character attributes
          dw      ATOM             ; [14] Environment attributes

          public  nextpage,lastpage,nextpara,PAGESIZE
nextpage  dw      9                ; Next unused page number
lastpage  dw	  9		   ; Will hold last page # for ext memory
nextpara  dw      0                ; Next available paragraph number
PAGESIZE  dw      MIN_PAGESIZE

;     Table of bit settings to "or" in
          public  bitable
bitable   dw      08000H,04000H,02000H,01000H,00800H,00400H,00200H,00100H
          dw      00080H,00040H,00020H,00010H,00008H,00004H,00002H,00001H

          public  rtn_name
rtn_name  db      "You didn't use the ENTER macro!",0

;     "Registers" for the Scheme Virtual Machine
          public  nil_reg,regs,reg0,reg0_pag,reg0_dis
nil_reg   dw      NIL_DISP
          dw      NIL_PAGE*2

regs      equ     $
reg0      equ     $                ; Virtual register 0 - always nil
reg0_dis  dw      NIL_DISP
reg0_pag  dw      NIL_PAGE*2

          public  reg1,reg1_pag,reg1_dis
reg1      equ     $                ; Virtual register 1
reg1_dis  dw      UN_DISP
reg1_pag  dw      UN_PAGE*2

          rept    NUM_REGS-2       ; define the VM's remaining registers
          dw      UN_DISP,UN_PAGE*2
          endm

          public  FNV_reg,FNV_pag,FNV_dis
FNV_reg   equ     $                ; Fluid Environment Pointer
FNV_dis   dw      NIL_DISP
FNV_pag   dw      NIL_PAGE*2

          public  GNV_reg,GNV_pag,GNV_dis
GNV_reg   equ     $                ; Global Environment Pointer
GNV_dis   dw      g_env-page8
GNV_pag   dw      ENV_PAGE*2

          public  CB_reg,CB_pag,CB_dis
CB_reg    equ     $                ; Code Base Pointer
CB_dis    dw      0
CB_pag    dw      14

          public  tmp_reg,tmp_page,tmp_disp ; GC'ed temporary register
tmp_reg   equ     $
tmp_disp  dw      NIL_DISP
tmp_page  dw      NIL_PAGE*2
          public  tm2_reg,tm2_page,tm2_disp ; GC'ed temporary register
tm2_reg   equ     $
tm2_disp  dw      NIL_DISP
tm2_page  dw      NIL_PAGE*2
          public  tmp_adr,tm2_adr  ; addresses of temporary registers
tmp_adr   dw      tmp_reg
tm2_adr   dw      tm2_reg

;     Transcript File pointer
          public  TRNS_reg,TRNS_pag,TRNS_dis
TRNS_reg  equ     $
TRNS_dis  dw      NIL_DISP
TRNS_pag  dw      NIL_PAGE*2

;     Storage for interned symbol 'quote
          public  QUOTE_PA,QUOTE_DI
QUOTE_DI  dw      NIL_DISP
QUOTE_PA  dw      NIL_PAGE*2

          public  CONSOLE_,CON_PAGE,CON_DISP ; 'console interned symbol
CONSOLE_  equ     $
CON_DISP  dw      NIL_DISP
CON_PAGE  dw      NIL_PAGE*2


          public  S_pc
S_pc      dw      entry - page7

;      Storage for oblist hash table
          public  hash_pag,hash_dis
hash_pag  db      HT_SIZE dup (0)
hash_dis  dw      HT_SIZE dup (0)

;     Storage for property list hash table
          public  prop_pag,prop_dis
prop_pag  db      HT_SIZE dup (0)
prop_dis  dw      HT_SIZE dup (0)

;     Storage for object hash table
          public  obj_ht
obj_ht    db      OHT_SIZE*3 dup (0)


;     Stack storage (stack buffer)
          public  S_stack
S_stack   db      NIL_PAGE*2       ; caller's code base pointer
          dw      NIL_DISP
          db      SPECFIX*2        ; return address displacement
          dw      0
          db      SPECFIX*2        ; caller's FP
          dw      0
          db      ENV_PAGE*2       ; current heap environment
          dw      g_env-page8
          db      SPECFIX*2        ; static link
          dw      0
          db      NIL_PAGE*2       ; closure pointer ('nil means open call)
          dw      NIL_DISP
STK_HEAD  equ     $-S_stack
db        STKSIZE-STK_HEAD dup (0)

          public  TOS,FP,BASE,PREV_reg,PREV_pag,PREV_dis
TOS       dw      STK_HEAD-PTRSIZE ; current top-of-stack pointer
FP        dw      0                ; current stack frame pointer
BASE      dw      0                ; stack buffer base

PREV_reg  equ     $                ; pointer to previous stack segment
PREV_dis  dw      NIL_DISP
PREV_pag  dw      NIL_PAGE*2

;     State variables for (reset) and (scheme-reset)
          public  FP_save,FNV_save,STL_save,RST_ent,ERR_ent
FP_save   dw      0                ; save area for nominal stack
FNV_save  dw      NIL_DISP,NIL_PAGE*2 ; fluid enviornment pointer save area
STL_save  dw      NIL_DISP,NIL_PAGE*2 ; scheme-top-level value save area
RST_ent   dw      reset_x - page7  ; entry point for reset code
ERR_ent   dw      err_rtn - page7  ; entry point for error handler invocation

;     Flags for VM Control
          public  PC_MAKE,VM_debug,s_break
PC_MAKE   dw      1                ; PC's manufacturer flag
VM_debug  dw      0                ; flag indicating VM_debug mode
s_break   dw      0                ; flag indicating shift-break key depressed

;     Current port
          public  iooffs,ioseg
iooffs    dw      0
ioseg     dw      0

;     Stack pointer for abort
          public  abadr
abadr     dw      0

;     Special storage for nil
          align
          public  page0
page0     db      NIL_PAGE*2       ; Special constant:  (cons nil nil)
          dw      NIL_DISP
          db      NIL_PAGE*2
          dw      NIL_DISP
page0_end equ     $                ; end of Page 0

;     Special 32-bit floating point constants area
          align
          public  page4
page4     db      FLOTYPE,00,00,00,00,00,00,0F0H,0BFH ;-1.0
          db      FLOTYPE,00,00,00,00,00,00,00,00     ; 0.0
          db      FLOTYPE,00,00,00,00,00,00,0F0H,03FH ; 1.0
page4_end equ     $                ; end of Page 4

;     Define symbol constant
symbol    MACRO   str
          local   x,y
x         db      SYMTYPE          ; tag
          dw      y-x              ; length field
          db      NIL_PAGE*2       ; link field page number - initially null
          dw      NIL_DISP         ; link field displacement - initially null
          db      0                ; hash key - 0 for "special symbols"
          db      str              ; character data
y         equ     $
          endm

;     Special storage for single character symbols
          align
          public  page5
page5     equ     $
t_symbol  equ     $
          symbol  "#T"             ; #T for #!true for 't for true
          symbol  "#!UNASSIGNED"   ; the proverbial undefined value
          symbol  "#!NOT-A-NUMBER" ; undefined result of arithmetic
eof_sym   equ     $
          symbol  "#!EOF"          ; end-of-file indicator
non_prt   equ     $
          symbol  "#!UNPRINTABLE"  ; value of *the-non-printing-object*

page5_end equ     $                ; end of Page 5

          align
          public  page6
page6     equ     $

BUFFSIZE  equ     256              ; buffer size

;     Standard Input Port (for now, a file)
stdinp    db      PORTTYPE         ; tag=PORT
          dw      stdinp_-stdinp   ; length of object in bytes
          db      0,0,0            ; null pointer
          dw      03Eh             ; flags (r/w,window,open,transcript,binary)
          dw      0                ; handle (stdin CON)
          dw      0                ; cursor line
          dw      0                ; cursor column
          dw      0                ; upper left line
          dw      0                ; upper left column
          dw      DEFAULT_NUM_ROWS ; number of lines
          dw      DEFAULT_NUM_COLS ; number of columns
          dw      -1               ; border attributes (none)
          dw      000FH            ; text attributes (white, enable)
          dw      1                ; window flags (wrap)
          dw      0                ; current buffer position
          dw      0                ; current end of buffer
          db      BUFFSIZE dup (0) ; input buffer
          db      "CON"            ; pathname
stdinp_   equ     $

;   The following point object is now used for the pcs-status-window
stdoutp   db      PORTTYPE         ; tag=PORT
          dw      stdoutp_-stdoutp ; length of object in bytes
          db      0,0,0            ; null pointer
          dw      02Eh             ; flags (r/w,window,open,no transcript,bin)
          dw      1                ; handle (stdout CON)
          dw      0                ; cursor line
          dw      0                ; cursor column
          dw      DEFAULT_NUM_ROWS - 1 ; upper left line
          dw      0                ; upper left column
          dw      1                ; number of lines
          dw      DEFAULT_NUM_COLS ; number of columns
          dw      -1               ; border attributes (none)
          dw      001CH            ; text attrs (reverse video, green, enable)
          dw      1                ; window flags (wrap)
          dw      0                ; current buffer position
          dw      0                ; current end of buffer
          db      BUFFSIZE dup (0) ; output buffer
          db      "CON"            ; pathname
stdoutp_  equ     $
page6_end equ     $                ; end of Page 6

fxn       MACRO   val
          db      SPECFIX*2
          dw      val
          endm

;     Environments
          align
          public  page8
ENV_PAGE  equ     8
page8     equ     $
;     define USER-GLOBAL-ENVIRONMENT
g_env     db      ENVTYPE
          dw      (HT_SIZE*3)+BLK_OVHD+PTRSIZE
          db      0,0,0            ; parent pointer (there is no parent)
          db      HT_SIZE*3 dup (0)
;     define USER-INITIAL-ENVIRONMENT
u_env     db      ENVTYPE
          dw      (HT_SIZE*3)+BLK_OVHD+PTRSIZE
          db      ENV_PAGE*2
          dw      g_env-page8
          db      HT_SIZE*3 dup (0)
env_nxt   equ     $
;;;       dw      MIN_PAGESIZE-(env_nxt-page8)
;;;       db      MIN_PAGESIZE-($-page8) dup (0)
page8_siz equ     (env_nxt-page8)+(1*ENV_SIZE) ;allow room for 1 environment
          db      FREETYPE
          dw      page8_siz-(env_nxt-page8)
          db      page8_siz-($-page8) dup (0)
page8_end equ     $

;     Assembly area for test programs
          include sasm.mac
          align
          public  page7
page7     equ     $
          db      CODETYPE         ; Block header
          dw      firstend-page7
          db      SPECFIX*2        ; Code starting offset
          dw      entry-page7
;     Constant (pointers) go here
cstart    equ     *
CSTL      equ     0
          db      0,0,0            ; "scheme-top-level" symbol goes here
CREAD     equ     1
          db      0,0,0            ; "read" symbol goes here
CEOF      equ     2
          db      0,0,0            ; interned "eof" symbol goes here
CINP      equ     3
          db      0,0,0            ; interned "input-port" symbol goes here
COUTP     equ     4
          db      0,0,0            ; interned "output-port" symbol goes here
CCONS     equ     5
          db      0,0,0            ; interned "console" symbol goes here
CNO_PRT   equ     6
          db      0,0,0            ; interned "*the-non-printing-object*" sym
CUGENV    equ     7
          db      0,0,0            ; interned "user-global-environment" sym
CUIENV    equ     8
          db      0,0,0            ; interned "user-initial-environment" sym
ERR_NAME  equ     9
          db      0,0,0            ; interned "*error-handler*" symbol
CWHO      equ     10
          db      0,0,0            ; interned "pcs-status-window"
T_        equ     11
          db      0,0,0            ; interned "t"
NIL_      equ     12
          db      0,0,0            ; interned "nil"
ENGINE_   equ     13
          db      0,0,0            ; interned "PCS-KILL-ENGINE"
CEOFX     equ     14
          db      SPECSYM*2        ; special non-interned "eof" symbol
          dw      eof_sym-page5
CNO_PRTX  equ     15
          db      SPECSYM*2        ; special non-interned "#!unprintable" sym
          dw      non_prt-page5
CUGENVX   equ     16
          db      ENV_PAGE*2       ; pointer to user-global-environment
          dw      g_env-page8
CUIENVX   equ     17
          db      ENV_PAGE*2       ; pointer to user-initial-environment
          dw      u_env-page8
CWHOX     equ     18
          db      SPECPOR*2        ; pointer to "who-line" window object
          dw      stdoutp-page6
CT_       equ     19
          db      SPECSYM*2        ; pointer to #!true
          dw      t_symbol-page5
;     Entry point follows
entry     equ       $
;         STRINGP_  R2          ; second input argument specified?
          JNIL_S_   R2,no_debug ; if not, don't begin debug (jump)
          DEBUG_                ; initiate debug mode
no_debug  equ       $

;     define "eof"
          LD_CON_   R63,CEOFX
          DEFINE_   R63,CEOF
;     define "*the-non-printing-object*" to "#!unprintable"
          LD_CON_   R63,CNO_PRTX
          DEFINE_   R63,CNO_PRT
;     define "user-global-environment" to point to said
          LD_CON_   R63,CUGENVX
          DEFINE_   R63,CUGENV
;     define "user-initial-environment" to point to said
          LD_CON_   R63,CUIENVX
          DEFINE_   R63,CUIENV
;     define "who-line"
          LD_CON_   R63,CWHOX
          DEFINE_   R63,CWHO
;     (define t #!true)
          LD_CON_   R63,CT_
          DEFINE_   R63,T_
;     (define nil '())
          DEFINE_   R0,NIL_
;     fluid-bind "input-port", "output-port" to 'console
          LD_CON_   R63,CCONS
          BIND_FL_  CINP,R63
          BIND_FL_  COUTP,R63
;     fluid-bind "scheme-top-level" to nil
          BIND_FL_  CSTL,R0
;     establish the default error handler
          LD_CON_   R63,ERR_NAME
          CLOSE_    R63,err_dflt,0
          DEFINE_   R63,ERR_NAME
;     establish the default PCS-KILL-ENGINE
          LD_CON_   R63,ENGINE_
          CLOSE_    R63,ret_dflt,0
          DEFINE_   R63,ENGINE_

;     check the input parameter to see if it's a filename
          FASL_     R1             ; fast load first program unit
next_rd   equ       $
          COPY_     R8,R0
          FASL_     R8
          LD_CON_   R9,CEOFX
          JEQ_S_    R9,R8,end_rd
          PUSH_     R8             ; save program just read
          EXECUTE_  R1             ; execute the previously read program
          POP_      R1             ; restore pointer to most recently read pgm
          JMP_S_    next_rd        ; see if more procedures follow
end_rd    equ       $
          EXECUTE_  R1             ; Load program-Create the closure
          COPY_     R2,R1          ; Copy returned value to R2
          SYMBOLP_  R2             ; Was a symbol returned?
          JNIL_S_   R2,not_sym     ; If not, don't try to look it up
          COPY_     R2,R1
          FLUID_P_  R2
          JNIL_S_   R2,glob_sym
          LD_FL_R_  R1,R1
          JMP_S_    not_sym
glob_sym  equ       $
          LD_GL_R_  R1,R1          ; Look up symbol in global environment
not_sym   equ       $
          COPY_     R2,R1
          CLOSURP_  R2
          JNIL_S_   R2,not_clos
          CALL_CL_  R1,0           ; Execute the closure
not_clos  equ       $
          LD_NIL_   R2
          PRINT_    R1,R2          ; Print the result (if any)

          HALT_

;     Reset Code
          S_RESET_                 ; debugger entry for forced reset
reset_x   equ       $
          LD_GLOBAL_ R1,ENGINE_    ; call PCS-KILL-ENGINE
          CALL_CL_   R1,0
          CLR_REG_                 ; clear all registers
          LD_FLUID_ R1,CSTL        ; load value for 'scheme-top-level
          CALL_CL_  R1,0           ; call said closure
          JMP_S_    reset_x        ; if control returns, reset again

;     Error Handler Invocation
err_rtn   equ       $
reg_ctr   =         R1
          rept      NUM_REGS-1
          PUSH_     reg_ctr
reg_ctr   =         reg_ctr+4
          endm
          LD_GLOBAL_ R1,err_name
          CALL_CL_  R1,0
reg_ctr   =         (NUM_REGS-1)*4
          rept      NUM_REGS-1
          POP_      reg_ctr
reg_ctr   =         reg_ctr-4
          endm
          EXIT_
err_dflt  equ       $
          DEBUG_
ret_dflt  equ       $
          EXIT_

firstend  equ       $              ; end of first code block
page7_end equ       $
data      ends
          end

