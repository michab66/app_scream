;							=====> CPRINT1.ASM
;***************************************
;*     TIPC Scheme  Runtime Support    *
;*	  S-Expression printing        *
;*				       *
;*    (C) Copyright 1985 by Texas      *
;*     Instruments Incorporated.       *
;*	  All rights reserved.	       *
;*				       *
;* Date Written:  24 March 1986        *
;* Last Modification: 10 Feb 1987      *
;*				       *
;*  tc 2/10/87 fixed problem printing  *
;*	       circular data structs   *
;*  rb 1/21/88 binary I/O uses         *
;*             line-length = 0;        *
;*             set dirty bit on writes * 
;*             (commented out)         *
;*				       *
;***************************************
	    page    60,132
	    include scheme.equ

P_FLAGS     equ     6
TEST_NUM    equ     8
RETURN	    equ     0Dh
SPACE	    equ     20h
CUR_COL     equ     12
N_COLS	    equ     20
SYM_OVHD    equ     7
HEAPERR     equ     -3

DGROUP	    group   data
data	    segment word public 'DATA'
	    assume  DS:DGROUP
	    public  display, show, detail, ccount
	    extrn   port_seg:word
	    extrn   port_d:word
	    extrn   port_r:word
	    extrn   direct:word
	    extrn   test_ch:word
	    extrn   t_array:word
ab_write    db	    "[WARNING: Output aborted by SHIFT-BREAK]",0
deep_str    db	    "#<DEEP!>",0
port_str    db	    "#<PORT>",0
parens	    db	    "()",0
cont_str    db	    "#<CONTINUATION>",0
ary_str     db	    "#("
free_str    db	    "#<FREE>",0
code_str    db	    "#<CODE>",0
env_str     db	    "#<ENVIRONMENT>",0
clos_str    db	    "#<PROCEDURE",0
display     dw	    1			     ; whether to use | and "
show	    dw	    1			     ; whether to send actual char
detail	    dw	    1			     ; whether to show detail
ccount	    dw	    0			     ; character count
branchtab   dw	    sp_list		     ; [0] LISTTYPE
	    dw	    sp_fix		     ; [1] FIXTYPE
	    dw	    sp_flo		     ; [2] FLOTYPE
	    dw	    sp_big		     ; [3] BIGTYPE
	    dw	    sp_sym		     ; [4] SYMTYPE
	    dw	    sp_str		     ; [5] STRTYPE
	    dw	    sp_ary		     ; [6] ARYTYPE
	    dw	    sp_cont		     ; [7] CONTTYPE
	    dw	    sp_clos		     ; [8] CLOSTYPE
	    dw	    sp_free		     ; [9] FREETYPE
	    dw	    sp_code		     ; [10] CODETYPE
	    dw	    sp_ref		     ; [11] REFTYPE
	    dw	    sp_port		     ; [12] PORTTYPE
	    dw	    sp_char		     ; [13] CHARTYPE
	    dw	    sp_env		     ; [14] ENVTYPE
data	    ends

PGROUP	    group   prog
prog	    segment byte public 'PROG'
	    assume  CS:PGROUP
spt_arg     struc
	    dw	    ?			     ; caller's BP
	    dw	    ?			     ; caller's return address
pg	    dw	    ?			     ; location of item to be printed
dis	    dw	    ?
ppg	    dw	    ?			     ; location of output port
pds	    dw	    ?
spt_arg     ends
	    extrn   setabort:near
	    extrn   ssetadr:near
	    public  sprint
sprint	    proc    near
	    push    BP
	    mov     BP,SP
	    call    setabort		     ; set address when abort
	    xor     AX,AX
	    mov     ccount,AX
	    pushm   <[BP].pds, [BP].ppg>
	    call    ssetadr		     ; set port address
	    mov     SP,BP

;fix for random i/o - note a write has taken place
	    lea	    SI,port_r
	    mov	    BX,[SI].C_page
	    LoadPage ES,BX
	    mov	    SI,port_d
	    or	    word ptr ES:[SI+P_FLAGS],DIRTY

	    pushm   <[BP].dis, [BP].pg>
	    call    subsprin		     ; print it
	    mov     SP,BP
	    mov     AX,ccount		     ; return number of characters
	    pop     BP
	    ret
sprint	    endp
;**************************************************************************
	    extrn   take_cdr:near
	    extrn   restart:near
	    extrn   stkspc:near
	    extrn   get_sym:near
	    extrn   givechar:near
	    extrn   gvchars:near
	    extrn   copybig:near
	    extrn   fix2big:near
	    extrn   big2asc:near
	    extrn   get_flo:near
	    extrn   isspace:near
	    extrn   abort:near

subp_arg    struc
tmp_reg1    dw	    ?
tmp_reg2    dw	    ?
tmp_reg3    dw	    ?
tmp_pg	    dw	    ?
tmp_SI	    dw	    ?
ch_buf	    db	    14 dup (0)		     ; character buffer
subp_BP     dw	    ?			     ; caller's BP
	    dw	    ?			     ; caller's ES
	    dw	    ?			     ; caller's return address
spg	    dw	    ?			     ; page number
sdis	    dw	    ?			     ; displacement
subp_arg    ends

subsprin    proc    near
	    push    ES
	    push    BP
	    sub     SP,offset subp_BP	     ; allocate local storage
	    mov     BP,SP
	    cmp     s_break,0		     ; check for SHIFT-BREAK
	    je	    subp_10
kill_out:   mov     AX,RETURN		     ; carriage return
	    push    AX
	    call    givechar
	    mov     SP,BP
	    mov     AX,41		     ; length of message
	    lea     BX,ab_write
	    pushm   <AX, BX>
	    call    printstr		     ; display message
	    mov     SP,BP
	    cmp     show,0
	    je	    kill_01
	    xor     AX,AX
	    jmp     kill_02
kill_01:    mov     AX,2
kill_02:    push    AX			     ; instruction length
	    C_call  restart		     ; link to scheme debugger
					     ; control does not return to here
subp_10:    call    stkspc		     ; check stack space
	    cmp     AX,64		     ; stack low?
	    jge     subp_20		     ; no, jump
	    mov     AX,8
	    lea     BX,deep_str
	    pushm   <AX, BX>
	    call    printstr		     ; print no deeper
	    mov     SP,BP
	    jmp     subp_ret
; act on object type
subp_20:    shl     [BP].spg,1		     ; adjust page number
	    mov     BX,[BP].spg
	    mov     DI,ptype+[BX]	     ; get port type
	    jmp     branchtab+[DI]

;; the individual type handlers

; handle for list
sp_list:    test    BX,BX		     ; null page?
	    jnz     sp_l01		     ; no, jump
	    mov     AX,2
	    lea     BX,parens
	    pushm   <AX, BX>
	    call    printstr		     ; print "()"
	    mov     SP,BP
	    jmp     subp_ret
sp_l01:     mov     DX,28h		     ; '('
	    push    DX
	    call    printcha
	    mov     SP,BP
	    mov     BX,[BP].spg 	     ; Get page
	    LoadPage ES,BX		     ; Get paragraph address of page
	    mov     SI,[BP].sdis	     ; dispacement
sp_l02:     mov     [BP].tmp_pg,BX	     ; Save page
	    mov     [BP].tmp_SI,SI	     ;	and displacement
	    xor     DH,DH
	    mov     DL,byte ptr ES:[SI]      ; Get car's page
	    shr     DX,1		     ; Change to number for subsprin
	    mov     CX,word ptr ES:[SI+1]    ; Get car's displacement
	    pushm   <CX, DX>
	    call    subsprin		     ; Go print it
	    mov     SP,BP
	    mov     BX,[BP].tmp_pg	     ; Restore page
	    LoadPage ES,BX		     ; Its para address
	    mov     SI,[BP].tmp_SI	     ;	 and displacement
	    mov     BL,byte ptr ES:[SI+3]    ; Get cdr's page offset
	    mov     SI,word ptr ES:[SI+4]    ;	 and displacement
	    test    BX,BX		     ; more items in list?
	    jz	    sp_l04		     ; no, jump
	    mov     [BP].tmp_SI,SI	     ; save registers
	    mov     [BP].tmp_reg1,BX
	    mov     DX,SPACE		     ; print ' '
	    push    DX
	    call    printcha
	    mov     SP,BP
	    mov     BX,[BP].tmp_reg1	     ; restore registers
	    mov     SI,[BP].tmp_SI
	    LoadPage ES,BX		     ; Get paragraph address of page
	    cmp     byte ptr ptype+[BX],LISTTYPE*2 ; check port type
	    je	    sp_l02
; last cdr not nil
	    mov     [BP].tmp_SI,SI	     ; save registers
	    mov     [BP].tmp_reg1,BX
	    mov     DX,2Eh		     ; print '.'
	    push    DX
	    call    printcha
	    mov     SP,BP
	    mov     DX,SPACE		     ; print ' '
	    push    DX
	    call    printcha
	    mov     SP,BP
	    mov     BX,[BP].tmp_reg1	     ; restore registers
	    mov     SI,[BP].tmp_SI
	    shr     BX,1		     ; corrected page number
	    pushm   <SI, BX>
	    call    subsprin
	    mov     SP,BP
sp_l04:     mov     DX,29h		     ; print ')'
	    push    DX
	    call    printcha
	    mov     SP,BP
	    jmp     subp_ret
; handle for fixnum
sp_fix:     mov     AX,5
	    mov     [BP].tmp_reg2,AX
	    push    AX
	    C_call  getmem
	    mov     SP,BP
	    cmp     AX,0
	    je	    mem_err
	    mov     [BP].tmp_reg1,AX	     ; address of divider
	    mov     SI,[BP].sdis	     ; get the value
	    shl     SI,1
	    sar     SI,1
	    pushm   <AX, SI>
	    mov     AX,DS
	    mov     ES,AX		     ; get the right ES segment
	    call    fix2big		     ; change to bignum
	    mov     SP,BP
	    jmp     printint
mem_err:    mov     AX,HEAPERR		     ; memory not available
	    push    AX
	    call    abort
	    mov     SP,BP
	    jmp     subp_ret		     ; return
; handle for flonum
sp_flo:     mov     SI,[BP].sdis	     ; displacement
	    shr     BX,1		     ; corrected page number
	    pushm   <SI, BX>
	    call    get_flo		     ; get a floating point value
	    pushm   <AX, BX, CX, DX>	     ; in AX:BX:CX:DX
	    C_call  printflo,,Load_ES
	    mov     SP,BP
	    jmp     subp_ret
; handle for array
sp_ary:     mov    AX,2
	    LoadPage ES,BX		     ; page segment
	    lea    BX,ary_str		     ; print "#("
	    pushm  <AX, BX>
	    call   printstr
	    mov    SP,BP

	    LoadPage ES,[BP].spg	     ; Get page address of array
;;;	    mov    ES,word ptr pagetabl+[BX]
	    mov    SI,[BP].sdis 	     ;	and segment
	    mov    CX,word ptr ES:[SI+1]
	    sub    CX,BLK_OVHD		     ; length of array
	    mov    BX,BLK_OVHD
	    mov    [BP].tmp_reg1,CX
sp_a01:
	    cmp    BX,[BP].tmp_reg1
	    jle    sp_a04
	    jmp    sp_l04
sp_a04:     mov    AL,byte ptr ES:[SI+BX]    ; AX <= page of array element
	    mov    DX,word ptr ES:[SI+BX+1]  ; DX <= disp. of array element
	    xor    AH,AH
	    shr    AX,1 		     ; Page number for subsprin
	    mov    [BP].tmp_reg2,BX	     ; Save registers
	    mov    [BP].tmp_SI,SI
	    pushm  <DX, AX>
	    call   subsprin		     ; print element
	    mov    SP,BP
	    mov    BX,[BP].tmp_reg2	     ; restore BX
	    cmp    BX,[BP].tmp_reg1	     ; last element?
	    jge    sp_a02
	    mov    DX,SPACE		     ; print ' '
	    push   DX
	    call   printcha
	    mov    SP,BP
	    mov    BX,[BP].tmp_reg2	     ; restore registers
sp_a02:     mov    SI,[BP].tmp_SI
	    add    BX,PTRSIZE
	    LoadPage ES,[BP].spg	     ; Reload page address of array
	    jmp    sp_a01
; handle for continuation
sp_cont:    mov    AX,15
	    lea    BX,cont_str
	    pushm  <AX, BX>
	    call   printstr
	    mov    SP,BP
	    jmp    subp_ret
; handle for closure
sp_clos:    mov    AX,11
	    lea    BX,clos_str
	    pushm  <AX, BX>
	    call   printstr		     ; print "#<PROCEDURE"
; fetch information operand from closure object
	    LoadPage ES,[BP].spg	     ; Get address of page
	    mov    SI,[BP].sdis
	    lea    BX,[BP].tmp_reg1
	    xor    AH,AH
	    mov    AL,byte ptr ES:[SI+3]     ; Page # of information op
	    mov    [BX].C_page,AX
	    mov    AX,word ptr ES:[SI+4]     ; Disp of information op
	    mov    [BX].C_disp,AX
; follow information operand list to cdr of last list cell
sp_c001:    mov    DI,[BX].C_page
	    cmp    DI,0
	    je	   sp_c01
	    cmp    byte ptr ptype+[DI],LISTTYPE*2
	    jne    sp_c01
	    push   BX
	    call   take_cdr
	    lea    BX,[BP].tmp_reg1
	    jmp    sp_c001
; If final operand is a symbol, print it
sp_c01:     cmp    byte ptr ptype+[DI],SYMTYPE*2
	    jne    sp_c04
	    LoadPage ES,DI
	    mov    SI,[BX].C_disp
	    mov    BX,word ptr ES:[SI+1]     ; get the object size
	    sub    BX,BLK_OVHD+PTRSIZE
	    push   DI			     ; temp-save DI
	    push   BX
	    dec    BX
	    mov    [BP].tmp_reg3,BX	     ; BX = symbol length
	    C_call getmem
	    pop    DI			     ; (get getmem arg off stack)
	    pop    DI			     ; temp-restore DI
	    mov    SP,BP
	    cmp    AX,0 		     ; memory available?
	    jne    sp_c02
	    jmp    mem_err		     ; no, jump
sp_c02:     mov    [BP].tmp_reg2,AX
	    sar    DI,1
	    mov    CX,DS		     ; ES points to DS segment
	    mov    ES,CX
	    pushm  <[BP].tmp_reg1, DI, AX>   ; [tmp_reg1] = disp
	    call   get_sym		     ; get the symbol name
	    mov    SP,BP
	    mov    DX,SPACE
	    push   DX
	    call   printcha		     ; print ' '
	    mov    SP,BP
	    pushm  <[BP].tmp_reg3, [BP].tmp_reg2>
	    call   printstr		     ; print the symbol name
	    mov    SP,BP
	    mov    BX,[BP].tmp_reg3
	    inc    BX
	    pushm  <BX, [BP].tmp_reg2>
	    C_call rlsmem
	    mov    SP,BP
sp_c04:     mov    DX,3Eh
	    push   DX
	    call   printcha		     ; print '>'
	    mov    SP,BP
	    jmp    subp_ret
; handle for free
sp_free:    mov    AX,7
	    lea    BX,free_str
	    pushm  <AX, BX>
	    call   printstr		     ; print #<FREE>
	    mov    SP,BP
	    jmp    subp_ret
; handle for code block
sp_code:    mov    AX,7
	    lea    BX,code_str
	    pushm  <AX, BX>
	    call   printstr		     ; print #<CODE>
	    mov    SP,BP
	    jmp    subp_ret
; handle for environment
sp_env:     mov    AX,14
	    lea    BX,env_str
	    pushm  <AX, BX>
	    call   printstr		     ; print #<ENVIRONMENT>
	    mov    SP,BP
	    jmp    subp_ret
; handle for symbol
sp_sym:     mov    AX,7Ch
	    mov    CX,SYM_OVHD
	    mov    SI,[BP].sdis
	    shr    BX,1 		     ; corrected page number
	    pushm  <AX, CX, SI, BX>
	    C_call printatm,,Load_ES	     ; print the symbol
	    mov    SP,BP
	    jmp    subp_ret
; handle for string
sp_str:     LoadPage ES,BX		     ; Get address of page
	    mov    SI,[BP].sdis 	     ;	 and displacement
	    mov    CX,word ptr ES:[SI+1]
	    cmp    CX,0 		     ; check for small string
	    jge    sp_s01
	    add    CX,BLK_OVHD+PTRSIZE
sp_s01:     sub    CX,BLK_OVHD		     ; get the string length
	    mov    [BP].tmp_reg1,CX	     ; save the string length
	    mov    DX,ccount
	    add    DX,CX
	    mov    ccount,DX
	    cmp    show,0
	    jne    sp_s02
	    jmp    subp_ret
sp_s02:     add    SI,BLK_OVHD		     ; advance pointer to string
	    mov    [BP].tmp_SI,SI
	    cmp    display,0
	    jne    sp_s02a
	    jmp    sp_sdis
; write, need to print double quotes, escape characters
sp_s02a:    xor    BX,BX
	    mov    DX,2 		     ; strange = 2
sp_s001:    cmp    BX,CX
	    jge    sp_s05
	    mov    AL,byte ptr ES:[SI+BX]
	    cmp    AL,5Ch		     ; check for \
	    je	   sp_s03
	    cmp    AL,22h		     ; check for "
	    jne    sp_s04
sp_s03:     inc    DX
sp_s04:     inc    BX
	    jmp    sp_s001
sp_s05:     add    DX,CX		     ; strange + len
	    push   DX
	    call   wrap
	    mov    AX,22h
	    push   AX
	    call   givechar		     ; print " for string
	    mov    SP,BP
	    xor    BX,BX
	    mov    SI,[BP].tmp_SI
sp_s06:     cmp    BX,[BP].tmp_reg1	     ; finish the string?
	    jge    sp_s10
	    cmp    s_break,0		     ; check for SHIFT-BREAK
	    je	   sp_s07
	    jmp    kill_out		     ; yes, jump
sp_s07:
	    LoadPage ES,[BP].spg	     ; Ensure string page loaded
	    mov    DL,byte ptr ES:[SI+BX]    ; Get one character
	    xor    DH,DH
	    mov    [BP].tmp_reg2,BX	     ; save registers
	    cmp    DL,5Ch		     ; \?
	    je	   sp_s08
	    cmp    DL,22h		     ; "?
	    jne    sp_s09
sp_s08:     mov    AX,5Ch
	    mov    [BP].tmp_reg3,DX	     ; save the character
	    push   AX
	    call   givechar		     ; print the \ for special
	    mov    SP,BP
	    mov    DX,[BP].tmp_reg3
sp_s09:     push   DX
	    call   givechar		     ; print the character
	    mov    SP,BP
	    mov    SI,[BP].tmp_SI	     ; restore registers
	    mov    BX,[BP].tmp_reg2
	    inc    BX
	    jmp    sp_s06
sp_s10:     mov    AX,22h
	    push   AX
	    call   givechar		     ; print "
	    mov    SP,BP
	    jmp    subp_ret
; display, just print the string
sp_sdis:    push   CX
	    call   wrap
	    xor    BX,BX
	    mov    SI,[BP].tmp_SI
sp_s11:     cmp    BX,[BP].tmp_reg1	     ; finish the string?
	    jl	   sp_s12
	    jmp    subp_ret		     ; yes, return
sp_s12:     cmp    s_break,0		     ; check for SHIFT-BREAK
	    je	   sp_s13
	    jmp    kill_out		     ; yes, jump
sp_s13:     xor    AH,AH
	    LoadPage ES,[BP].spg	     ; Ensure string page loaded
	    mov    AL,byte ptr ES:[SI+BX]    ; get the character
	    push   AX
	    mov    [BP].tmp_reg2,BX	     ; save registers
	    call   givechar		     ; print the character
	    mov    SP,BP
	    mov    BX,[BP].tmp_reg2	     ; restore registers
	    mov    SI,[BP].tmp_SI
	    inc    BX			     ; increment the index
	    jmp    sp_s11
; handle for character
sp_char:    mov    SI,[BP].sdis
	    and    SI,00FFh		     ; get the low byte for character
	    cmp    display,0
	    je	   sp_c10
	    mov    AX,SI		     ; AL = character
	    lea    SI,[BP].ch_buf
	    mov    byte ptr [SI],23h	     ; #
	    mov    byte ptr [SI+1],5Ch	     ; \
	    mov    byte ptr [SI+2],AL	     ; character
	    mov    byte ptr [SI+3],0	     ; end of string
; check for a special multi-character character constant
	    xor    BX,BX
	    lea    DI,test_ch
sp_ch01:    cmp    BX,TEST_NUM		     ; end of comparison?
	    jl	   sp_ch02
	    mov    BX,3 		     ; yes
	    jmp    sp_ch12
sp_ch02:    cmp    AL,byte ptr [DI+BX]	     ; compare with special char
	    je	   sp_ch05
	    inc    BX
	    jmp    sp_ch01
sp_ch05:    lea    DI,t_array
	    shl    BX,1 		     ; get the word offset
	    mov    DI,word ptr [DI+BX]	     ; pointer to special char string
	    mov    BX,2
sp_ch03:    cmp    byte ptr [DI],0	     ; end of string?
	    je	   sp_ch04		     ; yes, jump
	    mov    AL,byte ptr [DI]
	    mov    byte ptr [SI+BX],AL	     ; move character by character
	    inc    BX
	    inc    DI
	    jmp    sp_ch03
sp_ch04:    mov    byte ptr [SI+BX],0	     ; end of string
sp_ch12:    pushm  <BX, SI>		     ; BX = length of buffer
	    call   printstr
	    mov    SP,BP
	    jmp    subp_ret
; print character without escapes
sp_c10:     push   SI
	    call   printcha
	    mov    SP,BP
	    jmp    subp_ret
; handle for bignum
sp_big:     LoadPage ES,BX
	    mov    SI,[BP].sdis
	    mov    AX,word ptr ES:[SI+1]    ; get object size
	    dec    AX
	    mov    [BP].tmp_reg2,AX
	    push   AX
	    C_call getmem		    ; allocate memory for divider
	    mov    SP,BP
	    cmp    AX,0 		    ; memory available?
	    jne    sp_big1
	    jmp    mem_err		    ; no, error
sp_big1:    mov    [BP].tmp_reg1,AX	    ; address of divider
	    mov    BX,[BP].spg
	    shr    BX,1
	    pushm  <AX, [BP].sdis, BX>
	    mov    AX,DS
	    mov    ES,AX		    ; get the right ES segment
	    call   copybig		    ; copy bignum to buffer
printint:
	    mov    AX,[BP].tmp_reg2
	    mov    BX,3
	    mul    BX
	    sub    AX,5
	    mov    [BP].tmp_SI,AX
	    push   AX
	    C_call getmem		    ; allocate memory for char buffer
	    mov    SP,BP
	    cmp    AX,0 		    ; memory available?
	    jne    sp_big2
	    jmp    mem_err		    ; no, error
sp_big2:    mov    [BP].tmp_reg3,AX	    ; address of bigchars
	    pushm  <AX,[BP].tmp_reg1>
	    call   big2asc		    ; convert bignum to char string
	    mov    SP,BP		    ; AX = characters count
	    pushm  <AX, [BP].tmp_reg3>
	    call   printstr		    ; print the bignum
	    mov    SP,BP
	    pushm  <[BP].tmp_reg2, [BP].tmp_reg1>
	    C_call rlsmem
	    pushm  <[BP].tmp_SI, [BP].tmp_reg3>
	    C_call rlsmem
	    mov    SP,BP
	    jmp    subp_ret
; handle for port
sp_port:    mov     AX,7
	    lea     BX,port_str
	    pushm   <AX, BX>
	    call    printstr		    ; print #<PORT>
	    mov     SP,BP
sp_ref:
subp_ret:   add     SP,offset subp_BP	    ; release local storage
	    pop     BP
	    pop     ES
	    ret
subsprin    endp
;******************************************************************************
;	     Print a single character to the file, and send a newline
;      if necessary.
;******************************************************************************
pch_arg     struc
	    dw	?			     ; caller's BP
	    dw	?			     ; caller's return address
cha	    dw	?			     ; character
pch_arg     ends

printcha    proc    near
	    push    BP
	    mov     BP,SP
	    inc     ccount		     ; ccount++
	    cmp     show,0		     ; show?
	    je	    prch_ret		     ; no, return
	    call    currspc		     ; check spaces remaining
	    cmp     AX,0
	    jle     prch_01
prch_001:   push    [BP].cha
	    call    givechar
	    mov     SP,BP
	    jmp     prch_ret		     ; return to caller
prch_01:    test    direct,BINARY
	    jnz     prch_001
	    mov     AX,RETURN
	    push    AX
	    call    givechar		     ; newline
	    mov     SP,BP
	    push    [BP].cha
	    call    isspace		     ; after newline, print nonspaces
	    test    AX,AX
	    jnz     prch_ret		     ; space, return
	    jmp     prch_001
prch_ret:   pop     BP
	    ret 			     ; return to caller
printcha    endp
;******************************************************************************
;      Print the string with length LEN, first sending a newline
;   if necessary.
;******************************************************************************
str_arg     struc
	    dw	    ?			     ; caller's BP
	    dw	    ?			     ; caller's return address
str	    dw	    ?			     ; string pointer
len	    dw	    ?			     ; string length
str_arg     ends
	    public  printstr
printstr    proc    near
	    push    BP
	    mov     BP,SP
	    push    [BP].len
	    call    wrap		     ; check available spaces
	    mov     AX,ccount
	    add     AX,[BP].len 	     ; ccount += len
	    mov     ccount,AX
	    cmp     show,0		     ; show?
	    je	    pstr_ret		     ; no, return
	    pushm   <[BP].len, [BP].str>
	    call    gvchars		     ; display all characters
pstr_ret:   pop     BP
	    ret
printstr    endp
;******************************************************************************
;      Return number of spaces remaining on current line
;******************************************************************************
currspc     proc    near
	    pop     DI			     ; get the return address
	    push    ES
	    push    SI
	    lea     SI,port_r
	    mov     SI,[SI].C_page
	    LoadPage ES,SI
;;;	    LoadPage ES,port_seg	     ; Get port para address
	    mov     SI,port_d
	    mov     AX,word ptr ES:[SI+N_COLS] ; line length
	    test    AX,AX		     ; line length defined?
	    jnz     curr_01
	    mov     AX,-1		     ; no, return negative value
	    jmp     curr_02
curr_01:    sub     AX,word ptr ES:[SI+CUR_COL]
curr_02:    pop     SI
	    pop     ES
	    jmp     DI			     ; return to caller
currspc     endp
;******************************************************************************
;		   Return current column
;******************************************************************************
curr_col    proc    near
	    pop     DI			     	; get the return address
	    push    ES
	    push    SI
	    lea     SI,port_r
	    mov     SI,[SI].C_page
	    LoadPage ES,SI
;;;	    LoadPage ES,port_seg	     	; Get port para address
	    mov     SI,port_d
	    mov     AX,word ptr ES:[SI+N_COLS]  ; Get Number of columns
	    or	    AX,AX			; Maintaining column? 
	    jz 	    ccol_ret			; No,  just return 0
	    mov     AX,word ptr ES:[SI+CUR_COL]	; Yes, get column and return
ccol_ret:   pop     SI
	    pop     ES
	    jmp     DI			     ; return to caller
curr_col    endp
;******************************************************************************
;      Wrap issues a newline if there are less than LEN spaces
;   left on the current output line.
;   Note:  DX = LEN
;******************************************************************************
	    public  wrap
wrap	    proc    near
	    pop     DI			     ; get the return address
	    pop     DX			     ; get the length
	    cmp     show,0
	    jz	    wrap_ret
	    push    DI			     ; save return address
	    call    curr_col		     ; get the current column number
	    pop     DI			     ; restore return address
	    cmp     AX,1
	    jle     wrap_ret
	    push    DI			     ; save return address
	    call    currspc		     ; get the available spaces
	    pop     DI			     ; restore return address
	    cmp     AX,DX
	    jge     wrap_ret
	    mov     AX,RETURN		     ; issue a newline
	    push    AX
	    call    givechar
	    mov     SP,BP
wrap_ret:   jmp     DI			     ; return to caller
wrap	    endp

prog	    ends
	    end




