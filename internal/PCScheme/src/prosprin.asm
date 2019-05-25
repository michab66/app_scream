;							  ======> PROSPRIN.ASM
;************************************************************************
;*     	PC Scheme Runtime Support - Sexpression Print Routines		*
;*				   				        *
;*    			(C) Copyright 1985 by Texas      		*
;*     			  Instruments Incorporated.       		*
;*	  		     All rights reserved.	       		*
;*				       					*
;* Date Written:  24 March 1986        					*
;*				       					*
;* Modifications: 							*
;*									*
;*     11/27/87 (tc) Rewritten for protected mode scheme. Also		*
;*		     modified to buffer the output more effectively.    *
;*				       					*
;************************************************************************
		    page    60,132
		    title   PC Scheme Print Handlers
		    include scheme.equ
	            include sinterp.arg
	  	    include xli_pro.mac

NUMBER_SPECIAL_CHARS 	equ     8	;special chars defined in cread.asm

RETURN	    	     	equ     0Dh
SPACE	    	     	equ     20h
SYM_OVHD    	     	equ     7
HEAPERR     	     	equ     -3

DGROUP	    group   data
data	    segment word public 'DATA'
	    assume  DS:DGROUP
	    public  display, show, ccount
;from sread.asm
	    extrn   test_ch:word
	    extrn   t_array:word
;from xli_pro.asm
             extrn  rpc_handle:byte
             extrn  REAL_MODE_BUFFER:dword
	     extrn  REAL_BUF_OFFSET:word,REAL_BUF_SELECTOR:word
	     extrn  REAL_BUF_PARA:word,REAL_BUF_TOP:WORD
;from iosupport.asm
	     extrn  port_seg:word,port_pg:word,port_ds:word
;from ???
	     extrn  hicases:byte

;Table of strange characters used by printatm
stranges db	  " ,'"
	 db	  ';":()`'
	 db	  13,12,11,10,9,0

;
; The following global data is used to tell the print handlers about
; the print characteristics, ie. to surround strings with double quotes,
; to display escape characters, etc.
; 
;
display     dw	    0		; whether to surround atoms/strings with | or "
show	    dw	    1		; whether actually printing chars or not
ccount	    dw	    0		; char count used to determine print length

;
; Branch table of all Scheme object print handlers
;
branchtab   dw	    sp_list	; [0] LISTTYPE
	    dw	    sp_fix	; [1] FIXTYPE
	    dw	    sp_flo	; [2] FLOTYPE
	    dw	    sp_big	; [3] BIGTYPE
	    dw	    sp_sym	; [4] SYMTYPE
	    dw	    sp_str	; [5] STRTYPE
	    dw	    sp_ary	; [6] ARYTYPE
	    dw	    sp_cont	; [7] CONTTYPE
	    dw	    sp_clos	; [8] CLOSTYPE
	    dw	    sp_free	; [9] FREETYPE
	    dw	    sp_code	; [10] CODETYPE
	    dw	    subp_ret	; [11] REFTYPE
	    dw	    sp_port	; [12] PORTTYPE
	    dw	    sp_char	; [13] CHARTYPE
	    dw	    sp_env	; [14] ENVTYPE

;
; Following text will be output for those objects which have not
; printable representations.
;
port_str    db	    "#<PORT>",0
parens	    db	    "()",0
cont_str    db	    "#<CONTINUATION>",0
ary_str     db	    "#("
free_str    db	    "#<FREE>",0
code_str    db	    "#<CODE>",0
env_str     db	    "#<ENVIRONMENT>",0
clos_str    db	    "#<PROCEDURE",0
;
; Following are error messages
;
sp1_er      db      "WRITE",0
spc_er      db      "DISPLAY",0
spt_er      db      "PRINT",0
new_er      db      "NEWLINE",0
deep_str    db	    "#<DEEP!>",0
ab_write    db	    "[WARNING: Output aborted by SHIFT-BREAK]",0
bad_set     db      "[VM INTERNAL ERROR] setadr: bad port",CR,LF,0

data	    ends

PGROUP	    group   prog
prog	    segment byte public 'PROG'
	    assume  CS:PGROUP
            extrn   next_SP:near
	    extrn   src_err:near
            extrn   get_port:near
	    extrn   setabort:near,abort:near
	    extrn   take_cdr:near,restart:near,stkspc:near
	    extrn   copybig:near,fix2big:near,big2asc:near,get_flo:near
	    extrn   isspace:near
	    extrn   gvchars:near,ssetadr:near
	    extrn   scannum:near

comment |   Commented out 2/10/88 by TC - moved to realio routine

;******************************************************************************
;WRAP - Local macro definition. If there are less than LEN spaces left on
;	the local output line, make AX non-zero to denote wrap necessary.
;
; Note: es:di are destroyed
;******************************************************************************
wrap	macro	len,result
	local   wrapend
	push	es
	push	di
	xor	result,result		;result = default no wrap
	cmp	show,0			;are we actually printing?
	jz	wrapend			; no,  just return with default
	LoadPage es,port_pg
	mov	di,port_ds		;es:di => port object
	cmp	es:[di].pt_ncols,0	;maintaining line length?
	jz	wrapend		        ; no,  return default
	cmp	es:[di].pt_ccol,1	;in the first column already?
	jle	wrapend			; yes, return default
	mov	result,es:[di].pt_ncols	;ax = number cols
	sub	result,es:[di].pt_ccol	;ax = remaining spaces
	cmp	result,len		;any room left?
	mov	result,0		;
	jge	wrapend		        ; yes, return nowrap flag
	inc	result			; no,  return wrap flag
wrapend:
	pop	di
	pop	es
      	endm

|
 
prn_proc    proc    near
;;;****************************************************************************
;;;  		      VM Opcode handler for "WRITE"
;;;
;;;  Print an S-Expression with slashification
;;;
;;;****************************************************************************
            public  spprin1
spprin1:    lods    word ptr ES:[SI]          ; load register operand
            save    <SI>
            xor     BX,BX
            mov     BL,AH
            add     BX,offset reg0            ; BX = port object
            xor     AH,AH
            add     AX,offset reg0            ; AX = s-expression pointer
            mov     DI,AX
            save    <DI>
            mov     CX,1                      ; write indicator
            pushm   <CX, BX>
            C_call  get_port,,Load_ES         ; get port address
            mov     SP,BP
            test    AX,AX                     ; check return status
            jz      sp1_010
            lea     BX,sp1_er
            jmp     src_err                   ; link to error handler
sp1_010:
            inc     AX
            mov     display,AX
            mov     show,AX
            pushm   <tmp_disp, tmp_page>
            restore <DI>
            mov     BX,[DI].C_page
            shr     BX,1
            pushm   <[DI].C_disp, BX>
            call    sprint                    ; write
            mov     SP,BP
sp1_020:    restore <DI>                      ; get the register pointer
            mov     [DI].C_page,NPR_PAGE*2    ; return as non-printable object
            mov     [DI].C_disp,NPR_DISP
	    mov	    display,0		      ; default display = no
            jmp     next_SP                   ; return to interpreter
;;;****************************************************************************
;;;  		      VM Opcode handler for "DISPLAY"
;;;
;;;  Print an S-Expression without slashification
;;;
;;;****************************************************************************
            public  spprinc
spprinc:    lods    word ptr ES:[SI]          ; load register operand
            save    <SI>
            xor     BX,BX
            mov     BL,AH
            add     BX,offset reg0            ; BX = port object
            xor     AH,AH
            add     AX,offset reg0            ; AX = s-expression pointer
            mov     DI,AX
            save    <DI>
            mov     CX,1
            pushm   <CX, BX>
            C_call  get_port,,Load_ES         ; get port address
            mov     SP,BP
            test    AX,AX                     ; check return status
            jz      spc_010
            lea     BX,spc_er
            jmp     src_err                   ; link to error handler
spc_010:    mov     display,AX
            inc     AX
            mov     show,AX
            pushm   <tmp_disp, tmp_page>
            restore <DI>
            mov     BX,[DI].C_page
            shr     BX,1
            pushm   <[DI].C_disp, BX>
            call    sprint                    ; display
            mov     SP,BP
            jmp     sp1_020
;;;****************************************************************************
;;;  		      VM Opcode handler for "PRINT"
;;;
;;;  Print an S-Expression with spacing control
;;;
;;;****************************************************************************
            public  spprint
spprint:    lods    word ptr ES:[SI]          ; load register operand
            save    <SI>
            xor     BX,BX
            mov     BL,AH
            add     BX,offset reg0            ; BX = port object
            xor     AH,AH
            add     AX,offset reg0            ; AX = s-expression pointer
            mov     DI,AX
            save    <DI>
            mov     CX,1
            pushm   <CX, BX>
            C_call  get_port,,Load_ES         ; get port address
            mov     SP,BP
            test    AX,AX                     ; check return status
            jz      spt_010
            lea     BX,spt_er
            jmp     src_err                   ; link to error handler
spt_010:    mov     display,AX
            inc     AX
            mov     show,AX
            mov     DX,SPECCHAR
            mov     BX,RETURN                 ; carriage return
            pushm   <tmp_disp, tmp_page, BX, DX>
            call    sprint                    ; print it
            mov     SP,BP
            xor     AX,AX
            inc     AX
            mov     show,AX
            mov     display,AX
            pushm   <tmp_disp, tmp_page>
            restore <DI>
            mov     BX,[DI].C_page
            shr     BX,1
            pushm   <[DI].C_disp, BX>
            call    sprint                    ; print the s-expression
            mov     SP,BP
            mov     BX,SPACE
            mov     DX,SPECCHAR               ; space
            xor     AX,AX
            mov     display,AX
            inc     AX
            mov     show,AX
            pushm   <tmp_disp, tmp_page, BX, DX>
            call    sprint                    ; print it
            mov     SP,BP
            jmp     sp1_020
;;;****************************************************************************
;;;  		      VM Opcode handler for "NEWLINE"
;;;
;;;  Output a newline character
;;;
;;;****************************************************************************
            public  spnewlin
spnewlin:   lods    byte ptr ES:[SI]          ; load register operand
            save    <SI>
            add     AX,offset reg0            ; AX = port object
            mov     CX,1
            pushm   <CX, AX>
            C_call  get_port,,Load_ES         ; get port address
            mov     SP,BP
            test    AX,AX                     ; check return status
            jz      new_010
            lea     BX,new_er
            jmp     src_err                   ; link to error handler
new_010:    mov     display,AX
            inc     AX
            mov     show,AX
            mov     BX,SPECCHAR
            mov     DX,RETURN                 ; carriage return
            pushm   <tmp_disp, tmp_page, DX, BX>
            call    sprint
            mov     SP,BP
	    mov	    display,0		      ; default display = no
            jmp     next_SP                   ; return to interpreter
;;;****************************************************************************
;;;  		      VM Opcode handler for "LINE-LENGTH"
;;;
;;;  Determine the print length of a scheme object
;;;
;;;****************************************************************************
            public  prt_len
prt_len:    lods    byte ptr ES:[SI]          ; load register operand
            save    <SI>
            add     AX,offset reg0            ; AX = port object
            mov     DI,AX
            xor     CX,CX
            mov     display,CX                ; no display and show
            mov     show,CX
            save    <DI>
            mov     DX,OUT_PAGE*2
            mov     CX,OUT_DISP
            mov     BX,[DI].C_page
            shr     BX,1                      ; correct page number
            pushm   <CX, DX, [DI].C_disp, BX>
            call    sprint
            mov     SP,BP                     ; AX = print length
            restore <DI>
            mov     [DI].C_page,SPECFIX*2
            mov     [DI].C_disp,AX            ; get the print length
            jmp     next_SP                   ; return to interpreter
prn_proc    endp

;**************************************************************************
; SPRINT - Sexpression print routine
;
; Calling Sequence:  sprint(pds,ppg,dis,pg)
;
;    Where:	ppg:pds = page:displacement of port to output to
;		pg:dis  = scheme object to output
;
;    Upon Exit: AX = number of characters printed
;
;**************************************************************************

spt_arg     struc
	    dw	    ?			;caller's BP
	    dw	    ?			;caller's return address
pg	    dw	    ?			;page num  of object to print
dis	    dw	    ?			;page disp of object to print
ppg	    dw	    ?			;page num  of output port
pds	    dw	    ?			;page disp of output port
spt_arg     ends

	    public  sprint
sprint	    proc    near
	    push    BP
	    mov     BP,SP		;set up stack
	    call    setabort		;set address when abort
	    pushm   <[BP].pds, [BP].ppg>
	    call    ssetadr		;set port address
	    mov     SP,BP

;fix for random i/o - note a write has taken place
	    mov     AX,ES		;save extra segment
	    LoadPage ES,port_pg		;address port
	    mov	     SI,port_ds
	    or	    byte ptr ES:[SI].pt_pflgs,DIRTY ;note write has occurred
	    mov	    ES,AX		;restore extra segment

	    mov     ccount,0		;clear character count
	    pushm   <[BP].dis, [BP].pg>
	    call    subsprin		;go print the object
	    mov     SP,BP
	    mov     AX,ccount		;return number of characters
	    pop     BP
	    ret				;return to caller
sprint	    endp

;**************************************************************************
; SUBSPRIN - Recursive print routine
;
; Calling Sequence:  subsprin(dis,pg)
;
;    Where:	dis = displacement with pg of object to print
;		pg  = page of object to print
;
;**************************************************************************
	    public  subsprin
subp_arg    struc
tmp_reg1    dw	    ?
tmp_reg2    dw	    ?
tmp_reg3    dw	    ?
lst_pag	    dw	    ?
lst_dsp	    dw	    ?
subp_BP     dw	    ?			     ; caller's BP
	    dw	    ?			     ; caller's ES
	    dw	    ?			     ; caller's return address
spg	    dw	    ?			     ; page number
sdis	    dw	    ?			     ; displacement
subp_arg    ends

subsprin    proc    near
	    push    ES
	    push    BP
	    sub     SP,offset subp_BP	    ;allocate local storage
	    mov     BP,SP
	    cmp     s_break,0		    ;check for SHIFT-BREAK
	    je	    subp_10
kill_out:   mov     AX,RETURN		    ;carriage return
	    call    printcha
	    mov     AX,41		    ;length of message
	    lea     BX,ab_write
	    pushm   <AX, BX>
	    call    printtxt		    ;display message
	    mov     SP,BP
	    cmp     show,0
	    je	    kill_01
	    xor     AX,AX
	    jmp     kill_02
kill_01:    mov     AX,2
kill_02:    push    AX			    ;instruction length
	    C_call  restart		    ;link to scheme debugger
					    ;control does not return to here
subp_10:    call    stkspc		    ;check stack space
	    cmp     AX,64		    ;stack low?
	    jge     subp_20		    ;no, jump
	    mov     AX,8
	    lea     BX,deep_str		    ;#<DEEP!>
	    jmp     subp_ret		    ;print message and return
; act on object type
subp_20:    
	    shl     [BP].spg,1		    ;adjust page number
	    mov     BX,[BP].spg
	    mov     DI,ptype+[BX]	    ;get page type
	    jmp     branchtab+[DI]	    ;envoke appropriate handler
;
; any problems with getmem should exit here
mem_err:    
	    mov     AX,HEAPERR		    ;memory not available
	    push    AX
	    call    abort
	    mov     SP,BP
;
; return to caller
subp_ret:   
	    add     SP,offset subp_BP	    ;release local storage
	    pop     BP
	    pop     ES
	    ret


	    page    60,132
;-----------------------------------------------------------------------------
;
; Following are the print handlers for each object type, they will be invoked
; via an indirect call through BRANCHTAB.
;  
;  Upon entry: BX = an adjusted page number
;	       DI = the page type
;
;  Upon exit:  Jump to SUBP_RET for cleanup
;
;-----------------------------------------------------------------------------

;*******************************************************************************
;
; Print representation for code block object
;
;*******************************************************************************
	    public sp_code
sp_code:    mov    AX,7
	    lea    BX,code_str		    ; #<CODE>
	    jmp	   subp_txt
;*******************************************************************************
;
; Print representation for continuation object
;
;*******************************************************************************
	    public sp_cont
sp_cont:    mov    AX,15
	    lea    BX,cont_str		    ; #<CONTINUATION>
	    jmp	   subp_txt
;*******************************************************************************
;
; Print representation for environment object
;
;*******************************************************************************
	    public sp_env
sp_env:     mov    AX,14		
	    lea    BX,env_str		    ; #<ENVIRONMENT>
	    jmp	   subp_txt
;*******************************************************************************
;
; Print representation for free page
;
;*******************************************************************************
	    public sp_free
sp_free:    mov    AX,7
	    lea    BX,free_str		    ; #<FREE>
	    jmp	   subp_txt
;*******************************************************************************
;
; Print representation for port object
;
;*******************************************************************************
	    public sp_port
sp_port:    mov     AX,7
	    lea     BX,port_str		    ; #<PORT>
subp_txt:
	    pushm   <AX, BX>		    ;ax = length, bx = message address
	    call    printtxt		    ;print the message
	    mov     SP,BP		    ;clean up stack
	    jmp	    subp_ret		    ;and return to caller

;*******************************************************************************
;
; Print floating point value
;
;*******************************************************************************
	    public  sp_flo
sp_flo:     mov     SI,[BP].sdis	     ; displacement
	    shr     BX,1		     ; corrected page number
	    pushm   <SI, BX>
	    call    get_flo		     ; get a floating point value
	    pushm   <AX, BX, CX, DX>	     ; in AX:BX:CX:DX
	    C_call  printflo,,Load_ES
	    mov     SP,BP
	    jmp     subp_ret

;*******************************************************************************
;
; Print list
;
;*******************************************************************************
	    public  sp_list
sp_list:
	    test    bx,bx		;null page?
	    jnz     sp_l01		; no, go chase list
	    mov     ax,2
	    lea     bx,parens
	    pushm   <ax, bx>
	    call    printtxt		;print "()" for null list
	    mov     sp,bp
	    jmp     subp_ret
sp_l01:
	    mov	    al,byte ptr parens
	    call    printcha		;print open paren
	    mov     bx,[bp].spg 
	    LoadPage es,bx
	    mov     si,[bp].sdis	;es:si => list cell
sp_l02:
	    mov     [bp].lst_pag,bx
	    mov     [bp].lst_dsp,si	;save list cell
	    xor     dh,dh			
	    mov     dl,byte ptr es:[si]    ;get car of list
	    shr     dx,1		   ; make number for subsprin
	    mov     cx,word ptr es:[si+1]  ;get car's displacement
	    pushm   <cx, dx>		   
	    call    subsprin		   ;print car of list
	    mov     sp,bp
	    mov     bx,[bp].lst_pag	   ;restore list cell
	    LoadPage es,bx
	    mov     si,[bp].lst_dsp
	    mov     bl,byte ptr es:[si+3]  ;get cdr of list
	    mov     si,word ptr es:[si+4]
	    test    bx,bx		   ;is it null?
	    jz	    sp_l04		   ; yes, finished
	    pushm   <si,bx>		   ;tempsave si,bx
	    mov     al,' '
	    call    printcha		   ;print space as item seperator
	    popm    <bx,si>		   ;restore stack
	    LoadPage es,bx		            ;reload page of cdr
	    cmp     byte ptr ptype+[bx],LISTTYPE*2  ;is cdr a list cell?
	    je	    sp_l02			    ; yes, chase the list
; cdr is not a list cell - improper list
	    mov     dx," ."		;need " ." due to byte swapping
	    pushm   <si,bx,dx>		;tempsave si,bx - dx is text
	    mov	    si,sp
	    mov     dx,2
	    pushm   <dx,si>		;push length, address of text
	    call    printtxt		;print ". "
	    add	    sp,6		;dump last 3 args
	    popm   <bx,si>		;restore saved regs

	    shr     bx,1		;make page a number for subsprin
	    pushm   <si, bx>
	    call    subsprin		;go chase last cdr
	    mov     sp,bp		;dump args off stack
sp_l04:     
	    mov	    al,byte ptr parens+1
	    call    printcha		;and print it
	    mov     sp,bp
	    jmp     subp_ret		;return to caller

;*******************************************************************************
;
; Print array
;
;*******************************************************************************
	    public sp_ary
sp_ary:     mov    AX,2
	    LoadPage ES,BX		     ; page segment
	    lea    BX,ary_str		     ; print "#("
	    pushm  <AX, BX>
	    call   printtxt
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
	    mov    [BP].lst_dsp,SI
	    pushm  <DX, AX>
	    call   subsprin		     ; print element
	    mov    SP,BP
	    mov    BX,[BP].tmp_reg2	     ; restore BX
	    cmp    BX,[BP].tmp_reg1	     ; last element?
	    jge    sp_a02
	    mov    ax,SPACE		     ; print ' '
	    call   printcha
	    mov    BX,[BP].tmp_reg2	     ; restore registers
sp_a02:     mov    SI,[BP].lst_dsp
	    add    BX,PTRSIZE
	    LoadPage ES,[BP].spg	     ; Reload page address of array
	    jmp    sp_a01
;*******************************************************************************
;
; Print representation for closure object
;
;*******************************************************************************
	    public sp_clos
sp_clos:    mov    ax,11		
	    lea    bx,clos_str
	    pushm  <ax,bx>
	    call   printtxt		    ;print "#<PROCEDURE"
	    mov    SP,BP
; get info operand from closure object
	    LoadPage es,[bp].spg
	    mov    si,[bp].sdis		    ;es:si => closure object
	    lea    bx,[bp].tmp_reg1
	    xor    ah,ah		
	    mov    al,byte ptr es:[si+3]    ;pag # of information op
	    mov    [bx].C_page,ax	    ;  save in tmp_reg1	
	    mov    ax,word ptr es:[si+4]    ;displ of information op
	    mov    [bx].C_disp,ax	    ;  save in tmp_reg1
; follow info operand list
sp_c001:    
	    mov    di,[bx].C_page
	    cmp    di,0			    	   ;if reached end of list
	    je	   sp_c04		    	   ;  then exit loop
	    cmp    byte ptr ptype+[di],LISTTYPE*2  ;if cdr not list cell
	    jne    sp_c01			   ;  then exit loop
	    push   bx
	    call   take_cdr			   ;follow cdr of list
	    lea    bx,[bp].tmp_reg1
	    jmp    sp_c001
; If final operand is a symbol, print it
sp_c01:     
	    cmp    byte ptr ptype+[DI],SYMTYPE*2   ;do we have a symbol?
	    jne    sp_c04			   ;  no, jump

	    push   bx			    ;tempsave reg around call
	    mov    ax,SPACE
	    call   printcha		    ;print ' '
	    pop	   bx			    ;restore reg

	    push   display		     ;save around call
	    mov	   display,0		     ;don't print escape chars
	    shr	   [bx].C_page,1	     ;make page # for subsprin
	    pushm  <[bx].C_disp,[bx].C_page> ;push page:disp of symbol
	    call   subsprin		     ;go print the symbol
	    add	   sp,4			     ;dump args off stack
	    pop	   display		     ;restore display indicator
sp_c04:
	    mov    al,'>'
	    call   printcha		    ;print '>'
	    mov    SP,BP
	    jmp    subp_ret

;*******************************************************************************
;
; Print symbol to output port
;
;*******************************************************************************
	    public printatm 
printatm    label  near
sp_sym:
;
; Warning: local data segment is not used in code below
;
	    loadpage ds,bx
       	    mov	   si,[bp].sdis    	;ds:si => object
	    mov	   cx,[si]+1		;get object length
	    sub	   cx,SYM_OVHD		;cx = length of atom
     	    add	   SS:ccount,cx 	;update character count
     	    cmp	   SS:show,0		;do we want to print the object?
	    jne	   pra_010		; yes, continue
	    mov	   dx,ss		; no,  restore data segment
	    mov	   ds,dx
     	    jmp	   pra_exit		;      and return
pra_010:
					;cx = length of symbol
	    add	   si,SYM_OVHD		;ds:si => symbol name
	    GET_BUFFER			;es:di => real buffer
     	    call   atm2pbuf 	        ;move printname to print buffer
;
; Warning: local data segment is not used in code above
;
	    mov	   dx,ss
	    mov	   ds,dx		;restore local data segment
	    cmp	   cx,0	  		;if negative print length
	    jl	   pra_err		; then error, jump
;;;	    wrap   cx,dx		;cx = length, dx = result
	    mov	   dx,1			;check wrap flag
;cx = length, dx = check wrap flag, es:di => print buffer
     	    call   gvchars 		;go print the buffer
pra_exit:
	    RLS_BUFFER
	    jmp    subp_ret
pra_err:
	    jmp	   mem_err

;*******************************************************************************
;
; Print string to output port
;
;*******************************************************************************
	   public   printstr
printstr   label    near
sp_str:
;
; Warning: local data segment is not used in code below
;
	   loadpage ds,bx		
     	   mov	    si,[bp].sdis 	;ds:si => object
	   mov	    cx,[si]+1		;cx = length indicator
	   add	    si,BLK_OVHD		;ds:si => actual string
	   or	    cx,cx		;small string?
	   jge	    prs_005		; no,  jump
	   add	    cx,BLK_OVHD+PTRSIZE
prs_005:
	   sub	    cx,BLK_OVHD		;cx = length of string
	   add	    SS:ccount,cx	;update character count
	   cmp	    SS:show,0 		;actually printing?
	   jne	    prs_010		; yes, continue
	   mov	    dx,ss		; no,  restore data segment
	   mov	    ds,dx
     	   jmp	    pra_exit		;      and return
prs_010:
	   
					;cx = string length
	   GET_BUFFER			;es:di => buffer for print string
     	   call	    str2pbuf 		;move string to print buffer
;
; Warning: local data segment is not used in code above
;
  	   mov	    bx,ss		;restore local data segment
	   mov	    ds,bx
	   cmp	    cx,0		;if negative print length
	   jl	    prs_err		; then error, jump
;;;	   wrap	    cx,dx		;cx=length, dx=result
	   mov	    dx,1		;check wrap flag
;cx = length, dx = check wrap flag, es:di => print buffer
     	   call	    gvchars 		;go print the buffer
prs_exit:
	   RLS_BUFFER			;release the print buffer
	   jmp	    subp_ret

prs_err:   jmp	    mem_err		;must be a long jump


;*******************************************************************************
;
; Print character to output port
;
;*******************************************************************************
	    public sp_char
sp_char:
	   cmp      display,0		;display escape chars?
	   jne	    sp_ch10		; yes, jump
;print character without escapes
	   mov      ax,[BP].sdis	;get character
           xor	    ah,ah
	   call     printcha		;call print routine	
	   jmp      subp_ret		;get outa here
;print character with escapes
sp_ch10:
	   mov	    bx,14		;max size of character buffer
	   push     bx
	   C_call   getmem		;allocate space
	   mov	    sp,bp		;dump arg off stack
	   cmp	    ax,0
	   jne	    sp_ch00
	   jmp	    mem_err		;error allocating heap - jump
sp_ch00:
	   mov	    si,ax		;si => buffer
	   mov	    dx,ax		;dx => buffer
	   mov      ax,[BP].sdis	;get character
	   mov      byte ptr [si],23h	; #
	   mov      byte ptr [si+1],5Ch	; \
	   mov      byte ptr [si+2],AL	;character
	   mov      byte ptr [si+3],0	;end of string
	   mov	    bx,3		;length
;
; see if character one of the multi-character constants in cread.asm
;
	   mov	    cx,ds
	   mov	    es,cx 		;ensure ds=es

	   mov	    cx,NUMBER_SPECIAL_CHARS   ;cx = counter
	   lea	    di,test_ch
	   add	    di,NUMBER_SPECIAL_CHARS-1 ;di => last special char
	   std
	   repne    scasb		;search for special char
	   cld
	   jnz	    sp_ch20		;if none found, jump
	   shl	    cx,1		;make index into t_array

	   lea	    di,t_array
	   add	    di,cx
	   mov	    di,[di]
	   xchg     si,di		;ds:si => character string
	   add	    di,2		;es:di => character buffer
	   cld
	   xor	    al,al		;al = null terminator	
sp_chlp:
	   movsb			;move byte into character buffer	
	   cmp      al,[si]		;reached terminator?
	   jne	    sp_chlp
	   sub	    di,dx		;calc length
	   mov	    bx,di			
sp_ch20:    
	   pushm    <bx,dx>		;bx=buffer length, dx=buffer address
	   call     printtxt		;print the character constant
	   mov      SP,BP

	   mov	    bx,14		;length of character buffer
	   pushm    <bx,si>
	   C_call   rlsmem		;release character buffer
	   mov	    sp,bp		;dump args off stack
	   jmp      subp_ret

;*******************************************************************************
;
; Print integer value
;
;*******************************************************************************
	    public sp_fix
sp_fix:     mov     AX,5
	    mov     [BP].tmp_reg2,AX
	    push    AX
	    C_call  getmem
	    mov     SP,BP
	    or	    ax,ax
	    jnz	    sp_f10
	    jmp	    mem_err
sp_f10:
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
;*******************************************************************************
;
; Print bignum
;
;*******************************************************************************
	    public sp_big
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
	    mov    [BP].lst_dsp,AX
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
	    call   printtxt		    ; print the bignum
	    mov    SP,BP
	    pushm  <[BP].tmp_reg2, [BP].tmp_reg1>
	    C_call rlsmem
	    pushm  <[BP].lst_dsp, [BP].tmp_reg3>
	    C_call rlsmem
	    mov    SP,BP
	    jmp    subp_ret

subsprin    endp


;	    page    60,132
;	    title   Print Handler Support Routines
;
;******************************************************************************
;PRINTTXT - C callable routine to print a string, first sending a newline
;	    character if necessary. The string is assumed to be in the
;	    local data segment.
;  
;  Calling Sequence: printtxt(string,len)
;
;******************************************************************************
str_arg     struc
	    dw	    ?			     ; caller's BP
	    dw	    ?			     ; caller's ES
	    dw	    ?			     ; caller's return address
str	    dw	    ?			     ; string pointer
len	    dw	    ?			     ; string length
str_arg     ends
	    public  printtxt
printtxt    proc    near
	    push    es
	    push    bp
	    mov     bp,sp		     ;set up stack

	    mov	    cx,[bp].len		     ;cx = length of string
	    add     ccount,cx		     ;keep track of character count
	    cmp     show,0		     ;show?
	    je	    pstr_ret		     ; no, return

	    push    display
	    mov	    display,0
	    mov	    si,[bp].str		     ;ds:si => string to write
	    GET_BUFFER			     ;es:di => buffer for print string
	    call    str2pbuf		     ;move string to print buffer
	    pop	    display

;;;	    wrap    cx,dx		     ;cx=length, dx=result
	    mov	    dx,1		     ;check wrap flag
;cx = length, dx = check wrap flag, es:di => print buffer
     	    call   gvchars 		     ;go print the buffer
	    RLS_BUFFER			     ;release the print buffer
pstr_ret:   
	    pop     bp
	    pop	    es
	    ret
printtxt    endp


;******************************************************************************
;PRINTCHA - C callable routine to print a character, first sending a newline
;	    character if necessary. At least for now, this is performed by
;	    calling gvchars - in the future we may want output routines for
;	    outputting individual characters.
;  
;  Upon Entry: al contains character to print
;
;******************************************************************************
pcha_arg  struc
          dw      ?                     ;caller's BP
          dw      ?                     ;caller's ES
          dw      ?                     ;caller's return address
pcha_arg  ends

          public  printcha
printcha  proc    near
          push    es
          push    bp
          mov     bp,sp			;set up stack

	  inc	  ccount		;bump character count
	  cmp	  show,0		;if not actually printing
	  jz	  pcha_ret		; then return

comment |
;see if enough space left on current output line
	  LoadPage es,port_pg
	  mov	  di,port_ds		;es:di => port object
	  xor	  dx,dx			;dx = wrap flag (0 = nowrap)
	  mov	  cx,es:[di].pt_ncols	;maintaining line length?
	  jcxz	  pcha_010
	  sub	  cx,es:[di].pt_ccol	;cx = space remaining
          cmp	  cx,1			;any space left?
	  jge	  pcha_010
	  mov	  dx,1
 |
	  mov	  dx,1			;check wrap flag
pcha_010:
	  RESET_REAL_BUFFER_OFFSET
	  MOVE_BYTE_TO_BUF al,REAL_MODE_BUFFER
	  mov	  cx,1			;cx = length
	  call	  gvchars
pcha_ret: 
	  xor     ax,ax
          pop     bp
          pop     es
          ret
printcha  endp

COMMENT %
  The two routines below are written such that they may run either on the 
  hummingboard or out of 286/386 pritected memory. The hummingboard cannot 
  directly address the host's physical memory, so when displaying escape 
  chars within a string, it is first buffered into space allocated in the 
  heap, then written via a block move to the print buffer. Although this is
  somewhat clumsy, it significantly improves performance over writing 
  individual bytes to the print buffer with the BLOCK-MOVE dos function 
  provided by OSx86.
  If in the future the hummingboard os traps instructions which are writing
  to real memory, then the buffering may be discarded and simple memory
  moves may be performed.
%

;******************************************************************************
;ATM2PBUF - Move symbol printname to printbuffer
;
; Description:
;   The symbol name pointed to by ds:si is moved into a print buffer for output.
;
;   If the display flag is set, the symbol is being output by the "write" lisp
;   function and all backslashes and vertical bars (\,|) are written to the 
;   buffer preceeded by a backslash. In addition, the symbol must also be
;   surrounded by vertical bars (|) if lowercase letters, commas, semi-colons,
;   and other strange characters are encountered; or if the printname is a
;   ".", a name starting with a # (other than special symbols), or is numeric.
;
;   If the display flag is not set, then the printname is moved to the buffer
;   without performing any translation.
;
; Upon Entry:
;   cx    =  length of printname
;   ds:si => printname
;   es:di => print buffer
;
; Upon Exit:
;   cx    =  # characters placed in print buffer
;   es:di => print buffer
;
;******************************************************************************
p_struc    struc
dest_top   dw	?
dest_start dw	?
heap_top   dw	?
heap_str   dd	?
src_start  dw	?
src_str	   dd	?
dest_str   dd	?
stlen      dw	?
call_bp	   dw	?
p_struc    ends

	  public  atm2pbuf
atm2pbuf  proc	  near
	  BUFFER_IS_BUFFER	   	;real mode buffer treated as such
		       
	  cmp	  ss:display,0	   	;displaying escape chars?
	  jnz	  a2p_xlat	   	; yes, jump
;move printname to print buffer
	  MOVE_TO_REAL_BUF         	;move entire string over
	  ret				;return to caller
a2p_err:
	  mov	  [bp].stlen,-1
	  jmp	  a2p_fin

;move printname to print buffer, checking for backslashes and delimiters '|'
a2p_xlat:
	  push	  bp			;save callers bp
	  push	  cx			;save length
	  push	  es
	  push	  di			;save real buffer address
	  push	  ds
	  push	  si			;save string address
	  sub	  sp,src_str		;allocate local storage
	  mov	  bp,sp

	  mov	  [bp].src_start,si	;save start location of source string
	  mov	  [bp].dest_start,di	;save print buffer start
	  GET_REAL_BUFFER_TOP dx	
	  sub	  dx,pt_bfend+20
	  mov	  [bp].dest_top,dx      ;save print buffer end
	  
	  mov	  ax,ss
	  mov	  ds,ax			
	  mov	  es,ax			;setup for c call
	  mov	  ax,512
	  push	  ax
	  c_call  getmem		;allocate 512 bytes of storage
	  add	  sp,2			;dump arg from stack
	  cmp	  ax,0			;allocation successful?
	  jne	  a2p_write		; no, go write the buffer
	  mov	  [bp].stlen,-1		;indicate error
	  jmp	  a2p_fin2		; and exit
a2p_write:
	  mov	  [bp].heap_top,510	;note top of heap buffer
	  mov	  word ptr [bp].heap_str,ax   ;save heap buffer address
	  mov	  word ptr [bp].heap_str+2,es
	  mov	  di,ax			;es:di = buffer
	  mov	  byte ptr es:[di],'|'  ;start buffer with escape char
	  inc	  di			;di = address within buffer
	  mov	  dx,1 			;dx = # chars written
	  lds	  si,[bp].src_str	;ds:si addresses the string
	  mov	  cx,[bp].stlen		;cx = char count
	  xor	  bx,bx		   	;bh = strangeness
	  cmp	  cx,0 			;char count zero?
	  jne	  b2p_init		; no,  jump
	  or	  bh,080h		; yes, mark as strange
	  jmp	  b2p_post		;      and skip loop
b2p_init:	  
;dx = #chars written, di=heap buffer offset, si=string offset, cx=char count
b2plp:
	  cmp	  dx,[bp].dest_top	;room left in buffer?
	  jg	  a2p_err		; no,  return error status

	  lodsb 		   	;fetch char from printname
	  cmp	  al,'\'	   	;is char escape char?
	  je	  escit		   	;  yes, jump
	  cmp	  al,'|'	   	;is char delimiter?
	  jne	  storit	   	;  no,  just go store it
escit:
	  mov	  byte ptr es:[di],'\'	;write escape char to buffer
	  inc	  di			;bump print buffer ptr
	  inc	  dx			;bump # chars written
storit: 
	  stosb  			;write char to buffer
	  inc	  dx			;bump # chars written

	  test	  bh,80h	   ;do we already know that atom's strange?
	  jnz	  skptest	   ;  yes, skip following tests
;if lowercase letters or comma, semi-colon, etc. encountered, then it contains
;"strange" characters and must be delimited by '|'
	  push	  si		   	;tempsave pname offset
	  mov	  bl,al			;save copy of char in bl
	  lea	  si,hicases	   	;si => table of upper cases
	  xchg	  bx,si
	  mov	  ah,al 	        ;save char
	  xlat	  ss:hicases		;fetch upper-case equivalent
	  xchg	  bx,si
	  cmp	  ah,al			;are chars different?
	  jne	  mrkstrng	        ;  yes, indicate strangeness
	  mov	  si,offset stranges    ;si => strange-character string
strnglp:  lods	  byte ptr ss:[si]      ;fetch strange char
	  or	  al,al 	        ;finished searching for strange chars?
	  jz	  notstrng	        ;  yes, exit loop
	  cmp	  ah,al 	        ;Do we have a strange char?
	  jne	  strnglp	        ;  no,  try next
mrkstrng: or	  bh,80h	        ;  yes, mark strange bit
notstrng:
	  pop	  si			;restore pname offset
skptest:  
	  cmp	  di,[bp].heap_top	;heap buffer full?
	  jl	  a2p_cont		; no, continue
	  push	  cx			;save loop count
	  mov	  word ptr [bp].src_str,si  ;update string pointer
	  sub	  di,word ptr [bp].heap_str ;calc number chars written
	  mov	  cx,di			; and save in cx
	  lds	  si,[bp].heap_str	;ds:si = heap buffer
	  les	  di,[bp].dest_str	;es:di = real mode buffer
	  MOVE_TO_REAL_BUF autoinc     	;move string to print buffer
	  add	  word ptr [bp].dest_str,di  ;update next location in real buffer
	  pop	  cx			;restore count
	  lds	  si,[bp].src_str	;ds:si = pointer into string
	  les	  di,[bp].heap_str	;es:di = heap allocated string ptr
a2p_cont:
	  loop	  b2plp 	        ;look at next char in printname

; bh= strangeness, dx= #chars printed, di= end of printname
; write delimeter to heap allocated string, then copy to print buffer
b2p_post:
	  mov	  al,'|'		;follow with escape char
	  stosb				;write to heap buffer
	  inc	  dx			;bump character count

	  sub	  di,word ptr [bp].heap_str ;calc number chars written
	  mov	  cx,di			    ;  and save in cx
	  lds	  si,[bp].heap_str	    ;ds:si = heap buffer
	  les	  di,[bp].dest_str	    ;es:di = real mode buffer
	  MOVE_TO_REAL_BUF autoinc     	    ;move string to print buffer
	  mov	  [bp].stlen,dx		    ;save actual # chars written
	  mov	  ds,word ptr [bp].src_str+2 
	  mov	  si,[bp].src_start	    ;ds:si => start of source string
 
	  test	  bh,80h	            ;do we already know atom's strange?
	  jnz	  a2p_fin	            ;  yes, jump
; Check for ., #macro, or numeric confusion	  
	  cmp	  dx,3			;a single char?   (remember delimiters)
	  jne	  a2p_sharp		; no,  jump
	  mov	  al,byte ptr ds:[si]   ;get first byte of symbol
	  cmp	  al,'.'		;do we have a period - "." ?
	  je	  a2p_fin		; yes, delimits necessary
a2p_sharp:
	  cmp	  al,'#'		;macro designator?
	  jne	  a2p_num    		; no, jump
	  cmp	  dx,3			;a single sharp?  (remember delimiters)
	  je      a2p_fin		; yes, delimits necessary
	  cmp	  [bp].spg,SPECSYM*2	;special symbol
	  je	  a2p_nodelim		; yes, no delimeters required
	  jne	  a2p_fin    		; no,  delimits necessary
a2p_num:
     	  mov	  ax,10		   ;check for number
     	  push    ax 		   ;base 10
     	  push    si		   ;ds:si => printname
     	  call    scannum 	   ;check for number
     	  add     sp,4 		   ;dump args from stack
     	  test    ax,ax 	   ;is it a number?
     	  jnz	  a2p_fin      	   ; yes, jump
a2p_nodelim:
;although symbol being witten via "write", there are no stranges chars,
;or anything, so it can be written without delimiters.
	  inc	  [bp].dest_start	;position past initial delimiter
	  sub	  [bp].stlen,2		;exclude delimeters from length
a2p_fin:
	  mov	  ax,ss
	  mov	  ds,ax			;set up data segment
	  mov	  es,ax			; and extra segment
	  mov	  bx,512		;length of heap buffer
	  pushm   <bx,word ptr [bp].heap_str>
	  C_call  rlsmem,            	;release character buffer
	  add	  sp,4			;dump args off stack
a2p_fin2:
	  mov	  es,word ptr [bp].dest_str+2
	  mov	  di,[bp].dest_start	;es:di => real buffer start
	  mov	  cx,[bp].stlen		;cx = number characters written

	  lds	  si,[bp].src_str	;restore ds:si
	  add	  sp,call_bp		;dump args off stack
	  pop	  bp			;restore base pointer
	  ret

atm2pbuf  endp

;******************************************************************************
;STR2PBUF - Move string to printbuffer
;
; Description:
;   The print buffer is in real mode, the string is moved into the print
;   buffer (possibly surrounded by quotes '"' and containing escape
;   characters. 
;  
; Upon Entry:
;	cx    =  length of string
;	ds:si => string
;	es:di => print buffer
;
; Upon Exit:
;	cx    =  number of bytes written to print buffer
;	es:di => print buffer
;
;******************************************************************************

	  public  str2pbuf
str2pbuf  proc	  near
	  BUFFER_IS_BUFFER		;real mode buffer treated as such
					
	  cmp	  ss:display,0	   	;display escape chars?
	  jne	  s2p_xlat	   	; yes, jump
;move string to print buffer
	  MOVE_TO_REAL_BUF		;move string to print buffer   
	  ret				;and return to caller

;move string to print buffer, inserting double quotes and escape chars
s2p_xlat:
	  push	  bp			;save callers bp
	  push	  cx			;save length
	  push	  es
	  push	  di			;save real buffer address
	  push	  ds
	  push	  si			;save string address
	  sub	  sp,src_str		;allocate local storage
	  mov	  bp,sp

	  mov	  [bp].dest_start,di	;save buffer start
	  GET_REAL_BUFFER_TOP dx	
	  sub	  dx,pt_bfend+20
	  mov	  [bp].dest_top,dx	;save buffer end
	  
	  mov	  ax,ss
	  mov	  ds,ax			
	  mov	  es,ax			;setup for c call
	  mov	  ax,512
	  push	  ax
	  c_call  getmem		;allocate 512 bytes of storage
	  add	  sp,2			;dump arg from stack
	  cmp	  ax,0			;allocation successful?
	  jne	  s2p_write		; no, go write the buffer
	  mov	  cx,-1			;indicate error
	  jmp	  s2p_fin2		; and exit
s2p_write:
	  mov	  [bp].heap_top,510	;note top of heap buffer
	  mov	  word ptr [bp].heap_str,ax   ;save heap buffer address
	  mov	  word ptr [bp].heap_str+2,es
	  mov	  di,ax			;es:di = buffer
	  mov	  byte ptr es:[di],'"'  ;start buffer with escape char
	  inc	  di			;di = address within buffer
	  mov	  dx,1			;dx = number chars written

	  mov	  cx,[bp].stlen
	  jcxz	  s2p_done		;jump if null string

	  lds	  si,[bp].src_str	;ds:si addresses the string
;dx = #chars written, es:di=heap buffer offset, ds:si=string offset
s2p_loop:
	  cmp	  dx,[bp].dest_top	;room left in buffer?
	  jg	  s2p_err		; no,  return error status

	  lodsb 		   	;fetch char from string
	  cmp	  al,'\'	   	;Is char escape char?
	  je	  s2p_esc		;  yes, jump
	  cmp	  al,'"'	   	;Is char double quote?
	  jne	  s2p_stor	   	;  no, just go store it
s2p_esc:
	  mov	  ah,al
	  mov	  al,'\'
	  stosb				;store escape character
	  inc	  dx			;bump # chars written
	  xchg	  al,ah
s2p_stor:
	  stosb				;store escape character
	  inc	  dx			;bump # chars written
	  cmp	  di,[bp].heap_top	;heap buffer full?
	  jl	  s2p_cont		; no, continue
	  push	  cx			;save loop count
	  mov	  word ptr [bp].src_str,si  ;update string pointer
	  sub	  di,word ptr [bp].heap_str ;calc number chars written
	  mov	  cx,di			; and save in cx
	  lds	  si,[bp].heap_str	;ds:si = heap buffer
	  les	  di,[bp].dest_str	;es:di = real mode buffer
	  MOVE_TO_REAL_BUF autoinc     	;move string to print buffer
	  add	  word ptr [bp].dest_str,di  ;update next location in real buffer
	  pop	  cx			;restore count
	  lds	  si,[bp].src_str	;ds:si = pointer into string
	  les	  di,[bp].heap_str	;es:di = heap allocated string ptr
s2p_cont:
	  loop	  s2p_loop	        ;look at next char in printname
s2p_done:
	  mov	  al,'"'		;follow with escape char
	  stosb
	  inc	  dx	

	  sub	  di,word ptr [bp].heap_str ;calc number chars written
	  mov	  cx,di			; and save in cx
	  lds	  si,[bp].heap_str	;ds:si = heap buffer
	  les	  di,[bp].dest_str	;es:di = real mode buffer
	  MOVE_TO_REAL_BUF autoinc     	;move string to print buffer
	  mov	  cx,dx		        ;cx = actual # chars written
	  jmp	  s2p_fin		;finished
s2p_err:
	  mov	  cx,-1			;indicate error
s2p_fin:
	  push	  cx			;save length around call
	  mov	  bx,512		;length of heap buffer
	  pushm   <bx,word ptr [bp].heap_str>
	  C_call  rlsmem,,restore_es	;release character buffer
	  add	  sp,4			;dump args off stack
	  pop	  cx
s2p_fin2:
	  lds	  si,[bp].src_str	      ;restore ds:si
	  mov	  es,word ptr [bp].dest_str+2
	  mov	  di,[bp].dest_start	      ;es:di => real buffer start
	  add	  sp,call_bp		;dump args off stack
	  pop	  bp			;restore base pointer
	  ret			        ;return
str2pbuf  endp

prog	  ends
	  end

