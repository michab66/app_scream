;                                                       =====> PROIOSUP.ASM
;***************************************
;*   TIPC Scheme '84 Runtime Support   *
;*           I/O Utilities             *
;*                                     *
;*  (C) Copyright 1984,1985 by Texas   *
;*     Instruments Incorporated.       *
;*        All rights reserved.         *
;*                                     *
;* Date Written:  June 1984            *
;* Last Modification:  09 July 1985    *
;***************************************
          include scheme.equ

DGROUP    group   data
data      segment word public 'DATA'
          assume  DS:DGROUP
	  public  port_r, port_pg, port_ds, port_seg

;Current port data
port_r	  equ	  $
port_ds   dw      0
port_pg   dw      0                        ; port_reg
port_seg  dw	  0			   ; port segment address

;error messages
bad_set   db      "[VM INTERNAL ERROR] setadr: bad port",CR,LF,0

data      ends

PGROUP    group   prog
prog      segment byte public 'PROG'
          assume  CS:PGROUP

;For space and performance reasons, some procedures have been written in the
;  following style: the arguments are popped off the stack, and the
;  procedure ends in an indirect JMP instead of a RET.  In this source file,
;  the following are such procedures:
;                             isspace, copybig

;     Set Port Address
;       Calling sequence: ssetadr(page,disp)
;               Where ---- page: page number
;                          disp: displacement within page of port object
set_arg   struc
          dw      ?                         ; caller's BP
          dw      ?                         ; return address
pg        dw      ?                         ; adjusted page number
dis       dw      ?
set_arg   ends
          public  ssetadr               
ssetadr   proc    near
          push    bp
          mov     bp,sp
          mov     bx,[bp].pg		; adjusted page number
          cmp     byte ptr ptype+[bx],PORTTYPE*2 ; check port type
          je      sset_info			 ; jump if port
; Display error message
          lea     si,bad_set                ; address of error message
          push    si
          C_call  printf,,Load_ES           ; print error message
          mov     sp,bp
          C_call  force_de                  ; force_debug
          mov     sp,bp
          mov     ax,1                      ; return error status
          jmp     sset_ret
; get port information
sset_info:
	  mov	  port_pg,bx
          mov     bx,[bp].dis
          mov     port_ds,bx
          xor     ax,ax                     ; return status
sset_ret:
          pop     bp
          ret
ssetadr   endp

;     Save stack pointer in case of abort
;       Calling sequence: setabort()
;     NOTE: Due to the program-sensitive nature of this routine, a call to
;       SETABORT MUST be the very first in a C routine, and there must be
;       NO preassigned local variables.
          public  setabort
setabort  proc    near
          mov     BX,SP            ;Fetch stack pointer
          mov     SI,SS:[BX]       ;Fetch return address
          mov     CL,CS:[SI-6]     ;Fetch byte just before MOV BP,SP
          cmp     CL,55h           ;Compare with PUSH BP opcode
          je      nolocal          ;Jump if no extra stack space allocated
          xor     CH,CH            ;Clear CH
          add     BX,CX            ;Discount extra stack space
nolocal:  add     BX,2             ;Discount SETABORT's return address
          mov     DGROUP:abadr,BX  ;Save pointer
          ret
setabort  endp


;     Abort & set stack to saved pointer
;       Calling sequence: abort(code)
;               where:  code ---- type of error message to print
          public  abort
abort     proc
          pop     AX               ;Discard return address (leaving CODE)
          C_call  errmsg           ;Print error message
          pop     AX               ;Get "value"
          mov     SP,DGROUP:abadr  ;Restore stack for abort
          pop     BP               ;Restore BP
          ret                      ;Return (from aborted operation)
abort     endp


;     Find approximate space left on stack
;       Caling sequence: stkspc()
          extrn   _base:word
          public  stkspc
stkspc    proc    near
          mov     AX,SP
          sub     AX,DGROUP:_base
          ret
stkspc    endp

;     Parse input integer
;       Calling sequence: buildint(work,buf,base)
;               Where ---- work: pointer to some workspace
;                          buf:  pointer to integer characters
;                          base: numeric base
int_args  struc
          dw      ?                ;Caller's BP
          dw      ?                ;Return address
bigptr    dw      ?                ;Pointer to workspace
atptr     dw      ?                ;Pointer to integer characters
bas       dw      ?                ;Numeric base
int_args  ends
          public  buildint
buildint  proc    near
          push    BP
          mov     BP,SP
          cld                      ;Direction forward
          mov     SI,[BP].atptr    ;Point DS:SI to characters
          lodsb                    ;Fetch first character
          cmp     AL,'-'           ;Negative?
          pushf                    ;Save ZF
          je      negint           ;Jump if negative
          cmp     AL,'+'           ; or if signed positive
          je      negint
          dec     SI               ;Point SI back to first char
negint:   mov     CX,1             ;At first, bignum is one word
          add     word ptr[BP].bigptr,3  ;Point BIGPTR to bignum proper
skiplp:   lodsb                    ;Get first number char
          cmp     AL,'#'           ;We know the base - skip all #x's
          jne     skipped          ;All #x's skipped - parse number
          inc     SI               ;Otherwise check again
          jmp     skiplp
biglp:    lodsb                    ;Get next int character
skipped:  mov     DI,[BP].bigptr   ;Point ES:DI to workspace
          sub     AL,'0'           ;Character -> number
          js      bigend           ;Jump if number ended
          cmp     AL,9             ;Jump if ordinary digit
          jbe     orddig
          and     AL,7             ;Otherwise, parse extra hex digit
          add     AL,9
orddig:   xor     AH,AH            ;Clear AH
          call    bigx10           ;Multiply bignum by 10, adding digit
          jmp     biglp
bigend:   sub     DI,3             ;Point DI back to start of buffer
          mov     AX,CX            ;Save integer size
          stosw
          xor     AL,AL            ;Clear AX
          popf                     ;Get number's sign
          jne     stosgn           ;Store it
          inc     AL
stosgn:   mov     [DI],AL
          pop     BP               ;Restore BP
          ret
;BIGX10: Multiply bignum at ES:[DI], size=CX words, by BASE and add AX
bigx10:   push    CX
          mov     DX,AX            ;Transfer digit to add
          cld
x10lp:    mov     AX,[DI]          ;Get word to multiply
          call    wordx10          ;Multiply word by 10
          stosw                    ;Replace result
          loop    x10lp            ;Loop 'til done
          pop     CX               ;Restore CX
          or      DX,DX            ;Does a carry remain?
          jz      samlen           ;Jump if not
          mov     ES:[DI],DX       ;Otherwise, enlarge bignum
          inc     CX
samlen:   ret
;WORDX10: Multiply AX by BASE and add DX;   product in AX, carry in DX
wordx10:  push    CX                ;Save value of CX
          push    DX                ;Save carry in
          mul     word ptr[BP].bas  ;Multiply by BASE
          pop     CX                ;Restore carry to CX
          add     AX,CX             ;Add carry
          adc     DX,0
          pop     CX                ;Restore CX
          ret
buildint  endp

;     Copy bignum data to a math buffer
;       Calling sequence: copybig(pg,ds,buf)
;               Where: pg,ds ---- page & displacement of bignum
;                      buf ------ pointer to math buffer
cb_args   struc
          dw      ?                ;Caller's BP
          dw      ?                ;Return address
cbpg      dw      ?                ;Page
cbds      dw      ?                ;Displacement
cbbuf     dw      ?                ;Buffer pointer
cb_args   ends
          public  copybig
copybig   proc    near
          pop     BX               ;Pop return address to BX
          mov     DX,DS            ;Save DS in DX
          pop     SI               ;Fetch logical page number
          sal     SI,1             ;Convert
	  LoadPage DS,SI	   ;Get page segment
;;;       mov     DS,DGROUP:pagetabl+[SI]  ;Get page segment
          pop     SI               ;Get displacement
          mov     AX,[SI]+1        ;Get size of bignum proper (words)
          sub     AX,4
          shr     AX,1
          add     SI,3             ;Point DS:SI to sign byte
          pop     DI               ;Point ES:DI to math buffer
          cld                      ;Direction forward
          stosw                    ;Store bignum size in math buffer
          movsb                    ;Copy sign byte
          mov     CX,AX            ;Copy bignum proper
          rep     movsw
          mov     DS,DX            ;Restore DS
          jmp     BX               ;Return
copybig   endp

;     Convert buffered bignum to ASCII
;       Calling sequence: big2asc(mathbuf,charbuf)
;               Where: mathbuf --- pointer to buffered bignum
;                      charbuf --- pointer to ASCII charcater array
b2a       struc
          dw      ?                ;Caller's BP
          dw      ?                ;Return address
mbuf      dw      ?                ;Math buffer
cbuf      dw      ?                ;Character buffer
b2a       ends
          public  big2asc
big2asc   proc    near
          push    BP
          mov     BP,SP
          mov     SI,[BP].mbuf     ;Fetch math buffer pointer
          mov     DI,[BP].cbuf     ;Fetch character buffer pointer
          cld                      ;Direction forward
          lodsw                    ;Fetch bignum size
          mov     CX,AX
          lodsb                    ;Fetch sign
          test    AL,1             ;Skip on positive bignum
          jz      posbig
          mov     AL,'-'           ;First character: minus
          stosb
posbig:   mov     BX,10            ;Set divisor to 10
          and     AX,1             ;Push 0 or 1 (1 if start with -)
prtbglp:  push    AX
          call    divbig           ;Divide bignum by 10
          mov     AL,DL            ;Store digit
          add     AL,'0'
          stosb
          pop     AX               ;Increment character counter
          inc     AX
          or      CX,CX            ;Loop until bignum is zeroed
          jnz     prtbglp
          mov     CX,AX            ;Transfer & save character count
          push    AX
          sub     DI,CX            ;Point DI to beginning of string
          call    reverse          ;Reverse digits in ASCII bignum
          pop     AX               ;Restore character count
          pop     BP
          ret
;Divide bignum at DS:SI, length CX words, by BX (ES=DS)
divbig:   push    CX               ;Save count
          push    DI               ;Save DI
          add     SI,CX            ;Point SI to last word (most signif.)
          add     SI,CX
          sub     SI,2
          cmp     [SI],BX          ;Will working length be reduced?
          pushf
          mov     DI,SI            ;ES:DI = DS:SI
          std                      ;Direction backward
          xor     DX,DX            ;Clear carry in
divlp:    lodsw                    ;Fetch piece of dividend
          div     BX
          stosw                    ;Store quotient (retain remainder)
          loop    divlp
          add     SI,2             ;Point SI again to first word
          popf
          pop     DI
          pop     CX
          jae     divdone          ;Jump if bignum length not reduced
          dec     CX
divdone:  ret                      ;Remainder left in DX
;Reverse the string containing CX characters at ES:DI (ES=DS)
reverse:  cmp     byte ptr[DI],'-'  ;Start with minus?
          jne     revpos           ;No, reverse whole string
          inc     DI               ;Otherwise, don't include minus in reverse
          dec     CX
revpos:   mov     SI,DI            ;Point SI to last string char
          add     SI,CX
          dec     SI
          shr     CX,1             ;Number of switches
          or      CX,CX            ;Jump if no switches to make
          jz      revend
revlp:    mov     AL,[DI]          ;Exchange outside bytes
          xchg    AL,[SI]
          stosb
          dec     SI               ;Move pointers inward
          loop    revlp
revend:   ret
big2asc   endp

;     Is character a whitespace?
;       Calling sequence: isspace(ch)
;           Where ch = character to check
;       Returns zero iff not a whitespace
;     NOTE: Before use, the C macro ISSPACE must not be defined
isspargs  struc
          dw      ?                 ;Return address
issparg   dw      ?                 ;Argument
isspargs  ends
          public  isspace
isspace   proc    near
          pop     DI                ;Get return address
          pop     AX                ;Get argument
          cmp     AL,' '
          je      issp
          cmp     AL,9
          jb      isntsp
          cmp     AL,13
          jbe     issp
isntsp:   xor     AX,AX             ;Set to zero
issp:     jmp     DI                ;Return
isspace   endp

prog      ends
          end

