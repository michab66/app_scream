;                                                       =====> SSTACK.ASM
;***************************************
;*   TIPC Scheme '84 Runtime Support   *
;*   Interpreter -- Stack Operations   *
;*                                     *
;*  (C) Copyright 1984,1985 by Texas   *
;*     Instruments Incorporated.       *
;*        All rights reserved.         *
;*                                     *
;* Date Written:  2 May 1984           *
;* Last Modification:  22 October 1985 *
;***************************************
;*   Modification History:
;*      06 Mar 86 - Recoded the C_push and C_pop routines to attemp to
;*        (JCJ)     improve their performance and memory utilization.
;*
          include scheme.equ
          include sinterp.mac

          include sinterp.arg
          include stackf.equ       ; define stack frame format

XGROUP    group   PROGX
DGROUP    group   data
data      segment word public 'DATA'
          assume  DS:DGROUP
          public  stk_in,stk_out
stk_in    dd      0                ; number of bytes moved into the stack
stk_out   dd      0                ; number of bytes moved out of the stack
m_%exec   db      "%EXECUTE",0
m_stk_un  db      "[VM INTERNAL ERROR] Stack underflow",LF,0
m_stk_ov  db      LF,"[VM ERROR encountered!] Recursion too deep: Stack "
          db      "overflow",LF,0
clos_ptr  dw      0                ; register number containing closure pointer

m_APPLY   dw      APPLY_ARG_LIMIT_ERROR
m_AP1     db      "APPLY",0        ; text for "apply" function name
m_AP_adr  dw      m_AP1            ; address of above text
m_one     dw      1                ; a constant "one" (1)


data      ends

PGROUP    group   prog
prog      segment byte public 'PROG'
          assume  CS:PGROUP

stk_int   proc    near

;     Entry points defined in "sinterp.asm"
          extrn   next:near        ; Top of interpreter
          extrn   next_PC:near     ; Reload ES,SI at top of interpreter
          extrn   next_SP:near     ; All of the above, with "mov SP,BP" first
          extrn   src_err:near     ; "source operand error" message display
          extrn   sch_err:near     ; Link to Scheme Debugger
          extrn   printf_c:near    ; Error message print routine

          extrn   %allocbl:far     ; Far linkage to "alloc_block"

;************************************************************************
;*                                                      AL              *
;* Push register onto stack                     PUSH    reg             *
;*                                                                      *
;* Purpose:  Interpreter support to cause the contents of one of the    *
;*              VM's general registers to be pushed onto the VM's       *
;*              runtime stack                                           *
;************************************************************************
          public  spush
spush:    lods    byte ptr ES:[SI] ; load number of register to push
spush1:   mov     DI,TOS           ; load top of stack pointer
          cmp     DI,STKSIZE-PTRSIZE ; test for overflow
          jge     spush2           ; jump if overflow will occur
          add     DI,PTRSIZE       ; decrement stack top pointer
          mov     TOS,DI           ; update TOS pointer in memory
          mov     BX,AX            ; copy register number
          mov     AL,byte ptr reg0_pag+[BX] ; load page number from register
          mov     S_stack+[DI].car_page,AL ;   and move to the stack
          mov     AX,reg0_dis+[BX] ; same for displacement
          mov     word ptr S_stack+[DI].car,AX
          jmp     next
;     process stack overflow-- copy contents to heap
spush2:   pushm   <AX,SI,ES>       ; preserve "important" regs across call
          call    stk_ovfl         ; handle overflow situation
          popm    <ES,SI,AX>       ; restore "important" registers
          jmp     spush1           ; re-try push


;************************************************************************
;*                                                      AL              *
;* Pop register from stack                      POP     reg             *
;*                                                                      *
;* Purpose:  Interpreter support to cause the contents of one of the    *
;*              VM's general registers to be replaced by popping the    *
;*              value off the top of the VM's runtime stack             *
;*                                                                      *
;* Note:  There's no need to check for stack underflow on a simple      *
;*      POP, because the stack is broken into segments only at stack    *
;*      frame boundaries.  Underflow can occur only when stack space    *
;*      for a stack frame is released (i.e., during an EXIT).           *
;************************************************************************
          public  spop
spop:     lods    byte ptr ES:[SI] ; load number of register to pop
          mov     DI,TOS           ; load top of stack pointer
          mov     BX,AX            ; copy register number
          mov     AL,S_stack+[DI].car_page ; move page no. from stack
          mov     byte ptr reg0_pag+[BX],AL ; and update in register
          mov     AX,word ptr S_stack+[DI].car ; same for displacement
          mov     reg0_dis+[BX],AX
          sub     DI,PTRSIZE       ; decrement TOS pointer
          mov     TOS,DI           ; update TOS pointer in memory
          jmp     next


;************************************************************************
;*                                                      AL              *
;* Drop-- remove top elements from stack        DROP    n               *
;*                                                                      *
;* Purpose:  Interpreter support to cause the top "n" elements of the   *
;*              VM's runtime stack to be discarded.  "n" is determined  *
;*              from the operand of the DROP instruction                *
;*                                                                      *
;* Note:  There's no need to check for stack underflow on a DROP        *
;*      because the stack is broken into segments only at stack         *
;*      frame boundaries.  Underflow can occur only when stack space    *
;*      for a stack frame is released (i.e., during an EXIT).           *
;************************************************************************
          public  sdrop
sdrop:    lods    byte ptr ES:[SI] ; load number of elements to drop
          mov     DX,AX            ; multiply by 3 (size of element)
          shl     AX,1
          add     AX,DX
          sub     TOS,AX           ; update TOS pointer in memory
          jmp     next             ; return to interpreter


;************************************************************************
;*                                                      AL   AH         *
;* Local from local stack frame                 LDLOCAL dest,entry      *
;************************************************************************
          public  ld_local
ld_local: lods    word ptr ES:[SI] ; load dest reg, entry number operands
          mov     BL,AL            ; copy destination register number
          mov     DI,BX            ;  into DI (clear high order byte)
          mov     BL,AH            ; copy the entry number (clear high byte)
          mov     AX,BX            ; BX <- entry * 3
          sal     AX,1
          add     BX,AX
          add     BX,FP            ; BX <- FP + (entry * 3)
          mov     AL,S_stack+[BX].sf_dat_p ; move page number of entry to
          mov     byte ptr reg0_pag+[DI],AL ;  destination register
          mov     AX,word ptr S_stack+[BX].sf_dat_d  ; move displacement of
          mov     reg0_dis+[DI],AX ;  entry to destination register
          jmp     next

;************************************************************************
;*                                                      AL  AH          *
;* Store into local stack frame                 STLOCAL src,entry       *
;************************************************************************
          public  st_local
st_local: lods    word ptr ES:[SI] ; load dest reg, entry number operands
          mov     BL,AL            ; copy destination register number
          mov     DI,BX            ;  into DI (clear high order byte)
          mov     BL,AH            ; copy the entry number (clear high byte)
          mov     AX,BX            ; BX <- entry * 3
          sal     AX,1
          add     BX,AX
          add     BX,FP            ; BX <- FP + (entry * 3)
;         cmp     BX,TOS           ; store out of range?
;         jgt     st_err           ; if so, record error
          mov     AL,byte ptr reg0_pag+[DI] ; move page number of entry from
          mov     S_stack+[BX].sf_dat_p,AL ;  destination register
          mov     AX,reg0_dis+[DI] ; move displacement of entry from
          mov     word ptr S_stack+[BX].sf_dat_d,AX ;  destination register
          jmp     next


;************************************************************************
;*                                                      AL   AL    AH   *
;* Load from higher lexical level               LDLEX   dest,entry,lvl  *
;************************************************************************
          public  ld_lex
ld_lex:   lods    byte ptr ES:[SI] ; load destination register operand
          push    AX               ;  and save it
          lods    word ptr ES:[SI] ; load lexical level and entry number
          save    <SI>             ; save current location pointer
          mov     BL,AH            ; clear high order byte of the lexical
          mov     CX,BX            ;  level number delta and move to CX
          mov     BL,AL            ; align, and save entry number
          push    BX
          call    delta_lv         ; get pointer to parent's stack frame
          pop     AX               ; get entry number
          mov     BX,AX            ; BX <- entry number * 3
          shl     AX,1
          add     BX,AX
          pop     DI               ; get destination register number
          mov     AL,ES:[SI].sf_dat_p+[BX] ; copy lexical entry from stack
          mov     byte ptr reg0_pag+[DI],AL ; frame to destination register
          mov     AX,ES:[SI].sf_dat_d+[BX]
          mov     reg0_dis+[DI],AX
          jmp     next_PC          ; return to the interpreter


;************************************************************************
;*                                                      AL  AL    AH    *
;* Store into higher lexical level              STLEX   src,entry,lvl   *
;************************************************************************
          public  st_lex
st_lex:   lods    byte ptr ES:[SI] ; load source register operand
          push    AX               ;  and save it
          lods    word ptr ES:[SI] ; load lexical level and entry number
          save    <SI>             ; save current location pointer
          mov     BL,AH            ; clear high order byte of the lexical
          mov     CX,BX            ;  level number delta and move to CX
          mov     BL,AL            ; align, and save entry number
          push    BX
          call    delta_lv         ; get pointer to parent's stack frame
          pop     AX               ; get entry number
          mov     BX,AX            ; BX <- entry number * 3
          shl     AX,1
          add     BX,AX
          pop     DI               ; get source register number
          mov     AL,byte ptr reg0_pag+[DI] ; copy contents of register into
          mov     ES:[SI].sf_dat_p+[BX],AL ; lexical entry of stack
          mov     AX,reg0_dis+[DI]
          mov     ES:[SI].sf_dat_d+[BX],AX
          jmp     next_PC          ; return to the interpreter


;************************************************************************
;*                                              AX  AL        AH        *
;* Call local routine                   CALL    lbl,delta-lvl,delta-heap*
;************************************************************************
          public  call_lcl
call_lcl: mov     AX,offset PGROUP:next_PC ; For a "CALL", make a tail
          push    AX               ;  recursive call to following routine

cl_l_sub: lods    word ptr ES:[SI] ; load branch displacement
          mov     DX,AX            ;  and save in register DX

          lods    word ptr ES:[SI] ; load delta-level,delta-heap numbers
          inc     AL               ; increment releative lexical level
          mov     BL,AL            ; isolate delta-lvl and save it
          push    BX
          mov     BL,AH            ; isolate delta-heap and save it, too
          push    BX

          add     DX,SI            ; compute branch destination address
          mov     [BP].save_SI,DX  ; store updated location counter

          call    new_SF           ; allocate new stack frame on top of stack
          mov     SI,BX            ; save pointer to new stack frame

          pop     CX               ; restore the delta-heap argument
          call    delta_hp         ; determine new heap env pointer
          mov     S_stack+[SI].sf_hpage,BL ; store new heap env pointer into
          mov     word ptr S_stack+[SI].sf_hdisp,DI ;  new stack frame

          pop     CX               ; restore the delta-lvl argument
          push    SI               ; save new stack frame pointer
          call    delta_lv         ; get static link
          pop     SI               ; retrieve new stack frame pointer
          mov     word ptr S_stack+[SI].sf_sdisp,BX ; update static link

          mov     FP,SI            ; update current frame pointer
          ret                      ; return to interpreter, or call/cc support


;************************************************************************
;*                                              AX  AL        AH        *
;* Call local routine tail recursively  CALL-TR lbl,delta-lvl,delta-heap*
;************************************************************************
          public  call_ltr
call_ltr: mov     AX,offset PGROUP:next_PC ; For a "CALL-TR", make a tail
          push    AX               ;  recursive call to following routine

cl_lt_sb: lods    word ptr ES:[SI] ; load branch displacement
          mov     DX,AX            ;  and save in register DX

          lods    word ptr ES:[SI] ; load delta-level,delta-heap numbers
          inc     AL               ; increment releative lexical level
          mov     BL,AL            ; isolate delta-lvl and save it
          push    BX
          mov     BL,AH            ; isolate delta-heap and save it, too
          mov     CX,BX

          add     DX,SI            ; compute branch destination address
          mov     [BP].save_SI,DX  ; store updated location counter

          mov     AX,FP            ; load pointer to current stack frame
          mov     SI,AX
          add     AX,SF_OVHD-PTRSIZE
          mov     TOS,AX           ; drop any local var's off top of stack

          call    delta_hp         ; determine new heap env pointer
          mov     S_stack+[SI].sf_hpage,BL ; store new heap env pointer into
          mov     word ptr S_stack+[SI].sf_hdisp,DI ;  new stack frame

          mov     S_stack+[SI].sf_cl_pg,NIL_PAGE*2 ; nil out closure pointer
          mov     word ptr S_stack+[SI].sf_cl_ds,NIL_DISP ; entry in stack frame

          pop     CX               ; restore the delta-lvl argument
          push    SI               ; save pointer to stack frame
          call    delta_lv         ; get static link
          pop     SI               ; retrieve pointer to stack frame
          mov     word ptr S_stack+[SI].sf_sdisp,BX ; update static link

          ret                      ; return to interpreter, or call/cc support


;************************************************************************
;*                                                      AL  AH          *
;* Call closed procedure                CALL-CLOSURE    ftn,#args       *
;*                                                                      *
;* Purpose:  Scheme interpreter support for procedure calls to fully    *
;*              closed functions                                        *
;************************************************************************
          public  call_clo
call_clo: mov     AX, offset PGROUP:next_PC ; For a "CALL-CLOSURE" make a tail
          push    AX               ;  recursive call to the following routine

          lods    word ptr ES:[SI] ; fetch ftn reg, number of args passed
cl_c_sub: mov     BL,AH            ; isolate the number of arguments
          push    BX               ;  passed and save it
          mov     BL,AL            ; copy the procedure object register
          mov     DI,reg0_pag+[BX] ; load page number of closure pointer
          cmp     byte ptr ptype+[DI],CLOSTYPE*2
          je      call_cok         ; if a regular closure, jump
          jmp     call_cnt         ; otherwise, a continuation (probably)
;     Procedure call to a closed procedure
call_cok: push    BX               ; save number of procedure pointer reg
          call    new_SF           ; allocate a new stack frame
          pop     SI               ; restore reg number with closure pointer

;     Load the pointer to the closure object from the operand register
call_xxx: mov     clos_ptr,SI      ; save number of register containing closure
          mov     DI,reg0_pag+[SI]
          mov     SI,reg0_dis+[SI]
          LoadPage ES,DI
;;;       mov     ES,pagetabl+[DI]

;     Put the closure pointer into the newly allocated stack frame
          mov     AX,DI            ; copy closure's page number to AL
          mov     S_stack+[BX].sf_cl_pg,AL ;  then copy into stack frame
          mov     word ptr S_stack+[BX].sf_cl_ds,SI ; put disp into frame, too

;     Copy the pointer to the procedure's heap environment from the closure
;       object to the new stack frame
          mov     AL,ES:[SI].clo_hpag
          mov     S_stack+[BX].sf_hpage,AL
          mov     AX,ES:[SI].clo_hdis
          mov     word ptr S_stack+[BX].sf_hdisp,AX

;     Dummy up the Static Link in the new Stack Frame
          mov     word ptr S_stack+[BX].sf_sdisp,0

;     Update the current frame pointer to point to new stack frame
          mov     FP,BX

;     Obtain the entry point address from the closure object
          mov     AX,ES:[SI].clo_cb_d ; define the code base register
          mov     CB_dis,AX
          add     AX,ES:[SI].clo_edis ; add the entry point offset
          mov     [BP].save_SI,AX  ;  and set up for load into location pointer
          xor     AX,AX
          mov     AL,ES:[SI].clo_cb_p
          mov     byte ptr CB_pag,AL
          mov     DI,AX            ; obtain the code block page's paragraph
          LoadCode AX,DI
;;;       mov     AX,pagetabl+[DI] ;  address and update in memory
          mov     [BP].save_ES,AX

;     Determine if the closed function is a mulambda
          pop     CX               ; get number of args passed
          mov     AX,ES:[SI].clo_narg ; load number of args expected
          shl     AX,1             ; sign extend the number of
          sar     AX,1             ;  arguments expected
          jl      call_mu          ; if #args negative, then a mulambda (jump)
          cmp     AX,CX            ; verify args passed/expected agree
          je      call_crt         ; if so, jump

;     ***Error-- wrong number of arguments passed to a closed function***
cl_wrng:  mov     AX,clos_ptr      ; load number of register w/ closure pointer
          add     AX,offset reg0
          pushm   <AX,CX>          ; push count of args passed, closure ptr
cl_wrng1: C_call  wrong_ar,,Load_ES ; print error message and fixup VM regs
          restore <SI>             ; load address of next instruction
          jmp     sch_err          ; link to Scheme error routine

call_crt: ret                      ; return to interpreter, or call/cc support

;     Funtion being called is a mulambda-- cons arguments into a list
call_mu:  mov     SI,CX            ; compute the address of the last
          sal     SI,1             ;  register which contains an argument
          sal     SI,1             ;  to be passed to the mulambda
          add     SI,offset reg0

          lea     DI,[SI]+size C_ptr ; load address of register page last arg
          mov     [DI].C_page,NIL_PAGE*2 ; put a value of "nil" into the
          mov     [DI].C_disp,NIL_DISP   ;  register for end of list

          mov     ES,[BP].C_ES     ; set up ES for calls to "cons"

          mov     DX,CX            ; save number of arguments passed
          add     CX,AX            ; adjust number of arguments passed
          inc     CX               ;  by number required
          je      mu_ret           ; if #passed = #required, jump
          jl      mu_wrng          ; if too few passed, jump

mu_loop:  push    DI               ; push addr of "cdr" register
          push    SI               ; push addr of "car" register
          push    SI               ; push addr of dest reg (result of cons)
          C_call  cons,<CX>        ; cons together ptrs in regs "n" and "n+1"
          add     SP,WORDINCR      ; drop one copy of SI from the 8088's stack
          pop     SI               ; restore value of SI
          pop     DI               ; restore value of DI
          restore <CX>             ; restore registers destroyed by the call
          mov     [DI].C_page,UN_PAGE*2 ; set register "n+1" to "***unbound***"
          mov     [DI].C_disp,UN_DISP
          mov     DI,SI            ; update pointers for next iteration
          sub     SI,size C_ptr
          loop    mu_loop          ; repeat for all arguments passed

mu_ret:   ret                      ; return to interpreter, or call/cc support

;     Too few required arguments-- inform user
mu_wrng:  mov     CX,DX            ; restore count of args passed
          jmp     cl_wrng          ; print "wrong number of args" message

;     Function call is invoking a continuation-- unless we've got an error
call_cnt: cmp     ptype+[DI],CONTTYPE*2
          je      cl_cn_ok
;     ***Error-- thing being called isn't a procedure object-- note***
;     Note:  at this point, the number of arguments passed has been pushed
;               onto the runtime stack
          add     BX,offset reg0    ; compute address of "functional" register
          push    BX                ;  and push as argument
          C_call  not_proc,,Load_ES ; call:  not_procedural_object(obj, #args);
          restore <SI>              ; load address of next instruction
          jmp     sch_err           ; link to Scheme debugger

;     Oh, wow! we've got a continuation to envoke (or is that invoke?)
;
;       Note:  the contents of the stack is restored by making the VM's
;               previous stack segment register point to the continuation
;               object and signaling an underflow condition.  This restores
;               the stack, BASE, TOS, PREV_pag, and PREV_dis.  The
;               remainder of this code fetches the values of CB_pag,
;               CB_dis, FP, and LP from the continuation object.
;
cl_cn_ok: push    BX               ; save pointer to continuation object
          mov     AL,byte ptr reg0_pag+[BX] ; copy continuation pointer into
          mov     byte ptr PREV_pag,AL      ;  PREV_reg
          mov     AX,reg0_dis+[BX]
          mov     PREV_dis,AX

          call    stk_unfl         ; signal a stack underflow condition

          pop     DI               ; retrieve ptr to reg with continuation ptr.
          mov     SI,reg0_pag+[DI] ; make ES:[SI] point to the continuation
          LoadPage ES,SI
;;;       mov     ES,pagetabl+[SI] ;  object
          mov     SI,reg0_dis+[DI]

          xor     BX,BX
          mov     BL,ES:[SI].con_cb_p
          mov     byte ptr CB_pag,BL
          LoadCode AX,BX
;;;       mov     AX,pagetabl+[BX] ; obtain the code block's paragraph address
          mov     [BP].save_ES,AX
          mov     AX,ES:[SI].con_cb_d ; restore code base pointer
          mov     CB_dis,AX

          add     AX,ES:[SI].con_ret ; restore return address displacement
          mov     [BP].save_SI,AX

          mov     AX,ES:[SI].con_ddis ; restore FP from dynamic link
          sub     AX,BASE          ; adjust for current stack buffer base
          mov     FP,AX

          mov     AL,ES:[SI].con_fl_p ; restore fluid environment (FNV_reg)
          mov     byte ptr FNV_pag,AL
          mov     AX,ES:[SI].con_fl_d
          mov     FNV_dis,AX

          mov     AL,ES:[SI].con_gl_p ; restore global environment (GNV_reg)
          mov     byte ptr GNV_pag,AL
          mov     AX,ES:[SI].con_gl_d
          mov     GNV_dis,AX

          pop     AX               ; get number of arguments passed
          cmp     AX,1             ; one argument passed?
          jne     cl_cn_er         ; if so, good!  we can continue (fall thru)
cl_cn_rt: ret                      ; return to interpreter, or call/cc support

;     ***error-- wrong number of arguments passed to a continuation***
cl_cn_er: add     DI,offset reg0   ; load address of continuation's register
          pushm   <DI,AX>          ; push continuation ptr, args passed
          jmp     cl_wrng1         ; process error condition


;************************************************************************
;*                                                      AL  AH          *
;* Call closed proc tail recursively    CALL-CLOSURE-TR ftn,#args       *
;*                                                                      *
;* Purpose:  Scheme interpreter support for procedure calls to fully    *
;*              closed functions tail recursively                       *
;************************************************************************
          public  call_ctr
call_ctr: mov     AX,offset PGROUP:next_PC ; For "CALL-CLOSURE-TR" make tail
          push    AX               ;  recursive call to the following routine

          lods    word ptr ES:[SI] ; fetch ftn reg, number of args passed
cl_ct_sb: mov     BL,AH            ; isolate the number of arguments
          push    BX               ;  passed and save it
          mov     BL,AL            ; copy the procedure object register
          mov     DI,reg0_pag+[BX] ; load page number of procedure object
          cmp     ptype+[DI],CLOSTYPE*2 ; is it a closure data object?
          je      call_cko         ; if a regular closure, jump
          jmp     call_cnt         ; otherwise, a continuation (probably)

;     Procedure call (tail recursive) to a closed procedure
call_cko: mov     SI,BX            ; copy reg number with closure pointer
          mov     AX,FP            ; use current stack frame for this call
          mov     BX,AX            ; drop any local vars from top of stack
          add     AX,SF_OVHD-PTRSIZE
          mov     TOS,AX           ; update TOS pointer

          jmp     call_xxx         ; continue processing as non-tr call


;************************************************************************
;* Call/cc local                CALL/CC         lbl,delta-lvl,delta-heap*
;*                                                                      *
;* Purpose:  Interpreter support for a local call with current          *
;*              continuation                                            *
;*                                                                      *
;* Description:                                                         *
;*              1.  The local CALL support is called to create a new    *
;*                  stack frame and to establish the VM's registers     *
;*                  for the branch to the called routine.               *
;*              2.  A stack overflow condition is signaled to cause     *
;*                  the contents of the stack to be saved on the heap   *
;*                  in a continuation object format.                    *
;*              3.  Fields in the continuation object are updated to    *
;*                  cause control to return to the correct place when   *
;*                  the continuation is invoked.                        *
;*              4.  Control returns to the Scheme interpreter.          *
;************************************************************************
          public  call_cc
call_cc:  call    cl_l_sub         ; call CALL's alternate entry point

call_cc1: call    stk_ovfl         ; signal stack overflow

          mov     BX,PREV_pag      ; move pointer to continuation into R1
          mov     DI,PREV_dis
          mov     reg1_pag,BX
          mov     reg1_dis,DI
          LoadPage ES,BX
;;;       mov     ES,pagetabl+[BX]

          mov     SI,FP            ; create a pointer to the current stack
          add     SI,offset S_stack ; frame (the new one)

          mov     AL,[SI].sf_cb_pag ; copy the value of the VM's code base
          mov     ES:[DI].con_cb_p,AL ;  into the continuation object
          mov     AX,[SI].sf_cb_dis
          mov     ES:[DI].con_cb_d,AX

          mov     AX,[SI].sf_ret   ; copy the return address displacement
          mov     ES:[DI].con_ret,AX ; into the continuation object

          mov     AX,[SI].sf_ddisp ; copy the dynamic link into the
          mov     ES:[DI].con_ddis,AX ; continuation object

          jmp     next_PC          ; return to the interpreter

;************************************************************************
;* Call/cc tail recursively     CALL/CC-TR      lbl,delta-lvl,delta-heap*
;*                                                                      *
;* Purpose:  Interpreter support for a tail recursive local call with   *
;*               current continuation                                   *
;*                                                                      *
;* Description:                                                         *
;*              1.  The local CALL-TR support is called to update the   *
;*                  current stack frame and to establish the VM's       *
;*                  registers for the branch to the called routine.     *
;*              2.  Control transfers to the CALL/CC support to create  *
;*                  the continuation object.                            *
;************************************************************************
          public  cl_cctr
cl_cctr:  mov     AX,offset PGROUP:call_cc1 ; define return address
          push    AX
          jmp     cl_lt_sb         ; tail recursive call to CALL-TR's
                                   ;  secondary entry point


;************************************************************************
;*                                                              AL      *
;* Call/cc with of procedure object     CALL/CC-CLOSURE         ftn     *
;*                                                                      *
;* Purpose:  Interpreter support for a call with current continuation   *
;*              of a fully closed function                              *
;*                                                                      *
;************************************************************************
          public  clcc_c
clcc_c:   lods    byte ptr ES:[SI] ; load register number pointing to closure
          mov     AH,1             ; indicate one argument being passed
          push    AX               ;  and save "operands"

          mov     AX,FP            ; save current stack frame pointer
          add     AX,BASE
          push    AX

          mov     AX,TOS           ; update FP to where it will be after
          add     AX,PTRSIZE       ;  the new stack frame is built
          mov     FP,AX

          call    stk_ovfl         ; signal stack overflow to create
                                   ;  continuation data object

          mov     BX,PREV_pag      ; load pointer to continuation
          mov     DI,PREV_dis
          LoadPage ES,BX
;;;       mov     ES,pagetabl+[BX]

          mov     AL,byte ptr CB_pag ; copy the value of the VM's code base
          mov     ES:[DI].con_cb_p,AL ;  into the continuation object
          mov     AX,CB_dis
          mov     ES:[DI].con_cb_d,AX

          sub     SI,AX
          mov     ES:[DI].con_ret,SI ; place return addr in continuation object
          add     SI,AX

          pop     AX               ; define dynamic link in continuation
          mov     ES:[DI].con_ddis,AX ;  object
          sub     AX,BASE          ; put FP back to where it should be
          mov     FP,AX            ; Note:  FP's now negative (TOS is 0)

;     Perform the Call-Closure-Tail-Recursive
          mov     AL,byte ptr PREV_pag ; save the pointer to the new
          mov     byte ptr tm2_page,AL ;  continuation
          mov     AX,PREV_dis
          mov     tm2_disp,AX
          pop     AX               ; recover "operands" to call-closure
          call    cl_c_sub         ; call CALL-CLOSURE
          mov     AL,byte ptr tm2_page ; move continuation pointer into
          mov     byte ptr reg1_pag,AL ;  VM register R1
          mov     AX,tm2_disp
          mov     reg1_dis,AX
          jmp     next_PC          ; return to interpreter



;************************************************************************
;*                                                              AL      *
;* Call/cc with of procedure object     CALL/CC-CLOSURE-TR      ftn     *
;*                                                                      *
;* Purpose:  Interpreter support for a tail recursive call with current *
;*              continuation of a fully closed function                 *
;*                                                                      *
;* Description:                                                         *
;*              1.  The CALL/CC-CLOSURE argument is fetched.            *
;*              2.  The current continuation is formed using the        *
;*                  caller's return address (since there's no way to    *
;*                  return here from the tail recursive call).          *
;*                  The pointer to the continuation is placed into      *
;*                  VM register 1.                                      *
;*              3.  The CALL-CLOSURE-TR code is called to complete the  *
;*                  call sequence.                                      *
;************************************************************************
          public  clcc_ctr
clcc_ctr: lods    byte ptr ES:[SI] ; load register number pointing to closure
          mov     AH,1             ; indicate one argument being passed
          push    AX               ;  and save "operands"

          call    stk_ovfl         ; signal stack overflow to create
                                   ;  continuation data object

          mov     BX,PREV_pag      ; load pointer to continuation
          mov     DI,PREV_dis
          LoadPage ES,BX
;;;       mov     ES,pagetabl+[BX]

          mov     SI,FP            ; create a pointer to the current stack
          add     SI,offset S_stack ; frame (the new one)

          mov     AL,[SI].sf_cb_pag ; copy the value of the VM's code base
          mov     ES:[DI].con_cb_p,AL ;  into the continuation object
          mov     AX,[SI].sf_cb_dis
          mov     ES:[DI].con_cb_d,AX

          mov     AX,[SI].sf_ret   ; copy the return address displacement
          mov     ES:[DI].con_ret,AX ; into the continuation object

          mov     AX,[SI].sf_ddisp ; copy the dynamic link into the
          mov     ES:[DI].con_ddis,AX ; continuation object

;     Perform the Call-Closure-Tail-Recursive
          mov     AL,byte ptr PREV_pag ; save the pointer to the new
          mov     byte ptr tm2_page,AL ;  continuation
          mov     AX,PREV_dis
          mov     tm2_disp,AX
          pop     AX               ; recover "operands" to call-closure-tr
          call    cl_ct_sb         ; call CALL-CLOSURE-TR
          mov     AL,byte ptr tm2_page ; move continuation pointer into
          mov     byte ptr reg1_pag,AL ;  VM register R1
          mov     AX,tm2_disp
          mov     reg1_dis,AX
          jmp     next_PC          ; return to interpreter


;************************************************************************
;*                                                      AL  AH          *
;* Apply closure                        APPLY-CLOSURE   ftn,args        *
;*                                                                      *
;* Purpose:  Interpreter support for the "apply" primitive.  The        *
;*              argument list (in register "args") are to be passed     *
;*              to the closure pointed to by the "ftn" register.        *
;*                                                                      *
;* Note:  The argument registers may be anything that the compiler      *
;*              decides on, so the "ftn" pointer could be destroyed     *
;*              in the process of loading the arguments of the argument *
;*              list ("args") into the VM general registers R1-Rn.      *
;*              So that the ftn pointer is not lost during this process,*
;*              this pointer is pushed onto the 8088 stack before the   *
;*              call to process the arguments, and it is restored into  *
;*              the last available register to complete the call        *
;*              sequence.                                               *
;*                                                                      *
;*        Garbage collection will not occur during the argument loading *
;*              process (arguments are copied, but no cons-ing occurs), *
;*              so it's safe to save the "ftn" pointer on the 8088      *
;*              stack temporarily.                                      *
;************************************************************************
last_pag  equ     reg0_pag + (NUM_REGS - 1) * size C_ptr
last_dis  equ     reg0_dis + (NUM_REGS - 1) * size C_ptr
          public  apply
apply:    lods    word ptr ES:[SI] ; load apply's arguments
          mov     BL,AL            ; copy closure pointer register number
          push    reg0_pag+[BX]    ; save value of register containing
          push    reg0_dis+[BX]    ;  the closure pointer
          save    <SI>             ; save registers across call
          call    aply_arg         ; expand arguments into R1-Rn
          restore <SI>             ; restore saved registers
          pop     last_dis         ; put "ftn" pointer into last VM register
          pop     last_pag
          mov     AH,CL            ; copy the argument count to AH, AL<="Rlast"
          mov     AL,(NUM_REGS - 1) * size C_ptr
          call    cl_c_sub         ; process the call
          jmp     next_PC          ; return to the interpreter


;************************************************************************
;*                                                       AL  AH         *
;* Apply closure, tail recursively      APPLY-CLOSURE-TR ftn,args       *
;*                                                                      *
;* Purpose:  Interpreter support for the "apply" primitive.  The        *
;*              argument list (in register "args") are to be passed     *
;*              to the closure pointed to by the "ftn" register.        *
;*                                                                      *
;* Note:  See notes in "APPLY-CLOSURE" support, above.                  *
;************************************************************************
          public  apply_tr
apply_tr: lods    word ptr ES:[SI] ; load apply-tr's arguments
          mov     BL,AL            ; copy closure pointer register number
          push    reg0_pag+[BX]    ; save value of register containing
          push    reg0_dis+[BX]    ;  the closure pointer
          save    <SI>             ; save registers across call
          call    aply_arg         ; expand arguments into R1-Rn
          restore <SI>             ; restore saved registers
          pop     last_dis         ; put "ftn" pointer into last VM register
          pop     last_pag
          mov     AH,CL            ; copy the argument count to AH, AL<="Rlast"
          mov     AL,(NUM_REGS - 1) * size C_ptr
          call    cl_ct_sb         ; process the call, tail recursively
          jmp     next_PC          ; return to the interpreter

;************************************************************************
;* Execute code block                           EXECUTE    CODE         *
;*                                                                      *
;* Purpose:  Interpreter support for the "execute" primitive operation. *
;*                                                                      *
;* Description:  The execute primitive causes a code block to be        *
;*              executed in a new environment.  This is accomplished    *
;*              by executing a procedure call to the code block with    *
;*              no static environment information available.  The       *
;*              new stack frame has a nil heap environment pointer, and *
;*              the static link is set to point to itself to prevent    *
;*              access to any higher lexical levels.  When the code     *
;*              block exits, control will return to the place where the *
;*              execute instruction was executed.                       *
;************************************************************************
          public  execute
execute:  lods    byte ptr ES:[SI] ; fetch register number with code pointer
          mov     BX,AX
execute1  label   far
          mov     DI,reg0_pag+[BX]
          cmp     byte ptr ptype+[DI],CODETYPE*2 ; pointer to code block?
          jne     load_ex1         ; if not, we've got to load before execute
          push    BX               ; save the code pointer's register number
          call    new_SF           ; create a new stack frame for the "call"
          mov     word ptr S_stack+[BX].sf_sdisp,0 ; make "nil" static link
          mov     AL,byte ptr GNV_pag ; default environment to global env
          mov     S_stack+[BX].sf_hpage,AL
          mov     AX,GNV_dis
          mov     word ptr S_stack+[BX].sf_hdisp,AX
          mov     FP,BX
          pop     BX               ; retrieve the code pointer's reg number
          mov     SI,reg0_dis+[BX] ; define the code base register
          mov     CB_dis,SI
          mov     BL,byte ptr reg0_pag+[BX]
          mov     byte ptr CB_pag,BL
          LoadCode ES,BX
;;;       mov     ES,pagetabl+[BX] ; load the code base page's para address
          save    <ES>             ;  and save it off
          add     SI,ES:[SI].cod_entr ; adjust location ptr for entry offset
          jmp     next             ; return to the interpreter

load_ex1: jmp     far ptr load_ex  ; long jump to loader
;
;     Object to be executed is not a code block, so we've got to create
;       one for a compiled program before executing it.  The format of an
;       object program is:
;
;       (tag #-constants #-codebytes (constant ...) (codebyte ...))
;
;     ***Error-- Invalid Object Module Format***
bad_obj2  label   far
          mov     AX,offset m_%exec ; load addr of "%EXECUTE"
          restore <BX>             ; load number of register containing
          add     BX,offset reg0   ;  the "code" pointer and compute its addr
          mov     CX,1             ; load argument count = 1
          pushm   <BX,CX,AX>       ; push arguments to set_src_err
          C_call  set_src_         ; call:  set_src_err("%EXECUTE", 1, code)
          restore <SI>             ; load next instruction's location
          jmp     sch_err          ; link to Scheme debugger

;************************************************************************
;* Exit from current procedure                  EXIT                    *
;*                                                                      *
;* Description:  The internal registers of the VM are reset from        *
;*              information stored in the current frame pointer to      *
;*              restore the environment at the point where the current  *
;*              procedure was called (i.e., control returns to the      *
;*              calling routine).                                       *
;************************************************************************
          public  s_exit
s_exit:   mov     AX,FP            ; load the current frame pointer
          mov     BX,AX
          add     BX,offset S_stack ; compute address of current stack frame

          sub     AX,PTRSIZE       ; reset the current TOS to previous
          mov     TOS,AX           ;  value [FP - sizeof(pointer)]

          xor     AX,AX            ; clear AX
          mov     AL,[BX].sf_cb_pag ; load CB's page number
          mov     byte ptr CB_pag,AL
          mov     DI,AX            ; save code block's page number
          LoadCode ES,DI
;;;       mov     ES,pagetabl+[DI] ; set paragraph address of page containing
          save    <ES>             ;  calling routine's code block
          mov     AX,[BX].sf_cb_dis ; update the current code base (CB)
          mov     CB_dis,AX

          add     AX,[BX].sf_ret   ; load return address' location pointer
          mov     SI,AX            ;  and add in starting offset of code block

          mov     AX,[BX].sf_ddisp ; compute pointer to caller's stack frame
          cmp     AX,BASE          ; is new FP outside stack buffer?
          jae     s_exit_1         ; if in bounds, jump
          pushm   <AX,SI,ES>       ; save new FP, new location pointer
          call    stk_unfl         ; process stack underflow
          popm    <ES,SI,AX>       ; restore saved new FP, new location pointer
s_exit_1: sub     AX,BASE          ; FP <- dynamic link - Base
          mov     FP,AX
          jmp     next             ; return to interpreter

stk_int   endp

;************************************************************************
;*                                                    AL   AL    AH     *
;* Create Closure                       CR-CLOSE      dest,label,nargs  *
;*                                                                      *
;* Purpose:  Scheme interpreter support for the creation of closure     *
;*              objects.                                                *
;************************************************************************
          public  cr_close
cr_close: lods    byte ptr ES:[SI] ; load destination register number
          mov     DI,AX            ;  and save it for now
          lods    word ptr ES:[SI] ; load address of entry label
          mov     CX,AX            ;  and save it, too
          lods    byte ptr ES:[SI] ; load number of arguments and
          cbw                      ;  convert it to a fullword integer
          shl     AX,1             ; clear high order bit of immediate value
          shr     AX,1
          add     CX,SI            ; add in current location pointer
          sub     CX,CB_dis        ;  and adjust for code block offset
          save    <AX,CX,SI,DI>    ; save nargs, entry point, location pointer
          mov     DX,CLOSTYPE      ; load tag=closure
          mov     AX,CLO_OVHD-PTRSIZE ; load size of closure object
          pushm   <AX,DX,tmp_adr>  ; push arguments
          C_call  alloc_bl,,Load_ES ; call: alloc_block(&reg, type, size)
          mov     SP,BP            ; drop arguments off TIPC's stack

          mov     BX,tmp_page      ; load pointer to closure object
          mov     DI,tmp_disp
          LoadPage ES,BX
;;;       mov     ES,pagetabl+[BX]

          mov     SI,[BP].save_DI  ; copy contents of destination register
          xchg    BL,byte ptr reg0_pag+[SI] ;  into the information operand of
          mov     ES:[DI].clo_ipag,BL ;  the newly allocated closure object.
          mov     AX,DI               ;  Make the destination register point
          xchg    AX,reg0_dis+[SI]    ;  to the closure object.
          mov     ES:[DI].clo_idis,AX

          mov     AL,SPECFIX*2     ; set tags for constant fields
          mov     ES:[DI].clo_etag,AL ; entry point tag=fixnum
          mov     ES:[DI].clo_atag,AL ; nargs tag=fixnum

          mov     AL,byte ptr CB_pag ; copy in pointer to current code base
          mov     ES:[DI].clo_cb_p,AL
          mov     AX,CB_dis
          mov     ES:[DI].clo_cb_d,AX

          restore <CX>             ; define entry point offset
          mov     ES:[DI].clo_edis,CX

          restore <AX>             ; define number of arguments
          mov     ES:[DI].clo_narg,AX

          mov     SI,FP            ; load pointer to current stack frame
          mov     AL,S_stack+[SI].sf_hpage ; define heap environment
          mov     ES:[DI].clo_hpag,AL
          mov     AX,word ptr S_stack+[SI].sf_hdisp
          mov     ES:[DI].clo_hdis,AX

          jmp     next_PC          ; return to interpreter


;************************************************************************
;* Local support - stack overflow handler                               *
;*                                                                      *
;* Purpose:  To move part of Scheme's runtime stack to the heap when    *
;*              stack overflow occurs.                                  *
;*                                                                      *
;* Description:  The contents of the stack which precede the current    *
;*              stack frame are moved to the heap (in a continuation    *
;*              object) and the current stack frame is moved to the     *
;*              top of the stack buffer.                                *
;*                                                                      *
;* Input Parameters:                                                    *
;*              TIPC register SI - the value to be placed in the        *
;*                      "return address displacement" field of the      *
;*                       continuation (needed only for call/cc)         *
;*              FNV_reg - the current fluid environment (saved by       *
;*                       call/cc)                                       *
;*              GNV_reg - the current global environment (saved by      *
;*                       call/cc)
;*              FP - the current stack frame pointer                    *
;*              BASE - the stack buffer base value                      *
;*              TOS - the current top-of-stack pointer                  *
;*              CB - the VM register which points to the current        *
;*                      code block                                      *
;*              PREV_pag,PREV_dis - the VM's previous stack segment     *
;*                      register                                        *
;*                                                                      *
;* Output Parameters:                                                   *
;*              PREV_pag,PREV_dis - a pointer to the continuation       *
;*                      object which was created                        *
;*              BASE - updated to the new base value (stack offset)     *
;*                      due to movement of some of the stack contents   *
;*                      to the heap                                     *
;*                                                                      *
;* Variables Modified:  (but logically unchanged)                       *
;*              FP - the current stack frame pointer                    *
;*              TOS - the current top of stack pointer                  *
;*                                                                      *
;* Example:  Stack Overflow Condition                                   *
;*                                                                      *
;*                                Before                                *
;*                                                                      *
;*                 +--------+-----------------+                         *
;*                 |  prev stk seg -> = nil   |                         *
;*                 +--------+-----------------+                         *
;*                    Stack Buffer (BASE = 0)                           *
;*                 +--------+-----------------+                         *
;*                 |         Contents         |                         *
;*                 :            of            :                         *
;*                 :          Stack           :                         *
;*                 |        (m bytes)         |                         *
;*                 |--------+-----------------|                         *
;*                 |         Current          |<-FP                     *
;*                 :          Stack           :                         *
;*                 |          Frame           |<-TOS                    *
;*                 +--------+-----------------+                         *
;*                                                                      *
;*                                 AFTER                                *
;*                                                                      *
;*                                          "Continuation" in Heap      *
;*   +--------+-----------------+        +--------+-----------------+   *
;*   |     prev stk seg ->      |------->|  cont  |  length (m+24)  |   *
;*   +--------+-----------------+        |--------+-----------------|   *
;*      Stack Buffer (BASE = m)          | segment's stack base = 0 |   *
;*   +--------+-----------------+        |--------+-----------------|   *
;*   |         Current          |<-FP    |    code base -> = n/a    |   *
;*   :          Stack           :        |--------+-----------------|   *
;*   |          Frame           |<-TOS   |  return addr disp = n/a  |   *
;*   |--------+-----------------|        |--------+-----------------|   *
;*   |       unused stack       |        | caller dynamic link = n/a|   *
;*   :                          :        |--------+-----------------|   *
;*   :                          :        |  fluid env -> = FNV_reg  |   *
;*   |                          |        |--------------------------|   *
;*   +--------+-----------------+        |  prev stk seg -> = nil   |   *
;*                                       |--------+-----------------|   *
;*                                       |  global env -> = GNV_reg |   *
;*                                       |--------+-----------------|   *
;*                                       |         Contents         |   *
;*                                       :            of            :   *
;*                                       :          Stack           :   *
;*                                       |        (m bytes)         |   *
;*                                       +--------+-----------------+   *
;*                                                                      *
;* Notes:  This routine handles both routine stack overflow, and stack  *
;*              overflow which is signaled during the creation of a     *
;*              full continuation because of a call/cc.  All of the     *
;*              fields of the continuation object are filled in by this *
;*              routine, but they are meaningless and will never be     *
;*              used in the case of simple stack overflow.              *
;************************************************************************
stk_arg   struc
stk_temp  dd      ?                ; temporary register
stk_SI    dw      ?                ; caller's SI (for continuation, rtn addr)
stk_BP    dw      ?                ; caller's BP
          dw      ?                ; return address
stk_arg   ends

stk_ovfl  proc    near
          push    BP               ; save caller's BP
          sub     SP,offset stk_BP
          mov     BP,SP
          mov     [BP].stk_SI,SI   ; save return address disp, if meaningful

;     test to see how to create continuation object
          mov     CX,FP            ; load current frame pointer,
          cmp CX,0                 ; length of stack contents zero?
          jg  stk_nz               ; if not, create new continuation (jump)

;     copy previous continuation
          mov     AX,PREV_pag      ; tmp_reg <- PREV_reg
          mov     tmp_page,AX
          mov     AX,PREV_dis
          mov     tmp_disp,AX
          mov     AX,offset PREV_reg ; load address of PREV_reg, tmp_reg
          pushm   <tmp_adr,AX>     ;    push as arguments
          C_call  copy_blk         ; call:  copy_blk(&PREV_reg, &tmp_reg)
          mov     SP,BP            ; drop arguments from stack
          jmp     stk_rtn          ; return copy of previous continuation

;     print warning concerning impending stack overflow
s_toobig: pushm   <ES,DI,BX,CX>    ; save active registers
          lea     BX,m_stk_ov      ; load error message text address
          push    BX               ;  and push as argument to printf
          C_call  printf,,Load_ES  ; call:  printf("***error... ");
          pop     BX               ; drop argument from TIPC's stack
          C_call  force_de         ; call:  force_debug();
          popm    <CX,BX,DI,ES>    ; restore active registers
          jmp     stk_go           ; continue executing where we left off

;     allocate a continuation object on the heap
stk_nz:   add     CX,offset con_data-PTRSIZE ;  and compute continuation's size
          mov     DX,CONTTYPE      ; load tag=CONTTYPE
          lea     BX,[BP].stk_temp ; load address of temporary result reg
          pushm   <CX,DX,BX>       ; push arguments, and call
          mov     BX,DS            ; set up ES segment register for C_call
          mov     ES,BX
          C_call  alloc_bl         ; "alloc_block(&reg,CONTTYPE,len)"
          mov     SP,BP            ; remove arguments from 8088's stack

;     load pointer to the continuation object just allocated
          mov     CX,FP            ; reload length of continuations stack data
          mov     BX,[BP].stk_temp.C_page ; load returned pointer to
          mov     DI,[BP].stk_temp.C_disp ;  continuation object
          LoadPage ES,BX
;;;       mov     ES,pagetabl+[BX] ; ES->continuation object's page

;     define continuation object fields
          mov     AL,SPECFIX*2
          mov     ES:[DI].con_btag,AL ; stack base tag=fixnum
          mov     ES:[DI].con_rtag,AL ; return address tag=fixnum
          mov     ES:[DI].con_dtag,AL ; dynamic link tag=fixnum

          mov     AL,byte ptr CB_pag  ; define code base pointer
          mov     ES:[DI].con_cb_p,AL
          mov     AX,CB_dis
          mov     ES:[DI].con_cb_d,AX

          neg     AX               ; subtract CB_dis from SI
          add     AX,[BP].stk_SI   ; use contents of SI for return addr disp
          mov     ES:[DI].con_ret,AX

          mov     AX,FP            ; define dynamic link
          mov     ES:[DI].con_ddis,AX

          mov     AX,BASE          ; set continuation's stack base
          mov     ES:[DI].con_base,AX
          add     AX,CX            ; compute new stack buffer base
          mov     BASE,AX          ;  [BASE <- BASE + FP]

;     Test for impending stack overflow
          cmp     AX,-STKSIZE      ; over stack buffer threshold?
          jae     s_toobig         ; if so, print warning (jump)

stk_go:   mov     AL,byte ptr FNV_pag ; set fluild environment pointer
          mov     ES:[DI].con_fl_p,AL
          mov     AX,FNV_dis
          mov     ES:[DI].con_fl_d,AX

          mov     AL,byte ptr GNV_pag ; set global environment pointer
          mov     ES:[DI].con_gl_p,AL
          mov     AX,GNV_dis
          mov     ES:[DI].con_gl_d,AX

          mov     AX,PREV_pag      ; set previous stack segment pointer
          mov     ES:[DI].con_spag,AL
          mov     AX,PREV_dis
          mov     ES:[DI].con_sdis,AX

          mov     PREV_pag,BX      ; make previous stack segment register
          mov     PREV_dis,DI      ;  point to the new continuation object

;     update the counter of bytes transfered to the heap
          add     word ptr stk_out,CX  ; record number of bytes transfered
          adc     word ptr stk_out+2,0 ; fix up high order part of sum

;     move stack data to continuation object in the heap
          lea     SI,S_stack       ; load stack address
          add     DI,offset con_data ; adjust for continuation header info
          mov     DX,CX            ; copy length (in bytes) and
          and     DX,1             ;  isolate lsb for fixup
          shr     CX,1             ; convert bytes to words
          cld                      ; clear direction flag (forward move)
rep       movsw                    ; move stack contents to heap (cont obj)
          mov     CX,DX            ; copy fixup length (0 or 1 bytes)
rep       movsb                    ; copy remaining byte, if needed

;     move data in current stack frame to top of stack buffer
          lea     SI,S_stack       ; load address of top of stack buffer
          mov     DI,SI            ; DI <- top of stack buffer (0)
          add     SI,FP            ; SI <- current stack frame
          mov     CX,DS
          mov     ES,CX            ; ES->data segment
          mov     CX,TOS           ; load current top of stack,
          sub     CX,FP            ;  subtract bytes moved to heap,
          mov     TOS,CX           ;  and define new TOS
          add     CX,PTRSIZE       ; compute bytes of stack to move up
          mov     DX,CX            ; copy length (in bytes) and
          and     DX,1             ;  isolate lsb for fixup
          shr     CX,1             ; convert bytes to words
rep       movsw                    ; move stack contents to top of stack buffer
          mov     CX,DX            ; copy fixup length (0 or 1 bytes)
rep       movsb                    ; copy remaining byte, if needed

          mov     FP,0             ; current frame now at top of stack buffer

;     return to caller
stk_rtn:  mov     SI,[BP].stk_SI   ; restore return address disp, if meaningful
          add     SP,offset stk_BP ; drop local variable storage
          pop     BP               ; restore caller's BP
          ret                      ; return
stk_ovfl  endp

;************************************************************************
;* Local support - stack underflow handler                              *
;*                                                                      *
;* Purpose:  To restore segments of the stack, which previously have    *
;*              been moved to the heap, back into the stack buffer.     *
;*                                                                      *
;* Description:  Previously saved stack segments (moved to the heap     *
;*              as the result of a stack overflow or a call/cc) are     *
;*              represented as continuation data objects.  When this    *
;*              routine is called, a "stack underflow" has occurred     *
;*              as an "EXIT" operation needs to access a stack frame    *
;*              higher in the stack, so data fields with a call/cc      *
;*              continuation are ignored.                               *
;************************************************************************
stk_unfl  proc    near
          push    BP               ; save caller's BP
          mov     BP,SP
          mov     BX,PREV_pag      ; fetch previous stack segment's page number
          cmp     BX,0             ; stack link nil?
          je      unfl_nil         ; if so, jump (real stack underflow)
          mov     SI,PREV_dis      ; load previous stack segment displacement

          push    DS               ; save caller's DS register
          mov     CX,DS
          mov     ES,CX            ; ES->stack's data group
          LoadPage DS,BX
;;;       mov     DS,pagetabl+[BX] ; DS->continuation object's page

          mov     AX,[SI].con_base ; update stack buffer's base
          mov     ES:BASE,AX

          mov     AL,[SI].con_spag ; update previous stack segment register
          mov     ES:byte ptr PREV_pag,AL
          mov     AX,[SI].con_sdis
          mov     ES:PREV_dis,AX

          mov     CX,[SI].con_len  ; load length of saved stack data
          sub     CX,offset con_data ; adjust length for continuation header
          add     SI,offset con_data ; adjust offset for continuation header
          lea     DI,S_stack       ; load address of bottom of stack
          mov     DX,CX            ; compute new top of stack
          sub     DX,PTRSIZE
          mov     ES:TOS,DX        ;  in memory (temporarily covered by ES)

          add     word ptr ES:stk_in,CX  ; update count of bytes transfered
          adc     word ptr ES:stk_in+2,0 ; fix up high order part of counter

          mov     DX,CX            ; copy the length (in bytes)
          and     DX,1             ;  and determine fixup (0 or 1 bytes)
          shr     CX,1             ; convert length from bytes to words
          cld                      ; set direction flag = forward
rep       movsw                    ; restore the stack's contents
          mov     CX,DX            ; copy fixup length and
rep       movsb                    ;  move the odd byte, if needed
          pop     DS               ; restore DS
          pop     BP
          ret                      ; return to caller
;     Error-- stack underflow
unfl_nil: lea     BX,m_stk_un
          push    BX
          C_call  printf,,Load_ES
          C_call  exit
stk_unfl  endp


;************************************************************************
;* Local support - Create new stack frame                               *
;*                                                                      *
;* Purpose:  To create and partially define a new stack frame prior     *
;*              to a procedure call                                     *
;*                                                                      *
;* Description:  This routine allocates space on the top of the stack   *
;*              for a new stack frame and defines the following fields: *
;*                                                                      *
;*                      code base pointer <- CB                         *
;*                      return addr disp <- SI (contents of reg)        *
;*                      dynamic link <- FP                              *
;*                      static link's tag <- fixnum                     *
;*                      heap env <- current heap env                    *
;*                      static link <- current static link              *
;*                      closure pointer <- nil (implies an open call)   *
;*                                                                      *
;* Input Parameters:                                                    *
;*              TIPC register SI - the VM's location pointer            *
;*              CB_pag,CB_dis - the VM's code base register             *
;*              FP - the VM's current frame pointer                     *
;*              TOS - the VM's top of stack pointer                     *
;*                                                                      *
;* Output Parameters:                                                   *
;*              TIPC register BX - pointer to new stack frame           *
;*                                      (displacement in stack)         *
;*              TOS - top of stack pointer updated for new stack length *
;*                                                                      *
;* Variables Modified:  The following variables will be modified if     *
;*              a stack overflow occurs during the push operation for   *
;*              the new stack frame:                                    *
;*                                                                      *
;*              FP - the VM's current frame pointer(logically unchanged)*
;*              BASE - the VM's stack buffer base                       *
;*              PREV_pag,PREV_dis - the VM's previous stack segment reg *
;************************************************************************
new_SF    proc    near
          mov     AX,TOS           ; load current top of stack pointer
          mov     BX,AX            ;  and make a copy
          add     AX,SF_OVHD       ; increment TOS by size of stack frame
          cmp     AX,STKSIZE-PTRSIZE ; is there room on stack for new frame?
          jg      new_FP_1         ; if not, process stack overflow (jump)
          mov     TOS,AX           ; update top of stack pointer
          add     BX,PTRSIZE       ; compute pointer to new stack frame

          mov     AL,SPECFIX*2     ; load tag for fixnum's
          mov     S_stack+[BX].sf_rtag,AL ; return address tag=fixnum
          mov     S_stack+[BX].sf_dtag,AL ; dynamic link tag=fixnum
          mov     S_stack+[BX].sf_stag,AL ; static link tag=fixnum

          xor     AX,AX            ; store '() into closure pointer
          mov     S_stack+[BX].sf_cl_pg,AL
          mov     word ptr S_stack+[BX].sf_cl_ds,AX

          mov     AL,byte ptr CB_pag ; move current code base pointer
          mov     S_stack+[BX].sf_cb_pag,AL ; into the new stack frame
          mov     AX,CB_dis
          mov     word ptr S_stack+[BX].sf_cb_dis,AX

          sub     SI,AX            ; compute ret addr relative to code block
          mov     word ptr S_stack+[BX].sf_ret,SI ; record the return address
          add     SI,AX            ; restore SI

;     copy the current heap environment pointer to the new stack frame
          mov     DI,FP            ; load the current stack frame pointer
          mov     AL,S_stack+[DI].sf_hpage
          mov     S_stack+[BX].sf_hpage,AL
          mov     AX,word ptr S_stack+[DI].sf_hdisp
          mov     word ptr S_stack+[BX].sf_hdisp,AX

;     copy the static link from the current frame to the new one
          mov     AX,word ptr S_stack+[DI].sf_sdisp
          mov     word ptr S_stack+[BX].sf_sdisp,AX

;     define the dynamic link field
          add     DI,BASE
          mov     word ptr S_stack+[BX].sf_ddisp,DI

          ret                      ; return to the caller

;     Process stack overflow
new_FP_1: push    SI               ; save current location pointer
          call    stk_ovfl         ; process the overflow
          pop     SI               ; restore location pointer
          jmp     new_SF           ; try again to allocate new stack frame
new_SF    endp


;************************************************************************
;* Local support - drop items from the heap environment                 *
;*                                                                      *
;* Purpose:  To drop "n" items off the local heap environment           *
;*                                                                      *
;* Input Parameters:                                                    *
;*              TIPC register CX - the number of items to drop          *
;*              FP - the current stack frame pointer                    *
;*                                                                      *
;* Output Parameters:                                                   *
;*              TIPC register BX - page number for the remaining        *
;*                                      heap environment list           *
;*              TIPC register DI - displacement pointer for the         *
;*                                      remaining heap environment      *
;*                                                                      *
;* Registers/Variables Modified:                                        *
;*              CX - decremented to zero                                *
;*              TIPC register ES - contents undefined                   *
;************************************************************************
delta_hp  proc    near
          mov     DI,FP            ; load the current stack frame pointer
          xor     BX,BX            ; clear register BX
          mov     BL,S_stack+[DI].sf_hpage ; load the current heap environment
          mov     DI,word ptr S_stack+[DI].sf_hdisp ;  pointer
          cmp     CX,0             ; drop zero elements?
          jle     del_h_rt         ; if drop zero, jump
del_h_lp: LoadPage ES,BX
;;;       mov     ES,pagetabl+[BX] ; load para addr of page holding list cell
          mov     BL,ES:[DI].cdr_page ; load link pointer (cdr field)
          mov     DI,ES:[DI].cdr
          loop    del_h_lp         ; cdr through list for "n" elements
del_h_rt: ret                      ; return updated heap env ptr in BX,DI
delta_hp  endp


;************************************************************************
;* Local support - Obtain Frame Pointer for given lexical level         *
;*                                                                      *
;* Input Parameters:                                                    *
;*              TIPC register CX - desired lexical level number         *
;*                      0=current lexical level,                        *
;*                      1=lexical parent's level, etc.                  *
;*              FP - current frame pointer                              *
;*              BASE - current stack buffer base                        *
;*                                                                      *
;* Output Parameters:                                                   *
;*              TIPC register BX - frame pointer for desired level      *
;*                      (absolute location in stack)                    *
;*              ES:[SI] - pointer to desired stack frame                *
;*                      (either in stack buffer, or in the heap)        *
;*                                                                      *
;* Notes:  Register usage:                                              *
;*              AX - zeroed, so page numbers can be loaded into AL      *
;*                      prior to copying to DI                          *
;*              BX - frame pointer for current level                    *
;*              CX - lexical level counter.  decremented at each level  *
;*              DX - base offset of the stack segment currently being   *
;*                      examined                                        *
;*              SI - stack segment's (continuation's) displacement      *
;*              DI - temporarily hold page number of next stack segment *
;************************************************************************
delta_lv  proc    near
          mov     BX,FP            ; load current frame pointer
          mov     DX,BASE          ;  and the stack buffer base
          cmp     CX,0             ; reference to current stack frame?
          jg      dlt_nt_0         ; if not, jump

;     current lexical level desired-- return active stack frame pointer
          lea     SI,S_stack+[BX]  ; compute addr of current frame pointer
          add     BX,DX            ; adjust for base of stack buffer
          mov     ES,[BP].C_ES     ; load pointer to data segment
          ret                      ; return BX, ES:[SI] to caller

;     find pointer to higher lexical level in stack buffer
dlt_loop: sub     BX,DX            ; adjust absolute frame ptr by base
          jb      dlt_in_h         ; still in stack buffer?  if not, jump
dlt_nt_0: mov     BX,word ptr S_stack+[BX].sf_sdisp ; fetch static link
          loop    dlt_loop         ; iterate until desired level found

;     pointer to desired level found in stack buffer
          mov     SI,BX            ; copy absolute frame pointer
          sub     SI,DX            ; adjust for current stack buffer base
          jb      dlt_nstk         ; still within stack buffer? if not, jump
          add     SI,offset S_stack ; compute address of frame in stack buffer
          mov     ES,[BP].C_ES     ; ES<-data segment
          ret                      ; return BX, ES:[SI]

;     Frame pointer found, but frame's not in stack buffer
dlt_nstk: mov     DI,PREV_pag      ; load pointer to previous stack segment
          mov     SI,PREV_dis
          LoadPage ES,DI
;;;       mov     ES,pagetabl+[DI]
          mov     DX,ES:[SI].con_base
          xor     AX,AX
dlt_nb:   cmp     BX,DX            ; is frame within this segment?
          jae     dlt_here         ; if so, jump
          mov     AL,ES:[SI].con_spag ; load pointer to its previous segment
          mov     DI,AX
          mov     SI,ES:[SI].con_sdis
          LoadPage ES,DI
;;;       mov     ES,pagetabl+[DI]
          mov     DX,ES:[SI].con_base ; load stack segment's base offset
          jmp     dlt_nb           ; search 'til segment containing frame found
dlt_here: mov     AX,BX            ; copy absolute frame pointer for level
          sub     AX,DX            ; subtract this stack segment's base
          add     SI,AX            ; add to continuation offset
          add     SI,offset con_data ; add fudge factor for continuation header
          ret                      ; return BX, ES:[SI] to caller

;     Desired level not found, but current reference not in stack buffer
dlt_in_h: add     BX,DX            ; compute absolute location in stack
          mov     DI,PREV_pag      ; load previous stack segment pointer
          mov     SI,PREV_dis
          LoadPage ES,DI
;;;       mov     ES,pagetabl+[DI]
          mov     DX,ES:[SI].con_base
          xor     AX,AX
dlt_in_n: cmp     BX,DX            ; is frame in this stack segment?
          jae     dlt_fnd          ; if so, jump
          mov     AL,ES:[SI].con_spag ; fetch pointer to next previous segment
          mov     DI,AX
          mov     SI,ES:[SI].con_sdis
          LoadPage ES,DI
;;;       mov     ES,pagetabl+[DI]
          mov     DX,ES:[SI].con_base ; load this segment's base offset
          jmp     dlt_in_n         ; keep searching stack segments
;     Segment containing stack frame found-- fetch static link
dlt_fnd:  sub     BX,DX            ; adjust frame displacement for seg base
          mov     BX,ES:[SI].con_data.sf_sdisp+[BX] ; load static link
          loop    dlt_in_n         ; follow chain to desired lexical level
          jmp     dlt_nb           ; found-- return pointer to stack frame

delta_lv  endp


;************************************************************************
;* Local support - Expand "apply's" argument list into registers R1-Rn  *
;*                                                                      *
;* Purpose:  To expand the argument list of an "apply" so that the      *
;*              operands are in the proper operand registers (R1-Rn)    *
;*              for a call to a closed procedure.                       *
;*                                                                      *
;* Input Parameters:  TIPC register AH - the number of the VM's         *
;*                      general register which contains the pointer to  *
;*                      the linked list of arguments.                   *
;*                                                                      *
;* Output Parameters:  TIPC register CX - a count of the arguments.     *
;*                                                                      *
;* Note:  The "apply" operation expects two operands which are a        *
;*              function and a 'list' of arguments.  In the event that  *
;*              the second argument is not a list, this routine simply  *
;*              substitutes that value as if it were an argument.  This *
;*              means that the "LIST" function is not actually needed   *
;*              for an argument list containing only one value.         *
;*              For example, the following are handled equivalently:    *
;*                                                                      *
;*              "correct" code                  "not-correct" code      *
;*              (apply ftn (list 1))            (apply ftn 1)           *
;*              (apply ftn (list a b))          (apply ftn (cons a b))  *
;*                                                                      *
;*              Although this could be viewed as an optimization, in    *
;*              that it saves one list cell each time the argument list *
;*              is created, the real reason it is done is to provide    *
;*              a fixup action when an error condition is detected.     *
;************************************************************************
aply_arg  proc    near
;     Count the number of arguments to make sure there aren't too many
          xor     BX,BX            ; copy the register number of the
          mov     BL,AH            ;  argument list to BX
          mov     SI,reg0_dis+[BX] ; load the argument list pointer
          mov     BX,reg0_pag+[BX]
          xor     CX,CX            ; zero the argument counter
aply_lp1: cmp     BL,0             ; is pointer nil?
          je      aply_ok          ; if so, the last argument has been moved
          inc     CX               ; increment the argument count
          cmp     CX,NUM_REGS-2    ; (can't use R0 or R63)
          jg      aply_err
          cmp     byte ptr ptype+[BX],LISTTYPE*2 ; pointer to a list cell?
          jne     aply_ok          ; if not, assume last argument
          LoadPage ES,BX
;;;       mov     ES,pagetabl+[BX] ; load para addr for list cell's page
          mov     BL,ES:[SI].cdr_page ; load the "cdr" pointer (next cell)
          mov     SI,ES:[SI].cdr
          jmp     aply_lp1         ; process 'til end of argument list

;     copy arguments into the registers
aply_ok:  mov     BL,AH            ; copy arg list register back into BX
          mov     SI,reg0_dis+[BX] ; load the argument list pointer
          mov     BX,reg0_pag+[BX]
          lea     DI,reg1          ; load the address of VM register R1

aply_lp:  cmp     BL,0             ; is pointer nil?
          je      aply_don         ; if so, the last argument has been moved
          cmp     byte ptr ptype+[BX],LISTTYPE*2 ; pointer to a list cell?
          jne     aply_huh         ; if not, we've got a problem (jump)
          LoadPage ES,BX
;;;       mov     ES,pagetabl+[BX] ; load para addr for list cell's page
          mov     AL,ES:[SI].car_page ; move the "car" portion of the list
          mov     byte ptr [DI].C_page,AL ; cell into the next available
          mov     AX,ES:[SI].car   ;  general register of the VM
          mov     [DI].C_disp,AX
          mov     BL,ES:[SI].cdr_page ; load the "cdr" pointer (next cell)
          mov     SI,ES:[SI].cdr
          add     DI,size C_ptr    ; increment next register's address
          jmp     aply_lp          ; process 'til end of argument list

;     If an element in the argument list is not a list cell, simply place
;       that pointer into the next register.
aply_huh: mov     [DI].C_page,BX
          mov     [DI].C_disp,SI

aply_don: ret                      ; return to caller

;     ***Error-- too many arguments to expand into register***
aply_err: restore <SI>             ; reload the current location pointer and
          sub     SI,3             ;  back it up to start of "apply" instruction
          pushm   <SI,m_AP_adr>    ; push function name, offset
          C_call  disassem,,Load_ES ; call:  disassemble("APPLY",offset);
          pushm   <tmp_adr,m_APPLY,m_one> ; push arguments
          C_call  set_nume         ; call:  set_numeric_error(1,code,tmp_reg)
          restore <SI>             ; reload the location pointer
          jmp     sch_err          ; Link to Scheme debugger
aply_arg endp


;************************************************************************
;     Lattice C callable routine to push a register onto Scheme's stack *
;       Calling Sequence:  C_push(reg)                                  *
;               where:  int reg[2] - register (pointer/value) to push   *
;************************************************************************
C_args    struc
C_BP      dw      ?                ; Caller's BP
          dw      ?                ; Return address
C_reg     dw      ?                ; Pointer to register
C_args    ends

          public  C_push
C_push1   proc    near
;     Process overflow-- copy contents of stack to the heap
C_push2:  push    ES               ; save ES across the call
          call    stk_ovfl         ; copy the stack contents
          pop     ES               ; restore ES
                                   ; retry the push (fall through)
C_push:   mov     AX,TOS           ; load the top of stack pointer
          cmp     AX,STKSIZE-PTRSIZE ; test for overflow
          jge     C_push2          ; jump, if overflow is going to occur
          add     AX,PTRSIZE       ; decrement stop of stack pointer
          mov     TOS,AX           ;  and update it in memory
          add     AX,offset S_stack ; load the address of the new TOS
          mov     DI,AX            ; copy TOS address into DI
          pop     DX               ; unload the return address
          pop     BX               ; load address of register to push
          mov     AL,byte ptr [BX].C_page ; load the page number,
          mov     [DI].car_page,AL ;  pointer displacement,
          mov     AX,[BX].C_disp   ;  and move onto the top of
          mov     [DI].car,AX      ;  Scheme's stack
          jmp     DX               ; return to caller
C_push1   endp

;************************************************************************
;     Lattice C callable routine to pop a register from Scheme's stack  *
;       Calling Sequence:  C_pop(reg)                                   *
;               where:  int reg[2] - register to hold the value popped  *
;************************************************************************
          public  C_pop
C_pop     proc    near
          mov     AX,TOS           ; load the top of stack pointer
          sub     AX,PTRSIZE       ; increment stop of stack pointer
          mov     TOS,AX           ;  and update it in memory
          add     AX,offset S_stack+PTRSIZE ; load the address of the old TOS
          mov     SI,AX            ; copy top of stack address into SI
          pop     DX               ; unload the return address
          pop     BX               ; fetch address of destination register
          mov     AL,[SI].car_page ; load page number,
          mov     byte ptr [BX].C_page,AL   ;  pointer displacement,
          mov     AX,[SI].car      ;  and update into
          mov     [BX].C_disp,AX   ;  receiving register
          jmp     DX               ; return to caller
C_pop     endp

;************************************************************************
;*         Lattice C callable routine to force a Scheme VM call         *
;*      Calling Sequence:  force_call(ret)                              *
;*              where:  int ret - the return address (relative to the   *
;*                                      current code block)             *
;************************************************************************
fc_args   struc
          dw      ?                ; caller's BP
          dw      ?                ; return address
fc_ret    dw      ?                ; Scheme return address
fc_args   ends

          public  force_ca
force_ca  proc    near
          push    BP               ; save the caller's BP register
          mov     BP,SP            ; establish local addressability
          mov     SI,[BP].fc_ret   ; load the Scheme program return address
          call    new_SF           ; create a new stack frame
          mov     FP,BX            ; update the current frame pointer
          pop     BP               ; restore the caller's BP
          ret                      ; return to caller
force_ca  endp

prog      ends

PROGX     segment byte public 'PROGX'
          assume  CS:XGROUP
bad_obj1: jmp     bad_obj2
load_ex   label   far
          save    <BX,SI>          ; save dest register, location pointer
          cmp     byte ptr ptype+[DI],LISTTYPE*2 ; is "code" pointer a list?
          jne     bad_obj1         ; if not, error (jump)
          %LoadPage ES,DI
;;;       mov     ES,pagetabl+[DI] ; load pointer to "compiled program"
          mov     SI,reg0_dis+[BX]
;     skip over "tag" portion of object program
          mov     BL,ES:[SI].cdr_page
          mov     SI,ES:[SI].cdr
;     fetch the number of constants and multiply by three bytes/constant
          cmp     byte ptr ptype+[BX],LISTTYPE*2 ; is this a list cell?
          jne     bad_obj1         ; if not, error (jump)
          %LoadPage ES,BX
;;;       mov     ES,pagetabl+[BX]
          cmp     ES:[SI].car_page,SPECFIX*2 ; is car's entry a fixnum?
          jne     bad_obj1         ; if not, error (jump)
          mov     AX,ES:[SI].car   ; fetch immediate value of fixnum
          shl     AX,1             ; sign extend immediate value
          sar     AX,1
          inc     AX               ; add a constant for entry point address
          mov     DX,AX            ; DX <- AX * 3
          shl     AX,1
          add     DX,AX
          mov     BL,ES:[SI].cdr_page ; follow cdr field of linked list
          mov     SI,ES:[SI].cdr
;     fetch the number of code bytes
          cmp     byte ptr ptype+[BX],LISTTYPE*2 ; is this a list cell?
          jne     bad_obj1         ; if not, error (jump)
          %LoadPage ES,BX
;;;       mov     ES,pagetabl+[BX]
          cmp     ES:[SI].car_page,SPECFIX*2 ; is car's entry a fixnum?
          jne     bad_obj1         ; if not, error (jump)
          mov     AX,ES:[SI].car   ; fetch immediate value of fixnum
          shl     AX,1             ; sign extend immediate value
          sar     AX,1
;     compute number of bytes needed and allocate a new code block
          add     AX,DX            ; add constants*3 + codebytes
          mov     BX,CODETYPE
          pushm   <AX,BX,tmp_adr>  ; push arguments onto TIPC's stack
          save    <DX>             ; preserve register DX across call
          mov     AX,DS            ; make ES point to the data segment
          mov     ES,AX
          call    %allocbl         ; allocate the code block
          mov     SP,BP            ; drop arguments from stack
;     load pointer to newly allocated code block
          mov     DI,tmp_page
          %LoadPage ES,DI
;;;       mov     ES,pagetabl+[DI]
;;;;      mov     DX,ES            ; save code block's paragraph address in DX
          mov     DX,DI            ; save code block's page number in DX
          mov     DI,tmp_disp
          add     DI,PTRSIZE       ; advance DI past block header
;     store entry point address into code block
          mov     AL,SPECFIX*2     ; store tag=fixnum for entry point address
          stosb
          mov     AX,[BP].save_DX  ; store entry point address
          add     AX,PTRSIZE       ; adjust entry point for block header
          stosw
;     reload pointer to object program [Note:  garbage collection may have
;       copied the linked list representation of the program, so pointers
;       held in TIPC registers may not be valid.]
          restore <BX>
          mov     SI,reg0_pag+[BX]    ; load pointer to "object program"
          %LoadPage ES,SI
;;;       mov     ES,pagetabl+[SI]
          mov     SI,reg0_dis+[BX]
          mov     BL,ES:[SI].cdr_page ; skip over "tag"
          mov     SI,ES:[SI].cdr
          %LoadPage ES,BX
;;;       mov     ES,pagetabl+[BX]
          mov     CX,ES:[SI].car   ; load number of constants
          shl     CX,1             ; sign extend immediate value
          sar     CX,1
          mov     BL,ES:[SI].cdr_page ; skip over number of constants
          mov     SI,ES:[SI].cdr
          %LoadPage ES,BX
;;;       mov     ES,pagetabl+[BX]
          mov     AX,ES:[SI].car   ; load number of code bytes
          shl     AX,1             ; sign extend immediate value
          sar     AX,1
          mov     BL,ES:[SI].cdr_page ; skip over number of codebytes
          mov     SI,ES:[SI].cdr
          cmp     byte ptr ptype+[BX],LISTTYPE*2
          je      ok_obj

;     ***error-- invalid object format***
bad_obj:  jmp     bad_obj2

ok_obj:   %LoadPage ES,BX
;;;       mov     ES,pagetabl+[BX]
          pushm   <AX,BX,SI,DS>    ; save # codebytes, ptr to const's list cell
          mov     BL,ES:[SI].car_page ; load constant list header
          mov     SI,ES:[SI].car
;;;;      mov     ES,DX
          %LoadPage0 ES,DX
          cmp     CX,0             ; zero length constants list?
          je      c_end            ; if no constants, skip loop
c_loop:   cmp     BL,0             ; end of constants list?
          je      bad_obj          ; if so, premature end of constant list
          cmp     byte ptr SS:ptype+[BX],LISTTYPE*2 ; is entry a list cell?
          jne     bad_obj          ; if not, error (jump)
          %LoadPage1 DS,BX
;;;       mov     DS,SS:pagetabl+[BX] ; fetch page's address
          movsb                    ; copy car field to code block constants
          movsw                    ;  area
          lodsb                    ; load cdr field to follow linked list
          mov     BL,AL
          mov     SI,[SI]
          loop    c_loop           ; continue through constants list
;     end of constants list-- process byte codes
c_end:
	  pop	  DS		   ; restore previously saved regs
	  pop	  SI
	  mov	  CX,BX		   ; tempsave current bx reg
	  pop	  BX		   ; bx = page number
	  %LoadPage ES,BX	   ; load segment register
	  mov	  BX,CX		   ; restore bx register
	  pop	  CX

          cmp     BL,0             ; end of list found?
          jne     bad_obj          ; if not, too many constants (jump)
;     fetch pointer to codebyte list
          mov     BL,ES:[SI].cdr_page
          mov     SI,ES:[SI].cdr
          cmp     byte ptr ptype+[BX],LISTTYPE*2 ; is next entry a list cell?
          jne     bad_obj          ; if not, error (jump)
          %LoadPage ES,BX
;;;       mov     ES,pagetabl+[BX]
          cmp     ES:[SI].cdr_page,0 ; last entry in object program list?
          je      next$0
          jmp     bad_obj          ; if not, error (jump)
next$0:   mov     BL,ES:[SI].car_page ; load header to bytecode list
          mov     SI,ES:[SI].car
          %LoadPage0 ES,DX          ; Restore code block's paragraph address
;;;       mov     ES,DX
          push    DS
d_loop:   cmp     BL,0             ; end of constants list?
          jne     d_l$0
          jmp     bad_obj          ; if so, premature end of constant list
d_l$0:
          cmp     byte ptr SS:ptype+[BX],LISTTYPE*2 ; is entry a list cell?
          je      d_l$1
          jmp     bad_obj          ; if not, error (jump)
d_l$1:
          %LoadPage1 DS,BX
;;;       mov     DS,SS:pagetabl+[BX] ; fetch page's address
          lodsb                    ; load car's page number
          cmp     AL,SPECFIX*2     ; is codebyte entry a fixnum?
          je      d_lp_nxt         ;   Yes, continue
          jmp     bad_obj          ;   No,  error
d_lp_nxt:
          lodsw                    ; load immediate value
          stosb                    ; store low order byte into code block
          lodsb                    ; load cdr field to follow linked list
          mov     BL,AL
          mov     SI,[SI]
          loop    d_loop           ; continue through codebyte list
;     end of codebyte list-- move code block pointer to destination register
          pop     DS               ; restore TIPC register DS
          cmp     BL,0             ; extraneous codebytes in list?
          jne     bad_obj3         ; if so, error (jump)
          restore <BX,SI>          ; re-fetch dest reg, location pointer
          mov     AL,byte ptr tmp_page ; copy code block pointer into
          mov     byte ptr reg0_pag+[BX],AL ;  destination register
          mov     AX,tmp_disp
          mov     reg0_dis+[BX],AX
          jmp     far ptr execute1 ; execute the code block
bad_obj3: jmp     bad_obj
PROGX     ends

          end
