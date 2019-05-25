;***************************************
;*   TIPC Scheme '84 Runtime Support   *
;*         Operation Support           *
;*                                     *
;*  (C) Copyright 1984,1985,1986 by    *
;*   Texas Instruments Incorporated.   *
;*        All rights reserved.         *
;*                                     *
;* Date Written:  19 April 1984        *
;* Last Modification:  26 February 1986*
;***************************************
          include scheme.equ

DGROUP    group   data
data      segment word public 'DATA'
          assume  DS:DGROUP
new_disp  dw      0
new_page  dw      0
data      ends

PGROUP    group   prog
prog      segment byte public 'PROG'
          assume  CS:PGROUP

;     CONS Support -- combine two pointers in a new list cell
con_arg  struc
          dw      ?                ; return address
con_res   dw      ?                ; address of result register
con_car   dw      ?                ; address of reg. containing car
con_cdr   dw      ?                ; address of reg. containing cdr
con_arg   ends

          extrn   alloc_li:near   ; C routine to allocate a list cell

          public  cons
cons      proc    near
;     Attempt a "short circuit" allocation of a list cell
          mov     BX,listpage      ; load current list cell allocation page no.
;;;       cmp     BX,END_LIST      ; is allocation page specified?
;;;       je      cons_no
          shl     BX,1
          mov     SI,nextcell+[BX] ; load next available cell offset
          cmp     SI,END_LIST
          je      cons_no
;     at this point, the allocation has succeeded
          mov     DX,ES            ; save the caller's ES register
          LoadPage ES,BX
;;;       mov     ES,pagetabl+[BX] ; load list cell page's segment address
          mov     AX,ES:[SI].car   ; load pointer to next available cell
          mov     nextcell+[BX],AX ;  and update free cell chain header

;     store CDR value into list cell
cons_ok:  mov     CX,BP            ; save the caller's base pointer
          mov     BP,SP            ;  and establish addressability for args
          mov     DI,[BP].con_cdr  ; fetch address of register containing CDR
          mov     AL,byte ptr [DI].C_page ; copy contents of register into
          mov     ES:[SI].cdr_page,AL     ;  the new list cell's CDR field
          mov     AX,[DI].C_disp
          mov     ES:[SI].cdr,AX

;     store CAR value into list cell
          mov     DI,[BP].con_car  ; fetch address of register containing CAR
          mov     AL,byte ptr [DI].C_page ; copy contents of register into
          mov     ES:[SI].car_page,AL     ;  the new list cell's CAR field
          mov     AX,[DI].C_disp
          mov     ES:[SI].car,AX

;     store pointer to new list cell in destination register
          mov     DI,[BP].con_res  ; fetch address of destination register
          mov     byte ptr [DI].C_page,BL
          mov     [DI].C_disp,SI

          mov     ES,DX            ; restore caller's ES register
          mov     BP,CX            ; restore caller's BP register
          ret                      ; return to caller

;     OOPS-- no list cell immediately available-- go through channels
cons_no:  mov     AX,offset new_disp ; push address of a dummy result
          push    AX               ;  register onto the TIPC's stack
          call    alloc_li         ; allocate a list cell
          add     SP,WORDINCR      ; drop argument from stack
          mov     BX,new_page      ; fetch list cell's page number
          mov     SI,new_disp      ;  and displacement
          mov     DX,ES            ; save the caller's ES register
          LoadPage ES,BX
;;;       mov     ES,pagetabl+[BX] ; make ES point to the new list cell
          jmp     cons_ok
cons      endp
prog      ends
          end
