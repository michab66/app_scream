;                                                       =====> SRELOCAT.ASM
;***************************************
;*     TIPC Scheme  Runtime Support    *
;*    GC Pointer Relocation Routines   *
;*                                     *
;*    (C) Copyright 1985 by Texas      *
;*     Instruments Incorporated.       *
;*        All rights reserved.         *
;*                                     *
;* Date Written:  23 September 1985    *
;* Last Modification:  18 October 1985 *
;***************************************
          include scheme.equ

DGROUP    group   data
XGROUP    group   PROGX
PGROUP    group   prog

data      segment word public 'DATA'
          assume  DS:DGROUP

msg_relp  db      "[VM INTERNAL ERROR] rel_ptr: invalid %x:%04x (unadjusted)"
          db      LF,0

page_sav  dw	  ?		   ; Page number save area

;     Branch table for processing each data type
btable    dw      rel_list         ; [0] List cells
          dw      rel_fix          ; [1] Fixnums
          dw      rel_flo          ; [2] Flonums
          dw      rel_big          ; [3] Bignums
          dw      rel_sym          ; [4] Symbols
          dw      rel_str          ; [5] Strings
          dw      rel_ary          ; [6] Arrays
          dw      rel_cont         ; [7] Continuations
          dw      rel_clos         ; [8] Closures
          dw      rel_free         ; [9] Free space (unallocated)
          dw      rel_code         ; [10] Code
          dw      rel_ref          ; [11] Reference cells
          dw      rel_port         ; [12] Port data objects
          dw      rel_char         ; [13] Characters
          dw      rel_env          ; [14] Environments

ctable    dw      rep_list         ; [0] List cells
          dw      rep_fix          ; [1] Fixnums
          dw      rep_flo          ; [2] Flonums
          dw      rep_big          ; [3] Bignums
          dw      rep_sym          ; [4] Symbols
          dw      rep_str          ; [5] Strings
          dw      rep_ary          ; [6] Arrays
          dw      rep_cont         ; [7] Continuations
          dw      rep_clos         ; [8] Closures
          dw      rep_free         ; [9] Free space (unallocated)
          dw      rep_code         ; [10] Code
          dw      rep_ref          ; [11] Reference cells
          dw      rep_port         ; [12] Port data objects
          dw      rep_char         ; [13] Characters
          dw      rep_env          ; [14] Environments

dtable    dw      fwd_list         ; [0] List cells
          dw      fwd_fix          ; [1] Fixnums
          dw      fwd_flo          ; [2] Flonums
          dw      fwd_big          ; [3] Bignums
          dw      fwd_sym          ; [4] Symbols
          dw      fwd_str          ; [5] Strings
          dw      fwd_ary          ; [6] Arrays
          dw      fwd_cont         ; [7] Continuations
          dw      fwd_clos         ; [8] Closures
          dw      fwd_free         ; [9] Free space (unallocated)
          dw      fwd_code         ; [10] Code
          dw      fwd_ref          ; [11] Reference cells
          dw      fwd_port         ; [12] Port data objects
          dw      fwd_char         ; [13] Characters
          dw      fwd_env          ; [14] Environments
data      ends

prog      segment byte public 'PROG'
          assume  CS:PGROUP
          extrn   %printf:far

;************************************************************************
;*                      Far Linkage to FORCE_DEBUG                      *
;************************************************************************
%forcede  proc    far
          extrn   force_de:near
          call    force_de
          ret
%forcede  endp

prog      ends

PROGX     segment byte public 'PROGX'
          assume  CS:XGROUP

;************************************************************************
;*          Garbage Collection -- Pointer Relocation Phase              *
;************************************************************************
          public  srelocat
srelocat  proc    near
          push    ES               ; save caller's ES register
          push    BP               ;  and BP register
          mov     BP,SP            ;  and establish addressability

;     relocate the pointers within each page
          mov     BX,DEDPAGES*WORDINCR ; initialize page counter
srel_lop: test    attrib+[BX],NOMEMORY
          jnz     srel_nxt
	  mov	  DI,SS:ptype+[BX] ; get data type for page
	  cmp	  DI,FREETYPE*2	   ; Free Page?
	  je	  srel_nxt	   ;  Yes...continue
          push    BX               ; save the page counter
          call    rel_page         ; relocate pointers in current page
          pop     BX               ; restore page counter
srel_nxt: add     BX,WORDINCR      ; increment page counter
          cmp     BX,NUMPAGES*WORDINCR ; all pages processed?
          jb      srel_lop         ; if more pages, jump

;     relocate registers R1-R63
          xor     BX,BX            ; clear BX
          mov     CX,NUM_REGS-1    ; load number of registers ('cept for R0)
          mov     DI,offset reg0 + size C_ptr ; load address of R1
srel_reg: call    rel_reg          ; relocate register Rn
          add     DI,size C_ptr    ; increment pointer to next reigster
          loop    srel_reg         ; loop until R1-R63 relocated

;     relocate the other internal registers
          mov     DI,offset FNV_reg
          call    rel_reg          ; relocate FNV_reg
          mov     DI,offset GNV_reg
          call    rel_reg          ; relocate GNV_reg
          mov     DI,offset PREV_reg
          call    rel_reg          ; relocate PREV_reg
          mov     DI,offset CB_reg
          call    rel_reg          ; relocate CB_reg
          mov     DI,offset TRNS_reg
          call    rel_reg          ; relocate TRNS_reg
          mov     DI,offset tmp_reg
          call    rel_reg          ; relocate tmp_reg
          mov     DI,offset tm2_reg
          call    rel_reg          ; relocate tm2_reg
          mov     DI,offset FNV_save
          call    rel_reg          ; relocate FNV_save
          mov     DI,offset STL_save
          call    rel_reg          ; relocate STL_save

;     relocate the system oblist and the property lists
          mov     CX,HT_SIZE       ; load iteration count
          xor     DX,DX            ; zero the index
rel_tab:  mov     DI,DX            ; copy loop index to DI
          mov     BL,hash_pag+[DI] ; fetch hash table entry page number
          shl     DI,1             ; double index value for use as word index
          mov     SI,hash_dis+[DI] ; fetch hash table entry displacement
          call    rel_ptr          ; relocate the pointer
          mov     hash_dis+[DI],SI ; store the relocated
          mov     SI,DX            ;  pointer back into the
          mov     hash_pag+[SI],BL ;  system hash table
          mov     BL,prop_pag+[SI] ; fetch property list entry page number
          mov     SI,prop_dis+[DI] ;  and displacement
          call    rel_ptr          ; relocate the property list entry pointer
          mov     prop_dis+[DI],SI ; store the relocated
          mov     DI,DX            ;  pointer back into the
          mov     prop_pag+[DI],BL ;  system property list table
          inc     DX               ; increment the loop index
          loop    rel_tab          ; continue 'til all entries processed

;     Relocate the pointers in the runtime stack
          mov     DI,offset S_stack ; load address of stack buffer
          mov     DX,TOS           ; load current top of stack and
          add     DX,DI            ;  compute stack's ending address
rel_stk:  mov     BL,[DI].car_page ; load next stack entry from the
          mov     SI,[DI].car      ;  stack buffer
          call    rel_ptr          ; relocate the pointer
          mov     [DI].car_page,BL ; store the relocated pointer back into
          mov     [DI].car,SI      ;  the stack buffer
          add     DI,PTRSIZE       ; increment the stack buffer pointer
          cmp     DI,DX            ; end of active stack buffer?
          jbe     rel_stk          ; if more entries in stack, jump

;     Relocate the pointers in the object hash table
          mov     CX,OHT_SIZE      ; load count of object hash table entries
          mov     DI,offset obj_ht ; load address of object hash table
rel_oht:  mov     BL,[DI].car_page ; load next entry in the
          mov     SI,[DI].car      ;  object hash table
          call    rel_ptr          ; relocate the pointer
          mov     [DI].car_page,BL ; store the relocated pointer back
          mov     [DI].car,SI      ;  into the object hash table
          add     DI,PTRSIZE       ; increment the loop index
          loop    rel_oht          ; continue until all entries processed

;     Return to caller
rel_rtn:  pop     BP               ; restore caller's BP register
          pop     ES               ;  and ES register
          ret                      ; return
srelocat  endp

;************************************************************************
;*         Local Support-- Relocate pointers in a single page           *
;************************************************************************
rel_page  proc    near
	  mov	  page_sav,BX	   ; Save this page number
	  %LoadPage ES,BX	   ; load the page's paragraph address
;;;       mov     ES,pagetabl+[BX] ; load the page's paragraph address
          mov     DX,psize+[BX]    ; load the current page size
          sub     DX,PTRSIZE       ;  and adjust for end of page boundary
          mov     SI,ptype+[BX]
          xor     DI,DI            ; zero the page index
          xor     BX,BX            ; zero BX
          jmp     btable+[SI]

rel_list:                          ; [0] List cells
          sub     DX,LISTSIZE-PTRSIZE
rel_l010: mov     BL,ES:[DI].car_page ; fetch the car field's page number
          cmp     BL,0FFh          ; unused list cell?
          je      rel_l020         ; if unused, jump
          test    byte ptr ES:[DI].list_gc,GC_BIT ; is this a relocated pointer?
          jnz     rel_l020         ; if a relocated ptr, leave it alone
          mov     SI,ES:[DI].car   ; fetch the car field's displacement field
          call    rel_ptr          ; relocate the pointer
	  %LoadPage ES,page_sav	   ; Re-load source page
          mov     ES:[DI].car_page,BL ; store the relocated car pointer
          mov     ES:[DI].car,SI   ;     back into the list cell
          mov     BL,ES:[DI].cdr_page ; fetch the cdr field from
          mov     SI,ES:[DI].cdr   ;     the list cell
          call    rel_ptr          ; relocate the pointer
	  %LoadPage ES,page_sav	   ; Re-load source page
          mov     ES:[DI].cdr_page,BL ; store the relocated cdr pointer
          mov     ES:[DI].cdr,SI   ;     back into the list cell
rel_l020: add     DI,LISTSIZE      ; increment the page index
          cmp     DI,DX            ; end of page?
          jbe     rel_l010         ; if more list cells to process, jump
          jmp     rel_ret          ; return

rel_sym:                           ; [4] Symbols
rel_port:                          ; [12] Port data objects
rel_s010: cmp     ES:[DI].sym_type,FREETYPE ; free block?
          je      rel_s020         ; if free block, jump
          test    ES:[DI].sym_gc,GC_BIT ; is this a relocated object?
          jnz     rel_s020         ; if a forwarding pointer, jump
          mov     BL,ES:[DI].sym_page ; load pointer operand from the
          mov     SI,ES:[DI].sym_disp ;  port or symbol object
          call    rel_ptr          ; relocate the pointer, if needed
	  %LoadPage ES,page_sav	   ; Re-load source page
          mov     ES:[DI].sym_page,BL ; store relocated pointer back in
          mov     ES:[DI].sym_disp,SI ;  the port or symbol
rel_s020: add     DI,ES:[DI].sym_len ; increment the page index
          cmp     DI,DX            ; end of page?
          jbe     rel_s010         ; if not end of page, jump
          jmp     rel_ret          ; return
          

rel_code:                          ; [10] Code
rel_c010: cmp     ES:[DI].cod_type,FREETYPE ; is this a free block?
          je      rel_c030         ; if unused block, jump
          test    ES:[DI].cod_gc,GC_BIT ; is this a relocated code block?
          jnz     rel_c030         ; if a forwarding pointer, jump
          mov     AX,DI            ; save starting offset of object
          mov     CX,ES:[DI].cod_entr ; load the entry point
          add     CX,DI            ;    and compute ending offset
          sub     CX,BLK_OVHD+PTRSIZE
          jmp     short rel_c025   ; test for code block with no constants
rel_c020: mov     BL,ES:[DI].cod_cpag ; load next pointer from the
          mov     SI,ES:[DI].cod_cdis ;  object
          call    rel_ptr          ; relocate pointer, if needed
	  %LoadPage ES,page_sav	   ; Re-load source page
          mov     ES:[DI].cod_cpag,BL ; store the relocated pointer
          mov     ES:[DI].cod_cdis,SI ;  back into the object
          add     DI,PTRSIZE       ; increment the page index
rel_c025: cmp     DI,CX            ; all pointers updated?
          jb      rel_c020         ; if more pointers, jump
          mov     DI,AX            ; restore starting offset of object
rel_c030: add     DI,ES:[DI].cod_len ; adjust index for free area
          cmp     DI,DX            ; end of page?
          jbe     rel_c010         ; if not end of page, jump
          jmp     rel_ret          ; return

rel_ary:                           ; [6] Arrays
rel_cont:                          ; [7] Continuations
rel_clos:                          ; [8] Closures
rel_env:                           ; [14] Environments
rel_v010: cmp     ES:[DI].vec_type,FREETYPE ; is this a free block?
          je      rel_v030         ; if unused block, jump
          test    ES:[DI].vec_gc,GC_BIT ; has object been relocated?
          jnz     rel_v030         ; if a forwarding pointer, jump
          mov     AX,DI            ; save starting offset of object
          mov     CX,ES:[DI].vec_len ; load the object's length
          add     CX,DI            ;    and compute ending offset
          sub     CX,BLK_OVHD      ; adjust ending offset for block header
          jmp     short rel_v025   ; test for zero length object
rel_v020: mov     BL,ES:[DI].vec_page ; load next pointer from the
          mov     SI,ES:[DI].vec_disp ;  object
          call    rel_ptr          ; relocate pointer, if needed
	  %LoadPage ES,page_sav	   ; Re-load source page
          mov     ES:[DI].vec_page,BL ; store the relocated pointer
          mov     ES:[DI].vec_disp,SI ;  back into the object
          add     DI,PTRSIZE       ; increment the page index
rel_v025: cmp     DI,CX            ; all pointers updated?
          jb      rel_v020         ; if more pointers, jump
          mov     DI,AX            ; restore starting offset of object
rel_v030: add     DI,ES:[DI].vec_len ; adjust index for free area
          cmp     DI,DX            ; end of page?
          jbe     rel_v010         ; if not end of page, jump
          jmp     rel_ret          ; return

rel_fix:                           ; [1] Fixnums
rel_flo:                           ; [2] Flonums
rel_big:                           ; [3] Bignums
rel_str:                           ; [5] Strings
rel_free:                          ; [9] Free space (unallocated)
rel_ref:                           ; [11] Reference cells (hope not...)
rel_char:                          ; [13] Characters

rel_ret:  ret                      ; return to caller
rel_page  endp

;************************************************************************
;*      Local Support-- Relocate a pointer contained in a register      *
;*                                                                      *
;* Parameters:  DI - address of register                                *
;************************************************************************
rel_reg   proc    near
          xor     BX,BX            ; clear BX
          mov     BL,byte ptr [DI].C_page ; fetch the register's
          mov     SI,[DI].C_disp   ;         contents
          call    rel_ptr          ; relocate the pointer
          mov     byte ptr [DI].C_page,BL ; store the relocated pointer
          mov     [DI].C_disp,SI   ;      back into the register
          ret                      ; return
rel_reg   endp

;************************************************************************
;*            Local Support-- Relocate a single pointer                 *
;*                                                                      *
;* Parameters:  BX - page number index (page*2)                         *
;*              SI - displacement                                       *
;************************************************************************
rel_ptr   proc    near
          cmp     BX,DEDPAGES*WORDINCR ; is this a special non-GCed page?
          jl      rep_ret1         ; if special page, no relocation done
          push    ES               ; save caller's ES
          push    DI               ;  and save caller's DI
	  %LoadPage ES,BX	   ; load the paragraph address for ptr's page
;;;       mov     ES,pagetabl+[BX] ; load paragraph address for pointer's page
          mov     DI,ptype+[BX]
          cmp     DI,NUMTYPES*2
          jae     rel_oops
          jmp     ctable+[DI]      ; jump according to pointer type

;     ***error-- invalid type/length code***
rel_oops: pushm   <AX,CX,DX,SI,BX> ; save registers; push page:disp
          mov     AX,offset msg_relp ; move address of "format"
          push    AX               ;    and push as argument to printf
          mov     AX,DS            ; make ES point to the data segment
          mov     ES,AX
          call    %printf          ; print the error message
          call    %forcede         ; invoke the VM debugger with next instr.
          popm    <AX,BX,SI,DX,CX,AX> ; restore registers
          jmp     short rep_ret    ; return

rep_list:                          ; [0] List Cells
          test    byte ptr ES:[SI].list_gc,GC_BIT ; has cell been relocated?
          jz      rep_ret          ; if not moved, return (jump)
          mov     BL,ES:[SI].car_page ; replace original pointer with
          mov     SI,ES:[SI].car   ;     the updated pointer
          and     SI,07FFFh        ; clear the GC bit
          jmp     short rep_ret    ; return

rep_flo:                           ; [2] Flonums
          test    byte ptr ES:[SI].flo_gc,GC_bit ; has flonum been relocated?
          jz      rep_ret          ; if not moved, return (jump)
          mov     BL,ES:[SI].flo_data ; replace original pointer with
          mov     SI,word ptr ES:[SI].flo_data+1 ;  the updated pointer
          jmp     short rep_ret    ; return

rep_big:                           ; [3] Bignums
rep_sym:                           ; [4] Symbols
rep_str:                           ; [5] Strings
rep_ary:                           ; [6] Arrays
rep_cont:                          ; [7] Continuations
rep_clos:                          ; [8] Closures
rep_code:                          ; [10] Code
rep_port:                          ; [12] Port data objects
rep_env:                           ; [14] Environments
          test    byte ptr ES:[SI].vec_gc,GC_bit ; has object been relocated?
          jz      rep_ret          ; if not moved, return (jump)
          mov     BL,ES:[SI].vec_page ; replace original pointer with
          mov     SI,ES:[SI].vec_disp ;  the updated pointer
          jmp     rep_ret          ; return

rep_fix:                           ; [1] Fixnums
rep_free:                          ; [9] Free space (unallocated)
rep_ref:                           ; [11] Reference cells (hope not...)
rep_char:                          ; [13] Characters

rep_ret:  pop     DI               ; restore caller's DI
          pop     ES               ; restore caller's ES
rep_ret1: ret                      ; return to caller
rel_ptr   endp

;************************************************************************
;*                      Complement GC (forwarding) Bits                 *
;************************************************************************
          public  toggleGC
toggleGC  proc    near
          push    ES               ; save caller's ES register
          push    BP               ;  and BP register
          mov     BP,SP            ;  and establish addressability
          mov     BX,DEDPAGES*WORDINCR ; initialize page counter
togl_lop: test    attrib+[BX],NOMEMORY
          jnz     togl_nxt
	  mov	  DI,SS:ptype+[BX] ; get data type for page
	  cmp	  DI,FREETYPE*2	   ; Free Page?
	  je	  togl_nxt	   ;  Yes...continue
          push    BX               ; save the page counter
          call    togl_pag         ; complement GC bits in current page
          pop     BX               ; restore page counter
togl_nxt: add     BX,WORDINCR      ; increment page counter
          cmp     BX,NUMPAGES*WORDINCR ; all pages processed?
          jb      togl_lop         ; if more pages, jump
          mov     SP,BP
          pop     BP
          pop     ES
          ret
toggleGC  endp


togl_pag  proc    near
	  %LoadPage ES,BX	   ; load the page's paragraph address
;;;       mov     ES,pagetabl+[BX] ; load the page's paragraph address
          mov     DX,psize+[BX]    ; load the current page size
          sub     DX,PTRSIZE       ;  and adjust for end of page boundary
          mov     SI,ptype+[BX]
          xor     DI,DI            ; zero the page index
          xor     BX,BX            ; zero BX
          jmp     dtable+[SI]

fwd_list:                          ; [0] List cells
          sub     DX,LISTSIZE-PTRSIZE
fwd_l010: cmp     ES:[DI].car_page,0FFh ; unused list cell?
          je      fwd_l020         ; if unused, jump
          xor     byte ptr ES:[DI].list_gc,GC_BIT ; toggle the GC (forward) bit
fwd_l020: add     DI,LISTSIZE      ; increment the page index
          cmp     DI,DX            ; end of page?
          jbe     fwd_l010         ; if more list cells to process, jump
          jmp     togl_ret         ; return


fwd_flo:                           ; [2] Flonums
          sub     DX,FLOSIZE-PTRSIZE
fwd_f010: cmp     byte ptr ES:[DI].flo_type,0FFh ; unused flonum?
          je      fwd_f020         ; if unused, jump
          xor     byte ptr ES:[DI].flo_gc,GC_BIT ; toggle the GC (forward) bit
fwd_f020: add     DI,FLOSIZE       ; increment the page index
          cmp     DI,DX            ; end of page?
          jbe     fwd_f010         ; if more flonums to process, jump
          jmp     togl_ret         ; return


fwd_str:                           ; [5] Strings

fwd_big:                           ; [3] Bignums
fwd_sym:                           ; [4] Symbols
fwd_ary:                           ; [6] Arrays
fwd_cont:                          ; [7] Continuations
fwd_clos:                          ; [8] Closures
fwd_code:                          ; [10] Code
fwd_port:                          ; [12] Port data objects
fwd_env:                           ; [14] Environments
fwd_v010: cmp     ES:[DI].vec_type,FREETYPE ; is this a free block?
          je      fwd_v030         ; if unused block, jump
          xor     ES:[DI].vec_gc,GC_BIT ; toggle GC (forward) bit
fwd_v030: mov     CX,ES:[DI].vec_len ; adjust index for free area
          cmp     CX,0             ;;; check for small string
          jge     fwd_v040
          mov     CX,BLK_OVHD+PTRSIZE
fwd_v040: add     DI,CX
          cmp     DI,DX            ; end of page?
          jbe     fwd_v010         ; if not end of page, jump
          jmp     togl_ret         ; return

fwd_fix:                           ; [1] Fixnums
fwd_free:                          ; [9] Free space (unallocated)
fwd_ref:                           ; [11] Reference cells
fwd_char:                          ; [13] Characters

togl_ret: ret                      ; return to caller
togl_pag  endp

PROGX     ends
          end
