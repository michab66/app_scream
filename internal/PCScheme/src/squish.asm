;                                                       =====> SQUISH.ASM
;***************************************
;*     TIPC Scheme  Runtime Support    *
;*      Memory Compaction Routines     *
;*                                     *
;*    (C) Copyright 1985 by Texas      *
;*     Instruments Incorporated.       *
;*        All rights reserved.         *
;*                                     *
;* Date Written:  23 September 1985    *
;* Last Modification:  22 October 1985 *
;*				       *
;*  rb 2/ 2/88 - put in TC's GC fix    *
;*				       *
;***************************************
	  .286c			;; Utilize the expanded 80286 instruction set
          include scheme.equ

DGROUP    group   data
XGROUP    group   PROGX
PGROUP    group   prog

MSDOS     equ     021h

data      segment word public 'DATA'
          assume  DS:DGROUP
ret_sav1  dw      0                ; return address save area
ret_sav2  dw      0                ; return address save area
;;;msg    db      " Compacting Memory *",0
;;;msg1a          db      "Moving List Cells",LF,0
;;;msg1b          db      "Moving Flonums",LF,0
;;;msg1c          db      "Moving Bignums",LF,0
;;;msg1d          db      "Moving Closures",LF,0
;;;msg1e          db      "Moving Code Blocks",LF,0
;;;msg1f          db      "Moving Vectors",LF,0
;;;msg1g          db      "Moving Continuations",LF,0
;;;msg1h          db      "Moving Symbols",LF,0
;;;msg1i          db      "Moving Strings",LF,0
;;;msg2   db      "About to Relocate Pointers",LF,0
;;;msg3   db      "Complementing GC Bits",LF,0
;;;msg4   db      "About to Sweep",LF,0
data      ends

prog      segment byte public 'PROG'
          assume  CS:PGROUP
          extrn   %allocbl:far     ; "alloc_block" linkage routine

;************************************************************************
;* Far Linkage to SUM_SPACE                                             *
;************************************************************************
%sumspac  proc    far
          pop     ret_sav1
          pop     ret_sav2
          extrn   sum_spac:near
          call    sum_spac
          push    ret_sav2
          push    ret_sav1
          ret
%sumspac  endp

;************************************************************************
;* Far Linkage to GCSWEEP                                               *
;************************************************************************
%gcsweep  proc    far
          pop     ret_sav1
          pop     ret_sav2
          extrn   gcsweep:near
          call    gcsweep
          push    ret_sav2
          push    ret_sav1
          ret
%gcsweep  endp

IFDEF EXPMEM

;************************************************************************
;* Far Linkage to GCCLEAN						*
;************************************************************************
%gcclean  proc	  far
	  pop	  ret_sav1
	  pop	  ret_sav2
	  extrn	  gcclean:near
	  call	  gcclean
	  push	  ret_sav2
	  push	  ret_sav1
	  ret
%gcclean  endp

ENDIF

;************************************************************************
;*     ***Temporary Long Linkage to PRINTF***                           *
;************************************************************************
          public  %printf,%sdebug
%printf   proc    far
          pop     ret_sav1
          pop     ret_sav2
          extrn   printf:near
          call    printf
          push    ret_sav2
          push    ret_sav1
          ret
%printf   endp

;************************************************************************
;*     ***Temporary Long Linkage to SDEBUG***                           *
;************************************************************************
%sdebug   proc    far
          pop     ret_sav1
          pop     ret_sav2
          extrn   sdebug:near
          call    sdebug
          push    ret_sav2
          push    ret_sav1
          ret
%sdebug   endp

prog      ends

PROGX     segment byte public 'PROGX'
          assume  CS:XGROUP

          extrn   srelocat:near    ; pointer relocation routine
          extrn   toggleGC:near    ; complement GC bits

;************************************************************************
;*              Garbage Collection -- Compaction Phase                  *
;************************************************************************
sq_args   struc
sq_free   dw      NUMPAGES dup (?) ; amount of free space within each page
sq_plist  dw      NUMPAGES dup (?) ; list of pages
sq_BP     dw      ?                ; caller's BP register
          dw      ?                ; caller's ES register
          dd      ?                ; return address (far call)
          dw      ?                ; return address (near call)
sq_args   ends

%squish   proc    far
          push    ES               ; save caller's ES register
          push    BP               ;  and BP register
          sub     SP,offset sq_BP  ; allocate local storage
          mov     BP,SP            ;  and establish addressability

;     Compute the amount of free space in each page
          lea     BX,[BP].sq_free  ; load address of size array
          push    BX               ;  and push as argument to "sum_space"
          call    %sumspac         ; determine available space in each page
          mov     SP,BP            ; drop argument from TIPC's stack

;     Initialize table of page numbers
          mov     AX,DS            ; make ES point to the data
          mov     ES,AX            ;  segment
          mov     CX,NUMPAGES      ; load page count
          lea     DI,[BP].sq_plist ; load address of page number table
          xor     AX,AX            ; initialize page number index to zero
pt_loop:  stosw                    ; set page number to current position
          add     AX,WORDINCR      ; increment page index
          loop    pt_loop          ; process all page numbers

;     Reset the similar page type chain headers
          mov     CX,NUMTYPES
          mov     AX,END_LIST
          mov     DI,offset pagelist
rep       stosw

;     Sort list of pages according to size available
          mov     DX,DEDPAGES*WORDINCR
sort_nxt: mov     SI,DX
          mov     DI,[BP].sq_plist+[SI]
          mov     AX,[BP].sq_free+[DI] ; load amount of space in base page
sort_mor: add     SI,WORDINCR      ; increment inner loop index
          mov     DI,[BP].sq_plist+[SI] ; load page index
          cmp     AX,[BP].sq_free+[DI] ; has current page less space?
          jbe     sort_no          ; if not, jump
          mov     AX,[BP].sq_free+[DI] ; load size of smaller free space
          mov     DI,DX
          mov     CX,[BP].sq_plist+[SI] ; exchange base page index
          xchg    CX,[BP].sq_plist+[DI] ;  with current page
          mov     [BP].sq_plist+[SI],CX ;  index
sort_no:  cmp     SI,NUMPAGES*WORDINCR-WORDINCR ; is inner loop complete?
          jl      sort_mor         ; if not, jump
          add     DX,WORDINCR      ; increment outer loop index
          cmp     DX,NUMPAGES*WORDINCR-WORDINCR ; is outer loop complete?
          jl      sort_nxt         ; if not, keep on loopin'

;     Update the similar page type chains
          mov     DI,DEDPAGES*WORDINCR
spt_loop: mov     SI,[BP].sq_plist+[DI]
          test    attrib+[SI],NOMEMORY
          jnz     spt_end
          mov     BX,ptype+[SI]
          mov     AX,pagelist+[BX]
          mov     pagelink+[SI],AX
          mov     AX,SI
          CORRPAGE AX
          mov     pagelist+[BX],AX
spt_end:  add     DI,WORDINCR
          cmp     DI,NUMPAGES*WORDINCR
          jl      spt_loop

IFDEF EXPMEM
	  call	  %gcclean	   ; Clean out Emm Page table for compaction
ENDIF

;     Note:  If printing messages, make ES point to the data segment
;;;       mov     AX,DS            ;* Make ES point to the data
;;;       mov     ES,AX            ;* segment

;     Compact List Cells
;;;       mov     AX,offset msg1a  ;*
;;;       push    AX               ; * print message indicating we're
;;;       call    %printf          ;*  compacting list cells
          call    sq_list

;     Compact Flonums
;;;       mov     AX,offset msg1b  ;*
;;;       push    AX               ; * print message indicating we're
;;;       call    %printf          ;*  compacting flonums
          call    sq_flo

;     Compact Bignums
;;;       mov     AX,offset msg1c  ;*
;;;       push    AX               ; * print message indicating we're
;;;       call    %printf          ;*  compacting bignums
          mov     AX,BIGTYPE*2     ; load type code index for bignums
          push    AX               ;  and push as argument to "sq_var"
          call    sq_var
          mov     SP,BP            ; drop arguments from stack

;     Compact Closures
;;;       mov     AX,offset msg1d  ;*
;;;       push    AX               ; * print message indicating we're
;;;       call    %printf          ;*  compacting closures
          mov     AX,CLOSTYPE*2    ; load type code index for closures
          push    AX               ;  and push as argument to "sq_var"
          call    sq_var
          mov     SP,BP            ; drop arguments from stack

;     Compact Code Blocks
;;;       mov     AX,offset msg1e  ;*
;;;       push    AX               ; * print message indicating we're
;;;       call    %printf          ;*  compacting code blocks
          mov     AX,CODETYPE*2    ; load type index for code blocks
          push    AX               ;  and push as argument to "sq_var"
          call    sq_var
          mov     SP,BP            ; drop arguments from stack

;     Compact Vectors
;;;       mov     AX,offset msg1f  ;*
;;;       push    AX               ; * print message indicating we're
;;;       call    %printf          ;*  compacting vectors
          mov     AX,VECTTYPE*2    ; load type index for vectors
          push    AX               ;  and push as argument to "sq_var"
          call    sq_var
          mov     SP,BP            ; drop arguments from stack

;     Compact Continuations
;;;       mov     AX,offset msg1g  ;*
;;;       push    AX               ; * print message indicating we're
;;;       call    %printf          ;*  compacting continuations
          mov     AX,CONTTYPE*2    ; load type index for continuations
          push    AX               ;  and push as argument to "sq_var"
          call    sq_var
          mov     SP,BP            ; drop arguments from stack

;;;   Note:  Let's not compact symbols for now.  There are a few "special"
;;;          symbols which mess things up in the runtime support if they
;;;          move.  Notably, CONSOLE_ and QUOTE_reg(?)
;;;;     Compact Symbols
;;;       mov     AX,offset msg1h  ;*
;;;       push    AX               ; * print message indicating we're
;;;       call    %printf          ;*  compacting symbols
;;;       mov     AX,SYMTYPE*2     ; load type index for symbols
;;;       push    AX               ;  and push as argument to "sq_var"
;;;       call    sq_var
;;;       mov     SP,BP            ; drop arguments from stack

;     Compact Strings
;;;       mov     AX,offset msg1i  ;*
;;;       push    AX               ; * print message indicating we're
;;;       call    %printf          ;*  compacting strings
          mov     AX,STRTYPE*2     ; load type index for strings
          push    AX               ;  and push as argument to "sq_var"
          call    sq_var
          mov     SP,BP            ; drop arguments from stack

;     Relocate all moved pointers
;;;       mov     AX,offset msg2   ;*
;;;       push    AX               ; * print a message that we're about
;;;       call    %printf          ;*  to perform pointer relocation
          call    srelocat         ; relocate all pointers

;     Toggle the GC bits used to denote forwarding
;;;       mov     AX,offset msg3   ;*
;;;       push    AX               ; * print a message that we're
;;;       call    %printf          ; * complementing the GC bits
          call    toggleGC         ; complement the GC (forwarding) bits

IFDEF EXPMEM
	  call	  %gcclean	   ; Clean out Emm Page table
ENDIF

;     Invoke the "sweep" portion of the garbage collector to reclaim memory
;;;       mov     AX,offset msg4   ;*
;;;       push    AX               ; * print a message that it's
;;;       call    %printf          ; * "sweep" time
          call    %gcsweep         ; reclaim all freed memory

;     Return to caller
          mov     SP,BP            ; deallocate stack temporaries
          add     SP,offset sq_BP  ; release local storage
          pop     BP               ; restore caller's BP register
          pop     ES               ;  and ES register
          ret                      ; return
%squish   endp

;************************************************************************
;*              Macro Support for List/Flonum Compaction                *
;*                                                                      *
;* Register usage during "move" phase of this routine:                  *
;*      AX - backward chain header (destination page index)             *
;*      BX - (scratch register)                                         *
;*      CX - word count for block move                                  *
;*      DX - forward chain header (source page index)                   *
;*      DS:[SI] - source list cell                                      *
;*      ES:[DI] - destination list cell                                 *
;************************************************************************
sql_arg   struc
sql_rev   dw      NUMPAGES dup (?) ; reversed linked list of list pages
sql_bptr  dw      ?                ; reversed list header
sql_BP    dw      ?                ; caller's BP
          dw      ?                ; caller's ES
          dw      ?                ; return address
sql_type  dw      ?                ; type code index (for variable len objects)
sql_arg   ends

sq_L_F    macro   uppercase,lowercase
          local   sql_go,sql_010,sql_020,sql_025,sql_030,sql_035
	  local   sql_040,sql_050,sql_060,sql_070,sql_done,sql_ret
          push    ES               ; save caller's ES
          push    BP               ; save caller's BP
          sub     SP,offset sql_BP ; allocate local storage
          mov     BP,SP            ; establish local addressability

;     Create a reverse order linked list of pages
          lea     BX,[BP].sql_rev  ; load addr of reverse linked list array
          mov     AX,uppercase&TYPE*2      ; load type code
          pushm   <AX,BX>          ; push type code, array addr as arguments
          call    sq_rever         ; create the reverse linked list
          mov     SP,BP            ; drop arguments off TIPC's stack
          cmp     AX,END_LIST      ; is list of pages empty?
          jne     sql_go           ; if list non-empty, continue (jump)
          jmp     sql_ret          ; if empty list, return
sql_go:   ADJPAGE AX               ; convert list header to page index value

;     Move list cells from least dense pages to most dense pages
          mov     DX,lowercase&page        ; load page number of least dense
          ADJPAGE DX               ;  page and convert to page index
          mov     BX,DX            ; copy page index into BX
          push    DS               ; save DS register

; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
; * * * WARNING:  The DS Register Doesn't Point to the Data Segment * * *
; * * *           in the code which follows:                        * * *
; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

	  %LoadPage0 DS,BX	   ; load paragraph address of source page

          mov     SI,-uppercase&SIZE       ; load source page index
          jmp     short sql_020    ; jump

;     Follow backward chain to get new destination page
sql_010:   mov    BX,AX            ; set next available cell address to
          mov     SS:nextcell+[BX],DI ;  END_LIST
          mov     BX,BP            ; calculate address of current element in
          add     BX,AX            ;  reversed page list
          mov     AX,SS:[BX].sql_rev ; load next page in backward chain
          ADJPAGE AX               ; convert page number to page index
sql_020:   cmp    AX,DX            ; another destination page available?
	  jne	  sql_025
          jmp     sql_done         ; if source page = destination page, jump
sql_025:
          mov     BX,AX            ; copy destination page index to BX

	  %LoadPage1 ES,BX	   ; load paragraph address of dest page
          mov     DI,SS:nextcell+[BX] ; load free cell header
IFDEF EXTMEM
	  mov	  BX,DX		   ; reload dest. page so it won't ever
	  %LoadPage0 DS,BX	   ; get swapped out
ENDIF

;     Make sure a cell is available in the destination page
sql_030:  cmp     DI,END_LIST
          je      sql_010

;     Is there a cell to move from the source page?
sql_040:   mov    BX,DX
          mov     BX,SS:psize+[BX] ; load the page size and
          sub     BX,uppercase&SIZE        ;  compute end of page boundary
sql_050:   add    SI,uppercase&SIZE        ; increment source page offset
          cmp     SI,BX            ; end of source page?
          ja      sql_070          ; if end of page, jump
          cmp     [SI].car_page,0FFh ; is this cell referenced?
          je      sql_050          ; if an unreferenced cell, jump

;     Move the cell from source page to destination page
sql_060:   mov    BX,ES:[DI].car   ; load offset of next free cell in dest page
IF        uppercase&SIZE - (uppercase&SIZE/2)*2
          mov     CX,uppercase&SIZE
rep       movsb
ELSE
          mov     CX,uppercase&SIZE/WORDINCR ; load number of words to move
rep       movsw                    ; copy the contents of the list cell
ENDIF
          sub     SI,uppercase&SIZE        ; back up the source and destination
          sub     DI,uppercase&SIZE        ;  pointers
IFIDN     <uppercase>,<LIST>
          mov     [SI].car_page,AL ; store a forwarding pointer into the car
          mov     [SI].car,DI      ;  field of the source list cell
ELSE
IFIDN     <uppercase>,<FLO>
          mov     [SI].flo_data,AL
          mov     word ptr [SI].flo_data+1,DI
ELSE
          OOPS    invalid data type: uppercase
ENDIF
ENDIF
          or      byte ptr [SI].&lowercase&_gc,GC_BIT ; set GC bit to indicate
                                                      ; forward
          mov     DI,BX            ; copy next free cell offset into DI
          jmp     sql_030          ; process next move

;     Follow forward pointer to get a next source page
sql_070:   mov    BX,DX            ; copy forward chain header to BX
          mov     DX,SS:pagelink+[BX] ; load next page in forward chain
          ADJPAGE DX               ; convert page number to page index
          mov     BX,DX

	  %LoadPage0 DS,BX	   ; load paragraph address of source page
IFDEF EXTMEM
	  mov	  BX,AX		   ; reload dest. page so it won't ever
	  %LoadPage1 ES,BX	   ; get swapped out
ENDIF

          mov     SI,-uppercase&SIZE       ; initialize source page index
          cmp     AX,DX            ; does source page = destination page?
	  je	  sql_035
          jmp     sql_040          ; if not, keep on moving cells (jump)
sql_035:
;     No more cells to move-- update destination page available cell header
          mov     BX,AX            ; update next available cell pointer
          mov     SS:nextcell+[BX],DI ;  in the destination page

; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
; * * * WARNING:  The DS Register Doesn't Point to the Data Segment * * *
; * * *           in the code above                                 * * *
; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

;     Copying complete
sql_done:   pop   DS               ; restore data segment register (DS)

sql_ret:   mov    SP,BP            ; clean up TIPC's stack
          add     SP,offset sql_BP ; deallocate local storage
          pop     BP               ; restore caller's BP
          pop     ES               ; restore caller's ES
          ret                      ; return to caller
          endm

;************************************************************************
;*                      List Cell Compaction                            *
;************************************************************************
sq_list   proc    near
          sq_L_F  LIST,list
sq_list   endp

;************************************************************************
;*                      Flonum Compaction                               *
;************************************************************************
sq_flo    proc    near
          sq_L_F  FLO,flo
sq_flo    endp

;************************************************************************
;*              Variable Length Object Compaction                       *
;*                                                                      *
;* Register usage during "move" phase of this routine:                  *
;*      AX - backward chain header (destination page index)             *
;*      BX - (scratch register)                                         *
;*      CX - word count for block move                                  *
;*      DX - forward chain header (source page index)                   *
;*      DS:[SI] - source list cell                                      *
;*      ES:[DI] - destination list cell                                 *
;*                                                                      *
;* Notes:                                                               *
;*                                                                      *
;*  1.  Any object which is less than 6 bytes in length cannot be moved *
;*      because there's no place to put a forwarding pointer.  If a     *
;*      page is encountered with such an object (e.g., a zero length    *
;*      vector) that object, and the remaining objects in that page are *
;*      not copied.  Processing continues with the next source page.    *
;*                                                                      *
;*  2.  The current code block cannot be relocated, since the offset    *
;*      into the current code block is held in register SI in most of   *
;*      the code of the Scheme Virtual Machine emulator.  Since it is   *
;*      not possible to update this offset, the page containing the     *
;*      current code block is skipped, if encountered during            *
;*      compaction.                                                     *
;************************************************************************
sq_var    proc    near
          push    ES               ; save caller's ES
          push    BP               ; save caller's BP
          sub     SP,offset sql_BP ; allocate local storage
          mov     BP,SP            ; establish local addressability

;     Create a reverse order linked list of pages
          lea     BX,[BP].sql_rev  ; load addr of reverse linked list array
          pushm   <[BP].sql_type,BX> ; push type code, array addr as arguments
          call    sq_rever         ; create the reverse linked list
          mov     SP,BP            ; drop arguments off TIPC's stack
          cmp     AX,END_LIST      ; is list of pages empty?
          jne     sqv_020          ; if list non-empty, continue (jump)
sqv_010:  jmp     sqv_ret          ; if empty list, return
sqv_020:  ADJPAGE AX               ; convert list header to page index value
          mov     [BP].sql_bptr,AX ; save destination list header

;     Move list cells from least dense pages to most dense pages
          mov     BX,[BP].sql_type ; load type index for page type
          mov     DX,pagelist+[BX] ; load page number of least dense
          ADJPAGE DX               ;  page and convert to page index
          cmp     AX,DX            ; destination page available?
          je      sqv_010          ; if source page = destination page, jump
          mov     BX,DX            ; copy page index into BX
          push    DS               ; save DS register

; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
; * * * WARNING:  The DS Register Doesn't Point to the Data Segment * * *
; * * *           in the code which follows:                        * * *
; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

          cmp     DL,byte ptr CB_pag ; does page contain current code block?
          je      sqv_052          ; if so, skip it
IFDEF EXPMEM
	  cmp	  psize+[BX],MIN_PAGESIZE  ; page size greater than minimum?
	  jne	  sqv_052
ENDIF	  
	  %LoadPage0 DS,BX	   ; load paragraph address of source page
;;;       mov     DS,pagetabl,[BX] ; load paragraph address of source page
          xor     SI,SI            ; load source page index

;     Is there an object to move from the source page?
sqv_040:  mov     BX,DX
          mov     BX,SS:psize+[BX] ; load the page size and
          sub     BX,BLK_OVHD      ;  compute end of page boundary
sqv_050:  cmp     SI,BX            ; end of source page?
          ja      sqv_052          ; if end of page, jump
          cmp     [SI].car_page,FREETYPE ; is this object referenced?
          jne     sqv_060          ; if a referenced object, jump
          add     SI,[SI].vec_len
          jmp     sqv_050

sqv_052:  jmp     sqv_070          ; process next source page

;     Find next possible destination page
sqv_054:  mov     BX,AX
          add     BX,BP
          mov     AX,SS:[BX].sql_rev
          ADJPAGE AX
          cmp     AX,DX
          jne     sqv_061
          jmp     sqv_done

;     Find a block into which to move the referenced object
sqv_060:  mov     CX,[SI].vec_len ; load length of object
          cmp     CX,0            ;;; check for small string
          jge     sqv_001
          mov     CX,BLK_OVHD+PTRSIZE ;;; get the right value
sqv_001:  cmp     CX,BLK_OVHD+PTRSIZE ; is object "too small" to relocate?
          jae     sqv001
          jmp     sqv_070         ; if "too small", abandon this page
sqv001:   mov     AX,[BP].sql_bptr ; load destination page list header
sqv_061:  mov     BX,AX           ; copy index for destination page
IFDEF EXPMEM
	  cmp	  psize+[BX],MIN_PAGESIZE  ; page size greater than minimum?
	  jne	  sqv_054
ENDIF	  
	  %LoadPage1 ES,BX	   ; load paragraph address of dest page
IFDEF EXTMEM
	  %LoadPage0 DS,DX	   ; reload src page so it won't get swapped out
ENDIF
          xor     DI,DI           ;  page and initialize its index pointer
          mov     BX,SS:psize+[BX] ; load page size and
          sub     BX,BLK_OVHD     ;   adjust for boundary check
          jmp     short sqv_064   ; jump over increment
sqv_062:  cmp     ES:[DI].vec_len,0 ;;; check for small string
          jge     sqv_002
          add     DI,BLK_OVHD+PTRSIZE ;;; add the exact length
          jmp     sqv_064
sqv_002:  add     DI,ES:[DI].vec_len ; advance destination page index
sqv_064:  cmp     DI,BX           ; end of page?
          ja      sqv_054         ; if end of page, jump
          cmp     ES:[DI].vec_type,FREETYPE ; free block?
          jne     sqv_062         ; if not a free block, keep looking (jump)
;     Free block found-- is it big enough?
          cmp     CX,ES:[DI].vec_len
          ja      sqv_062
          je      sqv_068          ; if an exact fit, jump
          sub     CX,ES:[DI].vec_len
          neg     CX
          cmp     CX,BLK_OVHD
          jge     sqv_066
          mov     CX,[SI].vec_len
          cmp     CX,0             ;;; check for small string
          jge     sqv_062
          mov     CX,BLK_OVHD+PTRSIZE  ;;; get the right value
          jmp     sqv_062
sqv_066:  cmp     [SI].vec_len,0   ;;; check for small string
          jge     sqv_003
          add     DI,BLK_OVHD+PTRSIZE  ;;; add the right value
          jmp     sqv_004
sqv_003:  add     DI,[SI].vec_len
sqv_004:  mov     ES:[DI].vec_type,FREETYPE
          mov     ES:[DI].vec_len,CX
          mov     CX,[SI].vec_len
          cmp     CX,0             ;;; check for small string
          jge     sqv_005
          mov     CX,BLK_OVHD+PTRSIZE
sqv_005:  sub     DI,CX
          
;     Move the cell from source page to destination page
sqv_068:  mov     BX,CX            ; save the number of bytes moved
rep       movsb                    ; copy object from source page to dest page
          sub     SI,BX            ; back up the source and destination
          sub     DI,BX            ;  pointers
          mov     [SI].vec_page,AL ; store a forwarding pointer into the car
          mov     [SI].vec_disp,DI ;  field of the source object
          or      byte ptr [SI].vec_gc,GC_BIT ; set GC bit to indicate forward
          add     SI,BX            ; advance source page index to next object
sqv_069:  jmp     sqv_040          ; process next move

;     Follow forward pointer to get a next source page
sqv_070:  mov     BX,DX            ; copy forward chain header to BX
          mov     DX,SS:pagelink+[BX] ; load next page in forward chain
          ADJPAGE DX               ; convert page number to page index
	  cmp	  AX,DX		   ; source = destination?	;rb for tc
	  je	  sqv_done	   ; yes, jump			;rb for tc
          cmp     DL,SS:byte ptr CB_pag ; current code block in this page?
          je      sqv_070          ; we can't relocate the current code block
          mov     BX,DX
IFDEF EXPMEM
	  cmp	  psize+[BX],MIN_PAGESIZE  ; page size greater than minimum?
	  jne	  sqv_070
ENDIF	  
	  %LoadPage0 DS,BX	   ; load paragraph address of source page
IFDEF EXTMEM
	  %LoadPage1 ES,AX	   ; reload dest page so it won't get swapped
ENDIF
          xor     SI,SI            ; initialize source page index
          cmp     AX,DX            ; does source page = destination page?
          jne     sqv_069          ; if not, keep on moving objects (jump)

; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
; * * * WARNING:  The DS Register Doesn't Point to the Data Segment * * *
; * * *           in the code above                                 * * *
; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

;     Copying complete
sqv_done: pop     DS               ; restore data segment register (DS)

sqv_ret:  mov     SP,BP            ; clean up TIPC's stack
          add     SP,offset sql_BP ; deallocate local storage
          pop     BP               ; restore caller's BP
          pop     ES               ; restore caller's ES
          ret                      ; return to caller
sq_var    endp

;************************************************************************
;*            Local Support-- Create Reverse Linked List                *

;*                                                                      *
;* Purpose:  To create a reversed copy of the similar page list for     *
;*              pages of a given type.                                  *
;*                                                                      *
;* Calling Sequence:  header = sq_rever(dest_array, type_index)         *
;*              header = header pointer of reversed list.               *
;*              dest_array = array to hold the pointers of the reversed *
;*                              linked list.                            *
;*              type_index = type index (type*2) of the page type for   *
;*                              which the similar page linked list is   *
;*                              to be reversed (e.g., LISTTYPE*2 causes *
;*                              the linked list for list cell pages to  *
;*                              be reversed.                            *
;************************************************************************
sqr_args  struc
          dw      ?                ; caller's BP
          dw      ?                ; return address
sqr_ary   dw      ?                ; pointer to reversed list array
sqr_typ   dw      ?                ; type code for desired page type
sqr_args  ends

sq_rever  proc    near
          push    BP               ; save caller's BP
          mov     BP,SP            ; establish addressability
          mov     BX,[BP].sqr_ary  ; load address of destination array
          mov     SI,[BP].sqr_typ  ; load type code for list to reverse
          mov     SI,pagelist+[SI] ; load list header to appropriate page type
          mov     AX,END_LIST      ; load an end of list indicator
sqr_loop: cmp     SI,END_LIST      ; end of list?
          je      sqr_ret          ; if end of list, return
          mov     DX,SI            ; save current page number in DX
          ADJPAGE SI               ; convert page number to page index
          mov     [BX]+[SI],AX     ; store prev page number into reversed array
          mov     SI,pagelink+[SI] ; fetch next page in linked list
          mov     AX,DX            ; prev page number <- current page number
          jmp     sqr_loop         ; continue 'til end of list
sqr_ret:  pop     BP               ; restore caller's BP
          ret                      ; return with reversed list header in AX
sq_rever  endp

PROGX     ends

prog      segment byte public 'PROG'
          assume  CS:PGROUP
;************************************************************************
;* Long Linkage to gcsquish                                             *
;*                                                                      *
;* Note:  The lines which are commented out in the following code were  *
;*        used to print the "* compacting memory *" message in the      *
;*        who-line.  Since it's a real pain in the a.. to allow the     *
;*        user to change the GC messages, it was decided that no        *
;*        message was the best way to go.                               *
;************************************************************************
          public  gcsquish
gcsquish  proc    near
          push    ES               ; save caller's ES register
          push    BP               ; save caller's BP register
          mov     BP,SP
          mov     AX,DS            ; make sure ES points to the data segment
          mov     ES,AX
          C_call  gc_on            ; light up the "garbage collecting" message
;;;       mov     AX,offset msg    ; load address of compaction message
;;;       push    AX               ;  and push as argument
;;;       C_call  who_writ         ; display "compacting memory" message
;;;       mov     SP,BP            ; drop argument from stack
          call    %squish          ; perform memory compaction
          C_call  gc_off           ; reset the garbage collection message
          pop     BP               ; restore caller's BP
          pop     ES               ; restore caller's ES
          ret                      ; return to caller
gcsquish  endp

prog      ends
          end
