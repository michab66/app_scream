;							=====> SAPROP.ASM
;***************************************
;*   TIPC Scheme '84 Runtime Support   *
;*	  Property List Support	       *
;*				       *
;*        (C) Copyright 1986 by        *
;*   Texas Instruments Incorporated.   *
;*	  All rights reserved.	       *
;*				       *
;* Date Written:  7 May 1986	       *
;* Last Modification:  11 May 1986     *
;***************************************
	  include scheme.equ

DGROUP	  group	  data
data	  segment word public 'DATA'
	  assume  DS:DGROUP
data	  ends

PGROUP	  group	  prog
prog	  segment byte public 'PROG'
	  assume  CS:PGROUP

;************************************************************************
;*		Search for Property in Property List			*
;*									*
;* Calling Sequence:  found? = prop_search(list,prop);			*
;*									*
;* Input Parameters:  list - the property list for a symbol.		*
;*		      prop - the property for which to search.		*
;* 									*
;* Output Parameters:  found? - if the property was found in the list,  *
;*			found?=1; else found?=0.			*
;*		       list - a pointer to the property/value pair	*
;*			for the specified property.  If not found, NIL. *
;*									*
;* Note:  This routine is an assembly language version of the following *
;*		C source:						*
;* prop_search(list, prop)						*
;* int list[2],prop[2];							*
;*  {									*
;*   int search[2];	/* current search entry in list */		*
;*   int temp[2];		/* temporary "register" */		*
;*   ENTER(prop_search);						*
;* 									*
;*   mov_reg(search, list);						*
;*   take_cdr(search);							*
;*   while(search[C_PAGE])						*
;*    {									*
;*     mov_reg(temp, search);						*
;*     take_car(temp);							*
;*     if (eq(temp,prop))						*
;*      {								*
;*       mov_reg(list, search);						*
;*       return(FOUND);							*
;*      }								*
;*     take_cddr(search);						*
;*    } /* end:  while(search[C_PAGE]) */				*
;*   return(NOT_FOUND);							*
;*  } /* end of function:  prop_search(list, prop) */			*
;************************************************************************
p_arg	  struc
	  dw	  ?		   ; caller's BP
	  dw	  ?		   ; caller's ES
	  dw	  ?		   ; return address
p_list	  dw	  ?		   ; addr of reg containing list to search
p_prop	  dw	  ?		   ; the property for which we're searching
p_arg	  ends

	  public  prop_sea
prop_sea  proc	  near
	  push	  ES		   ; save caller's ES register
	  push	  BP		   ; save caller's BP register
	  mov	  BP,SP		   ; establish addressability
;     Load up the property for which we're searching into CL:DX
	  mov	  BX,[BP].p_prop
	  mov	  CL,byte ptr [BX].C_page
	  mov	  DX,[BX].C_disp
;     Load up a pointer to the beginning of the property list
	  mov	  SI,[BP].p_list
	  xor	  BX,BX
	  mov	  BL,byte ptr [SI].C_page
	  mov	  DI,[SI].C_disp
	  jmp	  short start
;     Property didn't match-- keep searching list
no_match: mov	  BL,ES:[DI].cdr_page
	  mov	  DI,ES:[DI].cdr
;     Take CDR to get to first property/value pair or to follow list
start:	  cmp	  BL,0
	  je	  p_nf
	  cmp	  byte ptr ptype+[BX],LISTTYPE*2
	  jne	  p_nf
	  LoadPage ES,BX
;;;	  mov	  ES,pagetabl+[BX]
	  mov	  BL,ES:[DI].cdr_page
	  mov	  DI,ES:[DI].cdr
;     Test for valid list cell
	  cmp	  BL,0
	  je	  p_nf
	  cmp	  byte ptr ptype+[BX],LISTTYPE*2
	  jne	  p_nf
	  LoadPage ES,BX
;;;	  mov	  ES,pagetabl+[BX]
	  cmp	  DX,ES:[DI].car
	  jne	  no_match
	  cmp	  CL,ES:[DI].car_page
	  jne	  no_match
;     Property found!-- return pointer to it
	  mov	  byte ptr [SI].C_page,BL ; move pointer to property entry
	  mov	  [SI].C_disp,DI   ; into the "list" operand register
	  pop	  BP		   ; restore caller's BP register
	  pop	  ES		   ; restore caller's ES register
	  mov	  AX,1		   ; indicate property found
	  ret			   ; return
;     End of property list-- return not found
p_nf:	  xor	  AX,AX		   ; indicate no match found
	  pop	  BP		   ; restore caller's BP register
	  pop	  ES		   ; restore caller's ES register
	  ret			   ; return
prop_sea  endp

;************************************************************************
;*		Search for Symbol in Property List			*
;*									*
;* Calling Sequence:  sym_search(sym)					*
;*									*
;* Input Parameters:  sym - a register containing a symbol who's	*
;*			property list is to be located.			*
;* 									*
;* Output Parameters:  sym - the register is updated to point to the	*
;*			property list for the symbol.  If no property	*
;*			list exists, it is set to NIL.			*
;*									*
;* Note:  This routine is an assembly language version of the following *
;*		C source:						*
;* sym_search(sym)							*
;* int sym[2];								*
;*  {									*
;*   int hash_value;		/* symbol's hash value */		*
;*   int sym_save[2];		/* initial value of symbol argument */	*
;*   int temp[2];			/* temporary "register" */	*
;*   ENTER(sym_search);							*
;* 									*
;*   if (ptype[CORRPAGE(sym[C_PAGE])] == SYMTYPE*2)			*
;*    {									*
;*     /* save symbol's page and displacement for testing purposes */	*
;*     mov_reg(sym_save, sym);						*
;* 									*
;*     /* obtain hash chain to search */				*
;*     hash_value = sym_hash(sym);					*
;*     sym[C_PAGE] = prop_page[hash_value];				*
;*     sym[C_DISP] = prop_disp[hash_value];				*
;* 									*
;*     while(sym[C_PAGE])						*
;*      {								*
;*       mov_reg(temp, sym);						*
;*       take_caar(temp);						*
;*       if (eq(temp, sym_save))					*
;*        {								*
;* 	   /* symbol found-- return pointer to symbol's property list */*
;* 	   take_car(sym);						*
;* 	   break;							*
;*        }								*
;*       else								*
;*        {								*
;* 	take_cdr(sym);							*
;*        }								*
;*      } /* end:  while(sym[C_PAGE]) */				*
;*   }									*
;* } /* end of function:  sym_search(sym) */				*
;*									*
;************************************************************************
sym_args  struc
	  dw	  ?		   ; caller's ES register
	  dw	  ?		   ; caller's BP register
	  dw	  ?		   ; return address
s_sym	  dw	  ?		   ; address of symbol/result register
sym_args  ends

	  public  sym_sear
sym_sear  proc	  near
	  push	  BP		   ; save the caller's BP register
	  push	  ES		   ; save the caller's ES register
	  mov	  BP,SP		   ; establish addressability
;     Load a pointer to the symbol and get its hash value
	  mov	  SI,[BP].s_sym    ; load symbol register's address
	  mov	  BX,[SI].C_page   ; load symbol's page number
	  cmp	  byte ptr ptype+[BX],SYMTYPE*2 ; it is a symbol, isn't it?
	  jne	  s_nf		   ; if not a symbol, return NIL
	  mov	  SI,[SI].C_disp   ; load symbol's displacement and
	  LoadPage ES,BX
;;;	  mov	  ES,pagetabl+[BX] ;  paragraph address
	  mov	  CX,BX		   ; copy the symbol into CL:DX
	  mov	  DX,SI
	  mov	  BL,ES:[SI].sym_hkey ; load hash key
	  mov	  DI,BX		   ; copy hash key into DI and
	  shl	  DI,1		   ;  multiply by two for word index
	  mov	  BL,prop_pag+[BX] ; load property list header for this
	  mov	  DI,prop_dis+[DI] ;  symbol's bucket
	  jmp	  short go
;     Search the next entry in the bucket
s_next:	  mov	  BX,AX
	  LoadPage ES,BX
;;;	  mov	  ES,AX		   ; restore ES register for bucket entry
s_next1:  mov	  BL,ES:[DI].cdr_page ; load pointer to next bucket entry from
	  mov	  DI,ES:[DI].cdr   ;  the CDR field
go:	  cmp	  BL,0		   ; end of bucket?
	  je	  s_nf		   ; if so, jump
	  cmp	  byte ptr ptype+[BX],LISTTYPE*2 ; list cell?
	  jne	  s_nf		   ; if not a pair (?), jump
	  LoadPage ES,BX
	  mov	  AX,BX		   ; Save Bucket entry page number
;;;	  mov	  ES,pagetabl+[BX] ; load list cell's paragraph address
;     Fetch the property list from the CAR field of the bucket entry
	  mov	  BL,ES:[DI].car_page
	  mov	  SI,ES:[DI].car
	  cmp	  BL,0		   ; no property list for this bucket entry?
	  je	  s_next1	   ; if not (?), ignore it
	  cmp	  byte ptr ptype+[BX],LISTTYPE*2 ; it is a pair, isn't it?
	  jne	  s_next1	   ; if not (?), ignore it
;;;	  mov	  AX,ES		   ; save ES register for bucket entry
	  LoadPage ES,BX
;;;	  mov	  ES,pagetabl+[BX] ; load the paragraph addr of prop list entry
	  cmp	  DX,ES:[SI].car   ; entry for our symbol?
	  jne	  s_next	   ; if not, jump
	  cmp	  CL,ES:[SI].car_page ; entry for our symbol?
	  jne	  s_next	   ; if not, jump
;     Symbol's property list found-- return in symbol register (or return NIL)
	  mov	  DI,[BP].s_sym	   ; reload source/destination register address
	  mov	  byte ptr [DI].C_page,BL ; store prop list pointer into
	  mov	  [DI].C_disp,SI   ;  the register
	  pop	  ES		   ; restore the caller's ES register
	  pop	  BP		   ; restore the caller's BP register
	  ret			   ; return
;     Invalid list structure-- return NIL
s_nf:     xor	  AX,AX		   ; create a NIL pointer
	  mov	  DI,[BP].s_sym
	  mov	  byte ptr [DI].C_page,AL
	  mov	  [DI].C_disp,AX
	  pop	  ES
	  pop	  BP
	  ret
sym_sear  endp

prog	  ends
	  end
