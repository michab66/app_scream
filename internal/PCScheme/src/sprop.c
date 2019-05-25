/* TIPC Scheme Runtime Support - Property List Support
   (C) Copyright 1985,1986 by Texas Instruments Incorporated.
   All Rights Reserved.

   Author:  John C. Jensen
   Installation:  Texas Instruments Incorporated, Dallas, Texas
   Division:  Central Research Laboratories
   Cost Center:  Computer Science Laboratory
   Project:  Computer Architecture Branch
   Date Written:  27 March 1985
   Last Modification:  25 February 1986

   Note:  The property list structure has the following representation:

		    +-----------+   +-----------+   +-----------+
		    | sym |   o-|-->|prop |   o-|-->| val |   o-|--> etc.
		    +-----------+   +-----------+   +-----------+
   +------------+      ^
   |		|      |	       +--> next symbol's entry
   |  Property	|      |	       |
   | List Hash	|   +-----------+   +-----------+
   |   Table	|-->|	  |   o-|-->|	  |   o-|--> next entry in hash chain
   |		|   +-----------+   +-----------+
   +------------+
*/
#include "ctype.h"
#include "scheme.h"

#include "slist.h"

#define FOUND 1
#define NOT_FOUND 0

/************************************************************************/
/*			 Get Property Value				*/
/************************************************************************/
get_prop(sym,prop)
int sym[2];
int prop[2];
 {
  sym_search(sym);
  if (prop_search(sym,prop) == FOUND)
   {
    take_cadr(sym);
   }
  else
   { /* property (or symbol) not found-- return nil */
    sym[C_PAGE] = sym[C_DISP] = 0;
   }
 } /* end of function:	get_prop(sym,prop) */

/************************************************************************/
/*			 Get Property List				*/
/************************************************************************/
prop_list(name)
int name[2];
 {
  int retstat = 0;		/* the return status */

  if (ptype[CORRPAGE(name[C_PAGE])] == SYMTYPE*2)
   {
    sym_search(name);
    take_cdr(name);
   }
  else
   {
    set_src_err("PROPLIST", 1, name);
    retstat = -1;
   }
  return(retstat);
 } /* end of function:	prop_list(name) */

/************************************************************************/
/*			 Put Property Value				*/
/************************************************************************/
put_prop(name, value, prop)
int name[2];
int value[2];
int prop[2];
 {
  int hash_value;		/* hash key for the symbol */
  ENTER(put_prop);

  mov_reg(tmp_reg, name);
  if (ptype[CORRPAGE(name[C_PAGE])] == SYMTYPE*2)
   {
    sym_search(tmp_reg);
    if(tmp_reg[C_PAGE])
     { /* symbol found in property list table */
      if (prop_search(tmp_reg, prop) == FOUND)
       {
	take_cdr(tmp_reg);
	put_ptr(CORRPAGE(tmp_reg[C_PAGE]), tmp_reg[C_DISP]  , value[C_PAGE],
		 value[C_DISP]);
       }
      else
       { /* property not present in symbol's property list */
	mov_reg(name, tmp_reg);
	take_cdr(name);
	cons(name, value, name);
	cons(name, prop, name);
	put_ptr(CORRPAGE(tmp_reg[C_PAGE]), tmp_reg[C_DISP]+3, name[C_PAGE],
		 name[C_DISP]);
       }
     }
    else
     { /* symbol wasn't found in property list table */
      cons(tmp_reg, value, nil_reg);
      cons(tmp_reg, prop, tmp_reg);
      cons(tmp_reg, name, tmp_reg);
      hash_value = sym_hash(name);
      name[C_PAGE] = prop_page[hash_value];
      name[C_DISP] = prop_disp[hash_value];
      cons(tmp_reg, tmp_reg, name);
      prop_page[hash_value] = tmp_reg[C_PAGE];
      prop_disp[hash_value] = tmp_reg[C_DISP];
     }
    name[C_PAGE] = value[C_PAGE];
    name[C_DISP] = value[C_DISP];
   }
  else /* name operand is not a symbol */
   {
    set_src_err("PUTPROP", 3, name, value, prop);
    return(-1);
   }
  return(0);
 } /* end of function:	put_prop(name, value, prop) */


/************************************************************************/
/*			   Remove Property				*/
/************************************************************************/
rem_prop(sym, prop)
int sym[2];
int prop[2];
 {
  int search[2];
  int temp[2];
  ENTER(rem_prop);

  sym_search(sym);
  if(sym[C_PAGE])
   {
    mov_reg(search,sym);
    while (search[C_PAGE])
     {
      mov_reg(temp,search);
      take_cadr(temp);
      if (eq(temp,prop))
       {
	mov_reg(temp,search);
	take_cddr(temp);
	take_cdr(temp);
	put_ptr(CORRPAGE(search[C_PAGE]), search[C_DISP]+3, temp[C_PAGE],
		 temp[C_DISP]);
	break;
       }
      else
       {
	take_cddr(search);
       }
     } /* end:	while (search[C_PAGE]) */
   } /* end:  if(sym[C_PAGE]) */
 } /* end of function:	rem_prop(sym, prop) */




/************************************************************************/
/*			Symbol List Search				*/
/************************************************************************/

/********* Code rewritten in assembly language on 5/22/86 by JCJ ******

sym_search(sym)
int sym[2];
 {
  int hash_value;		/* symbol's hash value */
  int sym_save[2];		/* initial value of symbol argument */
  int temp[2];			/* temporary "register" */
  ENTER(sym_search);

  if (ptype[CORRPAGE(sym[C_PAGE])] == SYMTYPE*2)
   {
    /* save symbol's page and displacement for testing purposes */
    mov_reg(sym_save, sym);

    /* obtain hash chain to search */
    hash_value = sym_hash(sym);
    sym[C_PAGE] = prop_page[hash_value];
    sym[C_DISP] = prop_disp[hash_value];

    while(sym[C_PAGE])
     {
      mov_reg(temp, sym);
      take_caar(temp);
      if (eq(temp, sym_save))
       {
	/* symbol found-- return pointer to symbol's property list */
	take_car(sym);
	break;
       }
      else
       {
	take_cdr(sym);
       }
     } /* end:	while(sym[C_PAGE]) */
   }
 } /* end of function:	sym_search(sym) */

***** Code rewritten in assembly language on 5/22/86 by JCJ ******/



/************************************************************************/
/*			Search For a Given Property			*/
/************************************************************************/

/********* Code rewritten in assembly language on 5/22/86 by JCJ ******

prop_search(list, prop)
int list[2],prop[2];
 {
  int search[2];	/* current search entry in list */
  int temp[2];		/* temporary "register" */
  ENTER(prop_search);

  mov_reg(search, list);
  take_cdr(search);
  while(search[C_PAGE])
   {
    mov_reg(temp, search);
    take_car(temp);
    if (eq(temp,prop))
     {
      mov_reg(list, search);
      return(FOUND);
     }
    take_cddr(search);
   } /* end:  while(search[C_PAGE]) */
  return(NOT_FOUND);
 } /* end of function:	prop_search(list, prop) */

***** Code rewritten in assembly language on 5/22/86 by JCJ ******/


/************************************************************************/
/*		    Dump Contents of Property List			*/
/************************************************************************/
/***** Code turned off 22 OCT 85 (JCJ) *****
dump_prop()
 {
  int ent[2];		/* current property list entry */
  int hash_value;	/* current hash key value */
  int prop[2];		/* a property pointer */
  int temp[2];		/* temporary "register" */
  int sym[2];		/* pointer to a symbol whose prop list we're dumping */
  char *symbol; 	/* a symbol's print name */
  int val[2];		/* a value pointer */

  char *symbol_name();	/* retrieves a symbol's print name */

  ENTER(dump_prop);

  for (hash_value = 0; hash_value < HT_SIZE; hash_value++)
   {
    ent[C_PAGE] = prop_page[hash_value];
    ent[C_DISP] = prop_disp[hash_value];
    while (ent[C_PAGE])
     {
      ASSERT(ptype[CORRPAGE(ent[C_PAGE])] == LISTTYPE*2);
      mov_reg(temp, ent);
      take_car(temp);
      ASSERT(ptype[CORRPAGE(temp[C_PAGE])] == LISTTYPE*2);
      mov_reg(sym,temp);
      take_car(sym);
      ASSERT(ptype[CORRPAGE(sym[C_PAGE])] == SYMTYPE*2);
      symbol = symbol_name(CORRPAGE(sym[C_PAGE]),sym[C_DISP]);
      printf("\nProperty List for |%s|\n", symbol);
      rlsstr(symbol);

      take_cdr(temp);
      while(temp[C_PAGE])
       {
	ASSERT(ptype[CORRPAGE(temp[C_PAGE])] == LISTTYPE*2);
	mov_reg(prop,temp);
	take_car(prop);
	printf("  property: ");
	annotate(CORRPAGE(prop[C_PAGE]), prop[C_DISP]);
	take_cdr(temp);
	ASSERT(ptype[CORRPAGE(temp[C_PAGE])] == LISTTYPE*2);
	mov_reg(val,temp);
	take_car(val);
	printf("     value: ");
	annotate(CORRPAGE(val[C_PAGE]), val[C_DISP]);
	take_cdr(temp);
       } /* end:  while(temp[C_PAGE]) */
      take_cdr(ent);
     } /* end:	while (ent[C_PAGE]) */
   } /* end:  for (hash_value = 0; hash_value < HT_SIZE; hash_value++) */
 }
***** Code turned off 22 OCT 85 (JCJ) *****/
