/*							=====> SHASH.C       */
/* TIPC Scheme '84 Runtime Support - Symbol Support
   (C) Copyright 1984,1985,1986 by Texas Instruments Incorporated.
   All rights reserved.

   Author:  John C. Jensen
   Installation:  Texas Instruments Incorporated, Dallas, Texas
   Division:  Central Research Laboratories
   Cost Center:  Computer Science Laboratory
   Project:  Computer Architecture Branch
   Date Written:  5 June 1984
   Last Modification:  10 Feb 1987

   Modification History:
	10 Feb 87 - Modified the intern routine to recognize the special
	  (JCJ/TC)  constants #T and #F as ``true'' and ``false,''
		    respectively.  Also caused #!TRUE to be interned.
		    In a previous "fix", I caused #!EOF to be interned,
		    and this change reverses that decision.  When #!EOF is
		    interned, the READ procedure aborts every time it is
		    read.
*/
#include "ctype.h"
#include "scheme.h"

intern(reg, string, length)
int reg[2];                     /* "register" to return symbol's pointer */
char *string;                   /* characters comprizing symbol */
int  length;                    /* number of characters in the symbol */
 {
  int disp;                     /* displacement of the symbol's entry */
  int equal;                    /* equality indicator */
  int hash_value;               /* value returned from hashing function */
  int i,j;                      /* our old favorite index variables */
  int page;                     /* page number of the symbol's entry */
  char *ptr;			/* pointer to special constant name */

#define NUM_SPEC 6
  static char *special_constants[NUM_SPEC] =
    {"#T", "#F", "#!FALSE", "#!NULL", "#!TRUE", "#!UNASSIGNED"};
  static int spec_len[NUM_SPEC] =
    {2, 2, 7, 6, 6, 12};
  static int spec_page[NUM_SPEC] =
    {T_PAGE*2, NIL_PAGE*2, NIL_PAGE*2, NIL_PAGE*2, T_PAGE*2, UN_PAGE*2};
  static int spec_disp[NUM_SPEC] =
    {T_DISP, NIL_DISP, NIL_DISP, NIL_DISP, T_DISP, UN_DISP};

  if (string[0] == '#')
   {
    for (i=0; i<NUM_SPEC; i++)
     {
      if (length == spec_len[i])
       {
	for (j=0, ptr=special_constant[i]; j<length; j++)
	  if (string[j] != *ptr++) goto no_match;
	reg[C_PAGE] = spec_page[i];
	reg[C_DISP] = spec_disp[i];
	goto routine_exit;
       } /* end:  if (length == spec_len[i]) */
no_match:
     } /* end:  for (i=0; i<NUM_SPEC; i++) */
   } /* end:  if (string[0] == '#') */
  hash_value = hash(string, length);
  if (hash_page[hash_value] != 0)
   {
    page = CORRPAGE(hash_page[hash_value]);
    disp = hash_disp[hash_value];
    while (page != 0)
     {
      if (sym_eq(page, disp, string, length))
       {
        reg[C_PAGE] = ADJPAGE(page);
        reg[C_DISP] = disp;
        goto routine_exit;
       }
      /* Follow hash chain link pointer to next symbol */
      i = CORRPAGE(get_byte(page,disp+3));
      disp = get_word(page,disp+4);
      page = i;
     } /* end:  while (page != 0) */
    /* if loop exits, symbol not found-- add to oblist */
   }

  /* add symbol to oblist */
  alloc_sym(reg, length);
  page = CORRPAGE(reg[C_PAGE]);
  disp = reg[C_DISP];
  put_sym(string, page, disp, hash_page[hash_value], hash_disp[hash_value],
          hash_value);
  hash_page[hash_value] = ADJPAGE(page);
  hash_disp[hash_value] = disp;

routine_exit:
 } /* end of function:  intern(reg, string, length) */

/************************************************************************/
/*                       Hashing Function                               */
/************************************************************************/
/***** 
hash(sym, len)
char *sym;                         /* symbol to be "hashed" */
int len;                           /* number of characters in "sym" */
 {
  unsigned acc = 0;
  int i;
  for (i = 0; i < len; i++) acc += sym[i];
  return (acc % HT_SIZE);
 } /* end of function:  hash(sym, len) */
*****/
