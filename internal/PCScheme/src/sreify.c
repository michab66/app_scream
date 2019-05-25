/*							=====> SREIFY.C     */
/* TIPC Scheme '84 Runtime Support - Reification
   (C) Copyright 1985,1986 by Texas Instruments Incorporated.
   All rights reserved.

   Author:  John C. Jensen
   Installation:  Texas Instruments Incorporated, Dallas, Texas
   Division:  Central Research Laboratories
   Cost Center:  Computer Science Laboratory
   Project:  Computer Architecture Branch
   Date Written:  5 March 1985
   Last Modification:  25 February 1986
*/
#include "scheme.h"

/************************************************************************/
/*			    Reification Support 			*/
/************************************************************************/
reify (direction, obj, index, val)
int direction;		/* 0=fetch; 1=store */
int obj[2];		/* pointer to object to "reify" */
int index[2];		/* index into said object */
int val[2];		/* for stores, the value to store */
 {
  int disp,page;	/* page and displacement components of a pointer */
  int i;		/* index value */

  ENTER(reify);

  if (index[C_PAGE] == SPECFIX*2)
   {
    if ((i = ((get_fix(CORRPAGE(index[C_PAGE]), index[C_DISP]) + 1) * 3)) < 0)
	goto bad_opnd;
    page = CORRPAGE(obj[C_PAGE]);
    disp = obj[C_DISP];
    switch (ptype[page]>>1)
     {
      case LISTTYPE:  if (i) goto bad_opnd;
		      obj[C_DISP] = PTRSIZE*2;
return_fixnum:
		      obj[C_PAGE] = SPECFIX*2;
		      break;

      case REFTYPE:
      case FIXTYPE:
      case CHARTYPE:  if (i) goto bad_opnd;
		      obj[C_DISP] = PTRSIZE;
		      goto return_fixnum;

      case FLOTYPE:   if (i) goto bad_opnd;
		      obj[C_DISP] = FLOSIZE;
		      goto return_fixnum;

      case PORTTYPE:
      case SYMTYPE:  if (i > 3) goto bad_opnd;
		     goto reify_anyway;

      case CODETYPE: if (i >= get_word(page, disp+4)) goto bad_opnd;
		     goto reify_anyway;

      case BIGTYPE:
      case STRTYPE:  if (i) goto bad_opnd;

      case VECTTYPE:
      case CONTTYPE:
      case CLOSTYPE:
      case ENVTYPE:
		      if (i >= get_word(page, disp+1)) goto bad_opnd;
reify_anyway:
		      if (direction)
		       {
			if (i)
			 {
			  put_ptr(page, disp+i, val[C_PAGE], val[C_DISP]);
			 }
			else goto bad_opnd;
		       }
		      else
		       {
			obj[C_PAGE] = (i ? get_byte(page, disp+i) : SPECFIX*2);
			obj[C_DISP] = get_word(page, disp+i+1);
		       }
		      break;

      default:	goto bad_opnd;

     } /* end:	switch (ptype[page]>>1) */
   }
  else
   {
bad_opnd:
    if (direction) set_src_er("%REIFY!", 3, obj, index, val);
    else set_src_er("%REIFY", 2, obj, index);
    return(-1);
   }
  return(0);
 } /* end of function:	reify (direction, obj, index, val) */
