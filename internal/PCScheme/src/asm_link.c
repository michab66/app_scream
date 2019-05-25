/*							=====> ASM_LINK.C    */
/* TIPC Scheme '84 Runtime Support - Linkage to non-Scheme Routines
   (C) Copyright 1984,1985 by Texas Instruments Incorporated.
   All rights reserved.

   Author:  John C. Jensen
   Installation:  Texas Instruments Incorporated, Dallas, Texas
   Division:  Central Research Laboratories
   Cost Center:  Computer Science Laboratory
   Project:  Computer Architecture Branch
   Date Written:  20 June 1985
   Last Modification:  18 October 1985
*/

#include "scheme.h"

asm_link(n_args, arg1, arg2, arg3, arg4, arg5, arg6, arg7)
int n_args;		/* the number of actual arguments */
int arg1[2],arg2[2],arg3[2],arg4[2],arg5[2],arg6[2],arg7[2];
 {
  int i;		/* the usual index variable */
  int page,disp;	/* page and displacement components of a pointer */
  long *p[7];		/* pointers to arguments */
  int **ptr;		/* pointer to current argument register address */
  double result_value;	/* return area for result */
  double *result;	/* pointer to result returned */
  int return_code = 0;	/* return_code to send back to Scheme support */
  int stat;		/* status returned from called procedure */
  int str_len[7];	/* length of dynamically allocated strings */

  long integers[7];
  int chars[7];
  double flonums[7];
  char *strings[7];

  char *string_asciz();
  double get_flo();

  ENTER(asm_link);

  /* null out string pointers */
  for (i = 0; i < 7; i++)
   {
    p[i] = NULL;
    strings[i] = NULL;
    str_len[i] = 0;
   }

  /* create a C version of each argument */
  result = &result_value;
  ptr = &arg1;
  for (i = n_args-1; i >= 0; i--, ptr++)
   {
    page = CORRPAGE((*ptr)[C_PAGE]);
    disp = (*ptr)[C_DISP];
    switch (ptype[page])
     {
      case STRTYPE*2:  strings[i] = string_asciz(*ptr);
		       str_len[i] = get_word(page,disp+1);
		       if (str_len[i] < 0)	/* Adjust for small strings */
			str_len[i] = str_len[i] + BLK_OVHD + 1;
		       else
			str_len[i] = str_len[i] - BLK_OVHD + 1;
		       p[i] = (long *) strings[i];
		       break;

      case BIGTYPE*2:
      case FIXTYPE*2:  if (int2long(integers+i, *ptr))
			 goto bad_arg;
		       p[i] = (long *) (integers+i);
		       break;

      case FLOTYPE*2:  flonums[i] = get_flo(page, disp);
		       p[i] = (long *) (flonums+i);
		       break;

      case CHARTYPE*2:	chars[i] = get_char(page,disp);
			p[i] = (long *) (chars+i);
			break;
bad_arg:
      default:	return(1);
     } /* end:	switch (ptype[page]) */
   } /* end:  for (i = 0; i < n_args; i++, ptr++) */

  /* all arguments ready-- call the interface routine */
  stat = link(&result,p[0],p[1],p[2],p[3],p[4],p[5],p[6]);

  /* fetch result returned from low level return and make it a Scheme object */
  ptr = ((int **) &arg1) + (n_args - 1);
  switch (stat)
   {
    case 1:  /* 't or 'nil */
	     if (*((int *) result))
	      {
	       (*ptr)[C_PAGE] = T_PAGE*2;
	       (*ptr)[C_DISP] = T_DISP;
	      }
	     else
	      {
	       (*ptr)[C_PAGE] = (*ptr)[C_DISP] = 0;
	      }
    case 0:  /* no value returned */
	     break;

    case 2:  /* integer */
	     long2int(*ptr, *((long *) result));
	     break;

    case 3:  /* flonum */
	     alloc_flonum(*ptr, *result);
	     break;

    case 4:  /* character */
	     (*ptr)[C_PAGE] = SPECCHAR*2;
	     (*ptr)[C_DISP] = *((char *) result);
	     break;

    case 5:  /* string */
	     alloc_string(*ptr, result);
	     break;

    default:  /* error */
	      return_code = 1;
   } /* end:  switch (stat) */

  /* release memory allocated to character strings */
  for (i = 0; i < 7; i++)
   {
    if (strings[i])
      if (rlsmem(strings[i], str_len[i]))
	rlsmem_error(rtn_name);
   }

  return(return_code);
 } /* end of function:	asm_link(n_args, arg1, arg2, ... , arg7) */
