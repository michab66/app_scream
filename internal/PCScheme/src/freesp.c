/*							=====> FREESP.C   */
/*     TIPC Scheme '84  -- Freespace Utility
     (C) Copyright 1984,1985 by Texas Instruments Incorporated.
     All rights reserved.

     Author:  Terry Caudill
     Date Written:  07 August 1985
     Last Modification:
*/

#include "ctype.h"
#include "scheme.h"

/**********************************************************************/
/*		TIPC Scheme '84 Free Space                            */
/*								      */
/* Purpose:  This Routine will return the number of bytes of free     */
/*	     user memory.					      */
/**********************************************************************/
long int freesp()
 {
  int space[NUMPAGES];		 /* Free memory per page array */
  int i;			 /* index var */
  long int bytes_free;		 /* word to sum bytes available */

   ENTER(freesp);

  sum_space(space);
  bytes_free = 0;

  for (i = DEDPAGES; i < lastpage; i++)
    if (ptype[i] == FREETYPE*2)
      bytes_free = bytes_free + psize[i];
    else
      bytes_free = bytes_free + space[i];

 return (bytes_free);
 }
