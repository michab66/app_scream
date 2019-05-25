/*							=====> SBIGMEM.C     */
/* TIPC Scheme '84 Runtime Support - Memory Allocation Routines
   (C) Copyright 1984, 1985 by Texas Instruments Incorporated.
   All rights reserved.

   Author:  John C. Jensen
   Installation:  Texas Instruments Incorporated, Dallas, Texas
   Division:  Central Research Laboratories
   Cost Center:  Computer Science Laboratory
   Project:  Computer Architecture Branch
   Date Written:  3 December 1984
   Last Modification:
	4 June 1986 (TC) - Turn off assertion processing for better
			   performance.
*/
#include "scheme.h"

/* Turn off assertions in SBIGMEM.C for performance reasons */
#define ASSERT(arg) /* do nothing */
#define ENTER(xyz)  /* do nothing */

extern unsigned first_page; /* paragraph address of first physical page */

/************************************************************************/
/*		Allocate a Large BLock in Scheme's Memory               */
/************************************************************************/
alloc_big_block(reg, type, size)
int reg[2];		/* register to receive pointer to allocated block */
int type;		/* type code of block to be allocated */
unsigned size;		/* size of block in bytes (including block header) */
 {
  int number_of_pages;	/* number of pages required to satisy request */
  int page;		/* page number where large block can be allocated */

#ifdef PROMEM
  if (PAGESIZE == MAX_PAGESIZE)
   number_of_pages = 1;
  else
#endif
   number_of_pages = ((size + (PAGESIZE - 1)) / PAGESIZE);
  if ((page = find_big_block(number_of_pages)) == -1)
   {
    garbage();	/* Invoke Garbage Collector to reclaim memory */
    if ((page = find_big_block(number_of_pages)) == -1)
     {
      gcsquish();  /* Try compacting memory */
      if ((page = find_big_block(number_of_pages)) == -1)
       {
	out_of_memory();		/* attempt to execute SCHEME-RESET */
	/* Note:  The above procedure returns control elsewhere */
       }
     }
   }
  zero_page(page);
  put_ptr(page, 0, type, size);
  nextcell[page] = END_LIST;
  if (size <= psize[page] - BLK_OVHD)
   {
    put_ptr(page, size, FREETYPE, psize[page] - size);
    nextcell[page] = size;
   }
  ptype[page] = type + type;
  w_attrib[page] = pageattr[type];
  pagelink[page] = pagelist[type];
  pagelist[type] = page;

  reg[C_PAGE] = ADJPAGE(page);
  reg[C_DISP] = 0;
 } /* end of function:	alloc_big_block(reg, type, size) */


find_big_block(number_of_pages)
int number_of_pages;	/* number of contiguous pages required */
 {
  int free_pages[NUMPAGES+1]; /* free page table */
  int i,j;		/* our old favorite index variables */
  int page;		/* working page number */
  int page_found = -1;	/* page number of first page in big block */

ENTER(find_big_block);

  /* Initialize free_pages table */
  for (i = 0; i <= NUMPAGES; i++)
    free_pages[i] = -1;

  /* Record the number of all free pages */
  page = freepage;
  while (page != END_LIST)
   {
ASSERT(page >= 0 && page < NUMPAGES /* subchk? */);
ASSERT(free_pages[page] == -1 /* infinite loop? */);
    free_pages[page] = page;
    page = pagelink[page];
   }

  /* Search for a contiguous group of pages to satisfy request */
  for (i = 0; i < NUMPAGES - DEDPAGES - number_of_pages; i++)
   {
    if (free_pages[i] != -1)
     {
      j = 1;
      while (free_pages[i+j] != -1)
       {
	j++;
	if (j >= number_of_pages)
	 {
	  /* required number of pages found-- adjust page table */
	  page_found = free_pages[i];
	  #ifdef PROMEM
	  if (PAGESIZE != MAX_PAGESIZE)		/* pro version test */
	  #endif
	    psize[page_found] = PAGESIZE * number_of_pages;
	  free_pages[i] = -1;

	  for (j = 1; j < number_of_pages; j++)
	   {
	    page = free_pages[i+j];
	    psize[page] = 0;
	    attrib[page].nomemory = 1;
	    free_pages[i+j] = -1;
	   }

	  /* update list of free pages */
	  freepage = END_LIST;
	  for (i = NUMPAGES; i > DEDPAGES; i--)
	   {
	    if (free_pages[i] != -1)
	     {
	      pagelink[(j = free_pages[i])] = freepage;
	      freepage = j;
	     }
	   }
	  return(page_found);
	 }
       }
      i += j;
     }
   }
  return(-1);		/* no pages found */
 } /* end of function:	find_big_block(number_of_pages) */
