/*							=====> SBIGMXT.C     */
/* TIPC Scheme '84 Runtime Support - Memory Allocation Routines
   (C) Copyright 1984, 1985 by Texas Instruments Incorporated.
   All rights reserved.

   Author:  Terry Caudill
   Installation:  Texas Instruments Incorporated, Austin, Texas
   Division:  Data Systems Group
   Project:  PC Scheme
   Comments: This code was lifted from SBIGMEM.C, which was used to
	     allocate objects greater than on page in size. It was
	     modified to understand extended memory and is tied closely
	     to EXTSMMU.ASM
   Date Written:  4 June 86

*/
#include "scheme.h"

/* The following are specified in EXTSMMU.ASM and must be the same here */
#define FIXED	0x0080
#define SWAPPED 0x0001

/* Turn off assertions in SBIGMXT.C for performance reasons */
#define ASSERT(arg) /* do nothing */
#define ENTER(xyz)  /* do nothing */

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

  number_of_pages = ((size + (PAGESIZE - 1)) / PAGESIZE);
  if ((page = find_big_block(number_of_pages)) == -1)
   {
    garbage();	/* Invoke Garbage Collector to reclaim memory */
    if ((page = find_big_block(number_of_pages)) == -1)
     {
      gcsquish();  /* Try compacting memory */
again:
      if ((page = find_big_block(number_of_pages)) == -1)
       {
	out_of_memory();		/* attempt to execute SCHEME-RESET */
        goto again;
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
  int real_pages[NUMPAGES+1]; /* real memory page table */
  int i,j,k,l;		/* our old favorite index variables */
  int gap,temp; 	/* used for sort algorithm */
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

  /* Search for a contiguous group of real memory pages to satisfy request */
  for (i = 0; i < NUMPAGES - DEDPAGES - number_of_pages; i++)
   {
    if ((free_pages[i] != -1) && ((pagetabl[free_pages[i]] & SWAPPED) == 0))
     {
      j = 1;
      while ((free_pages[i+j] != -1) &&
	     ((pagetabl[free_pages[i+j]] & SWAPPED) == 0) &&
	     ((pagetabl[free_pages[i+j]] - pagetabl[free_pages[i+j-1]]) == PAGESIZE/16))
       {
	j++;
	if (j >= number_of_pages)
	 {
	  /* required number of real memory pages found-- adjust page table */
	  page_found = free_pages[i];
	  update_pages(free_pages,i,number_of_pages);
	  return(page_found);
	 }
       }
      i += j;
     }
   }

  /* At this point, there are no contiguous pages in real/conventional	*/
  /* memory. Get a list of all the swappable real memory pages and sort */
  /* them according to their paragraph address				*/

  for (i=0; i <= NUMPAGES; i++)
    if ((i <= DEDPAGES) || ((pagetabl[i] & FIXED+SWAPPED) != 0))
      real_pages[i] = 1000;
    else
      real_pages[i] = i;

  for (gap = NUMPAGES/2; gap > 0; gap /=2)
    for(i=gap; i<NUMPAGES; i++)
      for(j=i-gap;j>=0 && ((real_pages[j] == 1000) ? 65536 : pagetabl[real_pages[j]]) >
	   ((real_pages[j+gap] == 1000) ? 65536 : pagetabl[real_pages[j+gap]]);j-=gap)
	{
	  temp = real_pages[j];
	  real_pages[j] = real_pages[j+gap];
	  real_pages[j+gap] = temp;
	}

  /* Search for a contiguous group of extended memory pages to satisfy	*/
  /* request. Once a group of potential pages is found, search the	*/
  /* sorted real_pages for a contiguous group of real memory pages, ie	*/
  /* consecutive paragraph addresses, for pages which may be swapped to */
  /* extended memory. Swap them out, and use their prior paragraph	*/
  /* addresses to allocate the big block.				*/

  for (i = 0; i < NUMPAGES - DEDPAGES - number_of_pages; i++)
   {
    if ((free_pages[i] != -1) && ((pagetabl[free_pages[i]] & SWAPPED) == SWAPPED))
     {
      j = 1;
      /* Lets look for consecutive extended memory pages */
      while ((free_pages[i+j] != -1) &&
	     ((pagetabl[free_pages[i+j]] & SWAPPED) == SWAPPED) &&
	     ((free_pages[i+j] - free_pages[i+j-1]) == 1))
       {
	j++;
	if (j >= number_of_pages)
	 {
	  page_found = free_pages[i];

	  l=0;
	  /* Now lets look for contiguous real memory pages	*/
	  while (real_pages[l] != 1000)
	   {
	    k=1;
	    /* Note : Can't swap current code block page - CB_pag */
	    if ((real_pages[l]*2) != CB_pag)
	     {
	       while ((real_pages[l+k] != 1000) &&
		     ((real_pages[l+k]*2) != CB_pag) &&
		     ((pagetabl[real_pages[l+k]] - pagetabl[real_pages[l+k-1]]) == PAGESIZE/16))
		{
		 k++;
		 if (k > number_of_pages)
		  {
		   /* Move each real memory page to extended memory */
		   for (k=0; k<number_of_pages; k++)
		     move_page(real_pages[l+k]*2,free_pages[i+k]*2);
		   update_pages(free_pages,i,number_of_pages);
		   return(page_found);
		  }
		}
	     }
	    l += k;
	   }
	  return(-1); /* No contiguous real memory pages - exit */
	 }
       }
      i += j;
     }
   }
  return(-1);		/* no pages found */
 } /* end of function:	find_big_block(number_of_pages) */


update_pages(free_pages,index,num_pages)
   int free_pages[];
   int index,num_pages;
{
   int i,j,page;

   ENTER(update_pages);

   page = free_pages[index];
   pagetabl[page] = pagetabl[page] | FIXED;
   psize[page] = PAGESIZE * num_pages;
   free_pages[index] = -1;

  for (j = 1; j < num_pages; j++)
   {
    page = free_pages[index+j];
    pagetabl[page] = pagetabl[page] | FIXED;
    psize[page] = 0;
    attrib[page].nomemory = 1;
    free_pages[index+j] = -1;
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
} /* end of function update_pages */
