/*                                                      =====> SMEMORY.C     */
/* TIPC Scheme '84 Runtime Support - Memory Allocation Routines
   (C) Copyright 1984,1985, 1986 by Texas Instruments Incorporated.
   All rights reserved.

   Author:  John C. Jensen
   Installation:  Texas Instruments Incorporated, Dallas, Texas
   Division:  Central Research Laboratories
   Cost Center:  Computer Science Laboratory
   Project:  Computer Architecture Branch
   Date Written:  12 April 1984
   Last Modification:  21 October 1985
*/
#include "scheme.h"

/* Turn off assertions in SMEMORY.C for permance reasons */
#define ASSERT(arg) /* do nothing */
#define ENTER(xyz)  /* do nothing */

/**********************************************************************/
/*              Allocate a Page in Scheme's Memory                    */
/**********************************************************************/
alloc_page(type)
int type;
 {
  int page_allocated;
ENTER(alloc_page);

  if (freepage == END_LIST) return(END_LIST);  /* out of pages? */
  page_allocated = freepage;
  freepage = pagelink[freepage];

  /* Define page management characteristics for this type page */
  w_attrib[page_allocated] = pageattr[type];
  pagelink[page_allocated] = pagelist[type];
  ptype[page_allocated] = type + type;
  pagelist[type] = page_allocated;

  /* Initialize this page to all 0's */
  zero_page(page_allocated);

  /* Initialize free storage chains for appropriate data type */
  switch (type)
   {
    case LISTTYPE:
    case FLOTYPE:
    case REFTYPE:
                   swpage(page_allocated);
                   break;

    case BIGTYPE:
    case SYMTYPE:
    case STRTYPE:
    case ARYTYPE:
    case CLOSTYPE:
    case CONTTYPE:
    case CODETYPE:
    case FREETYPE:
    case PORTTYPE:
    case ENVTYPE:
                    put_ptr(page_allocated, 0, FREETYPE, psize[page_allocated]);
                    nextcell[page_allocated] = 0;
                    break;

/***            Note:  fixnums and characters handled as immediates
    case FIXTYPE:
    case CHARTYPE:
***/
    default: printf("[VM INTERNAL ERROR] alloc_page: Invalid type: %d\n",
                    type);
             getch();
   } /* end:  switch (type) */

  /* re-define page attributes and type (GC thinks this is a free page) */
  w_attrib[page_allocated] = pageattr[type];
  ptype[page_allocated] = type + type;

  return(page_allocated);
 } /* end of function:  alloc_page(type) */


/**********************************************************************/
/*                      Allocate a List Cell                          */
/*                                                                    */
/*   Note:  this routine will always return a list cell unless        */
/*   memory is exhausted, in which case Scheme terminates             */
/*   abnormally                                                       */
/**********************************************************************/
alloc_list_cell(reg)
int reg[2];
 {
  ENTER(alloc_list_cell);
  find_list_cell(reg);  /* attempt to find a free cell */
  if (reg[C_PAGE] == -1)  /* did allocation succeed? */
   {
    reg[C_PAGE] = NIL_PAGE*2;   /* legitimize pointer before GC */
    garbage();  /* no, invoke garbage collector */
    find_list_cell(reg);  /* try again to find a free cell */
    if (reg[C_PAGE] == -1)  /* did allocation succeed? */
     {
      reg[C_PAGE] = NIL_PAGE*2; /* legitimize pointer before GC */
      gcsquish();  /* no, invoke garbage collector */
      find_list_cell(reg);  /* try yet again to find a free cell */
      if (reg[C_PAGE] == -1)
       {
        out_of_memory();      /* Memory Exhausted-- Attempt SCHEME-RESET */
	                      /* control will not return here            */
       } /* end:  if (reg[C_PAGE] == -1) */
     } /* end:  if (reg[C_PAGE] == -1) */
   } /* end:  if (reg[C_PAGE] == -1) */
 } /* end of function:  alloc_list_cell(reg) */


/* Find a List Cell */
find_list_cell(reg)
int reg[2];
 {
  int disp;
  /*  int i; */
ENTER(find_list_cell);

  /* check available cell list */
  while ((disp = nextcell[listpage]) == END_LIST)
   { /* No cells in this page-- try next one, or allocate new page */
    if ((listpage = pagelink[listpage]) == END_LIST)
      if ((listpage = alloc_page(LISTTYPE)) == END_LIST)
       {
	listpage = 0;	   /* just point to page 0 - null list 		*/
        reg[C_PAGE] = -1;  /* set failure value-- no success allocating */
        goto no_can_do;
       }
   } /* end:  while ((disp = nextcell[listpage]) == END_LIST) */

  /* allocate cell and update free cell list */
  /*  i = nextcell[listpage] = get_word(listpage, disp+1);	*/
  /*  ASSERT((i >= 0 && i < psize[listpage]) || i == END_LIST); */

  nextcell[listpage] = get_word(listpage, disp+1);

  /* return page number and displacement */
  reg[C_PAGE] = ADJPAGE(listpage);
  reg[C_DISP] = disp;
no_can_do:
 } /* end of function:  find_list_cell(reg) */


/**********************************************************************/
/*                         Allocate a Flonum                          */
/*   Note:  this routine will always return a flonum cell unless      */
/*   memory is exhausted, in which case Scheme terminates             */
/*   abnormally                                                       */
/**********************************************************************/
alloc_flonum(reg, value)
int reg[2];
double value;
 {
  int i;                /* temporary variable */
  ENTER(alloc_flonum);

  /* determine page for allocation-- a "special" flonum value? */
  if (value == 0.0 || value == 1.0 || value == -1.0)
   {
    reg[C_PAGE] = ADJPAGE(SPECFLO);
    i = value;
    reg[C_DISP] = FLOSIZE * (i + 1);
   }
  else
   {
af_again:
    find_flonum(reg);  /* attempt to find a free cell */
    if (reg[C_PAGE] == -1)  /* did allocation succeed? */
     {
      reg[C_PAGE] = NIL_PAGE*2; /* legitimize register before GC */
      garbage();  /* no, invoke garbage collector */
      find_flonum(reg);  /* try again to find a free cell */
      if (reg[C_PAGE] == -1)  /* did allocation succeed? */
       {
        reg[C_PAGE] = NIL_PAGE*2;       /* legitimize register before GC */
        gcsquish();  /* invoke memory compaction */
        find_flonum(reg);  /* try yet again to find a free cell */
        if (reg[C_PAGE] == -1)
         {
          out_of_memory();    /* Memory Exhausted-- Attempt SCHEME-RESET */
	  goto af_again;
         } /* end:  if (reg[C_PAGE] == -1) */
       } /* end:  if (reg[C_PAGE] == -1) */
     } /* end:  if (reg[C_PAGE] == -1) */
    /* store the value into the flonum cell */
    put_flo(CORRPAGE(reg[C_PAGE]), reg[C_DISP], value);
   }
 } /* end of function:  alloc_flonum(reg) */


find_flonum(reg)
int reg[2];
 {
  int disp;
  int i;
/*%%int page;*/
  ENTER(find_flonum);

  reg[C_PAGE] = -1; /* set status in case allocation fails */

  if (flopage == END_LIST)
   { /* No page of flonums allocated-- do so, if possible */
    if ((flopage = alloc_page(FLOTYPE)) == END_LIST) goto no_can_do;
   }

  /* check available cell list */
  while ((disp = nextcell[flopage]) == END_LIST)
   { /* No cells in this page-- try next one, or allocate new page */
    if ((flopage = pagelink[flopage]) == END_LIST)
      if ((flopage = alloc_page(FLOTYPE)) == END_LIST) goto no_can_do;
   } /* end:  while ((disp = nextcell[flopage]) == END_LIST) */

  /* allocate cell and update free cell list */
  i = nextcell[flopage] = get_word(flopage, disp+1);
ASSERT((i >= 0 && i < psize[flopage]) || i == END_LIST);

  /* return page number and displacement */
  put_byte(flopage, disp, FLOTYPE);        /* change tag field */
  reg[C_PAGE] = ADJPAGE(flopage);
  reg[C_DISP] = disp;
no_can_do:
 } /* end of function:  find_flonum(reg, value) */


/************************************************************************/
/*                      Allocate String Constant                        */
/************************************************************************/
alloc_string(reg, string)
int reg[2];             /* destination register */
char *string;           /* value of string */
 {
  alloc_block(reg, STRTYPE, strlen(string));
  put_str(string, CORRPAGE(reg[C_PAGE]), reg[C_DISP]);
 }

/********

		commented out 12/31/87 by tc
		new code in block.asm

/************************************************************************/
/*                      Allocate Variable Length Block                  */
/************************************************************************/
alloc_block(reg, type, size)
int reg[2];                     /* register to receive block pointer */
int type;                       /* type code for block */
int size;                       /* size (bytes) of data */
 {
  int *last_page;               /* current chain entry address */
  int page;                     /* page number of candidate
                                        page for allocation */
  int str_size;                 /***** for small string length *****/

  ENTER(alloc_block);

  str_size = size;                 /***** save for further calculation *****/
  if (type == STRTYPE && size < PTRSIZE) /***** check for small string *****/
    size = PTRSIZE;                /***** string length at least 3     *****/
  size += BLK_OVHD;  /* increment request size to account for block overhead */

  page = pagelist[type]; /* search page type chain */
  last_page = &pagelist[type]; /* remember position in chain */
  while (page != END_LIST)
   {
    find_block(reg, type, size, page);
    if (reg[C_PAGE] != -1) goto block_found;
    if (size <= SMALL_SIZE) *last_page = pagelink[page];
    last_page = &pagelink[page];
    page = pagelink[page];
   } /* end:  while (page != END_LIST) */

  /* normal block allocation failed-- test for large block */
  if (size > PAGESIZE)
   {
    /* allocate a block larger than one page */
    reg[C_PAGE] = NIL_PAGE*2;   /* make register legitimate in case of GC */
    alloc_big_block(reg, type, size);
    goto block_found; /* note:  allocation will succeed, or control will
                                not return from the "find_big_block" call */
   }
  else
   {
    /* block not found in allocated pages-- try to allocate a new one */
    if ((page = alloc_page(type)) == END_LIST)
     {
      reg[C_PAGE] = NIL_PAGE*2; /* legitimize register before GC */
      garbage(); /* invoke garbage collector to reclaim unreferenced data */
      page = pagelist[type]; /* search page type chain once again */
      last_page = &pagelist[type]; /* remember position in chain */
      while (page != END_LIST)
       {
        find_block(reg, type, size, page);
        if (reg[C_PAGE] != -1) goto block_found;
        if (size <= SMALL_SIZE) *last_page = pagelink[page];
        last_page = &pagelink[page];
        page = pagelink[page];
       } /* end:  while (page != END_LIST) */
      /* attempt a new page allocation after garbage collection */
ab_again:
      if ((page = alloc_page(type)) == END_LIST)
       {
        reg[C_PAGE] = NIL_PAGE*2;       /* legitimize register before GC */
        gcsquish(); /* invoke memory compaction */
        /* attempt a new page allocation after compaction */
        if ((page = alloc_page(type)) == END_LIST)
         {
          out_of_memory();      /* Memory Exhausted-- Attempt SCHEME-RESET */
	  goto ab_again;
         }
       }
     }
   }

  /* allocate block in newly allocated page */
  find_block(reg, type, size, page);
  ASSERT (reg[C_PAGE] != -1 /* allocation failure */);

block_found:
  if (type == STRTYPE && str_size < PTRSIZE)
  /***** for small strings, put the negative value for object length *****/
  /***** string with NULL length => object length -3                 *****/
  /***** string with length 1    => object length -2                 *****/
  /***** string with length 2    => object length -1                 *****/
    put_word(CORRPAGE(reg[C_PAGE]), reg[C_DISP]+1, str_size - PTRSIZE);
 } /* end of function:  alloc_block(reg, type, size) */

/************************************************************************/
/*      Try to Allocate a Variable Length Block in a Specific Page      */
/************************************************************************/
find_block(reg, type, size, page)
int reg[2];                     /* register to receive pointer to block */
int type;                       /* type code for the block */
int size;                       /* size (bytes) of the entire block */
int page;                       /* page in which to attempt allocation */
 {
  int disp;                     /* block displacement */
  int free_disp;                /* displacement of a free block */
  int free_size;                /* size of a block's free pool */
  int remaining;                /* temporary variable */

  ENTER(find_block);

  reg[C_PAGE] = -1;  /* initialize return register to "block not found" */

  /* First, see if there's space in the free pool of this block */
  if ((disp = nextcell[page]) != END_LIST)
   {
    /* This page has a free pool */
    free_size = get_word(page, disp+1);
    if (size <= free_size)
     {
      /* allocate block from free pool */
      put_ptr(page, disp, type, size);
      if ((free_disp = disp + size) <= psize[page] - BLK_OVHD)
       {
        /* still free space remaining */
        put_ptr(page, free_disp, FREETYPE, free_size - size);
        nextcell[page] = free_disp;  /* update free pool pointer */
       }
      else nextcell[page] = END_LIST; /* no more free space here */
      goto return_block;
     } /* end:  if (size <= free_size) */
   } /* end:  if ((disp = nextcell[page]) != END_LIST) */

  /* Can't allocate from free pool-- search for a fragment */
  disp = 0;
  remaining = psize[page] - size;
  while (disp <= remaining)
   {
    free_size = get_word(page, disp+1);
    if (get_byte(page, disp) == FREETYPE)
     {
      /* free block found */
      if (size == free_size)
       {
        /* exact match-- we were lucky */
        put_byte(page, disp, type);
        goto return_block;
       }
      else if (size < free_size - BLK_OVHD)
            {
             /* split free block to allocate */
             put_ptr(page, disp, type, size);
             free_disp = disp + size;
             put_ptr(page, free_disp, FREETYPE, free_size - size);
             goto return_block;
            } /* end:  if (size < free_size + BLK_OVHD) */
     } /* end:  if (size == free_size) */
    if (free_size < 0)                   /* small string? */
      disp = disp + BLK_OVHD + PTRSIZE;
    else disp += free_size;  /* advance to next block */
   } /* end:  while (disp <= remaining) */

  goto block_not_found;

return_block:
  reg[C_PAGE] = ADJPAGE(page);
  reg[C_DISP] = disp;
block_not_found:
 } /* end of function:  find_block(reg, type, size, page) */

	commented out 12/31/87 by tc

*********************/


/**********************************************************************/
/*                      Invoke garbage collection                     */
/**********************************************************************/
int gc_count = 0;               /* global counter for gc invocations */
int compact_every = 7;		/* perform compaction every 7 gc's   */

garbage1()
 {
  gc_on();      /* display "garbage collection" message */

  gc_count++;
  mark();
  gc_oht();     /* clean up the object hash table */
  gcsweep();
  if (listpage == END_LIST) listpage = 0;
  gc_off();     /* un-display "garbage collection" message */

 if (!(gc_count % compact_every)) /* see if its time to compact */
    gcsquish();

 }

/* mark everything pointed to for the garbage collector */
mark()
 {
  int i;
  int *j,*k;

  extern int FNV_save[2],STL_save[2];   /* reset variables */

  /* mark all objects pointed to by the Scheme VM's registers */
  j = &reg0_page;
  k = &reg0_disp;
  for (i = 0; i < NUM_REGS; i++,j+=2,k+=2)
    gcmark(*j, *k);
  gcmark(FNV_pag, FNV_dis);
  gcmark(GNV_pag, GNV_dis);
  gcmark(PREV_pag, PREV_dis);
  gcmark(CB_pag, CB_dis);
  gcmark(TRNS_pag, TRNS_dis);
  gcmark(tmp_page, tmp_disp);
  gcmark(tm2_page, tm2_disp);
  gcmark(FNV_save[C_PAGE], FNV_save[C_DISP]);
  gcmark(STL_save[C_PAGE], STL_save[C_DISP]);

  /* preserve everything pointed to by active stack entries */
  for (i = 0; i <= TOS; i += PTRSIZE)
   {
    gcmark(S_stack[i], (S_stack[i+2]<<8) + S_stack[i+1]);
   }

  /* preserve everything pointed to by the oblist */
  for (i = 0; i < HT_SIZE; i++)
   {
    if (hash_page[i]) gcmark(hash_page[i], hash_disp[i]);
   }

  /* preserve everything pointed to by the property list */
  for (i = 0; i < HT_SIZE; i++)
   {
    if (prop_page[i]) gcmark(prop_page[i], prop_disp[i]);
   }
 } /* end of function:  mark() */

/************************************************************************/
/*         Memory Exhausted-- Attempt to Perform SCHEME-RESET           */
/************************************************************************/
out_of_memory()
 {
  int i;

  if ( (nextpage < lastpage) && (nextpage < (NUMPAGES - 1)) )
    {
      freepage = nextpage;
      for (i=0; i<8 && (nextpage < (NUMPAGES - 1)); i++)
	{
	  pagelink[nextpage] = nextpage+1;
	  attrib[nextpage].nomemory = 1;
	  nextpage += 1;
        }
      pagelink[nextpage-1] = END_LIST;
     }
   else
     {
       printf("\n[VM ERROR encountered!] Out of memory\n%s\n%s",
              "Attempting to execute SCHEME-RESET",
              "[Returning to top level]\n");
       force_reset();
     }
 }

/************************************************************************/
/*                      Print Message and Exit Scheme                   */
/************************************************************************/
print_and_exit(msg)
char *msg;
 {
  printf(msg);          /* print the error message */
  getch();              /* wait for any key to be pressed */
  exit();               /* bye now */
 }
