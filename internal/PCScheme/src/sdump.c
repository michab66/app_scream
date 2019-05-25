/*                                                      =====> SDUMP.C       */
/* TIPC Scheme '84 - Diagnostic Dump Routines
   (C) Copyright 1984,1985,1987 by Texas Instruments Incorporated.
   All rights reserved.

   Author:  John Jensen
   Installation:  Texas Instruments Incorporated, Dallas, Texas
   Division:  Central Research Laboratories
   Cost Center:  Computer Science Laboratory
   Project:  Computer Architecture Branch
   Date Written:  11 April 1984
   Modification History:
     ?? 10/10/85 - ??
     rb  9/21/87 - lists, strings/symbols, and array-like data structures
	           print only within user-specified bounds;
		   also, hitting any key exits the printing
*/
#include "scheme.h"
#include "ctype.h"
#include "schars.h"
#include "slist.h"

#define INTR_OUTPUT if (char_rdy()) {(void) getch(); goto bye;}

static char *page_type[NUMTYPES] = {"LIST","FIX" ,"FLO" , "BIG","SYM" ,
                                    "STR" ,"ARY" ,"CONT","CLOS","FREE",
                                    "CODE","REF" ,"PORT","CHAR","ENV"};
/* character strings for getmem/rlsmem error message text */
static char *mem_fmt = "%s:  %smem error\n";
static char *getmem_error = "get";

char *getmem();
char char_rdy();

/**********************************************************************/
/*                 Format a dump of the Page Table                    */
/**********************************************************************/
dump_page_table()
{
  int i;  /* the usual index variable */
  int start, end; /* starting and ending limits of FREE pages */
  int space[NUMPAGES]; /* amount of free space in each page */
  ENTER(dump_page_table);

  /* determine the amount of free space in each page */
  sum_space(space);

  /* Print Page Table Dump Headings */
  printf("\nDump of Scheme Memory Management Page Tables\n\n%s\n%s\n%s\n",
           "Page   Page  Base    Next   Link          Free",
           " No    Type  Para   Avail   Page   Size   Bytes  Attributes",
           "----   ----  ----   -----   ----   ----   -----  ----------");
  start = end = -1;
  for (i = 0; i < nextpage; i++)
  {
    INTR_OUTPUT;
    if (ptype[i] == FREETYPE*2) {
      if (start < 0) start = i;
      end = i;
    }
    else {
      prt_free(&start, &end);
      printf("%4x  %5s   %04x   %4x   %4x%c  %4x   %4x   ", i,
                page_type[ptype[i]>>1], getbase(i*2), nextcell[i],
                pagelink[i], (i == pagelist[CORRPAGE(ptype[i])] ? '<' : ' '),
                psize[i], space[i]);
      /* print attributes for page */
      prt_atr(i);

      /* Flush line to output device */
      printf("\n");
    } /* end: else */
  } /* end:  for (i = 0; i < nextpage; i++) */
  prt_free(&start, &end);

  /* Print summary of pages which are not allocated */
  if (nextpage < NUMPAGES) {
    if (nextpage == NUMPAGES-1)
      printf("Page %x is not allocated\n", nextpage);
    else
      printf("Pages %x-%x are not allocated\n", nextpage, NUMPAGES-1);
  }
bye:
} /* end of function:  dump_page_table() */

/**********************************************************************/
/*                      Print Page Attributes                         */
/*                                                                    */
/* Purpose:  This routine prints the attributes of a page on the      */
/*           current print line.  Attributes are separated by commas. */
/**********************************************************************/
prt_atr(page)
int page;
 {
  unsigned bits;
  static char *things[16] = {"atom","list","fixnum","flonum","bignum",
                             "symbol","string","array","no memory","read only",
                             "continuation","closure","ref","port","code block",
                             "char"};
  char *comma_needed = "";
  int i = 0;
  ENTER(prt_atr);

  bits = w_attrib[page];
  while (bits)
   {
    if (bits & 0x8000)
     {
      printf("%s%s", comma_needed, things[i]);
      comma_needed = ",";
     }
    i++;
    bits = (bits << 1);
   } /* end:  while (bits) */
 }

/**********************************************************************/
/*              Print Free (unused) Pages of Memory                   */
/*                                                                    */
/* Purpose:  Given a range of unused pages of memory, this routine    */
/*           formats a message to indicate the presence of said pages.*/
/**********************************************************************/
prt_free(start, end)
int *start, *end;
 {
  int i;
  ENTER(prt_free);
  if ((i = *start) >= 0)
   {
    if (i == *end)
      printf("%4x   %4s   %04x   %4x   %4x   %4x   %4x   (unused)\n", i,
             page_type[ptype[i]>>1], getbase(i*2), nextcell[i],
             pagelink[i], psize[i], psize[i]);
    else
      printf("Pages %x-%x are allocated, but unused\n",
        i, *end);
    *start = *end = -1;
   }
 }

/**********************************************************************/
/*    Produce a Formatted Dump of an Area of Scheme's Address Space   */
/**********************************************************************/
dump_memory(page, disp, length)
int page;                       /* number of page to dump */
unsigned disp;                  /* starting displacement */
unsigned length;                /* number of bytes to dump */
 {
  int page_type;
  static char *description[NUMTYPES] = {"List Cells","Fixnums","Flonums",
                                        "Bignums","Symbols","Strings",
                                        "Arrays","Continuation Cells",
                                        "Closures","Nothing (unused)",
                                        "Code","Ref Cells","Ports",
                                        "Characters", "Environments"};
  ENTER(dump_memory);
  page_type = ptype[page]>>1;
  if (page_type >= 0 && page_type < NUMTYPES && page_type != FREETYPE)
   {
    printf("Page %x (hex) contains %s\nPage attributes:  ",
           page, description[page_type]);
    prt_atr(page);
    printf("\n");
    switch (page_type)
     {
      case LISTTYPE:
                     dump_list(page, disp, length);
                     break;
/***    Note:  Fixnums and Characters currently handled as immediates
      case CHARTYPE:
      case FIXTYPE:
                     dump_fix(page, disp, length);
                     break;
***/
      case SYMTYPE:
      case STRTYPE:
                     dump_str(page, disp, length);
                     break;

      case CODETYPE:
                     dump_code(page, disp, length);
                     break;

      case ARYTYPE:
      case CLOSTYPE:
      case CONTTYPE:
      case ENVTYPE:
                      dump_ary(page, disp, length);
                      break;

      case FLOTYPE:
/***                 dump_flo(page, disp, length);
                     break;                                     ***/

      case PORTTYPE:
/***                 dump_port(page,disp,length);
                     break;                                     ***/

      case BIGTYPE:
                      dump_hex(page, disp, length);
                      break;

      case REFTYPE:
      default:        printf("***Invalid page type: %d***\n", page_type);
     } /* end:  switch (page_type) */
   } /* end:  if (then clause) */
  else
    printf("***Invalid page type: %d***\n", page_type);
 } /* end of function:  dump_memory(page, disp, length) */


/**********************************************************************/
/*        Produce a Hex Dump of a Page of Scheme's Memory             */
/**********************************************************************/
dump_hex(page, disp, length)
int page, disp, length;
{
  int start, end;               /* delimiters for dump area */
  ENTER(dump_hex);

  start = disp & 0xFFF0;
  end = ((disp + length + 15) & 0xFFF0) - 1;

  while (start <= end) {
    INTR_OUTPUT;
    if ((start & 0x000F) == 0) { /* start of new line */
       printf("\n%2x:%04x  ", page, start);
    }
    printf("%02x ", get_byte(page, start));
    start++;
  }
  /* flush line to output device */
bye:  printf("\n");
}


/**********************************************************************/
/*      Produce Formatted Dump of a Page Containing List Cells        */
/*                                                                    */
/* Note:  the "disp" and "length" parameters currently are not used.  */
/*        (they are now -- rb 9/21/87)                                */
/**********************************************************************/
dump_list(page, disp, length)
int page;
int disp;
int length;
{
  int end_disp = disp + length;
  int count = 0;
  int i,j;                      /* the usual index variables */
  int next;
  int number_of_cells;          /* the number of list cells in current page */
  int page_length;              /* length of current page in bytes */
  int start = -1;
  int end = -1;
  char *unused_cells;
  ENTER(dump_list);

  /* if page zero, print nil */
  if (page == 0)
   {
    printf("000:0000  00 0000   00 0000  nil\n");
   } /* end:  if (page == 0) then ... */
  else /* page != 0 */
   {
    number_of_cells = (page_length = psize[page])/LISTSIZE;
    if (!(unused_cells = getmem(number_of_cells)))
     {
      printf(mem_fmt, rtn_name, getmem_error); goto end_routine;
     }
    /* count up available cells */
    for (i=0; i < number_of_cells; i++) unused_cells[i] = '\0';
    next = nextcell[page];
    while (next != END_LIST) {
      j = next/LISTSIZE;
ASSERT(j >= 0 && j < number_of_cells /* subchk? */ );
      unused_cells[j] = 'x';
      next = get_word(page, next+1);
      count++;
ASSERT(count <= number_of_cells /* infinite loop? */);
    } /* end while */
    printf("%d (decimal) List Cells Unused\n", count);

    /* print active List Cells */
    for (next = 0; next <= page_length - LISTSIZE; next += LISTSIZE) {
      INTR_OUTPUT;
      if (unused_cells[next/LISTSIZE]) { /* unused cell-- make a notation */
        if (start < 0) start = next;
        end = next;
      }
      else {
        if (end >= disp && start <= end_disp)
	  prt_unused(&start, &end);
        if (next+LISTSIZE >= disp && next < end_disp)
          printf("%3x:%04x  %02x %04x   %02x %04x\n", page, next,
                 CORRPAGE(get_byte(page, next)), get_word(page, next+1),
                 CORRPAGE(get_byte(page, next+3)), get_word(page, next+4));
      }
    } /* end for (next=0; ... */
    if (end >= disp && start <= end_disp)
      prt_unused(&start, &end);

    /* release memory for unused cell list */
bye:
    if (rlsmem(unused_cells, number_of_cells))
      rlsmem_error(rtn_name);
   } /* end:  else /* page != 0 */ */
end_routine:
 } /* end of function:  dump_list(page) */


/***** Code for dump_ref turned off 6 July 1985 by John Jensen *****
/**********************************************************************/
/*      Produce Formatted Dump of a Page Containing Reference Cells   */
/*                                                                    */
/* Note:  the "disp" and "length" parameters currently are not used.  */
/**********************************************************************/
dump_ref(page, disp, length)
int page;
int disp;
int length;
 {
  int count = 0;
  int end = -1;
  int i,j;                      /* the usual index variables */
  int next;
  int number_of_cells;          /* the number of cells in current page */
  int page_length;              /* size of the current page in bytes */
  int start = -1;
  char *unused_cells;
  ENTER(dump_ref);

  number_of_cells = (page_length = psize[page])/PTRSIZE;
  if (!(unused_cells = getmem(number_of_cells)))
   {
    printf(mem_fmt, rtn_name, getmem_error);  goto end_routine;
   }
  /* count up available cells */
  for (i=0; i < number_of_cells; i++) unused_cells[i] = '\0';
  next = nextcell[page];
  while (next != END_LIST)
   {
    j = next/PTRSIZE;
ASSERT(j >= 0 && j < number_of_cells /* subchk? */);
    unused_cells[j] = 'x';
    next = get_word(page, next+1);
    count++;
ASSERT(count <= number_of_cells /* infinite loop? */);
   }
  printf("%d (decimal) Reference Cells Unused\n", count);

  /* print active Ref Cells */
  for (next = 0; next <= page_length - PTRSIZE; next += PTRSIZE)
   {
    if (unused_cells[next/PTRSIZE])
     { /* unused cell-- make a notation */
      if (start < 0) start = next;
      end = next;
     }
    else
     {
      prt_unused(&start, &end);
      printf("%3x:%04x  %02x %04x\n", page, next,
      CORRPAGE(get_byte(page, next)), get_word(page, next+1));
     }
   } /* end: for next=0; next<=page_length-PTRSIZE; next+=PTRSIZE) */
  prt_unused(&start, &end);
  if (rlsmem (unused_cells, number_of_cells))
   {
    rlsmem_error(rtn_name);
   }
end_routine:
 } /* end of function:  dump_ref(page) */
***** Code for dump_ref turned off 6 July 1985 by John Jensen *****/

/**********************************************************************/
/*         Produce a Formatted Dump of a String/Symbol Page           */
/**********************************************************************/
dump_str(page, disp, length)
int page, disp, length;
{
  int end_disp = disp + length; /* display only between disp and end_disp */
  char buffer[70];              /* print line buffer */
  int ch;                       /* character (byte) begin formatted */
  int char_ptr;                 /* pointer into print line buffer */
  int count;                    /* count of bytes in print line buffer */
  int first_time;               /* flag indicating first time through loop */
  int hex_ptr;                  /* pointer into print line buffer */
  int hold_addr;                /* address value to print */
  int i,j;                      /* index variables */
  int incr;                     /* adjustment for header overhead */
  int next = 0;                 /* pointer to next block of memory in page */
  int page_length;              /* length in bytes of the page we're dumping */
  int size;                     /* size of string/symbol in bytes */
  int type;                     /* type code block of memory */
  static char hex_digit[16] = {'0','1','2','3','4','5','6','7','8','9',
                                'A','B','C','D','E','F'};
ENTER(dump_str);

  page_length = psize[page];
  while (next <= page_length - BLK_OVHD) {
    type = (i = get_byte(page, next)) & 0x003F;
    size = get_word(page, next+1);
    if (size < 0) size = BLK_OVHD + PTRSIZE;  /* check for small string */
 ASSERT(size >= 3 && size <= page_length - next /* invalid size */);
    if (next+size >= disp && next <= end_disp) {
      if (type == FREETYPE) {
	printf("%3x:%04x  unused block of %x (hex) bytes\n",
		  page, next, size);
      }
      else /* type != FREETYPE */ {
  printf("%3x:%04x  %02x %04x  block type = %d   size = %d (decimal) bytes\n",
		  page, next, i, size, type, size);
	if (type == SYMTYPE) {
	  printf("   Link:  %02x %04x     Hash Key: %02x\n",
			  get_byte(page,next+3), get_word(page,next+4),
			  get_byte(page,next+6));
	  incr = 7;
	} 
	else incr = 3;
	for (first_time = 1, count = 16, i = next+incr; i < next+size; i++) {
	  INTR_OUTPUT;
	  if (count >= 16) {
	    if (!first_time) printf("%3x:%04x  %s\n", page, hold_addr, buffer);
	    hold_addr = i;
	    for (j = 0; j < sizeof(buffer); j++) buffer[j] = ' ';
	    buffer[49] = buffer[66] = '|';
	    buffer[sizeof(buffer)-1] = '\0';
	    hex_ptr = count = first_time = 0;
	    char_ptr = 50;
	  } /* end:  if (then clause) */
	  if ((ch = get_byte(page, i)) != '\0') buffer[char_ptr] = ch;
	  buffer[hex_ptr] = hex_digit[ch >> 4];
	  buffer[hex_ptr+1] = hex_digit[ch & 0x000F];
	  count++;
	  char_ptr++;
	  hex_ptr += 3;
	} /* end for ... */
	printf("%3x:%04x  %s\n", page, hold_addr, buffer);
      } /* end else */
    } /* end if */
    next += size;
  } /* end:  while (next < page_length) */
bye:
} /* end of function:  dump_str(page, disp, length) */


/**********************************************************************/
/*              Format a Page Containing Code Blocks                  */
/**********************************************************************/
dump_code(page, start, length)
int page, start, length;
{
/*%%int ch;                       /* character (byte) begin formatted */*/
/*%%int char_ptr;                 /* pointer into print line buffer */*/
/*%%int count;                    /* count of bytes in print line buffer */*/
  int disp;
  int end;                      /* end of print range */
  int ent;                      /* entry offset of code */
/*%%int first_time;              /* flag indicating first time through loop */*/
/*%%int hex_ptr;                  /* pointer into print line buffer */*/
/*%%int hold_addr;                /* address value to print */*/
/*%%int i,j;                      /* index variables */*/
  int next = 0;                 /* pointer to next block of memory in page */
  int page_length;              /* length in bytes of the page we're dumping */
  int pc;
  int size;                     /* size of string/symbol in bytes */
  int tag;                      /* page number field of a constant ptr */
  char *title;                  /* pointer to "constants" title */
  int type;                     /* type code block of memory */
/*%%static char hex_digit[16] = {'0','1','2','3','4','5','6','7','8','9',*/
/*%%                            'A','B','C','D','E','F'};*/
ENTER(dump_code);

  end = start + length;
  page_length = psize[page];
  while (next <= page_length - BLK_OVHD) {
    type = get_byte(page, next) & 0x003F;
    size = get_word(page, next+1);
ASSERT(size >= 3 && size <= page_length - next /* invalid size */);
    if (type == FREETYPE) {
      if ((next >= start && next <= end) ||
          (next+size > start && next+size <= end) ||
          (next <= start && next+size-1 >= end)) {
        printf("%3x:%04x  unused block of %d (decimal) bytes\n",
                page, next, size);
      }
    }
    else /* type != FREETYPE */ {
      ent = get_word(page, next+4);
      if (next >= start && next <= end) {
        printf(
          "%3x:%04x  %02x %04x  block type = %d   size = %d (decimal) bytes\n",
          page, next, type, size, type, size);
        printf("Code begins at %d (decimal)\n", ent);
      }
      pc = next + PTRSIZE*2;
      title = "Constants:\n";
      while (pc < next + ent) {
	INTR_OUTPUT;
        if (pc >= start && pc <= end) {
          tag = CORRPAGE(get_byte(page, pc));
          disp = get_word(page, pc+1);
          printf("%s%3x:%04x  ", title, page, pc);
          annotate(tag, disp);  /* describe what's being pointed at */
          title = "";  /* print heading only once */
        }
        pc += PTRSIZE;
      } /* end:  while (pc < next + entry) */

      /* format the instructions in the block */
      while (pc < next + size) {
	INTR_OUTPUT;
        t_inst(page, &pc, 0, (pc >= start && pc <= end));
      } /* end:  while (pc < next + size) */
    } /* end:  else */
    next += size;
   } /* end:  while (next < page_length) */
 bye:
 } /* end of function:  dump_code(page, start, length) */


/**********************************************************************/
/*                        Dump the port page                          */
/*                                                                    */
/*      Note: DISP and LENGTH arguments are not used here.            */
/**********************************************************************/
/*****
dump_port(page,disp,length)
int page,disp,length;
 {
  int c;                        /* Next input character */
  int csrcol;                   /* Port cursor column */
  int i;                        /* Index variable */
  int lnlen;                    /* Port line length */
  int next = 0;                 /* Pointer to next block in page */
  int page_length;              /* Length in bytes of page */
  int ptype;                    /* Type of port */
  int size;                     /* Size of port in bytes */
  int type;                     /* Type code block of memory */

ENTER(dump_port);

  page_length = psize[page];
  while (next < page_length)
   {
    type = get_byte(page,next) & 0x003F;
    size = get_word(page,next+1);
ASSERT(size >= 46 && size <= page_length - next /* invalid size */);
    if (type == FREETYPE)
     {
      printf("%3x:%04x  unused block of %d (decimal) bytes\n",
                page,next,size);
     }
     else /* type != FREETYPE */
     {
      printf(
       "%3x:%04x  %02x %04x  block type = %d   size = %d (decimal) bytes \n",
       page, next, type, size, type, size);
      ptype = get_byte(page,next+3);
      switch (ptype)
       {
        case 0:
          printf("     Output port writing to ");
          break;
        case 1:
          printf("     Input port receiving from ");
          break;
       }
      /* Now print file name */
      i = 10;
      while ((i < 18) && ((c = get_byte(page,next+i)) != ' '))
       {
        printf("%c",toupper(c));
        i++;
       }
      i = 18;
      printf(".");
      while ((i < 21) && ((c = get_byte(page,next+i)) != ' '))
       {
        printf("%c", toupper(c));
        i++;
       }
      printf("\n");
      lnlen = get_word(page,next+4);
      csrcol = get_word(page,next+6);
      switch (ptype)
       {
        case 0:
          printf("     Line length: %d    Print column: %d\n",
                 lnlen, csrcol);
          break;
        case 1:
          c = get_byte(page, next+8);
          printf("    Most recent character: %02x\n",c);
          break;
       }
     } /* End of ELSE */
    next += size;
   } /* End of WHILE */
 } /* End of function:  dump_port(page,disp,length) */
*****/

/**********************************************************************/
/*      Format a Page Containing Arrays or Continuations              */
/**********************************************************************/
dump_ary(page, disp, length)
int page, disp, length;
{
/*%%int ent;                      /* entry offset of code */*/
  int start_disp = disp;        /* only values in range start_disp..end_disp
	                           are displayed */
  int end_disp = disp + length; 
  int i;
  unsigned next = 0;            /* pointer to next block of memory in page */
  unsigned page_length;         /* length of the current page in bytes */
  int pc;
  unsigned size;                /* size of string/symbol in bytes */
  int tag;                      /* page number field of a constant ptr */
  int type;                     /* type code block of memory */
ENTER(dump_ary);

  page_length = psize[page];
  while (next <= page_length - BLK_OVHD) {
    type = get_byte(page, next) & 0x003F;
    size = get_word(page, next+1);
ASSERT(size >= 3 && size <= page_length - next /* invalid size */);
    if (next+size >= start_disp && next <= end_disp) {
      if (type == FREETYPE) {
	printf("%3x:%04x  unused block of %d (decimal) bytes\n",
		  page, next, size);
      }
      else /* type != FREETYPE */ {
	printf(
	  "%3x:%04x  %02x %04x  tag = %d (%s)   size = %d (decimal) bytes\n",
	  page, next, type, size, type, page_type[type], size);
	pc = next + PTRSIZE;
	while (pc < next + size) {
	  INTR_OUTPUT;
	  tag = CORRPAGE(get_byte(page, pc));
	  disp = get_word(page, pc+1);
	  /* see if following array entries are same as the current one */
	  for (i = pc + PTRSIZE; i < next + size; i += PTRSIZE) {
	    if (tag != CORRPAGE(get_byte(page, i)) ||
		disp != get_word(page, i+1)) break;
          } /* end for (i = pc + PTRSIZE; ... */
          if (i >= start_disp && pc+PTRSIZE <= end_disp) {
	    if (i > pc + PTRSIZE) { /* consecutive entries with same value? */
	                            /* if so, print address range */
	      printf("%3x:%04x-%2x:%04x  ", page, pc, page, i-PTRSIZE);
	    }
	    else /* no consecutive entries with same value */ {
	      printf("%3x:%04x          ", page, pc);
	    } /* end else */
            annotate(tag, disp);  /* describe what's being pointed at */
          } /* end if (i >= disp ... */
	  pc = i;
        } /* end while (pc < next + size) */
      } /* end else */
    } /* end if */
    next += size;
  } /* end:  while (next < page_length) */
bye:
} /* end of function:  dump_ary(page, disp, length) */


/**********************************************************************/
/*                      Dump the runtime stack                        */
/**********************************************************************/
dump_stk()
{
  int next;
  int ptr_disp, ptr_page;
  ENTER(dump_stk);

  /* print the value of PREV_reg and the stack base */
  prt_reg(-4);
  printf("BASE     %04x (%u decimal)\n", BASE, BASE);

  /* print active Stack Cells */
  for (next = 0; next <= TOS; next += PTRSIZE) {
    INTR_OUTPUT;
    ptr_page = CORRPAGE(S_stack[next]);
    ptr_disp = (S_stack[next+2]<<8) + S_stack[next+1];
    printf("%s%04x  ", (next==FP ? "FP->" : "    "), next + BASE);

    /* for values, show the value the stack entry points to */
    annotate(ptr_page, ptr_disp);

  } /* end: for (next = 0; next <= TOS; next += PTRSIZE) */
bye:
} /* end of function:  dump_stk() */


prt_unused(start, end)
int *start, *end;
 {
  ENTER(prt_unused);
  if (*start >= 0)
   {
    if (*start == *end)
      printf("Location %04x unused\n", *start);
    else
      printf("Locations %04x-%04x unused\n", *start, *end);
    *start = *end = -1;
   }
 } /* end of function:  prt_unused(start, end) */


/**********************************************************************/
/*                      Dump the VM's Registers                       */
/**********************************************************************/
dump_regs()
{
  int i;                /* the usual index variable */
  int *reg_page, *reg_disp;
  int pc = S_pc;
  long unbound;         /* an "unbound" pointer */
  
  long make_ptr();

  unbound = make_ptr(UN_PAGE, UN_DISP);

  /* Print the Contents of the general purpose registers */
  reg_page = &reg0_page;
  reg_disp = &reg0_disp;
  for (i = 0; i < NUM_REGS; i++, reg_page+=2, reg_disp+=2)
    if (regs[i] != unbound) prt_reg(i);

  prt_reg(-1);  /* print FNV */
  prt_reg(-3);  /* print GNV */
  prt_reg(-2);  /* print CB  */
  if(tmp_page & 1)printf("odd tmp_page\n");
    printf("tmp_reg "); annotate(CORRPAGE(tmp_page), tmp_disp);
  (void) t_inst(CORRPAGE(CB_pag), &pc, 
                /* run= */ FALSE, /* display= */ TRUE);
} /* end of function:  dump_regs() */

prt_reg(reg)
int reg;        /* register number to print (-2=CB, -1=FNV) */
{
  int *disp;
  int *page;

  page = &reg0_page + reg + reg;
  disp = &reg0_disp + reg + reg;

  /* print the register name and contents */
  switch (reg) {
    case -1:  printf("FNV  ");
              page = &FNV_pag;
              disp = &FNV_dis;
              break;
    case -2:  printf("CB   ");
              page = &CB_pag;
              disp = &CB_dis;
              break;
    case -3:  printf("GNV  ");
              page = &GNV_pag;
              disp = &GNV_dis;
              break;
    case -4:  printf("PREV ");
              page = &PREV_pag;
              disp = &PREV_dis;
              break;
    default:  printf("R%d  ", reg);
  }

  /* expound on what pointer is */
  annotate(CORRPAGE(*page), *disp);
}

/*     Print information about what a pointer points to     */
annotate(page, disp)
int page, disp;
 {
  int ch;
  int count;
  char dlm = '\"';              /* string/symbol delimiter */
  int i;                        /* index variable */
  int incr = 3;                 /* adjustment for string/symbol header */
  char *str;                    /* pointer to a character string */
  int type;                     /* object type code */
  double get_flo();

  type = ptype[page]>>1;

  printf("%2x:%04x   %s", page, disp, page_type[type]);

  /* for values, show the value the register points to */
  switch (type)
   {
    case SYMTYPE:  incr = 7; /* note:  control falls through STRTYPE */
                   dlm = '|';

    case STRTYPE:  count = get_word(page, disp+1) - incr;
                   if (count > 40) count = 40;
                   disp += incr;
                   printf(" %c",dlm);
                   while (count > 0)
                    {
                     ch = get_byte(page, disp);
                     printf("%c", ch);  disp++;  count--;
                    }
                   printf("%c\n",dlm);
                   break;

    case FIXTYPE:  printf(" %d\n", get_fix(page, disp));
                   break;

    case FLOTYPE:  printf(" %g\n", get_flo(page,disp));
                   break;

    case CHARTYPE: ch = get_char(page, disp);
                   str = " ";
                   *str = ch;
                   for (i = 0; i < test_num; i++)
                    {
                     if (ch == test_char[i])
                      {
                       str = test_string[i];
                       break;
                      }
                    }
                   printf(" #\\%s\n", str);
                   break;

    case LISTTYPE:  if (page == 0) printf(" nil");
    default:       printf("\n");
   } /* end:  switch (type) */
 } /* end of function:  annotate(page, disp) */

/************************************************************************/
/*                          Dump Environment                            */
/************************************************************************/

dump_environment(page, disp)
int page;               /* page number of current environment entry */
int disp;               /* displacement of current environment entry */
{
  extern int display;
  extern int show;
  int search[2];
  int pair[2];
  int sym[2];
  char *symbol;         /* globally bound symbol (character representation) */

  char *symbol_name();  /* retrieves a symbol's print name */

  search[C_PAGE] = page;
  search[C_DISP] = disp;
  while (search[C_PAGE]) {
    INTR_OUTPUT;

    /* fetch pointer to symbol/value pair */
    mov_reg(pair, search);
    take_car(pair);

    /* fetch pointer to symbol */
    mov_reg(sym, pair);
    take_car(sym);

    /* retrieve symbol's print name and print said */
    symbol = symbol_name(CORRPAGE(sym[C_PAGE]), sym[C_DISP]);
    printf("%s\n", symbol);
    rlsstr(symbol);

    /* display the value currently bound to the symbol */
    take_cdr(pair);
    annotate(CORRPAGE(pair[C_PAGE]), pair[C_DISP]);
    ssetadr(ADJPAGE(OUT_PAGE), OUT_DISP);
    display = show = 1;
    sprint(CORRPAGE(pair[C_PAGE]), pair[C_DISP], ADJPAGE(OUT_PAGE),
                                  OUT_DISP);

    outchar('\015');
    outchar('\015');

    /* follow linked list in cdr field */
    take_cdr(search);

  } /* end:  while (search[C_PAGE]) */
bye:
} /* end of function:  dump_environment() */
