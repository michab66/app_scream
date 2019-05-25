/*     TIPC Scheme '84  -- Interactive Debug Utility
     (C) Copyright 1984,1985 by Texas Instruments Incorporated.
     All rights reserved.

     Author:  John C. Jensen
     Installation:  Texas Instruments Incorporated, Dallas, Texas
     Division:  Central Research Laboratories
     Cost Center:  Computer Science Laboratory
     Project:  Computer Architecture Branch
     Date Written:  13 April 1984
     Modification History:
       ?? 10/22/85 - ??
       rb  9/23/87 - make screen output interruptible
*/

#include "ctype.h"
#include "scheme.h"

#define BUFSIZE 160
#define DEFAULT_LENGTH 64
#define QUIT 0
#define PROCEED 1
#define HALT 1
#define INTR_OUTPUT if (char_rdy()) {(void) getch(); break;}

extern char char_rdy();

/**********************************************************************/
/*              TIPC Scheme '84 Interactive Debugger                  */
/*                                                                    */
/* Purpose:  This utility assists the compiler developer by allowing  */
/*           him or her to interactively display and modify the data  */
/*           structures of the Scheme Virtual Machine as a program    */
/*           executes.                                                */
/**********************************************************************/
sdebug()
{
  int after[NUMPAGES], before[NUMPAGES]; /* memory reclamation info */
  char buffer[BUFSIZE];
  int command = PROCEED;
  int disp;                      /* displacement into a page */
  int i,j,k;
  int idx;                       /* index into character string buffer */
  long ix;                       /* 32 bit integer temporary */
  unsigned length;               /* length (in bytes) of area to dump */
  int page;                      /* page number */
  int *reg_page,*reg_disp;       /* current register pointers */
  int sav_disp;
  int pc;			 /* local copy of VM's PC */
  
  long hex_word();
  unsigned hex_val();
  unsigned put_word();
  ENTER(sdebug);

  /* If we're not in VM debug mode, simply issue a SCHEME-RESET */
  if (!VM_debug) {
reset:
    printf("\nAttempting to execute SCHEME-RESET\n%s",
           "[Returning to top level]\n");
    CB_pag = SPECCODE*2;
    CB_dis = 0;
    S_pc = RST_ent - 1;
    goto run_it;
  }
   
  /* Print Welcome to the Debugging World */
  printf("\nPC Scheme Virtual Machine Debugger\n");

  /* Read the next command from the user person */
  while (command != QUIT) {
    zcuron();
    printf("\nCOMMAND: ");
    i = 0;
    ssetadr(ADJPAGE(IN_PAGE), IN_DISP);
    while ((j = take_ch()) != '\r')
      if (j != '\n') buffer[i++] = j;
    buffer[i] = '\0';

    /* decode instruction */
    if (i > 0) {
      switch(tolower(buffer[0])) {
	case 'a':  /* display accounting information */
		   accounting();
		   break;

	case 'd':  /* Dump Memory:  Page:Offset [length] */
		   i = tolower(buffer[1]); /* save second character */
		   if (i != 'f') {
		     idx = 1;
		     if (check_page(buffer, &idx, &page, &disp))
			break;
		     if ((length = hex_val(buffer, &idx)) == 0)
		       length = DEFAULT_LENGTH;
		     length = min (length, psize[page] - disp);
		   }

		   switch (i) {
		     case 'g':  /* dump global environment */
				page = CORRPAGE(GNV_pag);
				disp = GNV_dis;
				while (page) {
			          INTR_OUTPUT;
				  printf("\n\t*** NEW RIB ***\n\n");
				  sav_disp = disp;
				  disp += 6;
				  for (i = 0; i < HT_SIZE; i++,disp+=3) {
				    INTR_OUTPUT;
				    if (j = get_byte(page, disp))
				      dump_environment(j,
					      get_word(page, disp+1));
			          }
				  disp = get_word(page,sav_disp+4);
				  page = CORRPAGE(get_byte(page,sav_disp+3));
			        } /* end: while (page) */
				break;
		     case 'f':  /* dump fluid environment */
				dump_environment(FNV_pag, FNV_dis);
				break;
		     case 'h':  /* hexadecimal dump */
				dump_hex(page, disp, length);
				break;
/*****                 case 'p':  /* dump the property list */
				dump_prop();
				break;
*****/
		     case 's':  /* dump the runtime stack */
				dump_stk();
				break;
		     default:   /* regular ole dump of a page */
				dump_memory(page, disp, length);
		   } /* end switch */

		   break;

	case 'e':  /* Execute this here program */
run_it:
		   if (run(&S_pc, 0x7fff) == HALT) {
		     command = QUIT;
	           }
		   else {
		     if (!VM_debug) goto reset;
		   }
		   break;

	case 'g':  /* invoke garbage collector */
		   if (ask("run garbage collector"))
		    {
		     sum_space(before);
		     garbage();
		     sum_space(after);
		     for (i = DEDPAGES; i < NUMPAGES; i++)
		       if (before[i] != after[i])
			 printf("Page %3x -- %d bytes recovered\n", i,
			       after[i] - before[i]);
		     if (ask("run compaction phase"))
		      {
		       for (i = DEDPAGES, j = 0; i < NUMPAGES; i++)
			 if (ptype[i] == FREETYPE*2) j++;
		       gcsquish();    /* go for memory compaction */
		       for (i = DEDPAGES, k = 0; i < NUMPAGES; i++)
			 if (ptype[i] == FREETYPE*2) k++;
		       printf("%d pages reclaimed\n", k-j);
		      }
		    }
		   break;

	case '?':  /* print out commands currently defined */
/*	case 'h':  */
   printf("Valid Debugger Commands:\n\n%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s",
		  "  A - display accounting information\n",
		  "  DH [page:offset [length]] - dump memory hex\n",
		  "  D  [page:offset [length]] - dump memory formatted\n",
		  "  DF,DG,DS - dump fluids, globals, stack\n",
		  "  E - execute program (return to PC Scheme)\n",
		  "  G - invoke Garbage collection\n",
/*		  "  H - help (prints this information)\n",  */
		  "  I reg <CR> atom - input atom to register\n",
                  "  IP [n] - set IP to n; if none, decrement IP by 1\n", 
/*                "  M - run Mark phase of garbage collector\n", */
		  "  O - display registers as s-expressions\n",
		  "  P - dump page table\n",
		  "  Q - quit (return to DOS)\n",
		  "  R,RE - display registers, do scheme-reset\n",
/*                "  S - run Sweep phase of garbage collector\n", */
		  "  T [n] - trace n instructions\n",
		  "  WB [page:offset data ...] - write bytes\n",
		  "  WW [page:offset data ...] - write words\n",
		  "  X [n] - execute n instructions\n",
		  "  ? - help (prints this information)\n");
		   break;

	case 'i':  /* input atom into register */
		   if (tolower(buffer[1]) == 'p') {
		     idx = 2;
		     i = hex_val(buffer, &idx);
		     S_pc = (i > 0 ? i : S_pc - 1);
		   }
		   else {
		     idx = 1;
		     i = int_val(buffer,&idx) % NUM_REGS;
		     sread_atom(&reg0 + i, ADJPAGE(IN_PAGE), IN_DISP);
	           } /* end else */
		   break;

/*****
	case 'm':  /* run mark phase of garbage collection */
		   if (ask("run mark phase of garbage collector"))
		     mark();
		   break;
*****/

	case 'o':  /* print s-expressions pointed by regs */
		   reg_page = &reg0_page;
		   reg_disp = &reg0_disp;
		   for (i = 0; i < NUM_REGS; i++, reg_page+=2, reg_disp+=2)
		    {
		     if (*reg_disp != UN_DISP || *reg_page != UN_PAGE*2)
		       sprint_reg(i, *reg_page, *reg_disp);
		    }
		   break;

	case 'p':  /* print page table and page control information */
		   dump_page_table();
		   break;

	case 'q':  /* quit */
		   command = QUIT;
		   break;

	case 'r':  if (tolower(buffer[1]) == 'e')
		    {
		     CB_pag = SPECCODE*2;
		     CB_dis = 0;
		     S_pc = RST_ent - 1;
		    }
		   else dump_regs();  /* dump registers */
		   break;

/*****
	case 's':  /* run sweep portion of garbage collector */
		   if (ask("run sweep phase of garbage collector"))
		     gcsweep();
		   break;
*****/

	case 't':  /* trace instruction execution */
		   idx = 1;
		   if ((length = hex_val(buffer, &idx)) <= 0) length = 1;
		   while (length > 0) {
		     if ((i = t_inst(CORRPAGE(CB_pag), &S_pc,
			 /* run= */ TRUE, /* display= */ TRUE))) break;
		     length--;
		   } /* end while */
		   pc = S_pc;
		   (void) t_inst(CORRPAGE(CB_pag), &pc,
			 /* run= */ FALSE, /* display= */ TRUE);
		   if (i == HALT) halt_exec();
		   break;

	case 'w':  /* write memory-- determine if byte or word */
		   idx = 2;
		   if (check_page(buffer, &idx, &page, &disp))
		      break;
		   switch (tolower(buffer[1]))
		    {
		     case 'b':  /* write byte */
				while ((i = hex_byte(buffer, &idx)) >= 0)
				 {
printf("%3x:%04x  Previous contents: %02x   Replaced by: %02x\n",
				       page, disp,
				       get_byte(page, disp), i);
				  put_byte(page, disp, i);
				  disp++;
				 }
				break;

		     case 'w':  /* write word */
				while ((ix = hex_word(buffer, &idx)) >= 0L)
				 {
printf("%3x:%04x  Previous contents: %04x   Replaced by: %04lx\n",
				       page, disp,
				       get_word(page, disp), ix);
				  put_word(page, disp, ix);
				  disp += 2;
				 }
				break;

		     default:   goto bad_command;
		    } /* end:  switch (buffer[1]) */
		   break;

	case 'x':  /* instruction execution */
		   idx = 1;
		   if ((length = hex_val(buffer, &idx)) <= 0) length = 1;
		   if (interp(&S_pc, length) == HALT) halt_exec();
		   break;

	default:   /* unrecognized command */
bad_command:
		  printf("? unrecognized command\n");
		  break;
      } /* end:  switch(tolower(buffer[0])) */
    } /* end:  if (i > 0) */
  } /* end:  while (command != QUIT) */
} /* end of function: sdebug() */

halt_exec()
{
  printf("\n*** (exit) command executed ***\n");
}

/**********************************************************************/
/*                extract a decimal value from a string               */
/**********************************************************************/
int_val(str, idx)
char str[];
int *idx;
 {
  char ch;
  unsigned ret_val = 0;
  int i;
  ENTER(int_val);

  /* skip over any leading characters in string */
  while (str[*idx] != '\0' && !isdigit(str[*idx])) (*idx)++;

  /* continue to extract digits until end of string of delimiter */
  while ((ch = str[*idx]))
   {
    if ((i = get_int(ch)) >= 0)
      ret_val = (ret_val * 10) + i;
    else break;
    (*idx)++;
   }
  return((int) ret_val);
 } /* end of function:  int_val(str, idx) */

/**********************************************************************/
/*              extract a hexadecimal value from a string             */
/**********************************************************************/
unsigned hex_val(str, idx)
char str[];
int *idx;
 {
  char ch;
  unsigned ret_val = 0;
  int i;
  ENTER(hex_val);

  /* skip over any leading characters in string */
  while (str[*idx] != '\0' && !isxdigit(str[*idx])) (*idx)++;

  /* continue to extract digits until end of string of delimiter */
  while ((ch = str[*idx]))
   {
    if ((i = get_hex(ch)) >= 0)
      ret_val = (ret_val << 4) + i;
    else break;
    (*idx)++;
   }
  return(ret_val);
 } /* end of function:  hex_val(str, idx) */

/**********************************************************************/
/*              Extract a byte value from a string                    */
/**********************************************************************/
hex_byte(str, idx)
char str[];
int *idx;
 {
  int first_digit, second_digit;
  ENTER(hex_byte);
  while (str[*idx] == ' ') (*idx)++; /* skip leading blanks */
  if ((first_digit = get_hex(str[*idx])) < 0) return(-1);
  (*idx)++;
  if ((second_digit = get_hex(str[*idx])) < 0) return(first_digit);
  (*idx)++;
  return(first_digit * 16 + second_digit);
 }

/**********************************************************************/
/*              Extract a word value from a string                    */
/**********************************************************************/
long hex_word(str, idx)
char str[];
int *idx;
 {
  int digit,i;
  long ret_val;
  ENTER(hex_word);

  while (str[*idx] == ' ') (*idx)++; /* skip leading blanks */
  ret_val = -1L;
  for (i = 0; i < 4; i++)
   {
    if (str[*idx] == '\0') return(ret_val);
    if ((digit = get_hex(str[*idx])) < 0) return(ret_val);
    ret_val = (ret_val == -1L? digit : (ret_val << 4) | digit);
    (*idx)++;
   }
  return(ret_val);
 }

/**********************************************************************/
/*      Test for a hex digit-- if so, return its decimal value        */
/**********************************************************************/
get_hex(ch)
char ch; /* the character to be tested */
 {
  int i;
  static char hex_digit[16]={'0','1','2','3','4','5','6','7','8','9',
                             'A','B','C','D','E','F'};
  ENTER(get_hex);
  ch = toupper(ch);
  for (i = 0; i < 16; i++) if (ch == hex_digit[i]) return(i);
  return(-1);
 } /* end of function:  get_hex(ch) */

/**********************************************************************/
/*      Test for a decimal digit-- if so, return its value            */
/**********************************************************************/
get_int(ch)
char ch; /* the character to be tested */
 {
  ENTER(get_int);
  return(isdigit(ch) ? ch - '0' : -1);
 } /* end of function:  get_int(ch) */

/**********************************************************************/
/*              Verify page number, offset values                     */
/*                                                                    */
/* Purpose:  This routine checks the page number, displacement, and   */
/*           length parameters keyed in by the interactive debug user */
/*           to make sure they are within acceptable bounds.          */
/**********************************************************************/
check_page(buffer, idx, page, disp)
char buffer[];
int *idx, *page, *disp;
 {
/*%%  int len;*/
  int ret_val = -1;
  unsigned hex_val();
  ENTER(check_page);

  *page = hex_val(buffer, &(*idx));
  *disp = hex_val(buffer, &(*idx));

  /* Verify that page number is valid */
  if (*page < 0 || *page >= NUMPAGES)
   {
    printf("***page numbers must be in the range 00 to %x (hex)***\n",
            NUMPAGES-1);
   }
  else
   {
    if (attrib[*page].nomemory)
     {
      printf("***page %x (hex) has not been allocated***\n", *page);
     }
    else
     {
      if (*disp < 0 || *disp >= psize[*page])
        printf("***displacements must be in the range 0000 to %04x (hex)***\n",
                psize[*page]-1);
      else ret_val = 0;  /* valid page, displacement, length */
     }
   }
  return(ret_val);
 }


/**********************************************************************/
/*              Check with the user before doing something            */
/*                                                                    */
/* Purpose:  This routine is called before performing a potentially   */
/*           dangerous operation to make sure the interactive debug   */
/*           user has not entered the command in error.               */
/*                                                                    */
/* Calling Sequence:  answer = ask(question);                         */
/*      where:  int answer - FALSE = question was answered "no"       */
/*                           TRUE = question was answered "yes"       */
/*              char *question - a character string representing the  */
/*                               question to be asked.  A '?' will be */
/*                               appended to this string when it is   */
/*                               displayed to the user.               */
/**********************************************************************/
ask(str)
char *str;
 {
  int ch;
  printf("%s?\nType Y to proceed, any other character to abort\n",str);
  ch = getch();
  if (ch == 'y' || ch == 'Y') return(TRUE);
  printf("***command aborted***\n");
  return(FALSE);
 } /* end of function:  ask(str) */

/* Print s-expressive line of register contents to standard output*/
static char digit[10] = {'0','1','2','3','4','5','6','7','8','9'};
sprint_reg(name,pg,ds)
int name,pg,ds;
 {
  extern int display;
  extern int show;

  ssetadr(ADJPAGE(OUT_PAGE),OUT_DISP);
  outchar('R');
  if (name >= 10) outchar(digit[name/10]);
  outchar(digit[name%10]);
  outchar(':');
  outchar(' ');

  display = show = 1;
  ds = sprint(CORRPAGE(pg),ds,ADJPAGE(OUT_PAGE),OUT_DISP);
/***********
  ds = sprint(CORRPAGE(pg),ds,OUT_PAGE,OUT_DISP,TRUE,TRUE,FALSE);
**********/
  outchar('\n');
 }
