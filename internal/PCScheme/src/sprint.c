/*                                                      =====> SPRINT.C      */
/* TIPC Scheme '84 Runtime Support - S-Expression Printing
   (C) Cop[yright 1984,1985 by Texas Instruments Incorporated.
   All rights reserved.

   Author:  Mark E. Meyer
   Installation:  Texas Instruments Incorporated, Dallas, Texas
   Division:  Central Research Laboratories
   Cost Center:  Computer Science Laboratory
   Project:  Computer Architecture Branch
   Date Written:  13 June 1984
   Last Modification:  10 Feb 1987 by Terry Caudill

   Modification History:

    tc	2/10/87 modified PRINT-ATOM to recognize special atoms
		such as #T, #F, etc.

*/

#include "scheme.h"
/*******
#include "schars.h"
*********************/
extern char decpoint;  /* The current decimal point character */

extern int ccount;
extern int show;
extern int display;
/***********
static int ccount = 0;  /* Character count */
static int display = TRUE;  /* Whether to use | and " */
static int show = TRUE;    /* Whether to send actual characters */
static int detail = TRUE;  /* Whether to show details */

/***************************************************************/
/*         SPRINT(pg,ds,ppg,pds,dmode,smode,dtmode)            */
/*      Given a logical (0,1,2,...) page PG and a displacement */
/* DS, SPRINT prints the s-expression representing the object  */
/* at that location, through the port at (PPG,PDS).  SPRINT is */
/* recursive, and will go into an infinite loop if the         */
/* structure has a loop in it.  Only atoms and lists are       */
/* printed as they are; other data types are printed as, for   */
/* example, <ARRAY>.  SPRINT returns the number of characters  */
/* needed to print the item, not counting | around atoms and   */
/* " around strings.                                           */
/***************************************************************/
sprint(pg,ds,ppg,pds,dmode,smode,dtmode)
int pg,ds;        /* Location of item to be printed */
int ppg,pds;      /* Location of output port */
int dmode;        /* If TRUE, use | and " */
int smode;        /* If FALSE, do not print characters, only ocunt them */
int dtmode;       /* If TRUE, print details of unprintables */
 {
  setabort();
  setadr(ppg,pds,0);  /* Set port & make sure it's for output */
  ccount = 0;
  display = dmode;
  show = smode;
  detail = dtmode;
  subsprint(pg,ds);
  return(ccount);
 }


subsprint(pg,ds)
int pg,ds;
 {
  int ch;          /* a character being printed */
  int i,j,k;
  int len;         /* length of a data object */
  char *getmem();
  char *divider;   /* Buffer for division by 10 */
  char *bigchars;  /* ASCII representation of bignum */
  int lcl[2];      /* local "register" */
  char *str;       /* temporary string pointer */
  int strange;     /* number of characters in a string which must be escaped */
  double get_flo();
  char *symbol_name(); /* Retrieves print name for a symbol */

  /* Note:  the following character buffer must be long enough to hold
            the longest representation of a character.  The current
            length record holder is "#\backspace"                       */
  static char ch_buffer[14] = "#\\xxxxxxxxxxx";

/* If shift-break depressed, abort I/O */
  if (s_break)
   {
kill_output:
    printstr("\n[WARNING: Output aborted by SHIFT-BREAK]", 41);
    restart(show ? 0 : 2);
    /* Note:  control does not return from "restart" */
   }

/* If program stack low, print no deeper */
  if (stkspc() < 64)  printstr("#<DEEP!>",8);
   else
   {
/* Otherwise, act on object type */
    switch (ptype[pg]>>1)
     {
      case (LISTTYPE):
        if (pg)  /* If not NULL */
         {
          printchar('(');
          do
           {
            subsprint(CORRPAGE(get_byte((i=pg),ds)),get_word(pg,ds+1));
            pg = CORRPAGE(get_byte(pg,ds+3));
            ds = get_word(i,ds+4);
            if (pg)  /* If more items in the list */
              printchar(' ');
           }
          while (pg && ((ptype[pg]) == LISTTYPE*2));
          if (pg)  /* If last cdr not NIL */
           {
            printchar('.');
            printchar(' ');
            subsprint(pg,ds);
           }
          printchar(')');
         }
        else printstr("()",2);
        break;
      case (FIXTYPE):
        i = 5;
        if (!(divider = getmem(i)))  abort(HEAPERR);
        fix2big( ((ds<<1)>>1), divider);
        goto PRINTINT;
      case (FLOTYPE):
        printflo(get_flo(pg,ds));
        break;
      case (ARYTYPE):
        printstr("#(",2);
        len = get_word(pg, ds+1) - 3;  /* fetch length of array object */
        for (i = BLK_OVHD; i <= len; i+=PTRSIZE)
         {
          subsprint(CORRPAGE(get_byte(pg, ds+i)), get_word(pg,ds+i+1));
          if (i < len) printchar(' ');
         }
        printchar(')');
        break;
      case (CONTTYPE):
        printstr("#<CONTINUATION>",15); break;
      case (CLOSTYPE):
        printstr("#<PROCEDURE",11);
        /* fetch information operand from closure object */
        lcl[C_PAGE] = get_byte(pg,ds+3);
        lcl[C_DISP] = get_word(pg,ds+4);
        /* follow information operand list to cdr of last list cell */
        while (lcl[C_PAGE] && ptype[CORRPAGE(lcl[C_PAGE])] == LISTTYPE*2)
         {
          take_cdr(lcl);
         }
        /* if final operand is a symbol, print it */
        if (ptype[(i = CORRPAGE(lcl[C_PAGE]))] == SYMTYPE*2)
         {
          str = symbol_name(i, lcl[C_DISP]);
          printchar(' ');
          printstr(str, strlen(str));
          rlsstr(str);
         }
        printchar('>');
        break;
      case (FREETYPE):
        printstr("#<FREE>",7); break;
      case (CODETYPE):
        printstr("#<CODE>",7); break;
      case (ENVTYPE):
        printstr("#<ENVIRONMENT>",14); break;
      case (SYMTYPE):
        printatm(pg,ds,SYM_OVHD,'|');
        break;
      case (STRTYPE):
        len = get_word(pg, ds+1);
        if (len < 0) len = len + BLK_OVHD + PTRSIZE;/* check for small string */
        ccount += (len -= BLK_OVHD);
        if (show)
         {
          if (display)
           { /* write-- need to print double quotes, escape characters */
            for (i = 0, strange = 2;  i < len; i++)
             {
              ch = get_byte(pg,ds+BLK_OVHD+i);
              if (ch == '\\' || ch == '"') strange++;
             } /* end:  for (i = 0, strange = 2;  i < len; i++) */
            wrap(len + strange);
            givechar('"');
            for (i = 0; i < len; i++)
             {
              if (s_break) goto kill_output;
              ch = get_byte(pg,ds+BLK_OVHD+i);
              if (ch == '\\' || ch == '"') givechar('\\');
              givechar(ch);
             } /* end:  for (i = 0; i < len; i++) */
            givechar('"');
           }
          else /* display-- just print the string */
           {
            wrap(len);
            for (i = 0; i < len; i++)
             {
              if (s_break) goto kill_output;
              givechar(get_byte(pg,ds+BLK_OVHD+i));
             } /* end:  for (i = 0; i < len; i++) */
           } /* end:  else */
         } /* end:  if (show) */
        break;
      case (CHARTYPE):
        i = get_char(pg, ds);
        if (display)
         {
          ch_buffer[2] = i;       /* make character into string "#\?" */
          ch_buffer[3] = '\0';
          /* Check for a "special" multi-character character constant */
          for (j = 0; j < test_num; j++)
            if (i == test_char[j])
             {
              str = test_string[j];
              k = 2;
              while ((ch_buffer[k++] = *str++));
              break;
             }
          printstr(ch_buffer, strlen(ch_buffer));
         }
        else printchar(i); /* print character without escapes */
        break;
  /*  case (REFTYPE):
        printstr("#<REF>",6); break;
  */  case (BIGTYPE):
        i = get_word(pg,ds+1) - 1;
        if (!(divider = getmem(i)))  abort(HEAPERR);
        copybig(pg,ds,divider);
  PRINTINT:
        if (bigchars = getmem(j=(i*3-5)))
         {
          printstr(bigchars, big2asc(divider,bigchars));
          rlsmem(bigchars,j);
          rlsmem(divider,i);
          break;
         }
         else
         {
          rlsmem(divider,i);
          abort(HEAPERR);
         }
      case (PORTTYPE):
        printstr("#<PORT>",7); break;
     }
   }
 }


/***************************************************************/
/*                       PRINTCHAR(c)                          */
/*      Prints a single character to the file, and sends a     */
/* newline if necessary.                                       */
/***************************************************************/
printchar(c)
char c;
 {
  ccount++;
  if (show)
    if (currspc() > 0)
      return(givechar(c));
     else
     {
      givechar('\n');
      if (!isspace(c)) /* After newline, print only nonspaces */
        return(givechar(c));
       else
       {
        return(0);
       }
     }
   else
   {
    return(0);
   }
 }


/***************************************************************/
/*                      PRINTSTR(str,len)                      */
/*      This prints the string STR of length LEN, first        */
/* sending a newline if necessary.                             */
/***************************************************************/
printstr(str,len)
char *str;
int len;
 {
  int i;
  wrap(len);
  ccount += len;
  if (show)  gvchars(str,len);
 }
*************************************/


/***************************************************************/
/*                   PRINTATM(pg,ds,offs,c)                    */
/*      PRINTATM is used for printing both symbols (and        */
/* strings). The atom to be printed is located at logical page */
/* PG and displacement DS.  The argument OFFS tells how many   */
/* bytes from the top of the atom begin the characters to be   */
/* printed.  The atom printname will be bracketed with the     */
/* character CH at both ends if necessary.                     */
/*      ( CH=='|' for symbols, '"' for strings.)               */
/***************************************************************/
printatm(pg,ds,offs,ch)
int pg,ds,offs;
char ch;
 {
/*%%int i;*/
  int j;
  char *buf;
/*%%char c;*/
  int len;     /* Length of print name */
  int strange=0;   /* Number of strange characters */
  char *getmem();
  ENTER(printatm);

/* First stage: Copy pname into buffer, count needed escape    */
/* characters, and determine whether the pname is "strange".   */
  len = get_word(pg, ds+1) - offs;
  ds += offs;
  if (!(buf = getmem(offs=2*len+1)))  getmem_error(rtn_name);
  strange = (j=blk2pbuf(pg,ds,buf,len,ch,display)) & 1;
  j >>= 1;

/* Second stage: If necessary, check for numeric, dot, or      */
/* #-macro confusion.                                          */
if (!strange)
 if ((!strcmp(buf,".")) || (*buf=='#') && (pg!=SPECSYM) || (scannum(buf,10)))
   strange++;

/* Third stage: Send carriage-return if needed, and print      */
/* pname of atom, delimited if necessary.                      */
stage_3:
  ccount += len;   /* Update character count */
  if (show)
   {
    wrap(j + ((strange=(strange && display))? 2 : 0));
    if (strange)  givechar(ch);
    gvchars(buf,j);
    if (strange)  givechar(ch);
   }
  rlsmem(buf, offs);
 }

/***************************************************************/
/*                        PRINTFLO(f)                          */
/*      Given a double-length floating-point number, this      */
/* procedure formats and prints the ASCII representation of    */
/* the number.                                                 */
/***************************************************************/
printflo(f)
double f;
{
 char buf[32];
 printstr(buf, makeflo(f,buf,0,outrange(f)));
}

/***************************************************************/
/*                        OUTRANGE(f)                          */
/*      Returns a non-zero value if the value of the given     */
/* flonum F is not "close" to 1, zero otherwise.               */
/***************************************************************/
outrange(f)
double f;
{
 if (f<0)  f = -f;
 return((f<1.0e-3) || (f>=1.0e7));
}

/***************************************************************/
/*                 MAKEFLO(flo,buf,prec,ex)                    */
/*      Takes a flonum FLO and converts it to a human-readable */
/* form, storing the characters in the buffer BUF.  PREC       */
/* specifies the number of decimal places to be used (as many  */
/* as necessary, up to a maximum, if PREC is 0) and EX         */
/* specifies whether to use exponential (if nonzero) or fixed- */
/* decimal format.  MAKEFLO returns the number of characters   */
/* placed in BUF, and BUF should be at least 32 bytes.         */
/***************************************************************/
makeflo(flo,buf,prec,ex)
double flo;
char *buf;
int prec,ex;
{
 char digits[32];
 int scl = 0;
 if (flo==0.0)
  {
   *digits='0'; ex=0;
  }
  else
  {
   scale(&flo,&scl);
   flo2big(flo*1.0e15,buf);
   big2asc(buf,digits);
  }
 return(formflo(digits,buf,scl,prec,ex));
}

/***************************************************************/
/*                       SCALE(&flo,&x)                        */
/*      Given a pointer FLO to a double-length flonum and a    */
/* pointer X to an integer, SCALE puts at those two locations  */
/* a new flonum and integer such that FLO equals the new       */
/* flonum times 10 to the integer's power and the new flonum   */
/* is in the interval [ 1.0, 10.0 ).                           */
/***************************************************************/
scale(flo,x)
double *flo;
int *x;
{
 double local;
 double squar = 10.0;
 double tensquar[9];
 int scale,wassmall,i;
 scale = wassmall = i = 0;
 local = ((*flo>0) ? *flo : -*flo);
 if (local == 0)
   *x = 0;
  else
  {
   if (local < 1.0)
    {
     wassmall = -1;
     local = 1.0/local;
    }
   tensquar[0] = 10.0;
   while (++i<9)
    {
     squar *= squar;
     tensquar[i] = squar;
    }
   while (--i>=0)
    {
     scale <<=1;
     if (local>=tensquar[i])
      {
       local /= tensquar[i];
       scale++;
      }
    }
   if (wassmall)
    {
     scale = -scale;
     local = 1.0/local;
     if (local!=1.0)
      {
       local *= 10;
       scale--;
      }
    }
   *x = scale;
   *flo = ((*flo < 0.0) ? -local : local);
  }
}
/******************
/***************************************************************/
/*                        WRAP(len)                            */
/*      WRAP issues a newline if there are less than LEN       */
/* spaces left on the current output line.                     */
/***************************************************************/
wrap(len)
int len;
 {
  if (show && currspc()<len && curr_col()>1)
    givechar('\n');
 }
*****************************/
