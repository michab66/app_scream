/* TIPC Scheme '84 Runtime Support - S-Expression Reading
   (C) Copyright 1984,1985 by Texas Instruments Incorporated.
   All rights reserved.

   Author:  Mark E. Meyer
   Installation:  Texas Instruments Incorporated, Dallas, Texas
   Division:  Central Research Laboratories
   Cost Center:  Computer Science Laboratory
   Project:  Computer Architecture Branch
   Date Written:  12 June 1984
   Last Modification:  18 October 1985 by John Jensen
*/

#include "scheme.h"
#include "schars.h"

extern char decpoint;   /* Current decimal point character */

#define ATOM 0     /* Codes returned by FINDTASK function */
#define NIL 1
#define LPAREN 2
#define RPAREN 3
#define QUOTE 4
#define DOT 5

#define DS 0   /* Register array subscripts */
#define PG 1
/************
static int scan = 0;
static char ch = ' ';  /* Most recently received character */
static int *mainreg = NULL;    /* Main register */
/** static int init_stk = 0;    /* Initial Scheme top-of-stack */  **/
static int limit;           /* Current size of atom buffer */
static char *atomb;         /* Atom buffer */
static int flag_eof;        /* Whether to flag end-of-file */

/**************************************************************/
/*                       REG2REG(sor,dest)                    */
/*      Copy the contents from one regsiter to another.       */
/**************************************************************/
#define reg2reg(sor,dest) (dest)[DS] = (sor)[DS]; (dest)[PG] = (sor)[PG];


/**************************************************************/
/*                        SKIPSPACE()                         */
/*      Skip over spaces in the input stream, and get a new   */
/* line of input if necessary                                 */
/**************************************************************/
#define skipspace() do {rcvchar();} while (isspace(ch));
**************/

/**************************************************************/
/*                      SREAD(reg,pg,ds)                      */
/*      Given a pointer to a register, SREAD constructs a     */
/* list structure, based on input from the port at (PG,DS),   */
/* leaving in the register a pointer to the structure.  This  */
/* memory-efficient algorithm has return pointers kept        */
/* within the structure itself, both minimizing overhead and  */
/* making the structure garbage-collector-compatible.         */
/* Throughout this source, pointers in the registers use      */
/* doubled pages, CORRected to single only when necessary.    */
/*      Aside from setting PORT once, a SUBSREAD routine is   */
/* used so that two additional registers known to the garabge */
/* collector can be easily utilized.  This means that READ    */
/* may only be called with one of the R-registers.            */
/**************************************************************/
/***** Code for 'read' turned off 16 July 1985 (JCJ) *****
sread(reg,pg,ds)
int *reg;
int pg,ds;
 {
  setabort();
  if (setadr(pg,ds,1))   /* Set the read port */
    abort(PORTERR);
  flag_eof = TRUE;  limit = 0;
  mainreg = reg;  /* Save pointer to main register */
  if (reg < &regs[3])
    subsread(reg, reg + 2, reg + 4);
   else
   {
    subsread(reg, reg - 2, reg - 4);
   }
  return(0);
 }
***** Code for 'read' turned off 16 July 1985 (JCJ) *****/

/***********
/**************************************************************/
/*                   SREAD_ATOM(reg,pg,ds)                    */
/*      Setup for the operation of reading a single atom from */
/* the given port.  Special characters such as ')' are parsed */
/* as lists(!) to tell them from ordianry atoms.              */
/**************************************************************/
sread_atom(reg,pg,ds)
int *reg;
int pg,ds;
 {
  setabort();
  if (setadr(pg,ds,1))
    abort(PORTERR);
  mainreg = reg;   /* Save pointer to result register */
  flag_eof = TRUE;  limit = /* init_stk = */ 0;
  do
   {
    skipspace(); skipcomments();
   } while (!ch);       /* if ch = '\0', flush it */
  return(read_atom(reg));
 }
***************/

/***** Code for 'read' turned off 16 July 1985 (JCJ) *****
/**************************************************************/
/*                   SUBSREAD(reg,pres,prev)                  */
/**************************************************************/
subsread(reg,pres,prev)
int reg[2];
int pres[2];  /* Pointer to the present cons cell */
int prev[2];  /* Ptr. to cell whose car is list containing [PRES] */
 {
  int task;
    C_push(pres);  /* Save old register values */
    C_push(prev);
    init_stk = TOS;  /* Save initial top of stack pointer */
    prev[PG] = prev[DS] = pres[PG] = pres[DS] = 0;
start:
    task = findtask();  /* Determine next action to take, */
    switch (task)       /* depending on what's coming up */
     {
      case NIL:         /* If atom coming, read it & leave */
      case ATOM:
        read_atom(reg);
        goto fastexit;
      case LPAREN:      /* Grab cons cell */
        descend(reg,pres,prev);
        C_push(pres);  /* Save start of list structure */
        reg[PG] = fixpage;  /* Bottom of stack for dot-lparens */
        C_push(reg);
        goto fillcar;       /* Read in the car */
      case QUOTE:       /* Form quote structure */
        buildq(pres,reg,&task);
        C_push(reg);    /* Save start */
        reg[PG] = fixpage;  /* Bottom of stack */
        C_push(reg);
        while (task == QUOTE)  /* Handle nested quotes */
          buildq(pres,reg,&task);
        switch (task)
         {
          case NIL:       /* Read atom and leave */
          case ATOM:
            read_atom(reg);
            reg2ptr(reg,pres);
            goto exit;
          case LPAREN:    /* Start reading list */
listquote:
            grabcell(reg);
            reg2ptr(reg,pres);
            reg2reg(reg,pres);
            goto fillcar;
          default:        /* Anything else is wrong! */
            abortread(QUOTERR);
         }
      case RPAREN:        /* Can't start with )! */
        abortread(RPARERR);
      case DOT:           /* Nor with .! */
        abortread(DOTERR);
     }
fillcar:
    task = findtask();
fillcar2:
    switch (task)
     {
      case NIL:   /* Deposit atom in car and read in cdr */
      case ATOM:
        read_atom(reg);
        reg2ptr(reg,pres);
        goto cross;
      case LPAREN:  /* Save PREV in cdr field and update PRES & PREV */
        reg2cdr(prev,pres);
        descend(reg,pres,prev);
        goto fillcar;
      case QUOTE:
        C_push(prev);  /* Save PREV & PRES */
        reg2reg(pres,prev);
        do             /* Build quote structure */
          buildq(pres,reg,&task);
        while (task == QUOTE);
        switch(task)
         {
          case NIL:   /* If atom, read in, restore PRES & PREV, and cross */
          case ATOM:
            read_atom(reg);
            reg2ptr(reg,pres);
            reg2reg(prev,pres);
            C_pop(prev);
            goto cross;
          case LPAREN:  /* If list, save old PREV in cdr of old PRES */
            C_pop(reg);
            reg2cdr(reg,prev);
            goto listquote;
          default:
            abortread(QUOTERR);
         }
      default:  /* This is the case of (. */
        abortread(DOTERR);
     }
cross:
    task = findtask();
    switch (task)
     {
      case RPAREN:  /* Try to ascend to higher list */
tryascend:
        C_pop(reg);
        while ((reg[PG] == prev[PG]) && (reg[DS] == prev[DS]))
          /* Do while there are dot-lparens to be closed */
          if (findtask() == RPAREN)
            C_pop(reg);
           else  /* Some dot-lparen wasn't closed */
           {
            abortread(DOTERR);
           }
        C_push(reg);   /* Restore stack */
        if (prev[PG])  /* Not done if not at top level of list */
         {
          ascend(pres,prev);
          goto cross;
         }
         else  /* Else exit */
         {
          goto exit;
         }
      case DOT:
        task = findtask();
        switch (task)
         {
          case NIL:    /* Fill cdr field */
          case ATOM:
            read_atom(reg);
            reg2cdr(reg,pres);
dotatom:
            if (findtask() == RPAREN)  /* Ascend only if ) follows */
              goto tryascend;
             else  /* more than one thing after dot */
             {
              abortread(DOTERR);
             }
          case LPAREN:  /* Push PREV onto stack */
            C_push(prev);
            buildcdr(reg,pres);  /* And treat as if dot-lparen */
            goto fillcar;        /* didn't happen */
          case QUOTE:
            task = -1;   /* Fill cdr field first */
            do
              buildq(pres,reg,&task);
            while (task == QUOTE);
            switch (task)
             {
              case NIL:   /* If atom, go fill space in quote structure */
              case ATOM:
                read_atom(reg);
                reg2ptr(reg,pres);
                goto dotatom;
              case LPAREN:
                C_push(prev);  /* Save dot syntax indicator */
                goto listquote;
              default:
                abortread(QUOTERR);
             }
          default:
            abortread(DOTERR);
         }
      default:  /* Otherwise, affix a cons cell to the cdr of PRES */
        buildcdr(reg,pres);
        goto fillcar2;
     }
exit:
    TOS -= PTRSIZE;  /* Discard fixnum */
    C_pop(reg);  /* Get ptr to structure */
fastexit:
    C_pop(prev);  /* Restore old values */
    C_pop(pres);
 }


/**************************************************************/
/*                        FINDTASK()                          */
/*      Scan the input stream and find out what's coming up   */
/* (atom, ", (, etc.).                                        */
/**************************************************************/
findtask()
 {
  char *stpchr();

  skipspace();
  skipcomments();
  switch (ch)
   {
    case '(':
      skipspace();
      skipcomments();
      if (ch == ')')
       {
        ch = ' '; return(NIL);
       }
       else
       {
        pushchar();
        return(LPAREN);
       }
    case ')':
      return(RPAREN);
    case '\'':
      return(QUOTE);
    case '.':
      rcvchar();
      if (isspace(ch))
        return(DOT);
       else
       {
        pushchar();
        if (stpchr( "()'|\";[]{}" , ch))
         {
          return(DOT); /* because dot is followed by non-atomic char */
         }
         else  /* the dot is the first char of a symbol */
         {
          ch = '.';   /* Restore initial dot */
          return(ATOM);
         }
       }
    default:
      return(ATOM);
   }
 }


/***************************************************************/
/*                  BUILDQ(pres,reg,task)                      */
/*      BUILDQ's purpose is to build a list of the form (quote */
/* nil), and point the field pointed to by PRES to the list.   */
/* On exit, PRES points to the cdr of the (quote nil),         */
/* enabling easy handling of nested quotes; REG points to the  */
/* (quote nil) itself, and TASK is set by FINDTASK().          */
/***************************************************************/
buildq(pres,reg,task)
int pres[2];
int reg[2];
int *task;
 {
  int temp[2];  /* Temporary register */
  C_push(pres);  /* Save destination pointer field */
  grabcell(reg);  /* Grab a cell and point its car to quote */
  pres[PG] = ADJPAGE(QUOTE_PAGE);
  pres[DS] = QUOTE_DISP;
  reg2ptr(pres,reg);
  grabcell(pres);       /* Grab another cell, point cdr(reg) to it */
  reg2cdr(pres,reg);
  reg2reg(pres,temp);  /* Save second cell address */
  C_pop(pres);         /* Point destination field to the quote list */
  if (pres[PG])        /*   if the field exists */
   {
    if (*task<0)
      reg2cdr(reg,pres);   /* Place in PRES's cdr field */
     else
     {
      reg2ptr(reg,pres);   /* Place in car field */
     }
   }
  reg2reg(temp,pres);  /* Recover second cell address */
  *task = findtask();
 }
***** Code for 'read' turned off 16 July 1985 (JCJ) *****/

/**************
/***************************************************************/
/*                      READ_ATOM(reg)                         */
/*      Read in an atom (symbol, string, number), using REG as */
/* a scratch register, and store the pointer to the atom in    */
/* REG.  Special characters such as ')' or ',' are read as     */
/* atoms themselves.  Normal atoms will end in a whitespace or */
/* a terminating macro character; strings end with the closing */
/* '"'.  Numbers in the requested base are interpreted as      */
/* such.  On exit, the next character in the buffer is the one */
/* following the last character of the atom.                   */
/***************************************************************/
read_atom(reg)
int reg[2];
 {
  register int i = 0;   /* Index within buffer */
  register char c;
  char k;
  char *bignum;  /* Bignum workspace */
  char *getmem();
  char *stpchr();
  int inputch = FALSE;  /* Whether the #\ macro is in effect */
  int escaped = FALSE;  /* Whether an escape character has been used */
  int j,pg,ds,siz,base,neg;
  double inflo;

  extern int CXFERR_status;

  CXFERR_status = 0;    /* set error code for normal return */

  if (ch==' ')    /* If ch is a space, form NIL */
   {
    reg[PG] = reg[DS] = 0;
    goto end_of_function;
   }
  flag_eof = FALSE;
  if (!(atomb = getmem(BUFSIZE)))  /* Create buffer if possible */
    abortread(HEAPERR);
  limit = BUFSIZE;  /* Initialize buffer size */
  base = 10;        /* Default base */
  switch (ch)   /* Read atom into buffer */
   {
    /* Special-character cases */
    case '[':
    case ']':
    case '{':
    case '}':
    case '(':
    case ')':
    case '\'':
    case '`': *atomb=ch; i++; goto speccase;
    /* String case */
    case '"': i = delimby('"',i);
              alloc_block(reg, STRTYPE, i);  /* Allocate string space */
              toblock(reg, 3, atomb, i);
              goto bye_now;
    /* Comma case */
    case ',': *atomb=','; i++;
              rcvchar();
              if ((ch=='@') || (ch=='.'))
               {
                atomb[1]=ch; i++; goto speccase;
               }
              goto norcvspec;
    /* Macro case */
    case '#': flag_eof = TRUE;
              while ((ch=='#') && (!i))
               {
                rcvchar();
                if (isspace(ch))  abortread(SHARPERR);
                switch (tolower(ch))
                 {
                  case 'b':  base=2;  break;
                  case 'd':  base=10; break;
                  case 'x':
                  case 'h':  base=16; break;
                  case 'o':  base=8;  break;
                  case '\\': rcvchar();
                             addchar(i++,ch);
                             inputch=TRUE; escaped=TRUE;
                             /* Fall through to BREAK below */
                  case 'i':  /* Ignore (currently) meaningless macros */
                  case 'e':
                  case 's':
                  case 'l':  break;
                  case '<':
                  case ')':  abortread(SHARPERR);
                  default:   *atomb = '#';
                             atomb[1] = ch;
                             i = 2;
                             if (ch=='(')  goto speccase;
                             break;
                 }
                rcvchar();
               }
              flag_eof = FALSE;
    /* Else a symbol */
    default:  while (!isspace(ch) &&
                     !stpchr( "()'`;,\"[]{}" , ch) &&
                     (ch!='\032'))
               {
                switch (ch=toupper(ch))
                 {
                  /* Multiple-escape: read chars until next | */
                  case '|':  escaped = TRUE;
                             i = delimby('|',i);
                             break;
                  /* Single-escape: get next char and put it in */
                  case '\\': escaped = flag_eof = TRUE;
                             rcvchar();
                             flag_eof = FALSE;
                  default: addchar(i++,ch);
                 }
                rcvchar();
               }
   }   /* End of switch (ch) */
endatom:
  addchar(i,'\0');  /* Put null at end of token */
 /* Now check for single, unescaped dot (SPECIAL!) */
  if ((i==1) && (*atomb=='.') && !escaped)  goto norcvspec;
 /* At this point a token has been accumulated.  Check for number */
  j = scannum(atomb,base);
  if ((j) && !escaped)   /* If a number */
   {
    if (j>0)   /* integer of some size */
     {
      siz = (j + 9)/2;  /* How many bytes needed for int? */
      if (!(bignum = getmem(siz)))
       {
        abortread(HEAPERR);
       }
      bignum[3] = '\0';
      bignum[4] = '\0';
      buildint(bignum,atomb,base);  /* Form integer */
      alloc_int(reg,bignum);
      rlsmem(bignum,siz);
     }
     else /* scan the flonum */
     {
      scanflo(atomb,&inflo,base);
      alloc_flonum(reg,inflo);
     }
   }
   else /* allocate character or interned symbol */
    {
     if (inputch)
      {
       reg[PG] = SPECCHAR*2;
       if (i==1)  reg[DS]=*atomb;
        else /* check for a multichar character constant */
        {
         *atomb = toupper(*atomb);      /* convert 1st symbol to uppercase */
         for (j=0; j<test_num; j++)
          {
           if (!strcmp(atomb, test_string[j]))
            {
             reg[DS]=test_char[j]; break;
            }
           if (j == test_num-1)
            {
             alloc_string(tmp_reg, atomb);
             set_error(0,"Invalid character constant",tmp_reg);
             reg[DS] = '?';
             CXFERR_status = -1;
            }
          } /* end of for(j=0; j<test_num; j++) */
        }
      }
      else /* not a character, but a symbol */
      {
       intern(reg,atomb,i);
      }
    }
  goto release;

speccase:                /* Process the special cases */
  intern(reg,atomb,i);   /* Intern the symbol and encase in a list */
  cons(reg,reg,nil_reg);
  goto bye_now;

norcvspec:
  intern(reg,atomb,i);   /* Intern the symbol and encase in a list */
  cons(reg,reg,nil_reg);

release:
  if (ch!='\032')  pushchar();   /* Put post-atom char back into buffer */

bye_now:
  rlsmem(atomb,limit);   /* Release the buffer space */
  flag_eof = TRUE;
  limit = 0;

end_of_function:
  return(CXFERR_status);
 }
******************************/

/**************************************************************/
/*                     SCANFLO(s,flo,base)                    */
/*      The string S, which ends in a control char, holds a   */
/* representation of a floating-point number.  The value of   */
/* this number is stored in *FLO.                             */
/**************************************************************/
scanflo(s,flo,base)
char *s;
double *flo;
int base;
{
 int i=0;
 int neg=0;
 int x=0;
 double place;
 switch (*s)
  {
   case '-': neg=-1;
   case '+': i++; break;
   default: break;
  }
 while (s[i]=='#') i+=2;
 *flo = 0.0;
 while (isdig(s[i],base))
  {
   *flo = (*flo * base) + digval(s[i++]);
  }
 if (!(s[i]==decpoint)) goto EXPON;
POINT:
 i++; place = 1.0;
 while (isdig(s[i],base))
  {
   place /= base;
   *flo += place*digval(s[i++]);
  }
 if (s[i]<' ') goto GOTFLO;
EXPON:
 i++;
 if (s[i]=='-')
  {
   i++; place = 1.0/base;
  }
  else place=base;
 while (isdigit(s[i]))
   x = (x*10) + digval(s[i++]);
 while (x)
  {
   if (x!=(x>>1)<<1)
     *flo *= place;
   if (place<1.0e153) place*=place;
   x >>= 1;
  }
GOTFLO:
 if (neg)
   *flo = -*flo;
}


/**************************************************************/
/*                     ALLOC_INT(reg,buf)                     */
/*      This allocates an integer, either a fixnum or a       */
/* bignum, depending on the size of the integer, i.e., if     */
/* the absolute value < 16384, then a fixnum is allocated.    */
/* The value is read from BUF.                                */
/**************************************************************/
alloc_int(reg,buf)
int reg[2];
char *buf;
 {
  unsigned i,j;
  int pg;
  i = 256*buf[1] + buf[0];
  j = 256*buf[4] + buf[3];
  pg = buf[2] & 1;
  if ((i == 1) && (j <= 16383+pg))  /* If fixnum */
    alloc_fixnum(reg, (pg ? -j : j));
   else
   {
    alloc_block(reg, BIGTYPE, 2*i + 1);
    toblock(reg, 3, buf+2, 2*i +1);
   }
 }

/**************
/**************************************************************/
/*                        DELIMBY(c,i)                        */
/*      DELIMBY takes characters from the input stream and    */
/* places them in the buffer ATOMB, starting at offset I and  */
/* ending when the delimiting character C is reached.  This   */
/* returns the number of characters stored in ATOMB, and CH   */
/* is left equal to C.                                        */
/**************************************************************/
delimby(c,i)
char c;
int i;
 {
  flag_eof = TRUE;   /* Signal any end-of-file error */
  rcvchar();
  while (ch != c)
   {
    if (ch != '\r')
     {
      if (ch == '\\') rcvchar();
      addchar(i++,ch);
     }
    rcvchar();
   }
  flag_eof = FALSE;
  return(i);
 }


/**************************************************************/
/*                        ADDCHAR(i,c)                        */
/*      ADDCHAR takes the character C and places it in the    */
/* dynamic atom buffer ATOMB, at offset I.  If the buffer can */
/* not contain any more characters, additional space is       */
/* allocated, and LIMIT is adjusted accordingly.              */
/**************************************************************/
addchar(i,c)
int i;
char c;
 {
  int j;  /* Scratch FOR variable */
  char *atom2;  /* New atom buffer, if necessary */
  char *getmem();  /* Additional memory allocator */
  if (i < limit)  /* If room for character, put it in */
    atomb[i] = c;
   else            /* Else create a new, larger buffer */
   {
    if (!(atom2 = getmem(limit + BUFSIZE)))
     {
      abortread(HEAPERR);
     }
    for (j = 0; j < limit; j++)  atom2[j] = atomb[j];
    rlsmem(atomb,limit);  /* Discard the old buffer */
    atomb = atom2;
    atomb[i] = c;
    limit += BUFSIZE;
   }
 }
********************************/
/***** Code for 'read' turned off 16 July 1985 (JCJ) *****
/**************************************************************/
/*                    DESCEND(reg,pres,prev)                  */
/*      Get a new cons cell, point the car field of [PRES] to */
/* the cell, and update PREV and PRES.                        */
/**************************************************************/
descend(reg,pres,prev)
int reg[2];
int pres[2];
int prev[2];
 {
  grabcell(reg);
  if (pres[PG])  /* Do only if PRES points to something */
    reg2ptr(reg,pres);
  reg2reg(pres,prev);
  reg2reg(reg,pres);
 }


/**************************************************************/
/*                      BUILDCDR(reg,pres)                    */
/*      Grab a cons cell, point the cdr field of [PRES] to    */
/* it, and update PRES.                                       */
/**************************************************************/
buildcdr(reg,pres)
int reg[2];
int pres[2];
 {
  grabcell(reg);
  pres[DS] += PTRSIZE;
  reg2ptr(reg,pres);
  reg2reg(reg,pres);
 }


/**************************************************************/
/*                      ASCEND(pres,prev)                     */
/*      Set PRES to PREV, and pluck the new value for PREV    */
/* from the cdr field of [PREV], replacing it with NIL.       */
/**************************************************************/
ascend(pres,prev)
int pres[2];
int prev[2];
 {
  int i,j;  /* Scratch integers */
  reg2reg(prev,pres);
  j = pres[DS] + PTRSIZE;
  i = CORRPAGE(pres[PG]);
  /* note:  put_byte/put_word return to contents of the byte/word replaced */
  prev[PG] = put_byte(i, j++, 0);
  prev[DS] = put_word(i, j, 0);
 }


/**************************************************************/
/*                       GRABCELL(reg)                        */
/*      Grab a cons cell, point REG to it, and set it to      */
/* (nil . nil).                                               */
/**************************************************************/
grabcell(reg)
int reg[2];
 {
  int i;
  alloc_list_cell(reg);
  i = CORRPAGE(reg[PG]);
  put_word(i, reg[DS], 0);
  put_word(i, reg[DS]+2, 0);
  put_word(i, reg[DS]+4, 0);
 }

/**************************************************************/
/*                      REG2PTR(reg,ptr)                      */
/*      Transfer the contents of REG to the pointer field     */
/* pointed to by PTR.                                         */
/**************************************************************/
reg2ptr(reg,ptr)
int reg[2];
int ptr[2];
 {
  put_byte(CORRPAGE(ptr[PG]), (ptr[DS])++, reg[PG]);
  put_word(CORRPAGE(ptr[PG]), (ptr[DS])--, reg[DS]);
 }

/**************************************************************/
/*                      REG2CDR(reg,ptr)                      */
/*      Transfer the contents of REG to the cdr field of the  */
/* cell pointed to by PTR.                                    */
/**************************************************************/
reg2cdr(reg,ptr)
int reg[2];
int ptr[2];
 {
  ptr[DS] += PTRSIZE;
  reg2ptr(reg,ptr);
  ptr[DS] -= PTRSIZE;
 }
***** Code for 'read' turned off 16 July 1985 (JCJ) *****/
/*************
/**************************************************************/
/*                        RCVCHAR()                           */
/* This fetches a character from the input stream             */
/**************************************************************/
rcvchar()
 {
  register int i;
  i = take_ch();
  if ((i<256) && (i!=26))
    ch = i;
  else
   {
    if (flag_eof) abortread(EOFERR);
     else
     {
      ch = '\032';   /* EOF character */
     }
   }
 }


/**************************************************************/
/*                      ABORTREAD(code)                       */
/*     Cancels the entire read operation via ABORT, after     */
/* resetting some vital registers.                            */
/**************************************************************/
abortread(code)
int code;
 {
  if (code == EOFERR)
   {
    mainreg[PG] = EOF_PAGE*2;       /* return "eof" indicator */
    mainreg[DS] = EOF_DISP;
   }
  else
   {
    mainreg[PG] = mainreg[DS] = 0;  /* NILify main register */
   }
  if (limit)  rlsmem(atomb,limit);  /* Release buffer memory */
/*****
  if (init_stk)
   {
    TOS = init_stk;
    C_pop((mainreg < &regs[3]) ? (mainreg + 4) : (mainreg - 4));
    C_pop((mainreg < &regs[3]) ? (mainreg + 2) : (mainreg - 2));
   }
*****/
  abort(code);
 }

/****************************************************************/
/*                      Skip over Comments                      */
/****************************************************************/
skipcomments()
 {
  while (ch == ';')
   {
    while (ch != '\r') rcvchar();
    skipspace();
   }
 } /* end of function:  skipcomments() */
****************************************************/

