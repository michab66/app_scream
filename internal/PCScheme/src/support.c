/*                                                      =====> SUPPORT.C     */
/* TIPC Scheme '84 Runtime Support - Non-Arithmetic Support
   (C) Copyright 1984,1985 by Texas Instruments Incorporated.
   All rights reserved.

   Author:  John C. Jensen
   Installation:  Texas Instruments Incorporated, Dallas, Texas
   Division:  Central Research Laboratories
   Cost Center:  Computer Science Laboratory
   Project:  Computer Architecture Branch
   Date Written:  24 May 1984
   Last Modification:  21 October 1985
*/
#include "scheme.h"
#include "slist.h"

char *getmem();         /* Lattice C's memory allocation routine */

/**********************************************************************/
/*                              Substring                             */
/**********************************************************************/
ssubstr(string, start_reg, end_reg)
int string[2];          /* register containing string pointer */
int start_reg[2];       /* register containing starting character position */
int end_reg[2];         /* register containing ending character position */
 {
  int end;              /* ending character position */
  int start;            /* starting character position */
  int str_page, str_disp;/* string's page and displacement pointers */
  int i;

  str_page = CORRPAGE(string[C_PAGE]);
  str_disp = string[C_DISP];

  /* validate input arguments */
  i = get_word(str_page,str_disp+1);
  if (i < 0)
    i = i + BLK_OVHD + PTRSIZE;              /* adjust for small string */
  if (ptype[str_page] == STRTYPE*2 &&
      start_reg[C_PAGE] == SPECFIX*2 &&
      end_reg[C_PAGE] == SPECFIX*2 &&
      (start = get_fix(CORRPAGE(start_reg[C_PAGE]),start_reg[C_DISP])) >= 0 &&
      (end = get_fix(CORRPAGE(end_reg[C_PAGE]),end_reg[C_DISP])) >= start  &&
      end <= i-BLK_OVHD)
   { /* arguments o.k.-- allocate new string and copy substring characters */
    alloc_block(tmp_reg, STRTYPE, end-start);
    msubstr(tmp_reg, string, start, end);
    string[C_PAGE] = tmp_page;
    string[C_DISP] = tmp_disp;
   }
  else
   { /* invalid arguments to substring */
    set_src_err("SUBSTRING", 3, string, start_reg, end_reg);
    return(-1);
   }
  return(0);
 } /* end of function:  ssubstr(string, start_reg, end_reg) */


/**********************************************************************/
/*                 Test if two pointers are equal?                    */
/**********************************************************************/
sequal_p(reg1, reg2)
int reg1[2], reg2[2];
 {
  int disp1,disp2;              /* displacements of the two pointers */
  int i;                        /* index & temporary variable */
  int length;                   /* length of a variable length object */
  int page1,page2;              /* page numbers of the two pointers */
  int result = FALSE;           /* result of the comparison */
  int temp_r1[2],temp_r2[2];    /* temporary "registers" for equal_p calls */
  int type;                     /* type of the pointers being compared */

  double get_flo();             /* fetch flonum from Scheme's memory */

  /* Localize page and displacement values */
  page1 = reg1[C_PAGE];
  page2 = reg2[C_PAGE];
  disp1 = reg1[C_DISP];
  disp2 = reg2[C_DISP];

recurse:
  /* Quick test in case the pointers are "eq?" */
  if (disp1 == disp2 && page1 == page2) return(TRUE);

  /* If pointer types are the same, check further */
  if ((type = ptype[(page1 = CORRPAGE(page1)) ]) ==
              ptype[(page2 = CORRPAGE(page2)) ] )
   {
    /* Check to see if shift-break key depressed */
    if (s_break) restart(3);
    /* Check to make sure we haven't recursed too deeply */
    if (stkspc() < 64)
     {
      printf("[VM ERROR encountered!] Stack overflow in EQUAL?\n%s%s",
             "Expression lists circular or too complex\n",
             "Attempting to execute SCHEME-RESET\n[Returning to top level]");
      force_reset();
      /* note:  control does not return here */
     }
    /* If okay, check the objects */
    switch (type>>1)
     {
      case LISTTYPE:    /* test if one pointer is nil */
                        if ((!page1 && page2) || (!page2 && page1))
                          return (FALSE);
                        ldreg(temp_r1, page1, disp1);
                        ldreg(temp_r2, page2, disp2);
                        /* test "equal?-ness" of "cars" */
                        if (sequal_p(temp_r1, temp_r2))
                         {
                          i = page1;
                          page1 = get_byte(i, disp1+3);
                          disp1 = get_word(i, disp1+4);
                          i = page2;
                          page2 = get_byte(i, disp2+3);
                          disp2 = get_word(i, disp2+4);
                          /* test "equal?-ness" of "cdrs" */
                          goto recurse;
                         }
                        break;

      case FLOTYPE:    result = (get_flo(page1, disp1) ==
                                 get_flo(page2, disp2));
                       break;
      case BIGTYPE:
      case STRTYPE:    /* Compare the objects */
                       result = mcmpstr(reg1, reg2);
                       break;

      case ARYTYPE:     /* test each entry of the arrays for equality */
                        if ((length = get_word(page1, disp1 + 1)) ==
                                      get_word(page2, disp2 + 1) )
                         {
                          for (i = PTRSIZE; i < length; i += PTRSIZE)
                           {
                            disp1 += PTRSIZE;
                            ldreg(temp_r1, page1, disp1);
                            disp2 += PTRSIZE;
                            ldreg(temp_r2, page2, disp2);
                            /* test "equal?-ness" of vector elements */
                            if (!(result = sequal_p(temp_r1, temp_r2))) break;
                           }
                         }
                        break;

      case FIXTYPE:
      case CHARTYPE:
      case SYMTYPE:
      case CONTTYPE:
      case CLOSTYPE:
      case CODETYPE:
      case PORTTYPE:
      case ENVTYPE:     /* For these types, assume that "eq?-ness" is o.k. */
                        break;

      case REFTYPE:
      case FREETYPE:
      default:  printf("[VM INTERNAL ERROR] EQUAL?: unexpected type=%d\n",type);
                getch();
     } /* end:  switch (type>>1) */
   }
  return(result);
 }

/************************************************************************/
/*                              String->Symbol                          */
/************************************************************************/
str_2_sym(reg)
int reg[2];
 {
  int page, disp, len;
  char *string;         /* pointer to character string */
  ENTER(str_2_sym);

  page = CORRPAGE(reg[C_PAGE]);
  disp = reg[C_DISP];
  if (ptype[page] != STRTYPE*2)
   {
    set_src_err("STRING->SYMBOL", 1, reg);
    return(-1);
   }
  else
   {
    len = get_word(page, disp+1);
    if (len < 0)
      len = len + BLK_OVHD + PTRSIZE;      /* adjust for small string */
    len -= BLK_OVHD;
    if (!(string = getmem(len+1))) getmem_error(rtn_name);
    get_str(string, page, disp);
    string[len] = '\0';
    intern(reg, string, len);
    rlsstr(string);
   }
  return(0);
 } /* end of function:  str_2_sym(reg) */

/************************************************************************/
/*                      String->Uninterned-symbol                       */
/************************************************************************/
str_2_usym(reg)
int reg[2];
 {
  int page, disp, len;
  char *string;         /* pointer to character string */
  ENTER(str_2_usym);

  page = CORRPAGE(reg[C_PAGE]);
  disp = reg[C_DISP];
  if (ptype[page] != STRTYPE*2)
   {
    set_src_err("STRING->UNINTERNED-SYMBOL", 1, reg);
    return(-1);
   }
  else
   {
    len = get_word(page, disp+1);
    if (len < 0)
       len = len + BLK_OVHD + PTRSIZE;      /* adjust for small string */
    len -= BLK_OVHD;
    if (!(string = getmem(len+1))) getmem_error(rtn_name);
    get_str(string, page, disp);
    string[len] = '\0';
    alloc_sym(reg, len);
    put_sym(string, CORRPAGE(reg[C_PAGE]), reg[C_DISP],NIL_PAGE*2,NIL_DISP,0);
    rlsstr(string);
   }
  return(0);
 } /* end of function:  str_2_usym(reg) */

/************************************************************************/
/*                              Symbol->String                          */
/************************************************************************/
sym_2_str(reg)
int reg[2];
 {
  int page;
  char *string;         /* pointer to character string */

  char *symbol_name();  /* retrieves symbol's print name */

  ENTER(sym_2_str);

  page = CORRPAGE(reg[C_PAGE]);
  if (ptype[page] != SYMTYPE*2)
   {
    set_src_err("SYMBOL->STRING", 1, reg);
    return(-1);
   }
  else
   {
    string = symbol_name(page, reg[C_DISP]);
    alloc_string(reg, string);
    rlsstr(string);
   }
  return(0);
 } /* end of function:  sym_2_str(reg) */

/************************************************************************/
/*                      Retrieve Symbol Name                            */
/*                                                                      */
/* Purpose:  To fetch the print name of a symbol from Scheme's memory   */
/*              and return it in a Lattice C string.                    */
/************************************************************************/
char *symbol_name(page,disp)
int page,disp;          /* pointer to the string data type */
 {
  char *name = NULL;
  int length;           /* length of symbol + 1 (characters) */
  ENTER(symbol_name);

  if (ptype[page] == SYMTYPE*2)
   {
    length = get_word(page, disp+1) - (BLK_OVHD + PTRSIZE);
    if (!(name = getmem(length))) getmem_error(rtn_name);
    get_sym(name, page, disp);
    name[length-1] = '\0';
   }
  return(name);
 } /* end of function:  char *symbol_name(page,disp) */

/************************************************************************/
/*                      Retrieve String Value                           */
/*                                                                      */
/* Purpose:  To fetch the value of a string from Scheme's memory        */
/*              and return it in a Lattice C string.                    */
/************************************************************************/
char *string_asciz(reg)
int reg[2];             /* register containing the string pointer */
 {
  char *name = NULL;    /* ASCIZ string to be returned to caller */
  int page,disp;        /* page and displacement components of string pointer */
  int length;           /* length of string + 1 (characters) */

  ENTER(string_asciz);

  page = CORRPAGE(reg[C_PAGE]);
  disp = reg[C_DISP];

  if (ptype[page] == STRTYPE*2)
   {
    length = get_word(page, disp+1);
    if (length < 0)
      length = length + BLK_OVHD + PTRSIZE;       /* adjust for small string */
    length = length - BLK_OVHD + 1;
    if (!(name = getmem(length))) getmem_error(rtn_name);
    get_str(name, page, disp);
    name[length-1] = '\0';
   }
  return(name);
 } /* end of function:  char *string_asciz(reg) */

/************************************************************************/
/*                           Release String                             */
/*                                                                      */
/* Purpose:  To release the memory allocated to a Lattice C character   */
/*              string.  If the string is null, the rlsmem is skipped.  */
/************************************************************************/
rlsstr(string)
char *string;
 {
  if (string) /* is the string allocated? */
   {
    if (rlsmem(string, strlen(string)+1)) /* release string's memory */
     {
      rlsmem_error("rlsstr"); /* if rlsmem error, print message */
     }
   }
 } /* end of function:  rlsstr(string) */

/************************************************************************/
/*              Convert Scheme Integer to C Long Integer                */
/*                                                                      */
/* Purpose:  To obtain the value of a Scheme integer (up to 32 bits)    */
/*              for manipulation by the Lattice C support routines.     */
/*                                                                      */
/* Description:  Given a Scheme pointer to an integer value, this       */
/*              routine returns the long integer corresponding to       */
/*              the value of the Scheme integer.                        */
/*                                                                      */
/* Calling Sequence:  stat = int2long(value, ptr)                       */
/*              where value - address of location where the long        */
/*                              integer result is to be stored.         */
/*                    ptr - a Scheme register address containing the    */
/*                              Scheme representation of the integer    */
/*                              value.                                  */
/*                    stat - return code; 0 = no errors, value returned */
/*                              1 = error, integer too large or ptr     */
/*                              was not an integer.                     */
/************************************************************************/
int2long(v, reg)
long *v;                /* pointer to location for long integer result */
int reg[2];             /* Scheme "register" containing the integer object */
 {
  int page;             /* Page field of register */

  page = CORRPAGE(reg[C_PAGE]);
  switch (ptype[page])
   {
    case FIXTYPE*2:  *v = get_fix(page, reg[C_DISP]);
                     return(0);
    case BIGTYPE*2:  return(ldlong(v, reg));
    default:  return(1);    /* Invalid argument type */
   } /* end:  switch (ptype[page]) */
 } /* end of function:  int2long(value, reg) */


/************************************************************************/
/*              Convert C Long Integer to Sheme Integer                 */
/*                                                                      */
/* Purpose:  To convert a Lattice C long integer value to the equivalent*/
/*              Scheme representation.                                  */
/*                                                                      */
/* Description:  Given a long integer value, this routine creates the   */
/*              equivalent Scheme integer object and returns it in the  */
/*              designated register.                                    */
/*                                                                      */
/* Calling Sequence:  long2int(reg, value)                              */
/*              where value - the Lattice C long integer value to be    */
/*                              converted to Scheme representation      */
/*                    reg - a Scheme register address to hold the       */
/*                              result.                                 */
/************************************************************************/
long2int(reg, value)
int reg[2];             /* Scheme register to hold the result */
long value;             /* the Lattice C long integer value to be converted */
 {
  /* determine if value can be represented as a fixnum */
  if (value <= 16383 && value >= -16384)
   {
    reg[C_PAGE] = SPECFIX*2;
    reg[C_DISP] = value & 0x7fff;
   }
  else /* value is a bignum */
   {
    enlarge(reg,value);
   }
 } /* end of function:  long2int(reg, value) */


/************************************************************************/
/*                          Append two lists                            */
/************************************************************************/
sappend(dest,src)
int dest[2];        /* Future result register */
int src[2];         /* List onto which to append */
 {
  int car[2];           /* Temporary car field pointer */
  int saved = FALSE;    /* Whether a list copy has been pushed */

  C_push(src);  C_push(src);
  mov_reg(tm2_reg, dest);  /* save destination operand, in case of error */
  while (dest[C_PAGE] && ptype[CORRPAGE(dest[C_PAGE])]==LISTTYPE*2)
   {
    if (s_break) restart(3);    /* shift-break? if so, start over */
    car[C_PAGE] = dest[C_PAGE];  car[C_DISP] = dest[C_DISP];
    take_car(car);
    cons(src, car, nil_reg);
    if (!saved)
     {
      C_push(src);  saved = TRUE;
     }
     else
     {
      asetcdr(tmp_reg, src);
     }
    tmp_page = src[C_PAGE];  tmp_disp = src[C_DISP];
    take_cdr(dest);
   }
  if (dest[C_PAGE])
   {
    if (saved)  C_pop(src);
    C_pop(src);
    C_pop(src);      /* Restore old SRC */
    set_src_err("APPEND", 2, tm2_reg, src);
    return(-1);
   }
  C_pop(dest);
  if (saved)
   {
    C_pop(tmp_reg);    /* Retrieve 2nd arg to append */
    asetcdr(src, tmp_reg);
   }
  C_pop(src);          /* Restore old SRC */
  return(0);
 }

/************************************************************************/
/*                         Get Current Time                             */
/************************************************************************/
ptime(reg)
int reg[2];
 {
  int i;        /* the usual index variable */
  int time[4];  /* array for result of get_time() call */

  get_time(time);  /* ask MS-DOS what time it is */

  /* cons the hours, minutes, seconds, hundredths into a list */
  tmp_page = SPECFIX*2;         /* set tmp_reg's tag=fixnum */
  reg[C_PAGE] = reg[C_DISP] = 0; /* set reg to nil */
  for (i = 3; i >= 0; i--)
   {
    tmp_disp = time[i];         /* move in immediate (fixnum) */
    cons(reg, tmp_reg, reg);    /* append value to front of list */
   }
 } /* end of function:  ptime(reg) */

/************************************************************************/
/*			Start PCS Engine Timer				*/
/************************************************************************/
cset_tim(value)
int value[2];		/* number to place in timer */
 {
  long int result;	/* temporary for long arith */
  unsigned hi,lo;	/* parts of 32-bit value for timer */
  int pg,ds;		/* page and displacement in register */
  pg=CORRPAGE(value[C_PAGE]); ds=value[C_DISP];
  hi = 0;
  switch (ptype[pg]>>1)
   {
    case BIGTYPE: switch (get_word(pg,ds+1))
		   {
		    case 8:  hi = get_word(pg,ds+6);
		    case 6:  lo = get_word(pg,ds+4);
			     break;
		    default: hi = lo = 65535; break;
		   }
		  break;
    case FIXTYPE: lo=ds; break;
    default:  set_src_err("%START-TIMER", 1, value);
   }
  #ifdef PROMEM
  /* Multiply the number of ticks by 1000 to get the number of */
  /* vm instructions to execute before timing out 	       */	
  result = ((hi * 65536) + lo) * 1000;
  lo = result & 0xffff;
  hi = result / 65536l;
  #endif
  if (!settimer(hi,lo))
   {
    set_error(1, "Timer already running", nil_reg);
    return(-1);
   }
  return(0);
 }

/************************************************************************/
/*		  Stop PCS Engine Timer and Return Value		*/
/************************************************************************/
crst_tim(value)
int value[2];
 {
  long int rsttimer();	   /* RSTTIMER returns a long int value */
  long int result;	   /* and puts it here */
  unsigned hi,lo;	   /* where it is broken into these pieces */
  int pg,ds;		   /* Page, displacement of new number */
  #ifdef PROMEM
  /* divide number of vm instructions remaining by 1000 to get the */
  /* number of engine ticks remaining				   */
  result = rsttimer() / 1000;
  #else
  result = rsttimer();
  #endif
  lo = result & 0xffff;
  hi = result / 65536l;
  if ((!hi) && (lo<16384))  alloc_fixnum(value,lo);
   else
   {
    alloc_block(value, BIGTYPE, (hi? 5 : 3));
    pg = CORRPAGE(value[C_PAGE]);
    ds = value[C_DISP]+3;
    put_byte(pg, ds++, 0);   /* Positive sign */
    put_word(pg, ds++, lo);
    if (hi)
     {
      ds++; put_word(pg, ds, hi);
     }
   }
  return(0);
 }

