/*							=====> PROCIOSP.C  */
/* TIPC Scheme '84 Runtime Support - File Input/Output Support
   (C) Copyright 1985 by Texas Instruments Incorporated.
   All rights reserved.

   Author:  John C. Jensen
   Installation:  Texas Instruments Incorporated, Dallas, Texas
   Division:  Central Research Laboratories
   Cost Center:  Computer Science Laboratory
   Project:  Computer Architecture Branch
   Date Written:  4 February 1985
   Last Modification:
	14 Jan 1987 - dbs  Modified to allow for random i/o.
	16 Mar 1987 - tc   Dos I/O errors call DOS-ERR now.
	24 Nov 1987 - tc   Renamed from SIN_OUT.C to IOSUPORT.C
			   Combined routines from sprint.c,sread.c
			   get_port.c and zcio.c
*/
#include "scheme.h"
#include "sport.h"
#include "slist.h"

#define FILE_NOT_FOUND 2   /* MS-DOS error code */
#define NON_RESTART    1   /* Operation not restartable */

extern char decpoint;      /* Current decimal point character */
extern unsigned GC_ING;    /* Garbage collecting indicator */

char *getmem(); 	   /* Lattice C's memory allocation support */



/************************************************************************/
/*			    Get Port Object				*/
/*									*/
/* Purpose:  To determine is a register contains a valid port object	*/
/*		representation and to return the appropriate port	*/
/*		pointer in "tmp_reg".                                   */
/************************************************************************/
get_port(reg, mode)
int reg[2], mode;
 {
  int disp;		/* displacement component of a pointer */
  int page;		/* page number component of a pointer */

  /* fetch page and displacement portions of port pointer */
  page = CORRPAGE(reg[C_PAGE]);
  disp = reg[C_DISP];

  /* check to see if port pointer is nil-- if so, search fluid env */
  if (!page)
   {
    if (mode) intern (tmp_reg, "OUTPUT-PORT", 11);
    else      intern (tmp_reg, "INPUT-PORT", 10);

    /* search fluid environment for interned symbol */
    ASSERT(fluid_lookup(tmp_reg));
    page = CORRPAGE(tmp_page);
    disp = tmp_disp;
   } /* end:  if (!page) */

  /* At this point, the page, disp should point to a port, or the
     symbol 'console */
  if (ptype[page] != PORTTYPE*2)
   {
    if (CORRPAGE(CON_PAGE) != page || CON_DISP != disp) return(1);
    tmp_page = SPECPOR*2;
    tmp_disp = (mode ? OUT_DISP : IN_DISP);
   }
  else
   {
    tmp_page = ADJPAGE(page);
    tmp_disp = disp;
   }
  return(0);
 } /* end of function:	get_port(reg, mode) */


/************************************************************************/
/*				Open a Port				*/
/************************************************************************/
spopen(file, mode)
int file[2];			/* pathname, 'console, nil, or #<port> */
int mode[2];			/* 'read, 'write, 'append */
 {
  				/* extern int prn_handle; (tc) */
  int direction;	/* 'read, 'write, 'append code */
  int disp;		/* displacement component of a pointer */
  int handle;		/* handle assigned to file by open */
  int hsize;		/* high word of file size - dbs */
  int lsize;		/* low word of file size - dbs */
  int i;		/* our old favorite index variable */
  int len;		/* length of file's pathname (plus 1) */
  int page;		/* page number component of a pointer */
  int p_flags;		/* port flags */
  int retstat = 0;	/* the return status */
  int stat;		/* status returned from open request */
  char *string; 	/* file pathname buffer pointer */
  float fsize;		/* file size - dbs */

  ENTER(spopen);		    

  /* identify mode value */
  if ((direction = get_mode(mode)) == -1) goto src_err;

  page = CORRPAGE(file[C_PAGE]);
  disp = file[C_DISP];

  switch(ptype[page])
   {
    case STRTYPE*2:  len = get_word(page, disp+1);
		     if (len < 0)		  /* Adjust for small string */
		       len = len + BLK_OVHD;
		     else
		       len = len - BLK_OVHD;

		     if (!(string = getmem(len+1))) getmem_error(rtn_name);
		     get_str(string, page, disp);
		     string[len] = '\0';
		     for (i=0; i<len; i++)
			string[i] = toupper(string[i]);
		     switch (direction)
		      {
		       case READ: if ((stat = zopen(&handle,
							string, direction,
							&hsize,&lsize)))
				   {
open_error:			   
		                    rlsstr(string);
				    /* Call to dos_err will not return */
				    stat += (IO_ERROR_START - 1);
				    dos_err(NON_RESTART,stat,file);
				   }
				  break;
		       case WRITE:  if ((stat = zcreate(&handle, string)))
				      goto open_error;
				/*
				    if (((stat = strcmp(string,"PRN")) == 0) ||
					((stat = strcmp(string,"LST")) == 0))
				      prn_handle = handle;
				*/
				    break;
		       case APPEND:  if ((stat = zopen(&handle, string,
					direction,&hsize,&lsize)) == FILE_NOT_FOUND)
				      {
				       if((stat = zcreate(&handle, string)))
					 goto open_error;
				       break;
				      }
				     if (stat) goto open_error;
				  /* do
				      {
				       if (zread(handle, buffer, &length))
					 break;
				      } while (length);  */
				    if (((stat = strcmp(string,"PRN")) == 0) ||
					((stat = strcmp(string,"LST")) == 0))
				       break;
				 mov_fptr(handle);
				 fsize = (hsize * 65536) + lsize;
		      }
		     mov_reg(tmp_reg, file); /* save pointer to filename */
		     alloc_block(file, PORTTYPE, WINDSIZE+BUFFSIZE);
		     page = CORRPAGE(file[C_PAGE]);
		     disp = file[C_DISP];
		     zero_blk(page, disp);
		     if (direction == WRITE)
			put_word(page, disp+UL_LINE, 1); 
		     else
		      if (direction == APPEND)
			{	 /* update the chunk# and buffer position */
			 i = fsize / 256;
			 put_word(page, disp+UL_LINE, i + 1);
			 i = fsize - (i * 256);
			 put_word(page, disp+BUF_POS, i);
			 direction = WRITE; /* unsets read flag - dbs */
			}
		     put_word(page, disp+P_FLAGS, OPEN+direction);
		     put_word(page, disp+N_COLS, 80);
		     put_word(page, disp+HANDLE, handle);
		     put_word(page, disp+N_LINES, hsize);
		     put_word(page, disp+B_ATTRIB, lsize);
		     /* put pointer to pathname into port object */
		     put_ptr(page, disp+STR_PTR, tmp_page, tmp_disp);
		     rlsstr(string);  /* release pathname buffer */
		     break;

    case SYMTYPE*2:  if (file[C_PAGE] != CON_PAGE ||
			 file[C_DISP] != CON_DISP) goto src_err;
		     break;

    case PORTTYPE*2:  p_flags = get_word(page, disp+P_FLAGS);
		      if (p_flags & OPEN) break;

src_err:
    default:  set_src_err("OPEN-PORT", 2, file, mode);
	      retstat = -1;
   } /* end:  switch(ptype[page]) */
end_of_function:
  return(retstat);
 } /* end of function:	spopen(file, mode) */

/************************************************************************/
/*				Close a Port				*/
/************************************************************************/
spclose(port)
int port[2];		/* register containing port pointer */
 {
  int page;		/* page number component of a pointer */
  int p_flags;		/* port flags */
  int retstat = 0;	/* the return status */
  int stat;		/* status returned from open request */

  static int err_code[2] = {0, SPECFIX*2};  /* close error status */

  if (!get_port(port, 0))
   {
    page = CORRPAGE(tmp_page);
    p_flags = get_word(page, tmp_disp+P_FLAGS);
    if (p_flags & OPEN && !(p_flags & WINDOW))
     {
      /***** write EOF to file before closing	     *****/
      stat = 0x1A;	  /* ascii code of EOF character */
      retstat = 1;	  /* number of bytes to write	 */

      if ((p_flags & WRITE) || (p_flags & READ_WRITE))
       if ((stat = zwrite(get_word(page,tmp_disp+HANDLE),&stat,&retstat)))
        {
	 stat += (IO_ERROR_START - 1);
	 goto io_err;
        }	

      if ((stat = zclose(get_word(page, tmp_disp+HANDLE))))
       {
	stat += (IO_ERROR_START - 1);
io_err:
	/* We will not return from dos_err */
	dos_err(NON_RESTART,stat,port);
       }
      put_word(page, tmp_disp+P_FLAGS, p_flags & (! OPEN));
      put_word(page, tmp_disp+BUF_POS, BUFFSIZE);
     }
   }
  else
   {
    set_src_err("CLOSE-PORT", 1, port);
    retstat = -1;
   }
  return(retstat);
 } /* end of function:	spclose(port) */

/************************************************************************/
/*	    Local Support:  Determine Input/Output Mode Value		*/
/************************************************************************/
get_mode(reg, in_or_out)
int reg[2];		/* mode register ('read, 'write, 'append) */
int in_or_out;		/* 0 = input, 1 = output */
 {
  if (ptype[CORRPAGE(reg[C_PAGE])] == SYMTYPE*2)
   {
    intern(tmp_reg, "READ", 4);
    if (tmp_disp == reg[C_DISP] && tmp_page == reg[C_PAGE]) return(0);
    intern(tmp_reg, "WRITE", 5);
    if (tmp_disp == reg[C_DISP] && tmp_page == reg[C_PAGE]) return(1);
    intern(tmp_reg, "APPEND", 6);
    if (tmp_disp == reg[C_DISP] && tmp_page == reg[C_PAGE]) return(2);
   }
  return(-1);
 } /* end of function:	get_mode(reg, in_or_out) */

/************************************************************************/
/*                              Clear Window                            */
/************************************************************************/
clear_window(reg)
int reg[2];             /* register containing port pointer */
 {
  int b_attrib;         /* border attributes */
  int disp;             /* displacement component of a pointer */
  int n_cols;           /* number of columns in the window */
  int n_lines;          /* number of lines in the window */
  int page;             /* page number component of a pointer */
  int retstat = 0;      /* the return status */
  char *string;         /* buffer pointer for label's text */
  int t_attrib;         /* text attributes */
  int ul_col;           /* upper left corner's column number */
  int ul_line;          /* upper left corner's line number */

  char *string_asciz(); /* fetches characters of a string */

  ENTER(clear_window);

  get_port(reg,0);
  page = CORRPAGE(tmp_page);
  disp = tmp_disp;
  if (ptype[page] == PORTTYPE*2 &&
      get_byte(page, disp+P_FLAGS) & WINDOW)
   {
    pt_flds4(tmp_reg, &ul_line, &ul_col, &n_lines, &n_cols);
    t_attrib = get_word(page, disp+T_ATTRIB);
    b_attrib = get_word(page, disp+B_ATTRIB);
    zclear(ul_line, ul_col, n_lines, n_cols, t_attrib);
    if (b_attrib != -1)
     {
      tmp_page = get_byte(page, disp+STR_PTR);
      tmp_disp = get_word(page, disp+STR_PTR+1);
      string = string_asciz(tmp_reg);
      zborder(ul_line, ul_col, n_lines, n_cols, b_attrib, string);
      rlsstr(string);
     }
    /* put the cursor in the "home" position (upper left hand corner) */
    put_word(page, disp+CUR_LINE, 0);
    put_word(page, disp+CUR_COL, 0);
   }
  else
   {
    set_src_err("WINDOW-CLEAR", 1, reg);
    retstat = -1;
   }
  return(retstat);
 } /* end of function:  clear_window(reg) */

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
 printtxt(buf, makeflo(f,buf,0,outrange(f)));
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

/************************************************************************/
/*                 Write "GC On"Message to the who-line                 */
/************************************************************************/
gc_on()
 {
  int lcl_reg[2];
  char *text;
  char *string_asciz();

  GC_ING = 1;

  intern(lcl_reg, "PCS-GC-MESSAGE", 14);
  if (sym_lookup (lcl_reg, GNV_reg) &&
      (text = string_asciz(lcl_reg)))
   {
    who_write("\n");
    who_write(text);
    rlsstr(text);
   }
  else
   {
    who_write("\n * Garbage Collecting *");
   }
 } /* end of function:  gc_on() */

/************************************************************************/
/*               Un-Write "GC On"Message to the who-line                */
/************************************************************************/
gc_off()
 {

  GC_ING = 0;

  who_clear();
 } /* end of function:  gc_off() */
