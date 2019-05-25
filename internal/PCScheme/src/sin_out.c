/*							=====> SIN_OUT.C   */
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
*/
#include "scheme.h"
#include "sport.h"
#include "slist.h"

#define FILE_NOT_FOUND 2	/* MS-DOS error code */
#define NON_RESTART    1	/* Operation not restartable */

char *getmem(); 	/* Lattice C's memory allocation support */

/************************************************************************/
/*				Open a Port				*/
/************************************************************************/
spopen(file, mode)
int file[2];			/* pathname, 'console, nil, or #<port> */
int mode[2];			/* 'read, 'write, 'append */
 {
  extern int prn_handle;/* handle assigned to printer *** JHAO ***/
/*%%char buffer[BUFFSIZE];/* read buffer for positioning at end of file */*/
  int direction;	/* 'read, 'write, 'append code */
  int disp;		/* displacement component of a pointer */
  int handle;		/* handle assigned to file by open */
  int hsize;		/* high word of file size - dbs */
  int lsize;		/* low word of file size - dbs */
  int i;		/* our old favorite index variable */
  int len;		/* length of file's pathname (plus 1) */
/*%%int length; 	  /* number of characters read */*/
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
				    if (((stat = strcmp(string,"PRN")) == 0) ||
					((stat = strcmp(string,"LST")) == 0))
				      prn_handle = handle;
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

/***** Code for 'read' turned off 17 July 1985 (JCJ) *****
/************************************************************************/
/*			Read an S-Expression				*/
/*									*/
/* Purpose:  Scheme interpreter support to read an s-expression from	*/
/*		a port. 						*/
/************************************************************************/
spread(reg)
int reg[2];
 {
  int retstat = 0;		/* the return status */

  if (!get_port(reg, 0))
   {
    sread(reg, CORRPAGE(tmp_page), tmp_disp);
   }
  else
   {
    set_src_err("READ", 1, reg);
    retstat = -1;
   }
  return(retstat);
 } /* end of function:	spread(reg) */
***** Code for 'read' turned off 17 July 1985 (JCJ) *****/
/********* The following codes are recoded in assembly
/************************************************************************/
/*			   Read an Atom 				*/
/*									*/
/* Purpose:  Scheme interpreter support to read an atom from a port.	*/
/************************************************************************/
srd_atom(reg)
int reg[2];
 {
  int retstat;			/* the return status */

  if (!get_port(reg, 0))
   {
    retstat = sread_atom(reg, CORRPAGE(tmp_page), tmp_disp);
   }
   else
   {
    set_src_err("READ-ATOM", 1, reg);
    retstat = -1;
   }
  return(retstat);
 } /* end of function:	srd_atom(reg) */

/************************************************************************/
/*		Print an S-Expression (w/ slashification)		*/
/*									*/
/* Purpose:  Scheme interpreter support to output an s-expression to	*/
/*		a port. 						*/
/************************************************************************/
spprin1(value,port)
int value[2];		/* value to be printed */
int port[2];		/* register containing port pointer */
 {
  int retstat = 0;	/* the return status */

  if (!get_port(port, 1))
   {
    sprint(CORRPAGE(value[C_PAGE]), value[C_DISP],
		CORRPAGE(tmp_page), tmp_disp, TRUE, TRUE, FALSE);
    value[C_PAGE] = NPR_PAGE*2;
    value[C_DISP] = NPR_DISP;
   }
  else
   {
    set_src_error("WRITE", 2, value, port);
    retstat = -1;
   }
  return(retstat);
 } /* end of function:	spprin1(value,port) */

/************************************************************************/
/*		Print an S-Expression (w/o slashification)		*/
/*									*/
/* Purpose:  Scheme interpreter support to output an s-expression to	*/
/*		a port. 						*/
/************************************************************************/
spprinc(value,port)
int value[2];		/* value to be printed */
int port[2];		/* register containing port pointer */
 {
  int retstat = 0;	/* the return status */

  if (!get_port(port, 1))
   {
    sprint(CORRPAGE(value[C_PAGE]), value[C_DISP],
		CORRPAGE(tmp_page), tmp_disp, FALSE, TRUE, FALSE);
    value[C_PAGE] = NPR_PAGE*2;
    value[C_DISP] = NPR_DISP;
   }
  else
   {
    set_src_err("DISPLAY", 2, value, port);
    retstat = -1;
   }
  return(retstat);
 } /* end of function:	spprinc(value,port) */

/************************************************************************/
/*		Print an S-Expression (w/ spacing control)		*/
/*									*/
/* Purpose:  Scheme interpreter support to output an s-expression to	*/
/*		a port. 						*/
/************************************************************************/
spprint(value,port)
int value[2];		/* value to be printed */
int port[2];		/* register containing port pointer */
 {
  int retstat = 0;	/* the return status */

  if (!get_port(port, 1))
   {
    /* print a newline */
    sprint(SPECCHAR, '\n',
		CORRPAGE(tmp_page), tmp_disp, FALSE, TRUE, FALSE);
    /* print the s-expression with slashification */
    sprint(CORRPAGE(value[C_PAGE]), value[C_DISP],
		CORRPAGE(tmp_page), tmp_disp, TRUE, TRUE, FALSE);
    /* print a space */
    sprint(SPECCHAR, ' ',
		CORRPAGE(tmp_page), tmp_disp, FALSE, TRUE, FALSE);
    value[C_PAGE] = NPR_PAGE*2;
    value[C_DISP] = NPR_DISP;
   }
  else
   {
    set_src_err("PRINT", 2, value, port);
    retstat = -1;
   }
  return(retstat);
 } /* end of function:	spprint(value,port) */

/************************************************************************/
/*			Print a "newline" Character                     */
/*									*/
/* Purpose:  Scheme interpreter support to output a newline character	*/
/*		to a port.						*/
/************************************************************************/
spnewlin(port)
int port[2];		/* register containing port pointer */
 {
  int retstat = 0;	/* the return status */

  if (!get_port(port, 1))
   {
    /* print a newline */
    sprint(SPECCHAR, '\n',
		CORRPAGE(tmp_page), tmp_disp, FALSE, TRUE, FALSE);
   }
  else
   {
    set_src_err("NEWLINE", 1, port);
    retstat = -1;
   }
  return(retstat);
 } /* end of function:	spnewlin(port) */

/************************************************************************/
/*		Find Print-length of an S-Expression			*/
/*									*/
/* Purpose:  Scheme interpreter support to determine the print length	*/
/*		of a Scheme object.					*/
/************************************************************************/
prt_len(value)
int value[2];		/* value (not) to be printed */
 {
  int len;		/* length of object */

  len = sprint(CORRPAGE(value[C_PAGE]), value[C_DISP],
		OUT_PAGE, OUT_DISP, FALSE, FALSE, TRUE);
  value[C_PAGE] = SPECFIX*2;
  value[C_DISP] = len;
 } /* end of function:	prt_len(value) */
*********************************************************/

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
