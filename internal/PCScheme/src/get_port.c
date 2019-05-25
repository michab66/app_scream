/*							=====> GET_PORT */
/* Copyright 1985 by Texas Instruments Incorporated.
   All Rights Reserved.

   Author:  John C. Jensen
   Installation:  Texas Instruments Incorporated, Dallas, Texas
   Project:  Computer Architecture Branch
   Date Written:  January 1985
   Last Modification:  18 October 1985
*/

#include "ctype.h"
#include "scheme.h"

char *getmem(); 	/* Lattice C's memory allocation function */

/************************************************************************/
/*			    Determine Port				*/
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

/***** Code turned off 17 May 1985 (JCJ) *****
/************************************************************************/
/*			file-exists? Predicate				*/
/*									*/
/* Purpose:  To support the "file-exists?" predicate for the Scheme     */
/*		interpreter.						*/
/*									*/
/* Author:  John C. Jensen						*/
/* Installation:  Texas Instruments Incorporated, Dallas, Texas 	*/
/* Department:	Computer Science Laboratory				*/
/* Project:  Computer Architecture Branch				*/
/* Date Written:  17 January 1985					*/
/* Last Modification:  17 January 1985					*/
/*									*/
/* Calling Sequence:  file_exists(reg)					*/
/*			where reg - VM register containing the string	*/
/*					which is the filename of the	*/
/*					file in question.  The contents */
/*					of this register is replaced	*/
/*					with the 't if the file exists  */
/*					or 'nil if it does not.         */
/************************************************************************/
file_exists(reg)
int reg[2];		/* parameter register */
 {
  char *buffer; 	/* character buffer for filename */
  int disp;		/* displacement component of a pointer */
  int handle;		/* file "handle" */
  int len;		/* length of the file name (bytes) */
  int page;		/* page number component of a pointer */
  int retstat = 0;	/* return status */
  int type;		/* type code of a pointer */
  ENTER (file_ex);

  page = CORRPAGE(reg[C_PAGE]);
  disp = reg[C_DISP];
  type = ptype[page];

  switch (type)
   {
    case STRTYPE*2:  len = get_word(page, disp+1) - BLK_OVHD;
		     if (!(buffer = getmem(len+1))) getmem_error(rtn_name);
		     get_str(buffer, page, disp);
		     buffer[len] = '\0';
		     if (zopen(&handle, buffer, 0, &retstat, &retstat))
		      { /* open failed-- file does not exist */
		       reg[C_PAGE] = reg[C_DISP] = 0;
		      }
		     else
		      { /* open succeeded-- close file and return 't */
		       zclose (handle);
		       reg[C_PAGE] = T_PAGE*2;
		       reg[C_DISP] = T_DISP;
		      }
		     rlsstr(buffer);
		     break;

    case SYMTYPE*2:  if (CON_PAGE == reg[C_PAGE] && CON_DISP == reg[C_DISP])
		      {
		       reg[C_PAGE] = T_PAGE*2;
		       reg[C_DISP] = T_DISP;
		       break;
		      }

    default:	     /* invalid source operand */
		     set_src_err("FILE-EXISTS?", 1, reg);
		     retstat = -1;
   } /* end:  case (type) */
  return(retstat);
 } /* end of function:	file_exists(reg) */
***** Code turned off 17 May 1985 (JCJ) *****/
