/*							=====> SLINK.C	     */
/* TIPC Scheme Runtime Support - Lattice C/Assembly Language Linkage
   (C) Copyright 1985 by Texas Instruments Incorporated.
   All rights reserved.

   Author:  John C. Jensen
   Installation:  Texas Instruments Incorporated, Dallas, Texas
   Division:  Central Research Laboratories
   Cost Center:  Computer Science Laboratory
   Project:  Computer Architecture Branch
   Date Written:  21 June 1985
   Last Modification:  18 October 1985
*/
#include "slink.h"
#include "ctype.h"
/************************************************************************/
/*	   Scheme to Lattice C (or assembly langauge) Interface 	*/
/*									*/
/* Purpose:  To provide the ability for a Scheme user to link to low	*/
/*		level routines not written in Scheme.			*/
/*									*/
/* Description:  This interface allows linkage to routines written in	*/
/*		Lattice C, or assembly langauge routines which use the	*/
/*		Lattice C linkage conventions.				*/
/*									*/
/* Limitations:  This interface may be used to call routines which	*/
/*		accept up to six (6) arguments of the Lattice C types:	*/
/*									*/
/*				int					*/
/*				long					*/
/*				char					*/
/*				char * (zero terminated string) 	*/
/*				float					*/
/*				double					*/
/*				unsigned (16 bit integer)		*/
/*									*/
/*		and which return a single Scheme value of one of the	*/
/*		following types:					*/
/*									*/
/*				fixnum (up to 32 bits)			*/
/*				flonum					*/
/*				character				*/
/*				string					*/
/*				't or '()                               */
/*									*/
/*		The C and/or assembly language routines may have side	*/
/*		effects and save state information, but they may not	*/
/*		have access to, or modify, the state of the Scheme	*/
/*		runtime (except through the passing of parameters).	*/
/*									*/
/* How to Use:								*/
/*									*/
/*	1.  Compile the routine you wish to call using the small model	*/
/*	    (small code, small data) Lattice C compiler.		*/
/*									*/
/*	2.  Modify this routine (SLINK.C) as follows, and compile it	*/
/*	    with the small model Lattice C compiler.			*/
/*									*/
/*	    a.	Add a declaration to indicate the type of the value to	*/
/*		be returned by your external routine (if no value is	*/
/*		declared, it will default to "int").  e.g.,             */
/*									*/
/*				char *dir1();				*/
/*		Here, the function "dir1" is declared to return         */
/*		(char *), which is the C representation for a character */
/*		string. 						*/
/*									*/
/*	    b.	Add an entry in the "switch" statement to call your     */
/*		routine.  You must explicitly indicate the type of each */
/*		argument you pass, as well as the value you wish to be	*/
/*		returned to Scheme.  e.g.,				*/
/*									*/
/*		   case 0:  RETURN_STRING( dir1( STRING(arg1) ) );	*/
/*									*/
/*		In this example, the function "dir1" is called using    */
/*		the first argument, which is a character string.  The	*/
/*		result of the "dir1" call is returned as a character    */
/*		string to Scheme.					*/
/*									*/
/*		Argument values may be obtained and converted to the	*/
/*		appropriate type using one of the following functions:	*/
/*									*/
/*				INTEGER(argn)				*/
/*				LONG_INTEGER(argn)			*/
/*				CHARACTER(argn) 			*/
/*				FLOAT(argn)				*/
/*				DOUBLE(argn)				*/
/*				STRING(argn)				*/
/*				UNSIGNED(argn)				*/
/*									*/
/*		Values must be returned using one of the following	*/
/*		functions:						*/
/*									*/
/*				RETURN_INTEGER(value);			*/
/*				RETURN_FLONUM(value);			*/
/*				RETURN_CHARACTER(value);		*/
/*				RETURN_STRING(value);			*/
/*				RETURN_T_OR_NIL(value); 		*/
/*									*/
/*	    c.	The case number in step b is the "function code" which  */
/*		is used to invoke the function.  The function code must */
/*		always be an integer and must be the first operand	*/
/*		passed to one of the "escape" Scheme functions.  The    */
/*		other operands follow the function code in the order	*/
/*		expected by the called routine.  The Scheme escape	*/
/*		functions are named %esc1, %esc2, .., %esc7, where the	*/
/*		number following the "%esc" designation is the total    */
/*		number of operands, INCLUDING the function code.	*/
/*		For example, to call the "dir1" function with one       */
/*		operand, we code:					*/
/*									*/
/*				(%esc2 0 "string")                      */
/*									*/
/*		where the first operand (0) is the function code and	*/
/*		"string" is the character string to be passed as the    */
/*		only argument.						*/
/*									*/
/*	    d.	To provide a more meaningful calling sequence and to	*/
/*		check for correct parameters, a Scheme routine should	*/
/*		be defined for each function to be called.  These	*/
/*		functions are normally placed in the SCHEME.INI file,	*/
/*		but may be installed "permanently" for a given          */
/*		application by converting them to fast-load format and	*/
/*		appending them to the FRONT of the COMPILER.FSL file,	*/
/*		which is automatically loaded when PCS begins.		*/
/*									*/
/*		A sample Scheme function for the "dir1" function is:    */
/*									*/
/*		    (define dir1					*/
/*		      (lambda (filespec)				*/
/*			 (if (string? filespec) 			*/
/*			    (%esc2 0 filespec)				*/
/*			    (error "Invalid Parameter to 'dir1'"        */
/*				   filespec)))) 			*/
/*									*/
/*		Here, the Scheme function "dir1" checks its argument    */
/*		to make sure that it's a string and, if it is, uses the */
/*		escape (%esc2) opcode to invoke the function.  If the	*/
/*		argument is not a string, an error is reported through	*/
/*		the Scheme error procedure.				*/
/*									*/
/*	    e.	The Scheme runtime must be re-linked with your Lattice	*/
/*		C and/or assembly language routines included.  Modify	*/
/*		the file SCHEME.LNK file (the link edit control file)	*/
/*		to include your modules by adding them to the end of	*/
/*		the "includes" (e.g., ...+YOUROBJ).                     */
/*									*/
/* Comments:								*/
/*									*/
/*	1.  The Scheme/C interface loses all typing information.  All	*/
/*	    arguments must be of the correct type or the results will	*/
/*	    be unpredictable.  Scheme functions (such as the "dir1"     */
/*	    example in d. (above)) should be used to force arguments	*/
/*	    to the expected types.					*/
/*									*/
/*	2.  There is a limited amout of procedure (code) and data	*/
/*	    memory available to user supplied functions.  Very large	*/
/*	    functions (in terms of either code space or data space)	*/
/*	    may cause linking to fail or Scheme to overflow its runtime */
/*	    stack.							*/
/************************************************************************/
link(result, fc, arg1, arg2, arg3, arg4, arg5, arg6)
long **result,*fc,*arg1,*arg2,*arg3,*arg4,*arg5,*arg6;
 {
  int ftncode;			/* function code */
  int status;			/* return code */
  int int_number;		/* software int number */
  char *t_;			/* local temporary */
  char* strng;			/* Used by function 8 */

  extern unsigned zapcurs;    	/* Denote cursor no longer in use */	
  extern int compact_every;	/* Indicates when to compact	  */

  /**************************************************************/
  /* a.  Declare each function which is to be called so that	*/
  /*	 its type is known by the Lattice C compiler.		*/
  /**************************************************************/
  char *dir1(); 	/* dir1 returns a character string */
  char *dir2(); 	/* dir2 returns a character string */
  long int freesp();
  long int filesize();
  char *chgdir();
#ifndef PROMEM
  long int isw_int();	/* software interrupt - return integer	*/
  int	   tsw_int();	/* software interrupt - return t or nil */
  char	  *ssw_int();	/* software interrupt - return string	*/
  double   fsw_int();	/* software interrupt - return float	*/
  char	  *flo2hex();	/* return hex representation of float	*/
#endif
  int hash();		/* return hash value of symbol		*/
  void randomiz();	/* seed random number generator 	*/
  #ifndef PROMEM
     unsigned xlidbg();	/* XLI debug hook			*/
  #endif

  ftncode = *fc;	/* make a local copy of the function code */

  strng = "0000000000000000";


  /**************************************************************/
  /* b.  Add a case entry in the following "switch" statement   */
  /*	 to call your external procedure.  The "case" number    */
  /*	 is the function code which you must use to invoke your */
  /*	 function.						*/
  /**************************************************************/
  switch (ftncode)
   {
    case 0:  /* function code 0:  find file match */
	     RETURN_STRING(dir1(STRING(arg1)));

    case 1:  /* function code 1:  step through directory, matching files */
	     RETURN_STRING(dir2());

    case 2:  /* function code 2:  bid another MS-DOS task */
	     status = bid(STRING(arg1),STRING(arg2),INTEGER(arg3),INTEGER(arg4));

	     if (status < 0) print_and_exit(
       "[VM FATAL ERROR] DOS-CALL error: unable to restore PC Scheme memory\n");
	     RETURN_INTEGER(status);

    case 3:  /* function code 3:  get the free space of heap */
	     RETURN_INTEGER(freesp());

    case 4:  /* function code 4:  scroll window up one line */
	     zscroll(INTEGER(arg1),INTEGER(arg2),INTEGER(arg3),
		     INTEGER(arg4),INTEGER(arg5));
	     RETURN_T_OR_NIL(1);

    case 5:  /* function code 5:  scroll window down one line */
	     scroll_d(INTEGER(arg1),INTEGER(arg2),INTEGER(arg3),
		      INTEGER(arg4),INTEGER(arg5));
	     RETURN_T_OR_NIL(1);

    case 6:  /* function code 6:  copy protect test - This function was  */
	     /* removed in version 2.0 and always returns True. 	 */
	     RETURN_T_OR_NIL(1);

    case 7: 
#ifndef PROMEM 
	    /* function code 7:  software interrupt */
	    int_number = *arg1;   /* 1st arg = interrupt number   */
	    status     = *arg2;   /* 2nd arg = return result type */
	    switch (status)
	    {
	    case 0: /* return integer result */
	      RETURN_INTEGER(isw_int(int_number,arg3,arg4,arg5,arg6));

	    case 1: /* return t or nil result */
	      RETURN_T_OR_NIL(tsw_int(int_number,arg3,arg4,arg5,arg6));

	    case 2: /* return string result */
	      RETURN_STRING(ssw_int(int_number,arg3,arg4,arg5,arg6));

	    case 3: /* return string result */
	      RETURN_FLONUM(fsw_int(int_number,arg3,arg4,arg5,arg6));

	    default: return(-1);  /* unrecognized return type */
	     }
#endif
    case 8:  /* function code 8: float->hex conversion */
	    RETURN_STRING(flo2hex(strng,arg1,INTEGER(arg2)));

    case 9:  /* function code 9: return hash value of symbol */
	    RETURN_INTEGER(hash(STRING(arg1),strlen(arg1)));

    case 10: /* function code 10: delete a file */
	     RETURN_INTEGER(delete(STRING(arg1)));

    case 11: /* function code 11: copy a file */
	     RETURN_INTEGER(copy_fil(STRING(arg1),STRING(arg2)));

    case 12: /* function code 12: rename files under current directory */
	     RETURN_INTEGER(rename(STRING(arg1),STRING(arg2)));

    case 13: /* function code 13: turn the cursor on */
	     zapcurs = 0;
	     zcuron();
	     RETURN_T_OR_NIL(1);

    case 14: /* function code 14: turn the cursor off */
	     zcuron(); zcuroff();
	     zapcurs = 1;
	     RETURN_T_OR_NIL(1);

    case 15: /* function code 15: get the file size */
	     RETURN_INTEGER(filesize(STRING(arg1)));

    case 16: /* function code 16: change current directory */
	     RETURN_STRING(chgdir(STRING(arg1)));

    case 17: /* function code 17: change current drive */
	     chgdrv(toupper(CHARACTER(arg1)));
	     RETURN_T_OR_NIL(1);
#ifndef PROMEM
    case 18: /* function code 18: XLI debug hook */
	     RETURN_INTEGER(xlidbg(INTEGER(arg1)));
#endif

    case 19: /* function code 19: unused */
	     return(-1);

    case 20: /* function code 20: seed random number generator */
	     randomiz(INTEGER(arg1));
	     RETURN_T_OR_NIL(1);

    case 21: /* function code 21: return compaction variable */
	     RETURN_INTEGER(compact_every);

    case 22: /* function code 22: set compaction variable */
	     compact_every = (INTEGER(arg1));
	     RETURN_INTEGER(compact_every);

    default:  return(-1); /* unrecognized function code */
   } /* end:  switch (ftncode) */
 } /* end of function:	link(result,fc,arg1,arg2,arg3,arg4,arg5,arg6) */

  /**************************************************************/
  /* Note:  If you wish, your Lattice C functions may be	*/
  /*	    included in this file following this message.	*/
  /**************************************************************/
