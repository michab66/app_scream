/*							=====> SLINK.H       */
/* PC Scheme Lattice C Macros to Support Scheme to C Interface
   Copyright 1985 by Texas Instruments Incorporated.
   All Rights Reserved.

   Author:  John C. Jensen
   Installation:  Texas Instruments Incorporated, Dallas, Texas
   Division:  Central Research Laboratories
   Cost Center:  Computer Science Laboratory
   Project:  Computer Architecture Branch
   Date Written:  22 June 1985
   Last Modification:  23 June 1985

   Purpose:  The macros within this module provide the capability to
		fetch values passed from the Scheme Runtime and return
		values to the Scheme Runtime.

   Description:  For a description of parameter passing conventions, see the
		module header in the file SLINK.C.
*/

#define INTEGER(x) *((int *)x)
#define LONG_INTEGER(x) *x
#define FLOAT(x) *((float *)x)
#define DOUBLE(x) *((double *)x)
#define CHARACTER(x) *((char *)x)
#define STRING(x) ((char*)x)

#define RETURN_NOVALUE() return(0)
#define RETURN_T_OR_NIL(x) **result = (x); return(1)
#define RETURN_INTEGER(x) **result = (x); return(2)
#define RETURN_FLONUM(x) *((double *) *result) = (x); return(3)
#define RETURN_CHARACTER(x) *((char *) *result) = (x); return(4)
#define RETURN_STRING(x) t_=(x);if(t_){*result=(long *)t_;return(5);}else{**result=0;return(1);}
