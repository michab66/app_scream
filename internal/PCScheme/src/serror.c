/*							=====> SERROR.C      */
/* TIPC Scheme '84 Runtime Support - Error Processors
   (C) Copyright 1984,1985 by Texas Instruments Incorporated.
   All rights reserved.

   Author:  John C. Jensen
   Installation:  Texas Instruments Incorporated, Dallas, Texas
   Division:  Central Research Laboratories
   Cost Center:  Computer Science Laboratory
   Project:  Computer Architecture Branch
   Date Written:  13 June 1984
   Last Modification:
	(tc) 05 June 1986 - Set or Ref of Fluid variable which is not
			    defined in fluid environment is now non-
			    restartable from error processor or inspector.
        (rb) 21 Jan 1988  - getmem errors return to Scheme toplevel rather 
			    than aborting to DOS
        (tc) 16 Feb 1988  - added PRO_ERROR routine for protected mode
			    
*/
#include "ctype.h"
#include "scheme.h"

#include "slist.h"

static char digits[10]={'0','1','2','3','4','5','6','7','8','9'};
static char bdigits[10]={' ','1','2','3','4','5','6','7','8','9'};

/************************************************************************/
/*		 Wrong Number of Arguments to a Closure 		*/
/************************************************************************/
#define NUM_ARGS 16 /* offset of operand count in a closure object */
wrong_args(args_passed, closure)
int args_passed;		/* number of arguments passed */
int closure[2]; 		/* pointer to called closure object */
 {
  int args_expected;		/* the number of arguments expected */
/*%%int i;			  /* our old friend the index variable */*/
  int page,disp;		/* page/displacement parts of closure pointer */
/*%%long *this_reg;		  /* pointer to a register during fixup */*/
/*%%long unbound;		  /* pointer to the symbol ***unbound*** */*/

  static char *msg=
"Invalid argument count: Function expected xx argument(s)\nbut was called with yy as follows:";
/*	   11111111112222222222333333333344444444445555555 5556666666666777777777788888888889
 012345678901234567890123456789012345678901234567890123456 7890123456789012345678901234567890
*/
  static int insert_offset = 77; /* offset in msg for arguments passed */

  long make_ptr();		/* makes a Scheme pointer from page, disp */
  ENTER(wrong_args);

  /* determine the number of arguments expected */
  page = CORRPAGE(closure[C_PAGE]);
  disp = closure[C_DISP];
  if (ptype[page] == CONTTYPE*2)
   {
    args_expected = 1;
   }
  else
   {
    args_expected = get_fix(SPECFIX, get_word(page, disp+NUM_ARGS));
    if (args_expected < 0) args_expected = ~ args_expected;
   }

  /* Insert arguments expected into error message text */
  msg[42] = bdigits[args_expected / 10];
  msg[43] = digits[args_expected % 10];

  /* set Scheme variables to reflect error condition */
  arg_err(closure, args_passed, msg, insert_offset);

/***** Argument "fixup" code turned off 25 July 1985 (JCJ) *****
  /* if too few arguments, set undefined registers to "***unbound***" */
  if (args_expected > args_passed)
   {
    unbound = make_ptr(UN_PAGE, UN_DISP); /* create ***unbound*** pointer */
    this_reg = (&reg0)+args_expected; /* pointer to last undefined register */
    while (args_expected > args_passed)
     {
      *this_reg = unbound; /* set register contents to ***unbound*** */
      this_reg--;
      args_expected--;
     } /* end:	while (args_expected > args_passed) */
   } /* end:  if (args_expected > args_passed) */
***** Argument "fixup" code turned off 25 July 1985 (JCJ) *****/

 } /* end of function:	wrong_args(args_passed, closure) */

/************************************************************************/
/* Local Support-- Cons up "call" expression, output message text       */
/************************************************************************/
arg_err(ftn, args_passed, msg, offset)
int ftn[2];		/* function object */
int args_passed;	/* the count of arguments passed */
char *msg;		/* the error message text */
int offset;		/* offset in "msg" for inserting argument count */
 {
  int i;		/* index variable */
  long *this_reg;	/* pointer to a register during "consing" */

  /* insert count of arguments passed into the error message string */
  msg[offset] = bdigits[args_passed / 10];
  msg[offset+1] = digits[args_passed % 10];

  /* cons up the function and arguments into a list */
  this_reg = (&reg0)+args_passed; /* pointer to last argument register */
  tmp_page = tmp_disp = 0;	  /* initialize end-of-list to nil */
  for (i = 0; i < args_passed; i++, this_reg--)
   cons(tmp_reg, this_reg, tmp_reg);
  cons(tmp_reg, ftn, tmp_reg); /* put procedure object at front of list */

  /* set up the error message text and irritant */
  set_error(1, msg, tmp_reg);
 } /* end of function:	arg_err(ftn, args_passed, msg, offset) */

/************************************************************************/
/*	   Error-- Attempted to call a non-procedural object		*/
/************************************************************************/
not_proc(non_ftn_obj, args_passed)
int non_ftn_obj[2];	/* pointer to object in functional position */
int args_passed;	/* the number of arguments in the call */
 {
  static char *msg=
"Attempt to call a non-procedural object with xx argument(s) as follows:";
/*	   1111111111222222222233333333334444444444555555555566666666667
 012345678901234567890123456789012345678901234567890123456 7890123456789 */
  static int insert_offset = 45;  /* offset in msg for argument count */

  arg_err(non_ftn_obj, args_passed, msg, insert_offset);
 } /* end of function:	not_proc(non_ftn_obj, args_passed) */

/************************************************************************/
/*		   Error-- Symbol Not Fluidly Bound			*/
/************************************************************************/
not_fluidly_bound(page,disp,source)
int page;		/* symbol's page number */
int disp;		/* symbol's displacement */
int source[2];		/* register containing the value to be bound */
 {
  /* create pointer to symbol and set up error parameters */
  tmp_page = ADJPAGE(page);
  tmp_disp = disp;
  set_numeric_error(1, SET_FLUID_ERROR, tmp_reg);

 } /* end of function:	not_fluidly_bound(page,disp,source) */

/************************************************************************/
/*		   Error-- Symbol Not Globally Bound			*/
/************************************************************************/
not_globally_bound(page,disp,source)
int page;		/* symbol's page number */
int disp;		/* symbol's displacement */
int source[2];		/* register containing the value to be bound */
 {
  /* create pointer to symbol and set up error parameters */
  tmp_page = ADJPAGE(page);
  tmp_disp = disp;
  set_numeric_error(0, SET_GLOBAL_ERROR, tmp_reg);

 } /* end of function:	not_globally_bound(page,disp,source) */

/************************************************************************/
/*		   Error-- Symbol Not Lexically Bound			*/
/************************************************************************/
not_lexically_bound(page, disp)
int page;		/* symbol's page number */
int disp;		/* symbol's displacement */
 {
  /* create pointer to symbol and set up error parameters */
  tmp_page = ADJPAGE(page);
  tmp_disp = disp;
  set_numeric_error(0, SET_LEXICAL_ERROR, tmp_reg);

 } /* end of function:	not_lexically_bound(page, disp) */

/************************************************************************/
/*			   Error-- Symbol Not Bound			*/
/************************************************************************/
sym_undefined(page,disp,env,dest)
int page;		/* symbol's page number */
int disp;		/* symbol's displacement */
int env[2];		/* the environment supposed to contain said symbol */
int dest[2];		/* register into which the value was to be loaded */
 {
  int error_number;	/* numeric error code */
  int error_restart;	/* Can you resume from error? 0=yes,1=no */

  error_restart = 0;			/* Default to resumable */
  if (env == GNV_reg)
    error_number = REF_GLOBAL_ERROR;
  else
   {
    if (env == FNV_reg)
      {
	error_number = REF_FLUID_ERROR;
	error_restart = 1;		/* Can't continue from fluid error */
      }
    else
      error_number = REF_LEXICAL_ERROR;
   }

  /* create pointer to undefined symbol and set message parameters */
  tmp_page = ADJPAGE(page);
  tmp_disp = disp;
  set_numeric_error(error_restart, error_number, tmp_reg);

 } /* end of function:	sym_undefined(page,disp,env,dest) */

/************************************************************************/
/*			    getmem error				*/
/************************************************************************/
getmem_error(routine)
char *routine;
 {
  void rbrk();
  void force_re();
  
  printf("[VM INTERNAL ERROR] %s: getmem error\n", routine);
  printf("Press any key to return to Scheme toplevel.\n");      /*rb*/
  getch();
  rbrk();							/*rb*/
  force_re();	/* we won't return */				/*rb*/
  exit();
 }

/************************************************************************/
/*			    rlsmem error				*/
/************************************************************************/
rlsmem_error(routine)
char *routine;
 {
  printf("[VM INTERNAL ERROR] %s: rlsmem error", routine);
 }

/************************************************************************/
/*			set error condition				*/
/************************************************************************/
set_error(code, message, irritant)
int code;		/* error condition code */
char *message;		/* text of error condition */
int irritant[2];	/* object causing the error */
 {
  /* bind error code to the symbol |*error-code*| */
  C_push(tmp_reg);
  intern (tm2_reg, "*ERROR-CODE*", 12);
  tmp_page = SPECFIX*2;
  tmp_disp = code & 0x7fff;
  sym_bind(tm2_reg, tmp_reg, GNV_reg);

  /* bind error message text to the symbol |*error-message*| */
  intern (tm2_reg, "*ERROR-MESSAGE*", 15);
  alloc_string(tmp_reg, message);
  sym_bind(tm2_reg, tmp_reg, GNV_reg);

  /* bind irritant to the symbol |*irritant*| */
  C_pop(tmp_reg);
  intern (tm2_reg, "*IRRITANT*", 10);
  sym_bind(tm2_reg, irritant, GNV_reg);

 } /* end of function:	set_error(code, message, irritant) */


/************************************************************************/
/*		      set numeric error condition			*/
/************************************************************************/
set_numeric_error(code, error_number, irritant)
int code;		/* error condition code */
int error_number;	/* numeric error code for a given error condition */
int irritant[2];	/* object causing the error */
 {
  int lcl_reg[2];	/* a temporary register for fixnum values */

  /* bind error code to the symbol |*ERROR-CODE*| */
  intern (tm2_reg, "*ERROR-CODE*", 12);
  lcl_reg[C_PAGE] = SPECFIX*2;
  lcl_reg[C_DISP] = code & 0x7fff;
  sym_bind(tm2_reg, lcl_reg, GNV_reg);

  /* bind error message text to the symbol |*ERROR-MESSAGE*| */
  intern (tm2_reg, "*ERROR-MESSAGE*", 15);
  lcl_reg[C_DISP] = error_number;
  sym_bind(tm2_reg, lcl_reg, GNV_reg);

  /* bind irritant to the symbol |*IRRITANT*| */
  intern (tm2_reg, "*IRRITANT*", 10);
  sym_bind(tm2_reg, irritant, GNV_reg);

 } /* end of function:	set_numeric_error(code, error_number, irritant) */


/************************************************************************/
/*		Process Invalid Source Operand Condition		*/
/************************************************************************/
set_src_err(op, args, arg1, arg2, arg3, arg4)
char *op;		/* name of instruction failing */
int args;		/* number of arguments (operands) to instruction */
int arg1[2],arg2[2],arg3[2],arg4[2]; /* register argument(s) */
 {
  int i;		/* the usual index variable */
  int *reg_ptr;
/*%%int sym[2]; 	  /* local "register" for symbol name */*/
  mov_reg(tmp_reg, nil_reg);
  reg_ptr = (&arg1) + args - 1;
  for (i = 0; i < args; i++, reg_ptr--)
    cons(tmp_reg, *reg_ptr, tmp_reg);
  intern(tm2_reg, op, strlen(op));
  cons(tmp_reg, tm2_reg, tmp_reg);
  set_numeric_error(1, INVALID_OPERAND_ERROR, tmp_reg);
 } /* end of function:	set_src_err(op, args, arg1, arg2, arg3, arg4) */


#ifdef PROMEM
/************************************************************************/
/*		Process Protected Mode Error            		*/
/************************************************************************/
pro_error(rtn,fnc,errnum)
char *rtn,*fnc;
int  errnum;
  {
   char ch;
   printf("\nFatal Error during %s , performing %s - Error # %d",
	  rtn,fnc,errnum);
   printf("\nPress any key for attempt to SCHEME-RESET");
   ch = getch();
   printf("\n[Returning to top level]\n");
   force_reset();
  }
#endif
