/*							=====> STRACE.C      */
/* TIPC Scheme '84 Runtime Support - Driver
   (C) Copyright 1984, 1985, 1987 by Texas Instruments Incorporated.
   All rights reserved.

   Author:  John C. Jensen
   Installation:  Texas Instruments Incorporated, Dallas, Texas
   Division:  Central Research Laboratories
   Cost Center:  Computer Science Laboratory
   Project:  Computer Architecture Branch
   Date Written:  12 April 1984
   Last Modification:
	  11 February 1986 - Remainder Divide by Zero fix
			     (see also sinterp.asm)
      dbs 21 Oct 86 - changed %graphics to seven arguments
			     (see also sinterp.asm)
       rb 13 Mar 87 - added %xesc
*/
#include "scheme.h"

static char *opcodes[256] = {
/* 000 */ "load",       "ld-const",     "ld-imm",       "ld-nil",
	  "ld-local",   "ld-lex",       "ld-env",       "ld-global",
/* 008 */ "ld-fluid",   "ld-vec-s",     "ld-vec-l",     "ld-vec-r",
	  "st-local",   "st-lex",       "st-env",       "st-global",
/* 016 */ "st-fluid",   "st-vec-s",     "st-vec-l",     "st-vec-r",
	  "set-car!",   "set-cdr!",     "set-ref!",     "swap-ref!",
/* 024 */ "pop",        "push",         "drop",         "ld-global-r",
	  "(unused)",   "bind-fl",      "unbind-fl",    "define!",
/* 032 */ "jmp-s",      "jmp-l",        "j-nil-s",      "j-nil-l",
	  "jnnil-s",    "jnnil-l",      "jatom-s",      "jatom-l",
/* 040 */ "jnatom-s",   "jnatom-l",     "jeq-s",        "jeq-l",
	  "jneq-s",     "jneq-l",       "deref",        "ref",
/* 048 */ "call",       "call-tr",      "call/cc",      "call/cc-tr",
	  "call-cl",    "call-cl-tr",   "call/cc-cl",   "call/cc-cl-tr",
/* 056 */ "apply-cl",   "apply-cl-tr",  "execute",      "exit",
	  "close",      "drop-env",     "mk-hash-env",  "ld-fluid-r",
/* 064 */ "%%car",      "%%cdr",        "caar",         "cadr",
	  "cdar",       "cddr",         "caaar",        "caadr",
/* 072 */ "cadar",      "caddr",        "cdaar",        "cdadr",
	  "cddar",      "cdddr",        "cadddr",       "cons",
/* 080 */ "add",        "add-imm",      "sub",          "mul",
	  "mul-imm",    "div",          "div-imm",      "quotient",
/* 088 */ "remainder",  "%car",         "%cdr",         "random",
	  "<",          "<=",           "=",            ">",
/* 096 */ ">=",         "!=",           "max",          "min",
	  "eq?",        "eqv?",         "equal?",       "memq",
/* 104 */ "memv",       "member",       "reverse!",     "reverse",
	  "assq",       "assv",         "assoc",        "list",
/* 112 */ "append!",    "append",       "delq!",        "delete!",
	  "get-prop",   "put-prop",     "proplist",     "remprop",
/* 120 */ "list2",      "(unused)",     "(unused)",     "(unused)",
	  "(unused)",   "bitwise-xor",  "bitwise-and",  "bitwise-or",

/* 128 */ "atom?",      "closure?",     "code?",        "continuation?",
	  "even?",      "float?",       "fluid-bound?", "integer?",
/* 136 */ "null?",      "number?",      "odd?",         "pair?",
	  "port?",      "proc?",        "ref?",         "string?",
/* 144 */ "symbol?",    "vector?",      "zero?",        "negative?",
	  "positive?",  "abs",          "float",        "minus",
/* 152 */ "floor",      "ceiling",      "truncate",     "round",
	  "char?",      "env?",         "(unused)",     "(unused)",
/* 160 */ "ascii->char","char->ascii",  "(unused)",     "(unused)",
	  "(unused)",   "length",       "last-pair",    "substr",
/* 168 */ "alloc-vector","vector-size", "vector-fill",  "mk-pack-vector",
	  "substr-display","(unused)",  "%start-timer", "%stop-timer",
/* 176 */ "open-port",  "close-port",   "prin1",        "princ",
	  "print",      "newline",      "read",         "(unused)",
/* 184 */ "print-length","(unused)",    "read-line",    "read-atom",
	  "read-char",  "%transcript",  "read-char-ready?","fasl",
/* 192 */ "char=",      "char-equal?",  "char<",        "char-less?",
	  "char-upcase","char-downcase","string-length","string-ref",
/* 200 */ "string-set!","make-string",  "string-fill!", "str->sym",
	  "str->un-sym","sym->str",     "srch-next",    "srch-prev",
/* 208 */ "%make-window","%reify-port!","%reify-port",  "%clear-window",
	  "%save-window","%restore-window","%str-append","%graphics",
/* 216 */ "%reify",     "mk-env",       "env-parent",   "env-lookup",
	  "define-env", "push-env",     "drop-env",     "ld-env",
/* 224 */ "st-env",     "set-glob-env!","%reify!",      "obj-hash",
	  "obj-unhash", "%reify-stack", "%reify-stack!","set-file-position!",
/* 232 */ "%esc1",      "%esc2",        "%esc3",        "%esc4",
	  "%esc5",      "%esc6",        "%esc7",        "%xesc",
/* 240 */ "(unused)",   "(unused)",     "(unused)",     "(unused)",
	  "(unused)",   "(unused)",     "(unused)",     "%gc2",
/* 248 */ "%halt",      "%gc",          "ptime",        "reset",
	  "scheme-reset","clear-regs",  "(escape)",     "begin-debug"};

/*  Format Codes: */
#define NO_OPERANDS	0  /* no operands */
#define REG		1  /* reg */
#define R_R		2  /* reg,reg */
#define R_R_R		3  /* reg,reg,reg */

#define SB		4  /* short offset (signed) */
#define UB		5  /* short offset (unsigned) */
#define SW		6  /* long offset (signed) */

#define UB_R		7  /* byte (unsigned),reg */

#define R_SB		8  /* reg,short offset (signed) */
#define R_UB		9  /* reg,short offset (unsigned) */
#define R_SW		10 /* reg,long offset (signed) */

#define R_UB_R		11 /* reg,byte (unsigned),reg */
#define R_UW_R		12 /* reg,word (unsigned),reg */

#define R_UB_SB 	13 /* reg,byte (unsigned),byte (signed) */
#define R_SW_UB 	14 /* reg,word (signed),byte (unsigned) */
#define SW_SB_UB	15 /* word (signed),byte (signed),byte (unsigned) */

#define R_4		16 /* reg,reg,reg,reg */
#define R_5		17 /* reg,reg,reg,reg,reg */
#define R_6		18 /* reg,reg,reg,reg,reg,reg */
#define R_7		19 /* reg,reg,reg,reg,reg,reg,reg */

#define UB_R_VR 	20 /* length, reg, zero or more regs */

/* this array is indexed by the format codes just above */
static int n_ops[21] = {0,1,2,3,-1,-1,-1,-1,2,-1,-1,-1,-1,-1,-1,-1,4,5,6,7,-1};

static char format[256] = {
/* 000 */ R_R,		R_UB,		R_SB,		REG,
	  R_UB, 	R_UB_SB,	R_UB,		R_UB,
/* 008 */ R_UB, 	R_UB,		R_SW /*cheat*/, R_R,
	  R_UB, 	R_UB_SB,	R_UB,		R_UB,
/* 016 */ R_UB, 	R_UB_R, 	R_UW_R, 	R_R_R,
	  R_R,		R_R,		R_R,		R_R,
/* 024 */ REG,		REG,		UB,		R_R,
	  NO_OPERANDS,	UB_R,		UB,		R_UB,
/* 032 */ SB,		SW,		R_SB,		R_SW,
	  R_SB, 	R_SW,		R_SB,		R_SW,
/* 040 */ R_SB, 	R_SW,		R_SB,		R_SW,
	  R_SB, 	R_SW,		REG,		REG,
/* 048 */ SW_SB_UB,	SW_SB_UB,	SW_SB_UB,	SW_SB_UB,
	  R_UB, 	R_UB,		REG,		REG,
/* 056 */ R_R,		R_R,		REG,		NO_OPERANDS,
	  R_SW_UB,	UB,		REG,		R_R,
/* 064 */ R_R,		R_R,		R_R,		R_R,
	  R_R,		R_R,		R_R,		R_R,
/* 072 */ R_R,		R_R,		R_R,		R_R,
	  R_R,		R_R,		R_R,		R_R_R,
/* 080 */ R_R,		R_SB,		R_R,		R_R,
	  R_SB, 	R_R,		R_SB,		R_R,
/* 088 */ R_R,		REG,		REG,		REG,
	  R_R,		R_R,		R_R,		R_R,
/* 096 */ R_R,		R_R,		R_R,		R_R,
	  R_R,		R_R,		R_R,		R_R,
/* 104 */ R_R,		R_R,		REG,		REG,
	  R_R,		R_R,		R_R,		REG,
/* 112 */ R_R,		R_R,		R_R,		R_R,
	  R_R,		R_R_R,		REG,		R_R,
/* 120 */ R_R,		NO_OPERANDS,	NO_OPERANDS,	NO_OPERANDS,
	  NO_OPERANDS,	R_R,		R_R,		R_R,

/* 128 */ REG,		REG,		REG,		REG,
	  REG,		REG,		REG,		REG,
/* 136 */ REG,		REG,		REG,		REG,
	  REG,		REG,		REG,		REG,
/* 144 */ REG,		REG,		REG,		REG,
	  REG,		REG,		REG,		REG,
/* 152 */ REG,		REG,		REG,		REG,
	  REG,		REG,		NO_OPERANDS,	NO_OPERANDS,
/* 160 */ REG,		REG,		NO_OPERANDS,	NO_OPERANDS,
	  NO_OPERANDS,	REG,		REG,		R_R_R,
/* 168 */ REG,		REG,		R_R,		R_R_R,
	  R_5,		NO_OPERANDS,	REG,		REG,
/* 176 */ R_R,		REG,		R_R,		R_R,
	  R_R,		REG,		REG,		NO_OPERANDS,
/* 184 */ REG,		NO_OPERANDS,	NO_OPERANDS,	REG,
	  REG,		REG,		REG,		REG,
/* 192 */ R_R,		R_R,		R_R,		R_R,
	  REG,		REG,		REG,		R_R,
/* 200 */ R_R_R,	R_R,		R_R,		REG,
	  REG,		REG,		R_4,		R_4,
/* 208 */ REG,		R_R_R,		R_R,		REG,
	  REG,		R_R,		R_7,		R_7,
/* 216 */ R_R,		REG,		REG,		R_R,
	  R_R_R,	UB,		UB,		R_UB,
/* 224 */ R_UB, 	REG,		R_R_R,		REG,
	  REG,		REG,		R_R,		R_R_R,
/* 232 */ REG,		R_R,		R_R_R,		R_4,
	  R_5,		R_6,		R_7,		UB_R_VR,
/* 240 */ NO_OPERANDS,	NO_OPERANDS,	NO_OPERANDS,	NO_OPERANDS,
	  NO_OPERANDS,	NO_OPERANDS,	NO_OPERANDS,	NO_OPERANDS,
/* 248 */ NO_OPERANDS,	NO_OPERANDS,	REG,		NO_OPERANDS,
	  NO_OPERANDS,	NO_OPERANDS,	NO_OPERANDS,	NO_OPERANDS};

static int page,disp,display;

t_inst(_page, pc, run, _display)
int _page, *pc, run, _display;
 {
  int len = 3;			/* instruction length (number of bytes) */
  int op;
  int reg1,reg2,reg3;
  int nregs;			/* #regs in a variable-length instruction */
  int stat = 0; 		/* status returned from "interp" */

  disp = *pc;
  page = _page;
  display = _display;

  op = get_byte(page, disp);
  if (display) printf("\t\t\t\t%3x:%04x  %12s", page, *pc, opcodes[op]);
  reg1 = reg2 = reg3 = -1;
  switch(format[op])
   {
    case NO_OPERANDS:  /* no operands */
	     if (display) printf("\n");
	     len = 1;
	     break;

    case REG:  /* one register operand */
	     reg1 = get_reg(1);
	     fmt_regs(1);
	     len = 2;
	     break;

    case R_R:  /* two register operands */
	     reg1 = get_reg(1);
	     reg2 = get_reg(2);
	     fmt_regs(2);
	     break;

    case R_R_R:  /* three register operands */
	     reg1 = get_reg(1);
	     reg2 = get_reg(2);
	     reg3 = get_reg(3);
	     fmt_regs(3);
	     len = 4;
	     break;

    case R_4:  /* four register operands */
	     reg1 = get_reg(1);
	     reg2 = get_reg(2);
	     reg3 = get_reg(3);
	     fmt_regs(4);
	     len = 5;
	     break;

    case R_5:  /* five register operands */
	     reg1 = get_reg(1);
	     reg2 = get_reg(2);
	     reg3 = get_reg(3);
	     fmt_regs(5);
	     len = 6;
	     break;

    case R_6:  /* six register operands */
	     reg1 = get_reg(1);
	     reg2 = get_reg(2);
	     reg3 = get_reg(3);
	     fmt_regs(6);
	     len = 7;
	     break;

    case R_7:  /* seven register operands */
	     reg1 = get_reg(1);
	     reg2 = get_reg(2);
	     reg3 = get_reg(3);
	     fmt_regs(7);
	     len = 8;
	     break;

    case SB:  /* short offset (signed byte) */
	     if (display) printf("   %d\n", (get_w(1) << 8) >> 8);
	     len = 2;
	     break;

    case SW:  /* long offset (signed word) */
	     if (display) printf("   %d\n", get_w(1));
	     break;

    case UB:  /* unsigned short offset (byte) */
	     if (display) printf("   %d\n", get_b(1));
	     len = 2;
	     break;

    case UB_R:	/* unsigned short offset (byte) plus register */
	     reg1 = get_reg(2);
	     if (display) printf("   %d,R%d\n", get_b(1), reg1);
	     break;

    case R_SB:	/* one register plus short offset (signed) */
	     reg1 = get_reg(1);
	     if (display) printf("   R%d,%d\n", reg1, (get_b(2) << 8) >> 8);
	     break;

    case R_UB:	/* one register plus short offset (unsigned) */
	     reg1 = get_reg(1);
	     if (display) printf("   R%d,%d\n", reg1, get_b(2));
	     break;

    case R_SW:	/* one register plus long offset (signed) */
	     reg1 = get_reg(1);
	     if (display) printf("   R%d,%d\n", reg1, get_w(2));
	     len = 4;
	     break;

    case R_UB_R:  /* register, short offset (unsigned), register */
	     reg1 = get_reg(1);
	     reg2 = get_reg(3);
	     if (display) printf("   R%d,%d,R%d\n", reg1, get_b(2), reg2);
	     len = 4;
	     break;

    case R_UW_R:  /* register, long offset (unsigned), register */
	     reg1 = get_reg(1);
	     reg2 = get_reg(4);
	     if (display) printf("   R%d,%d,R%d\n", reg1, get_w(2), reg2);
	     len = 5;
	     break;

    case R_UB_SB:  /* register, unsigned byte, signed byte */
	     reg1 = get_reg(1);
	     if (display) printf("   R%d,%d,%d\n", reg1, get_b(2),
						(get_b(3) << 8) >> 8);
	     len = 4;
	     break;

    case R_SW_UB:  /* register, signed word, unsigned byte */
	     reg1 = get_reg(1);
	     if (display) printf("   R%d,%d,%d\n", reg1, get_w(2), get_b(4));
	     len = 5;
	     break;

    case SW_SB_UB:  /* signed word, signed byte, unsigned byte */
	     if (display) printf("   %d,%d,%d\n", get_w(1),
				(get_b(3) << 8) >> 8, get_b(4));
	     len = 5;
	     break;

    case UB_R_VR: /* unsigned length byte, register, zero or more registers */
	     len = get_b(1);	   /* length byte = #bytes in inst - 1) */
	     nregs = len - 2;	   /* # optional registers */
	     reg1 = get_reg(2);
	     if (nregs >= 1) reg2 = get_reg(3);
	     if (nregs >= 2) reg3 = get_reg(4);
	     if (display) {
	       printf("   %d,R%d\n",len,reg1);
	       if (nregs > 0) {
		 printf("\t\t\t\t\t\t      ");
		 disp += 2;	   /* move over opcode, length */
		 fmt_regs(nregs);  /* enough regs will give ugly wraparound */
	       } /* end if (nregs ...) */
	     } /* end if (display) */
	     len = len + 1;
	     break;

    default:  /* ? */
	     printf("t_inst:  invalid instruction format-- op=%02x\n",op);

   } /* end:  switch(format[op]) */

  if (run) {
    if (display) {
      /* dump the registers prior to execution */
      if (reg2 == reg1) reg2 = -1;
      if (reg3 == reg1 || reg3 == reg2) reg3 = -1;
      if (reg1 >= 0) prt_reg(reg1);
      if (reg2 >= 0) prt_reg(reg2);
      if (reg3 >= 0) prt_reg(reg3);
     } /* end:	if (display) */

    /* execute the instruction */
    stat = interp(pc, 1);

    if (display) {
      /* dump the registers after execution */
      if (reg1 >= 0) {
	printf("After execution:\n");
	prt_reg(reg1);
	if (reg2 >= 0) prt_reg(reg2);
	if (reg3 >= 0) prt_reg(reg3);
      } /* end:  if (reg1 >= 0) */
    } /* end:	if (display) */
  } /* end:  if (run) */
  else (*pc) += len;
  return(stat);
 } /* end of function:	t_inst(page,disp) */

/************************************************************************/
/*		   Format a Series of Register Operands 		*/
/************************************************************************/
fmt_regs(n)
int n;		/* the number of register operands */
 {
  int i;	/* the usual index variable */
  char *comma;	/* text used to separate assembly language operands */
  if (display)
   {
    comma = "   ";  /* output blanks to separate instruction, first operand */
    for (i = 1; i <= n; i++)
     {
      printf("%sR%d", comma, get_reg(i)); /* print the next register */
      comma = ",";  /* subsequent items separated by a comma */
     }
    printf("\n");    /* output a newline character */
   }
 } /* end of function:	fmt_regs(n) */

/************************************************************************/
/*			Return Register Number				*/
/************************************************************************/
get_reg(offset)
int offset;
 {
  return(get_byte(page, disp+offset) >> 2);
 } /* end of function:	get_reg(offset) */

/************************************************************************/
/*			Return Word Value				*/
/************************************************************************/
get_w(offset)
int offset;
 {
  return(get_word(page, disp+offset));
 } /* end of function:	get_reg(offset) */

/************************************************************************/
/*			Return Byte Value				*/
/************************************************************************/
get_b(offset)
int offset;
 {
  return(get_byte(page, disp+offset));
 } /* end of function:	get_reg(offset) */

/************************************************************************/
/*   "Disassemble" a Scheme Instruction for Error Message *IRRITANT*    */
/*									*/
/* Note:  This routine works for instructions with only registers for	*/
/*	  operands.  Immediates, offsets, etc., will cause a list to	*/
/*	  be created with only the function name in the first position. */
/*									*/
/*	  The "offset" operand is the absolute displacement of the      */
/*	  instruction in the page containing the current code block,	*/
/*	  not the displacement relative to the beginning of the code	*/
/*	  block.							*/
/************************************************************************/
disassemble(function,offset)
char *function; 	/* string containing function name */
int offset;		/* offset in PAGE containing current code block
			   of the instruction to be disassembled */
 {
  int reg_addr[10];	/* register addresses of the instruction's operands */
  int i;		/* index variable */
  int number_of_operands; /* number of operands for the instruction */
  int op;		/* opcode for the instruction */
  static int fix_reg[2] = {0,SPECFIX*2}; /* special "register" for immediates */

  /* determine characteristics of the instruction with which we're dealing */
  page = CORRPAGE(CB_pag);
  op = get_byte(page,offset++);
  tmp_page = tmp_disp = 0;
  if ((number_of_operands = n_ops[format[op]]) > 0)
   {
    /* compute the register address for each operand */
    for (i = 0; i < number_of_operands; i++)
      reg_addr[i] = get_byte(page,offset++) + (int)(&reg0);

    /* if last operand is an immediate operand, phoney up a register for it */
    if (format[op] == R_SB)
     {
      reg_addr[i-1] = (int) fix_reg;
      fix_reg[C_DISP] = (get_byte(page,offset-1)<<8)>>8;
     }

    /* cons up argument list */
    for (i = number_of_operands - 1; i >= 0; i--)
      cons(tmp_reg, reg_addr[i], tmp_reg);
   }

  /* create a symbol for the function name and cons on front of argument list */
  intern(tm2_reg, function, strlen(function));
  cons(tmp_reg, tm2_reg, tmp_reg);
 } /* end of function:	disassemble(function,offset) */

/************************************************************************/
/*		     Display Accounting Information			*/
/************************************************************************/
accounting()
 {
  extern int gc_count;		/* garbage collector invocation count */
  extern long stk_in, stk_out;	/* bytes transfered to/from the stack */
  ENTER(accounting);

  printf("\nGarbage collector invoked %d times\n", gc_count);

  printf("\n%9ld bytes transfered from stack to heap\n%9ld%s",
	 stk_out, stk_in, " bytes transfered from heap to stack\n");
 } /* end of function:	accounting() */
