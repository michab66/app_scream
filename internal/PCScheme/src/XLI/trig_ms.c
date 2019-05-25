/*
   This program is the Microsoft C (ver. 4.0) implementation of the
   transcendental functions.  The official implementation used by PC SCHEME
   was done under the Lattice C version.  This purpose of this program is to
   show you what is needed to communicate with XLI via MS C.  The
   lines containing comments that start out with @@ designate lines that
   will need to be modified by you when you use this template program
   for your own purpose.

   Note:  Before linking this program you will need to use the Macro
	  Assembler to assemble the file GLUE_MS.ASM.  The output of
	  the assembler, GLUE_MS.OBJ will then be linked with the object
	  of this file created by MSC.

   The command line to compile is:
      MSC trig_ms;

   The command line to assemble is:
     MASM glue_ms;

   The command line to link this program is:
     LINK trig_ms+glue_ms;
*/

#include "dos.h"
#include "math.h"
#include "stdlib.h"

#define F_NEAR	 0x0001  /*  Set model flag to near.		      USED */
#define F_INT	 0x0002  /*  Set integer flag to 16 bits.	  NOT-USED */
#define F_REL	 0x0004  /*  Set release env block by extern pgm flag. N-U */
#define F_PAD	 0x0008  /*  Set parm blocking flag to unblocked.     USED */

#define RT_INTEGER 0  /* Set return type to be an integer.  NOT-USED */
#define RT_BOOLEAN 1  /* Set return type to be boolean.     NOT-USED */
#define RT_STRING  2  /* Set return type to be a string.    NOT-USED */
#define RT_DOUBLE  3  /* Set the return type to be a float num. USED */

typedef unsigned short WORD;		/* 16-bit unsigned value */

/*
   Xwait and xbye will contain the actual addresses, XLI entry points, that
   we'll jump to when we call XLI so they need to be big enough to hold FAR
   pointers.
*/
WORD xwait[2];
WORD xbye[2];

WORD tsize;  /* Will contain the length of this program in paragraphs. */

struct xli_file_struct {
  WORD id;
  WORD flags;
  WORD table[2];			/* offset in 0, segment in 1 */
  WORD parm_block[2];
  WORD reserved[8];
} file_block;

struct xli_routine_struct {
  WORD select;
  WORD special_service;
  WORD ss_args[8];
  WORD reserved[8];
  WORD return_type;
  double return_value;
  double arg1;		/* @@ Add as many args as you need. */
  double arg2;
} parm_block;

char table[] =
/*
   @@ The following string contains the names of the functions that can
   be called from within SCHEME when this file is loaded thru XLI.

   0	1   2	3   4	 5    6    7	 8   9	  10 11    12
*/
  "sqrt/sin/cos/tan/asin/acos/atan/atan2/exp/expt/ln/log10/log//";


void main()
/*
  Within the main body of code the only portions that you will need to
  change are:
   1)  The value of the FILE-BLOCK.FLAGS and
   2)  The functions that you will call from within the CASE stmts.
*/
{
  struct SREGS segregs;

/*
   These function are defined in the assembly file GLUE_MS.ASM and are
   the functions that interface with Schemes XLI.
*/
  int xli_wait();
  void xli_bye();


  union {
    WORD far * psp_ptr;  /* declare as a far 32 bit pointer. */
    WORD half_ptrs[2];	 /* declare two 16 bit words for Seg & Off. */
  } ptr;

/*
   The following code will initialize the File Block as needed.
*/
  file_block.id = 0x4252;
  file_block.flags = F_NEAR+F_PAD;  /* @@ Set flags as appropiate. */

  segread(&segregs);  /* Obtain the register information. */
  file_block.table[0] = (WORD) table;
  file_block.table[1] = segregs.ds;
  file_block.parm_block[0] = (WORD) &parm_block;
  file_block.parm_block[1] = segregs.ds;


/*
   The word at the PSP+2 is set by MS C to contain the next available
   paragraph number after this program.  So if we subtract from this
   address the address of the PSP then we get the size of this program
   in paragraphs.
*/
  ptr.half_ptrs[0] = 2;    /* Set the offset to two, */
  ptr.half_ptrs[1] = _psp; /* and the segment to the PSP. */
  tsize = *(ptr.psp_ptr) - _psp;


  ptr.half_ptrs[0] = 0;    /* Set the offset to zero, */
  ptr.half_ptrs[1] = _psp; /* and the segment to the PSP. */

/*
   Establish the connection between C and the PSP.
*/
  ptr.psp_ptr[46] = (WORD far) &file_block; /* Set into the PSP the offset and */
  ptr.psp_ptr[47] = segregs.ds; 	    /* segment address of file_block */

  xwait[0] = ptr.psp_ptr[5];  /* Store into XWAIT the offset and */
  xwait[1] = ptr.psp_ptr[6];  /* segment address of DOS's terminate routine. */

  xbye[0] = xwait[0];  /* Copy the termination segment and */
  xbye[1] = xwait[1];  /* offset address, from above, into here. */

  xwait[0] += 3;  /* incr by 3 for normal call */
  xbye[0] += 6;   /* incr by 6 for termination */

  while (xli_wait()) {
    switch (parm_block.select) {
      case  0: parm_block.return_value = sqrt(parm_block.arg1);  break;
      case  1: parm_block.return_value = sin(parm_block.arg1);	 break;
      case  2: parm_block.return_value = cos(parm_block.arg1);	 break;
      case  3: parm_block.return_value = tan(parm_block.arg1);	 break;
      case  4: parm_block.return_value = asin(parm_block.arg1);  break;
      case  5: parm_block.return_value = acos(parm_block.arg1);  break;
      case  6: parm_block.return_value = atan(parm_block.arg1);  break;
      case  7: parm_block.return_value =
		  atan2(parm_block.arg1,parm_block.arg2);	 break;
      case  8: parm_block.return_value = exp(parm_block.arg1);	 break;
      case  9: parm_block.return_value =
		  pow(parm_block.arg1,parm_block.arg2); 	 break;
      case 10: parm_block.return_value = log(parm_block.arg1);	 break;
      case 11: parm_block.return_value = log10(parm_block.arg1); break;
      case 12: parm_block.return_value =
		  log(parm_block.arg1) / log(parm_block.arg2);	 break;
      default: ;
    } /* end switch */

    parm_block.return_type = RT_DOUBLE;

  } /* end while */

  xli_bye();

} /* end main */
