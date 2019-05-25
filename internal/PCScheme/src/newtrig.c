#include "dos.h"
#include "math.h"

#define F_NEAR	  0x0001
#define F_PAD	  0x0008

#define RT_DOUBLE  3

typedef unsigned short WORD;		/* 16-bit unsigned value */

extern WORD _psp;			/* Lattice C variables */
extern WORD _tsize;

/* Note xwait and xbye are the actual addresses we'll jump to when we
   call XLI from the glue routine.  C calls the glue routine at the
   two entry points xli_wait and xli_bye.  These 2 routines set
   up the stack for calling xwait and xbye.  */
WORD xwait[2];				/* XLI entry points */
WORD xbye[2];

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
  double arg1;
  double arg2;
} parm_block;

char table[] =
/* 0	    2	    4	      6 	 8	  10	   12	  */
  "sqrt/sin/cos/tan/asin/acos/atan/atan2/exp/expt/ln/log10/log//";


void main(argc,argv)
int argc;
char *argv[];
{
  WORD psp;
/*WORD memsize;     */
  WORD buffer[2];
  struct SREGS segregs;
  int xli_wait();
  void xli_bye();

  /* Note PSP@ is not necessarily directly accessible in any
     Lattice C memory model. */
  psp = *(&_psp+1);			/* get seg addr of PSP */

  /* init file block */
  file_block.id = 0x4252;
  file_block.flags = F_NEAR+F_PAD;
  file_block.table[0] = (WORD) table;
  file_block.parm_block[0] = (WORD) &parm_block;
  segread(&segregs);
  file_block.table[1] = segregs.ds;
  file_block.parm_block[1] = segregs.ds;

  /* determine link address */
  buffer[0] = (WORD) &file_block;
  buffer[1] = segregs.ds;

  /* determine size to keep */
/*memsize = _tsize;   */	   /* done in glue routine for S Lattice */

  /* establish the link addresses between C and PCS */
  poke((int) psp, 0x5c, (char *) buffer, 4);  /* poke file block@ into PSP */
  peek((int) psp, 0x0a, (char *) xwait, 4);   /* get DOS ret@ */
  xbye[0] = xwait[0];
  xbye[1] = xwait[1];
  xwait[0] += 3;			      /* incr by 3 for normal call */
  xbye[0] += 6; 			      /* incr by 6 for termination */

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
		 pow(parm_block.arg1,parm_block.arg2);		 break;
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