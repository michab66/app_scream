/*---------------------------------------------------------*/
/*	 PC Scheme 3.0 Transcendental Function Support	   */
/*  (c) Copyright 1987 by Texas Instruments Incorporated   */
/*		       All Rights Reserved.		   */
/*---------------------------------------------------------*/

/*
   This program is a Large Model Lattice C (version 3.0) implementation of 
   the transcendental functions in PC Scheme, version 3, and is the one
   officially supported. The associated assembly language file GLUE_LLC.ASM 
   is required set up the "wait" and "bye" routines.

   To build TRIG_LLC.EXE, perform the following steps; you may need to 
   substitute directory names and set your path accordingly.

	lc  -ml trig_llc
	masm glue_llc;
	link  \lc\l\c+trig_llc+glue_llc,trig_llc,,\lc\l\lcm+\lc\l\lc
*/


#include "dos.h"
#include "math.h"

#define RT_DOUBLE  3	        /* Designates return value of double float */

typedef unsigned long DWORD;	/* 32-bit unsigned value */
typedef unsigned short WORD;	/* 16-bit unsigned value */

extern char *_psp;		/* Lattice C variable - ptr to psp address */
extern WORD _tsize;		/* Lattice C variable - size of program    */

/*
Note xwait and xbye are the actual addresses we'll jump to when we call XLI 
from the glue routine. We call the glue routine at the two entry points xli_wait
& xli_bye.  These 2 routines set up the stack for calling xwait and xbye.  
*/
WORD xwait[2];			/* XLI entry points */
WORD xbye[2];

struct xli_file_struct {
  WORD id;
  WORD flags;
  char *table;			/* pointer to lookup table    */
  char *parm_block;		/* pointer to parameter block */
  WORD reserved[8];
} file_block;

struct xli_routine_struct {
  WORD select;
  WORD special_service;
  WORD ss_args[8];
  WORD reserved[8];
  WORD return_type;
  double *return_value;		/* return value = pointer to double float */
  DWORD  dummy;			/* dummy out rest of return value field   */
  double *arg1;			/* pointer to argument 1, a double float  */
  double *arg2;			/* pointer to argument 2, a double float  */
} parm_block;

char table[] =			
/* 0	    2	    4	      6 	 8	  10	   12	  */
  "sqrt/sin/cos/tan/asin/acos/atan/atan2/exp/expt/ln/log10/log//";


void main(argc,argv)
int argc;
char *argv[];
{
  int xli_wait();		/* xli glue routines		      */
  void xli_bye();

  char *buffer;			/* temp to hold address of file block */

  /* initialize the file block */
  file_block.id = 0x4252;		    /* identify as xli routines   */
  file_block.flags = 0;			    /* far model, don't pack args */
  file_block.table = table;		    /* address of lookup table    */
  file_block.parm_block = &parm_block;	    /* address of parameter block */

  buffer = (char *) &file_block;	    /* hold address of file block,*/
  movmem(&buffer, (_psp+0x5c), 4);	    /* then move it into PSP+0x5C */

  movmem((_psp+0xa), (char *) &xwait, 4);   /* get callers return address */
  xbye[0] = xwait[0];			    /* into xwait and xbye	  */
  xbye[1] = xwait[1];
  xwait[0] += 3;			    /* calc normal return address */
  xbye[0] += 6; 			    /* calc termination address   */

  /* 
     Initialization complete - return to xli. The following loop will execute
     until xli gives us a non-zero value
  */
  while (xli_wait()) {	
    parm_block.return_value = parm_block.arg1;  /* use arg1 for return value */
    switch (parm_block.select) {
      case  0: *parm_block.return_value = sqrt(*parm_block.arg1);  break;
      case  1: *parm_block.return_value = sin(*parm_block.arg1);   break;
      case  2: *parm_block.return_value = cos(*parm_block.arg1);   break;
      case  3: *parm_block.return_value = tan(*parm_block.arg1);   break;
      case  4: *parm_block.return_value = asin(*parm_block.arg1);  break;
      case  5: *parm_block.return_value = acos(*parm_block.arg1);  break;
      case  6: *parm_block.return_value = atan(*parm_block.arg1);  break;
      case  7: *parm_block.return_value =
		 atan2(*parm_block.arg1,*parm_block.arg2);	   break;
      case  8: *parm_block.return_value = exp(*parm_block.arg1);   break;
      case  9: *parm_block.return_value =
		 pow(*parm_block.arg1,*parm_block.arg2);	   break;
      case 10: *parm_block.return_value = log(*parm_block.arg1);   break;
      case 11: *parm_block.return_value = log10(*parm_block.arg1); break;
      case 12: *parm_block.return_value =
		 log(*parm_block.arg1) / log(*parm_block.arg2);	   break;
      default: ;
    } /* end switch */
    parm_block.return_type = RT_DOUBLE;  /* return type = double float */
  } /* end while */

  xli_bye();				    /* terminate xli routine */

} /* end main */
