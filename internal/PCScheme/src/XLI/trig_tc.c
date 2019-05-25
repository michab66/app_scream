/*
   This program is the TURBO C (ver. 1.0) implementation of the
   transcendental functions.  The official implementation used by SCHEME was
   done under the Lattice C version.  This purpose of this program is to
   show you what is needed to communicate with XLI via TURBO C.  The
   lines containing comments that start out with @@ designate lines that
   will need to be modified by you when you use this template program
   for your own purpose.

   Note:  In order to compile this program you will need to have a path
   set up to get to the Macro Assembler.

   The command line to compile is:
     TCC -ID:\TURBOC\INCLUDE -LD:\TURBOC\LIB -B -c TRIG_TC

   The command line to link this program is:
     TLINK d:\turboc\lib\c0new.obj TRIG_TC,,,d:\turboc\lib\emu
	   d:\turboc\lib\maths d:\turboc\lib\cs

   All pathnames will need to be changed to reflect your directories.  The
   command for the link needs to be on one line only.  The file C0NEW.OBJ
   was my changed copy of the C0S.OBJ file; the changes you need to make to
   it are described below.

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
   _tsize is set to the size of the program from within the Start Up Code
   file called C0.ASM.	You need to modify this file in order for this
   variable to be set.

   The file C0.ASM is supplied to you on the TURBO C diskette.	It is
   in assembly source form so that you can modify the Start Up Code to
   do what you need it to do.  In this case we will make the following
   changes to capture the in-memory size of this file in paragrahps.

   The following two pieces of code are extracted out of the file
   C0.ASM.  They show you where the changes need to be made so that _tsize
   will contain the size of this, or your, program.  Only two lines need
   to be added to the C0.ASM file.  You will then need to execute the batch
   file that TURBO provides you, called BUILD-C0, in order to build a new
   C0x.OBJ file, where the 'x' represents the model.  To create a new small
   model after making the changes shown below you would execute the batch
   stream by typing BUILD-C0 SMALL.

ExcessOfMemory	label	near
  mov bx, di
  add bx, dx
  mov word ptr _heapbase@ + 2, bx
  mov word ptr _brklvl@ + 2, bx
  mov ax, _psp@
  sub bx, ax	     ; BX = Number of paragraphs to keep
  mov _tsize@, bx    ; 1st change  *** Line added for XLI *****.
  mov	es, ax	       ; ES = Program Segment Prefix address
  mov	ah, 04Ah
  int	021h
   .
   .
   .
PubSym@ 	_envLng, <dw  0>, __CDECL__
PubSym@ 	_envseg, <dw  0>, __CDECL__
PubSym@ 	_envSize,<dw  0>, __CDECL__
PubSym@ 	_psp,	 <dw  0>, __CDECL__
PubSym@ 	_tsize,  <dw  0>, __CDECL__   ; 2nd change *** line added ***.
PubSym@ 	_version,<label word>,__CDECL__
PubSym@ 	_osmajor,<db  0>, __CDECL__
  .
  .
  .
*/
extern WORD _tsize;


/*
   Xwait and xbye will contain the actual addresses, XLI entry points, that
   we'll jump to when we call XLI so they need to be big enough to hold FAR
   pointers.
*/
WORD xwait[2];
WORD xbye[2];


struct xli_file_struct {
  WORD id;
  WORD flags;
  WORD table[2];       /* offset in 0, segment in 1 */
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
/*
   @@ The following string contains the names of the functions that can
   be called from within SCHEME when this file is loaded thru XLI.

   0	1   2	3   4	 5    6    7	 8   9	  10 11    12
*/
  "sqrt/sin/cos/tan/asin/acos/atan/atan2/exp/expt/ln/log10/log//";


int xli_wait()
/*
   This function takes advantage of TURBO C's ability to have actual
   Assembly Code inline.  Its purpose is to set up the stack, call the
   XLI function XWAIT, and upon returning pop the stack and return the
   value in AX which will either be non-zero or zero.
*/

{
   asm	push _psp
   asm	push _tsize
   asm	call dword ptr [xwait]
   asm	pop  ax
   asm	pop  ax
   return (_AX);
}

void xli_bye()
/*
   This function takes advantage of TURBO C's ability to have actual
   Assembly Code inline.  If the previous function, XLI_WAIT, return
   a zero then this function will end up being called and it will
   allow XLI to clean up after us prior to the termination of Scheme.
*/
{
   asm	call dword ptr [xbye]
}

void main()
/*
  Within the main body of code the only portions that you will need to
  change are:
   1)  The value of the FILE-BLOCK.FLAGS and
   2)  The functions that you will call from within the CASE stmts.
*/
{
  struct SREGS segregs;
  int xli_wait();
  void xli_bye();

  union {
    WORD far * psp_ptr;
    WORD p_array[2];
  }  p1;


  p1.p_array[0] = 0;	/* The offset and */
  p1.p_array[1] = _psp; /* segment address for the PSP. */

/*
   The following code will initialize the File Block as needed.
*/
  file_block.id = 0x4252;
  file_block.flags = F_NEAR+F_PAD;  /* @@ Set flags as appropriate. */

  segread(&segregs); /* Get the register information */
  file_block.table[0] = (WORD) table;
  file_block.table[1] = segregs.ds;
  file_block.parm_block[0] = (WORD) &parm_block;
  file_block.parm_block[1] = segregs.ds;


/*
   Establish the connection between C and the PSP.
*/
  p1.psp_ptr[46] = (WORD) &file_block; /* Set into the PSP the offset and */
  p1.psp_ptr[47] = segregs.ds;	       /* segment address of file_block */

  xwait[0] = p1.psp_ptr[5];   /* Store into XWAIT the offset and the */
  xwait[1] = p1.psp_ptr[6];   /* segment address of DOS's terminate routine.*/

  xbye[0] = xwait[0];  /* Copy the termination offset and */
  xbye[1] = xwait[1];  /* segment address from above into here. */

  xwait[0] += 3;   /* incr by 3 for normal call */
  xbye[0] += 6;    /* incr by 6 for termination */

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