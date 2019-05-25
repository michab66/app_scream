/* 
   This program demonstrates how regular DOS executable files can be
   run from XLI and represents an alternative to DOS-CALL.  It also
   provides an example that uses string arguments and the "swap"
   special service call to access them.

   User documentation is available under XLI\EXEC.DOC.  EXEC.EXE is
   already provided and can be used immediately by inserting its pathname
   in your .XLI control file.

   To generate EXEC.EXE yourself, do the following (substituting
   directory names and setting the path as needed; Lattice C version 3.0
   was used):

   	lc exec
	masm glue_lc;
	link \lc\s\c+exec+glue_lc,exec,,\lc\s\lc

   When EXEC.EXE is loaded, it allocates a block of memory from DOS
   before returning to PCS.  Further external programs, and the Scheme
   heap, are allocated with this block unavailable to them.  On the
   first (XCALL "exec" ...), the block is returned to DOS, and then
   DOS can use it to run other programs in.  In this approach, nothing
   of Scheme needs to be saved or restored, so running another program
   is quick.  On the other hand, Scheme's heap is that much smaller,
   meaning a smaller workspace and more garbage collections.  When PCS
   terminates, this program's termination code makes certain that the
   block gets deallocated (in case it never got called in the first place).
*/
   
	



#include "dos.h"

#define F_NEAR	  0x0001
#define F_INTEGER 0x0002
#define F_PAD	  0x0008

#define RT_INTEGER 0

#define CARRY_BIT 1

typedef unsigned short WORD;		/* 16-bit unsigned value */

extern WORD _psp;			/* Lattice C variables */
extern WORD _tsize;
extern WORD _oserr;

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
  int return_value;
  int dummy[3];
  char *arg[16];		  /* position 0 == filename */
				  /* positions 1..15 are for args */
} parm_block;

char table[] =
/* 0	    2	    4	      6 	 8	  10	   12	  */
  "exec//";


void main(argc,argv)
int argc;
char *argv[];
{
  int i,flags,allocated;
  WORD psp;
/*WORD memsize;     */
  WORD buffer[2];
  WORD block_ptr;
  union  REGS  regs;
  struct SREGS segregs;
  int xli_wait();
  void xli_bye();
  char *getenv();
  long atol();
  char cmd[128];
  char *local_argv[17]; 	   /* use positions 1..16 */

/* -------------------- XLI-specific initialization ----------------------- */

  /* Note PSP@ is not necessarily directly accessible in any
     Lattice C memory model. */
  psp = *(&_psp+1);			/* get seg addr of PSP */

  /* init file block */
  file_block.id = 0x4252;
  file_block.flags = F_NEAR+F_INTEGER;
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

/* ==================== start program-specific actions ==================== */

/* ----------------------------- initialization --------------------------- */

  /* allocate a block of memory */
  regs.x.ax = 0x4800;		/* alloc mem */
  { /* Set size from "XLI" env variable if available; unit size is Kb.
       If var doesn't exist, use 64 Kb.  Convert to paragraphs.   */
    char *block_reserve;

    block_reserve = getenv("XLI");
    regs.x.bx = (block_reserve ? atol(block_reserve) * 1024 / 16
			       : 0x1000);
  }
  flags = intdos(&regs,&regs);
  block_ptr = (flags & CARRY_BIT) ? 0 : regs.x.ax;
  allocated = 1;

  /* set all args to -1; since there are a variable # of args,
     a -1 after them delimits them */
  for (i = 0; i < 16; i++) parm_block.arg[i] = (char *) -1;

/* ----------------------------- handler loop	--------------------------- */

  while (xli_wait()) {

    if (!block_ptr) continue;		/* couldn't alloc, just skip */

    /* deallocate the block to leave a hole in which we can bid programs */
    if (allocated) {
      regs.x.ax = 0x4900;		/* dealloc mem */
      segregs.es = block_ptr;		/* @block we previously allocated */
      flags = intdosx(&regs,&regs,&segregs);
      allocated = 0;
    } /* end if */

    switch (parm_block.select) {
      int i,error;

      case  0: /* get name of executable file */
	       parm_block.special_service = 1;
	       parm_block.ss_args[0] = 0;
	       parm_block.ss_args[1] = 128;
	       parm_block.ss_args[2] = (WORD) cmd;
	       (void) xli_wait();
	       *(cmd + parm_block.ss_args[0]) = '\0';

	       /* get arguments to executable file */
	       for (i = 1; i < 17; i++) local_argv[i] = NULL;
	       for (i = 1; i < 16; i++) {
		 if ((int) parm_block.arg[i] == -1) break;
		 local_argv[i] = cmd + parm_block.ss_args[0] + 1;
		 parm_block.special_service = 1;
		 parm_block.ss_args[0] = i;
		 parm_block.ss_args[1] = cmd + 128 - local_argv[i];
		 parm_block.ss_args[2] = (WORD) local_argv[i];
		 (void) xli_wait();
		 *(local_argv[i] + parm_block.ss_args[0]) = '\0';
	       }

	       /* exec the file and return the termination code */
	       /* or -1 if the file doesn't exist */
	       error = forkvp(cmd,local_argv);
	       parm_block.return_value = (error == -1 ? -1 : wait());
	       break;
      default: ;
    } /* end switch */
    parm_block.return_type = RT_INTEGER;
    for (i = 0; i < 16; i++) parm_block.arg[i] = (char *) -1;
  } /* end while */

/* ----------------------------- termination	--------------------------- */

  /* in case this program was never called, the block we reserved */
  /* is still allocated, so deallocate it */
  if (allocated) {
    regs.x.ax = 0x4900; 	      /* dealloc mem */
    segregs.es = block_ptr;	      /* @block we previously allocated */
    flags = intdosx(&regs,&regs,&segregs);
    allocated = 0;
  } /* end if */

  xli_bye();

} /* end main */
