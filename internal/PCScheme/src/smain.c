/*							=====> SMAIN.C	     */
/* TIPC Scheme '84 Runtime Support - Driver
   (C) Copyright 1984,1985,1986,1987,1988 by Texas Instruments Incorporated.
   All rights reserved.

   Author:  John C. Jensen
   Installation:  Texas Instruments Incorporated, Dallas, Texas
   Division:  Central Research Laboratories
   Cost Center:  Computer Science Laboratory
   Project:  Computer Architecture Branch
   Date Written:  12 April 1984
   Last Modification:  23 October 1985
	rb 5/21/86 - added PCS 2.0 command line parameters
	tc 6/03/86 - added call to initmem, include version.h
       dbs 10/21/86 - added EGA support
	rb 2/20/87 - PCS 3.0 command line parsing; XLI initialization
	rb 5/11/87 - pcs-sysdir is determined earlier so XLI can use it
*/

#include "version.h"
#include "scheme.h"
#include "sport.h"
#include "pcmake.h"

#define COPYRIGHT  "\n(C) Copyright 1988 by Texas Instruments"
#define AIACOPY    "\n(C) Copyright A.I. Architects, Inc. 1987,1988."
#define RIGHTS	   "\n          All Rights Reserved.\n"

#define N_SYMBS 14	/* number of special symbols to be interned */
static char *spec_symbs[N_SYMBS] = {"SCHEME-TOP-LEVEL", "READ", "EOF",
		"INPUT-PORT", "OUTPUT-PORT", "CONSOLE",
		"*THE-NON-PRINTING-OBJECT*", "USER-GLOBAL-ENVIRONMENT",
		"USER-INITIAL-ENVIRONMENT", "*ERROR-HANDLER*",
		"PCS-STATUS-WINDOW", "T", "NIL", "PCS-KILL-ENGINE"};

static char *app_default = "compiler.app";
static char *ini_default = "scheme.ini";
static char *ctl_default = "scheme.xli";
char *app_file; 		 /* VM bootstrap file */
char *ini_file; 		 /* Scheme startup file */
char *ctl_file; 		 /* XLI control file */
char *pcs_sysdir;		 /* PCS system directory name */

/* Definition of Lattice C's memory management variables */
/*%%unsigned _top;	    /* "top" of the runtime stack */*/
/*%%unsigned _base;	    /* "bottom" of the runtime stack */*/
/*%%unsigned _paras;	    /* number of paragraphs of memory available */*/
/*%%unsigned _psp;	    /* program segment prefix paragraph address */*/
/*%%unsigned first_page;    /* paragraph address for first physical page */*/

/* Up Lattice C's runtime stack space to 12K bytes */
int _stack = 12288;

main(argc,argv)
int argc;
char *argv[];
{
  int i,j;  /* the usual index variable(s) */
  int page_count;  /* count of pages allocated during memory initialization */
/*%%int paragraphs_per_page; /* the number of paragraphs in a standard page */*/
  int *ptr;		/* pointer to current register */
  int *ptr_r1;		/* pointer to VM register 1 */
  int sym_reg[2];	/* temporary register for symbol pointer */
/*%%int page, disp;	  /* temporary pointers for register contents */*/
  int stat;		/* status variable */

  static int fix4[2] = {4, SPECFIX*2};
  static int fix7[2] = {7, SPECFIX*2};
  static int fix24[2] = {24, SPECFIX*2};
  static int fix112[2] = {112, SPECFIX*2};	/* 70 hex   */
  static int fix135[2] = {135, SPECFIX*2};	/* 87 hex   */
  static int make[2] = {0, SPECFIX*2};
  static int in_ptr[2] = {IN_DISP, IN_PAGE*2};
  static int who_ptr[2] = {WHO_DISP, WHO_PAGE*2};

  extern unsigned _SS();  /* function to return the stack segment (SS) reg */
  extern unsigned _DS();  /* function to return the data segment (DS) reg */
  extern VID_MODE;	  /* current video mode */

  char *set_path();
  char *get_path();
  void parse_files();

  ENTER(main);

#ifdef PROMEM
  parse_files(&argc, &argv, &app_file, &ctl_file, &ini_file);
		    /* Gets the file mix and sets debug mode */

  pcs_sysdir = get_path(app_file);
		    /* get Scheme directory name; if can't load an .EXE */
		    /* file, try it again with this prefix		*/

  pcinit();	    /* Initialize PC specific things - see pro2real.asm */

#else

  pc_type();	    /* Check the system ROM's copyright notice to see   */
		    /* if this PC is made by TI or a competitor.	*/

  fix_intr();	    /* "Fixes" keyboard DSR to have SHIFT-BRK cause the */
		    /* debugger to "kick-in" on the next VM instruction */
		    /* "Fixes" 24H int DOS Fatal error too              */
		    /* The keyboard is restored in SC.ASM		*/

  pcinit();	    /* This calls a routine in XGROUP that will do most */
		    /* any "special" pc-dependent initialization.       */
		    /* For now it only does TIPC & IBM. (thank God)	*/

  parse_files(&argc, &argv, &app_file, &ctl_file, &ini_file);
		    /* Gets the file mix and sets debug mode */

  pcs_sysdir = get_path(app_file);
		    /* get Scheme directory name; if can't load an .EXE */
		    /* file, try it again with this prefix		*/
  if (pcs_sysdir) {
    xli_init();    /* Load in external language files. */
  }		    /* The error of no system directory name is detected */
		    /* further below.					 */
#endif

  page_count = initmem();

  /* Initialize the console window */

  zcuroff();
  clear_window(in_ptr);
  set_window_attribute(in_ptr, fix4, fix24);

  /* Print Welcome to Scheme */

  ssetadr(ADJPAGE(OUT_PAGE), OUT_DISP);
  outtext(VERSION, strlen(VERSION));
  outtext(COPYRIGHT,  strlen(COPYRIGHT));
  #ifdef PROMEM
  outtext(AIACOPY, strlen(AIACOPY));
  #endif
  outtext(RIGHTS, strlen(RIGHTS));

  /* Display the "who-line" */

  if (PC_MAKE != TIPC && VID_MODE < 14)
	set_window_attribute(who_ptr, fix7, fix112);
  if (PC_MAKE != TIPC && VID_MODE > 13)
	set_window_attribute(who_ptr, fix7, fix135);
  clear_window(who_ptr);
  who_clear();

  /* Print Out Data Concerning Memory Management */
/*****
  printf("Total number of paragraphs: >%x\n_top: >%x\n_base: >%x\n\n",
	  _paras,_top,_base);
*****/
  if (page_count <= 0) {
   print_and_exit("[VM FATAL ERROR] Unable to allocate memory for PC Scheme\n");
  }
  else {
    pagelink[nextpage-1] = END_LIST;
/*****
    printf("%d memory pages allocated for Scheme\n", page_count);
*****/
  }

  /* define PCS-INITIAL-ARGUMENTS: ("file(s)" "arg1" "arg2" ...) */

  ptr_r1 = (int *) &regs[1];	  /* VM reg 1 will get compiler filename */
  intern (sym_reg, "PCS-INITIAL-ARGUMENTS", 21);
  if (argc <= 1) {		  /* there are no command line arguments */
    regs[1] = 0;
  }
  else {
    ptr = (int *) &regs[3];	  /* stuff VM registers with parameters */
    for (i = 1; i < argc; i++) {
      alloc_string(ptr, argv[i]);
      ptr += 2;
    }
    ptr -= 2;
    cons(ptr_r1, ptr, nil_reg);       /* cons onto empty list */
    for (i = argc-2; i >= 1; i--) {   /* continue consing */
      ptr -= 2;
      cons(ptr_r1, ptr, ptr_r1);
    }
  }
  sym_bind(sym_reg, ptr_r1, GNV_reg);

  /* establish the Scheme system directory pathname */

  app_file = set_path(pcs_sysdir,app_file);
  alloc_string(ptr_r1,app_file);  /* put compiler name into VM register 1 */
  if (VM_debug) {		  /* put VM debug flag into VM register 2 */
     /* if flag on, then R2 = Scheme 0, i.e. tagged fixnum zero */
    *(ptr_r1+2) = 0;
    *(ptr_r1+3) = SPECFIX * 2;
  }
  else {
     /* if flag off, then R2 = binary 0, i.e. nil */
    regs[2] = 0;
  }

  /* Define the symbol 'quote */

  intern (tmp_reg, "QUOTE", 5);
  QUOTE_PAGE = CORRPAGE(tmp_page);
  QUOTE_DISP = tmp_disp;

  /* Create the special interned symbols */

  for (i = 0, j = 6; i < N_SYMBS; i++, j += PTRSIZE) {
    intern (tmp_reg, spec_symbs[i], strlen(spec_symbs[i]));
    put_ptr(SPECCODE, j, tmp_page, tmp_disp);
  }
  intern (CONSOLE_, "CONSOLE", 7);

  /* Define the global symbol *pc-make* */

  intern (fix4, "PCS-MACHINE-TYPE", 16);
  make[C_DISP] = PC_MAKE;
  sym_bind(fix4, make, GNV_reg);

  /* Execute loader-- run time "halt" or "debug" condition detected */

  while (!(stat = interp(&S_pc, 0x7FFF))) /* do nothing */ ;

  if (stat == 2) /* enter interactive debugger */
    sdebug();

} /* end of function:  main(argc,argv) */

/************************************************************************/
/*			"Clear" the Who-Line                            */
/************************************************************************/
who_clear()
{
  int lcl_reg[2];	/* local register */
  char *text;		/* "garbage collection" message text */
  char *string_asciz(); /* returns C equivalent of a Scheme string */

  intern(lcl_reg, "PCS-GC-RESET", 12);
  if (sym_lookup (lcl_reg, GNV_reg) &&
      (text = string_asciz(lcl_reg))) {
    who_write("\n");
    who_write(text);
    rlsstr(text);
  }
  else {
    who_write(VERSION);
  }
}

/************************************************************************/
/*	     Determine PC Scheme's System Directory Pathname            */
/************************************************************************/
char *set_path(directory,filespec)
char *directory;	/* PCS system directory pathname character string */
char *filespec; 	/* compiler filename */
{
  int fudge;		/* fudge factor-- 1=no extra '\' needed, 2='\' needed */
  int len;		/* length of the return string */
  int len_dir;		/* length of the directory pathname */
  char *ret_string;	/* complete filename to be returned */
  int sym_reg[2];	/* temporary register for symbol pointer */

  char *get_path();	/* Search PATH= routine */
  char *getmem();	/* Lattice C's memory allocation routine */

/*directory = get_path(filespec);*/	/* put earlier in smain so XLI can */
					/* refer to the pcs-sysdir name    */
  if (directory) {
    /* bind PCS-SYSDIR to the Scheme directory pathname */
    intern(sym_reg, "PCS-SYSDIR", 10);
    alloc_string(tm2_reg, directory);
    sym_bind(sym_reg, tm2_reg, GNV_reg);

    /* compute length of return string and allocate it */
    len_dir = strlen(directory);
    fudge = (len_dir ?	(directory[len_dir-1] == '\\' ? 1 : 2) : 1);
    len = strlen(filespec) + len_dir + fudge;
    if (!(ret_string = getmem(len))) getmem_error("set_path");

    /* concatenate directory path, "\" if needed, and filespec */
    strcpy(ret_string, directory);
    if (fudge == 2) strcat(ret_string, "\\");
    strcat(ret_string, filespec);
    rlsstr(directory);
  }
  else {
    printf("[VM FATAL ERROR] File Not Found: %s\n", filespec);
    getch();
    exit();
  }
  return(ret_string);
} /* end set_path */

/************************************************************************/
/*	       Determine the .APP, .CTL, .INI files			*/
/************************************************************************/

/* Actually, we don't care anything about INI-type files at the VM level.
   File PSTL.S does its own processing for INI files by examining
   PCS-INITIAL-ARGUMENTS, so our limit of 1 INI file here has no
   bearing on what PSTL.S sees or does. */

#define NFILES 3
void parse_files(argc,argv,app_file,ctl_file,ini_file)
int *argc;
char ***argv;				/* this is same as char *(*argv)[] */
					/* i.e. pointer to standard argv   */
char **app_file, **ctl_file, **ini_file;
{
  int i;				/* index variable */
  int intoken;				/* in-token flag */
  char *pfiles[NFILES]; 		/* ptrs to filenames inside argv[1] */
  char *p,*r;				/* scratch */
  char **q = &r;			/* scratch */
  char debug_char = '\xEB';             /* 253, "delta" */

  char *strchr(), *getmem();

  /* command line is:  PCS  */

  if (*argc <= 1) {
    *app_file = app_default;
    *ini_file = ini_default;
    *ctl_file = ctl_default;
    return;
  }

  /* command line is:  PCS file(s) arg1 ...  */

  /* look for debug char */

  p = strchr((*argv)[1],debug_char);
  if (p) (*p = ' ', VM_debug = TRUE);

  /* make a lowercase copy */

  p = getmem(strlen((*argv)[1])+1);
  (void) strcpy(p,(*argv)[1]);
  for (i = 0; i < strlen(p); i++) p[i] = tolower(p[i]);

  /* split out the filenames */

  for (i = 0; i < NFILES; i++) pfiles[i] = NULL;
  intoken = FALSE;
  i = 0;
  while (i < NFILES && *p) {
    switch (*p) {
      case ' ' : if (intoken) (i++, intoken = FALSE);
		 *p = '\0';
		 break;
      case '(' :
      case ')' : *p = '\0';
		 break;
      default  : if (!intoken) (pfiles[i] = p, intoken = TRUE);
    } /* end switch */
    p++;
  } /* end while */

  /* now determine who's who */

  *app_file = *ctl_file = *ini_file = NULL;
  for (i = 0; i < NFILES; i++) {
    if	    (stcpm(pfiles[i],".app",q)) *app_file = pfiles[i];
    else if (stcpm(pfiles[i],".xli",q)) *ctl_file = pfiles[i];
    else if (pfiles[i]) 		*ini_file = pfiles[i];
  } /* end for i */
  if (*app_file == NULL) *app_file = app_default;
  if (*ctl_file == NULL) *ctl_file = ctl_default;
} /* end parse_files */
#undef NFILES

/************************************************************************/
/*		      Assertion Processing Routine			*/
/************************************************************************/
#undef ASSERT
asrt$(rtn,str)
char *rtn, *str;
{
  char ch;
  printf("\n[VM INTERNAL ERROR] Assertion failure in %s\n'%s'\n%s",
	rtn, str, "\nPress 'Q' to quit, any other key to continue\n");
  ch = getch();
  if (tolower(ch) == 'q') exit();
}
