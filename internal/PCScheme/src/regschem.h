/*                                                      =====> SCHEME.H      */
   /* TIPC Scheme Data Declarations for Lattice C */
   /* Last Modification:  01 January 1986 */

extern char *rtn_name;
#define ASSERT(arg) if(!(arg))asrt$(rtn_name,"arg")
#define ENTER(xyz) static char *rtn_name = "xyz"

   /* Data conversion macros */
/* Adjust page number- this macro converts a logical page number to
   the representation which is stored in the interpreter's registers
   and pointers.  "CORRPAGE" performs the reverse transformation */
#define ADJPAGE(x) ((x)<<1)
/* Correct page number- this macro converts the interpreter's encoding
   of a page number into the logical page number.  "ADJPAGE" performs
   the reverse transformation.  */
#define CORRPAGE(x) ((x)>>1)

   /* Fetch value for Fixnum (immediate) from pointer */
#define get_fix(pg,ds) (((ds)<<1)>>1)
   /* Fetch value for Character (immediate) from pointer */
#define get_char(pg,ds) ((ds) & 0x00ff)

   /* define truth */
#define TRUE 1
#define FALSE 0
#define NULL 0          /* null pointer */

   /* Position of page/displacement values in "registers" */
#define C_DISP 0
#define C_PAGE 1

   /* Page Management Table Definitions */
#define NUMPAGES 128    /* maximum number of pages */
#define DEDPAGES 8      /* Number of dedicated pages */

#define MIN_PAGESIZE 0x0C00 /* minimum page size in bytes (fixed size) */
#define PTRMASK MIN_PAGESIZE-1 /* mask to isolate a pointer displacement */

#define PAGEINCR 2       /* increment to get to next page */
#define PAGEMASK 0x00FE  /* mask to isolate a page number */
#define WORDSIZE 16      /* computer's word size (bits/word) */
#define WORDINCR 2       /* number of address units/word */
#define HT_SIZE 211      /* the oblist's hash table size */
#define STKSIZE 900      /* the stack's length (bytes) */
#define BLK_OVHD 3       /* number of overhead bytes in a block header */
#define NUM_REGS 64      /* number of registers in the Scheme VM */

   /* Data Type Equates */
#define NUMTYPES 15                /* the number of data types */
#define LISTTYPE 0
#define FIXTYPE 1
#define FLOTYPE 2
#define BIGTYPE 3
#define SYMTYPE 4
#define STRTYPE 5
#define ARYTYPE 6
#define VECTTYPE ARYTYPE
#define CONTTYPE 7
#define CLOSTYPE 8
#define FREETYPE 9
#define CODETYPE 10
#define REFTYPE 11
#define PORTTYPE 12
#define CHARTYPE 13
#define ENVTYPE 14

#define EOFERR 1  /* Codes for function ERRMSG */
#define DOTERR 2
#define QUOTERR 3
#define RPARERR 4
#define OVERERR 5
#define DIV0ERR 6
#define SHARPERR 7
#define FULLERR -1
#define PORTERR -2
#define HEAPERR -3

#define BUFSIZE 80
#define SYM_OVHD 7

#define PTRSIZE 3
#define LISTSIZE 6
#define FIXSIZE 2
#define FLOSIZE 9
#define SMALL_SIZE 1024         /* a "small" length for a block */

#define SPECCHAR 1              /* special page of characters */
#define SPECFIX 3               /* special page of fixnums */
#define SFIXLEN 0               /* length (bytes) of special fixnum page */
#define SPECFLO 4               /* special page of flonums */
#define SFLOLEN 24              /* length (bytes) of special flonum page */
#define SPECSYM 5               /* special page of symbols */
#define SSYMLEN 0x51            /* length (bytes) of special symbol page */
#define SPECSTK 6
#define SPECPOR 6               /* special page of ports */
#define SPORLEN 92              /* length (bytes) of special port page */
#define SPECCODE 7              /* code page for the bootstrap loader */

#define END_LIST 0x7FFF         /* end of linked list marker */

#define NIL_PAGE 0              /* Location of "nil" */
#define NIL_DISP 0
#define T_PAGE SPECSYM          /* Location of "t" (for true) */
#define T_DISP 0x0000
#define UN_PAGE SPECSYM         /* Location of "#!unassigned" */
#define UN_DISP 0x0009
#define NTN_PAGE SPECSYM        /* Location of "#!not-a-number" */
#define NTN_DISP 0x001C
#define OVR_PAGE SPECSYM        /* Location of overflow designator */
#define OVR_DISP 0x001C         /* (same as "not a number" for now) */
#define DIV0_PAGE SPECSYM       /* Location of divide-by-zero designator */
#define DIV0_DISP 0x001C        /* (same as "not a number" for now) */
#define IN_PAGE SPECPOR         /* Location of standard input port */
#define IN_DISP 0
#define OUT_PAGE SPECPOR        /* Location of standard output port */
/* #define OUT_DISP 0x011f */
#define OUT_DISP 0              /* input=output for standard console device */
#define WHO_PAGE SPECPOR        /* Location of "who-line" port */
#define WHO_DISP 0x0123
#define EOF_PAGE SPECSYM        /* Location of non-interned "**eof**" symbol */
#define EOF_DISP 0x0031
#define NPR_PAGE SPECSYM        /* Location of "#!unprintable" */
#define NPR_DISP 0x003D

#define ADD_OP 0    /* addition */
#define SUB_OP 1    /* subtraction */
#define MUL_OP 2    /* multiplication */
#define DIV_OP 3    /* divide */
#define MOD_OP 4    /* modulo */
#define AND_OP 5    /* bitwise and */
#define OR_OP 6     /* bitwise or */
#define MINUS_OP 7  /* minus */
#define EQ_OP 8     /* equal comparison */
#define NE_OP 9     /* not equal comparison */
#define LT_OP 10    /* less than comparison */
#define GT_OP 11    /* greater than comparison */
#define LE_OP 12    /* less than or equal comparison */
#define GE_OP 13    /* greater than or equal comparison */
#define ABS_OP 14   /* absolute value */
#define QUOT_OP 15  /* quotient */
#define TRUNC_OP 16 /* truncate */
#define FLOOR_OP 17 /* floor */
#define CEIL_OP 18  /* ceiling */
#define ROUND_OP 19 /* round */
#define FLOAT_OP 20 /* float */
#define ZERO_OP 21  /* zero? */
#define POS_OP 22   /* positive? */
#define NEG_OP 23   /* negative? */

/*  Numeric Error Codes */
#define REF_GLOBAL_ERROR 1      /* reference of unbound global variable */
#define SET_GLOBAL_ERROR 2      /* SET! error-- global not defined */
#define REF_LEXICAL_ERROR 3     /* reference of unbound lexical variable */
#define SET_LEXICAL_ERROR 4     /* SET! error-- lexical variable not defined */
#define REF_FLUID_ERROR 5       /* reference of unbound fluid variable */
#define SET_FLUID_ERROR 6       /* SET-FLUID! error-- fluid not bound */
#define VECTOR_OFFSET_ERROR 7   /* vector index out of range */
#define STRING_OFFSET_ERROR 8   /* string index out of range */
#define SUBSTRING_RANGE_ERROR 9 /* invalid substring range */
#define INVALID_OPERAND_ERROR 10 /* invalid operand to VM instruction */
#define SHIFT_BREAK_CONDITION 11 /* SHFT-BRK key was depressed by user */
#define NON_PROCEDURE_ERROR 12  /* attempted to call non-procedural object */
#define TIMEOUT_CONDITION 13    /* timer interrupt */
#define WINDOW_FAULT_CONDITION 14 /* attempt to do I/O to a de-exposed window */
#define FLONUM_OVERFLOW_ERROR 15 /* flonum overflow/underflow */
#define ZERO_DIVIDE_ERROR 16    /* division by zero */
#define NUMERIC_OPERAND_ERROR 17 /* non-numeric operand */
#define APPLY_ARG_LIMIT_ERROR 18 /* too many arguments for APPLY to handle */
#define VECTOR_SIZE_LIMIT_ERROR 19 /* vector too big */
#define STRING_SIZE_LIMIT_ERROR 20 /* string too big */
#define DOS_FATAL_ERROR 21  /* DOS fatal i/o error (24H INT) */

/*  Scheme VM Control Flags  */
extern int PC_MAKE;            /* variable denoting PC's manufacturer & type */
extern int VM_debug;            /* VM debug mode flag */
extern int s_break;             /* shift-break indicator */

extern int QUOTE_PAGE;         /* Location of "quote" */
extern int QUOTE_DISP;

extern unsigned PAGESIZE;
extern unsigned pagetabl[NUMPAGES];    /* Paragraph Address (bases) */
extern struct {
     unsigned atom:1;
     unsigned listcell:1;
     unsigned fixnums:1;
     unsigned flonums:1;
     unsigned bignums:1;
     unsigned symbols:1;
     unsigned strings:1;
     unsigned arrays:1;
     unsigned nomemory:1;
     unsigned readonly:1;
     unsigned continu:1;
     unsigned closure:1;
     unsigned refs:1;
     unsigned ports:1;
     unsigned code:1;
     unsigned characters:1;
            } attrib[NUMPAGES]; /* Page Attribute Bits */
extern int w_attrib[NUMPAGES];  /* Re-define attribute bits as integer */
extern int nextcell[NUMPAGES];  /* Next Available Cell Pointers */
extern int pagelink[NUMPAGES];  /* Next Page of Same Type */
extern int ptype[NUMPAGES];     /* Page Type Index */
extern unsigned psize[NUMPAGES]; /* Page Size Table */

extern int pageattr[NUMTYPES];  /* Page attribute initialization table */
extern int pagelist[NUMTYPES];  /* Page allocation table (by types) */

extern int listpage;  /* Page for List Cell allocation */
extern int fixpage;   /* Page for Fixnum allocation */
extern int flopage;   /* Page for Flonum allocation */
extern int bigpage;   /* Page for Bignum allocation */
extern int sympage;   /* Page for Symbol allocation */
extern int strpage;   /* Page for String allocation */
extern int arypage;   /* Page for Array allocation */
extern int contpage;  /* Page for Continuation allocation */
extern int clospage;  /* Page for Closure allocation */
extern int freepage;  /* Free page allocation list header */
extern int codepage;  /* Page for Code Block allocation */
extern int refpage;   /* Ref cell page allocation list header */

extern int nextpage;       /* Next Page Number for Allocation in the
                               Logical Address Space */
extern unsigned nextpara;  /* Next Paragraph Address for Allocation */

/* Scheme's Virtual Registers */
extern long reg0, regs[NUM_REGS];
extern int nil_reg[2];
extern int reg0_page, reg0_disp, tmp_reg[2], tmp_page, tmp_disp;
extern int tm2_reg[2], tm2_page, tm2_disp;
extern int FNV_reg[2], GNV_reg[2], CB_reg[2], PREV_reg[2];
extern int FNV_pag, FNV_dis, GNV_pag, GNV_dis, CB_pag, CB_dis;
extern int PREV_pag, PREV_dis, FP, BASE;
extern int CONSOLE_[2], CON_PAGE, CON_DISP;
extern int TRNS_reg[2], TRNS_pag, TRNS_dis; /* transcript file pointer */
extern int condcode, S_pc;

/* Stack */
extern int TOS;         /* top of stack pointer (displacement in bytes */
extern char S_stack[STKSIZE]; /* the stack itself */

/* Hash Table */
extern char hash_page[HT_SIZE];
extern int  hash_disp[HT_SIZE];

/* Property List Hash Table */
extern char prop_page[HT_SIZE];
extern int  prop_disp[HT_SIZE];

/* State Variables for (reset) and (scheme-reset) */
extern int FP_save, RST_ent;
extern int FNV_save[2];
extern int STL_save[2];

/* Port fields */
#define pt_direc 6
#define pt_lnlen 20
#define pt_csrcol 12
#define dtaoffs 32

/* Error message text strings */
extern char m_error[], m_src[], m_dest[], m_first[], m_second[], m_third[];

/* Macros Normally Found in STDIO.H */
#define abs(x) ((x)<0?-(x):(x))
#define max(a,b) ((a)>(b)?(a):(b))
#define min(a,b) ((a)<=(b)?(a):(b))

/* Scheme Function Macros */
#define alloc_sym(dest,len) alloc_block(dest,SYMTYPE,len+PTRSIZE+1)

/* International Case Conversion Macros */
extern char locases[256];
extern char hicases[256];
#undef tolower
#define tolower(c) locases[(c)]
#undef toupper
#define toupper(c) hicases[(c)]
#undef islower
#define islower(c) ((c)!=hicases[(c)])
#undef isupper
#define isupper(c) ((c)!=locases[(c)])
#undef isspace
#undef isdigit
#define isdigit(c) isdig((c),10)
#undef isxdigit
#define isxdigit(c) isdig((c),16)
