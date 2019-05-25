/*                                                      =====> SFASL.C     */
/* TIPC Scheme '84 Runtime Support - Fast Loader
   (C) Copyright 1984,1985 by Texas Instruments Incorporated.
   All rights reserved.

   Author:  John C. Jensen
   Installation:  Texas Instruments Incorporated, Dallas, Texas
   Division:  Central Research Laboratories
   Cost Center:  Computer Science Laboratory
   Project:  Computer Architecture Branch
   Date Written:  28 December 1984
   Last Modification:  18 October 1985
*/
#include "stdio.h"
#include "scheme.h"
#include "slist.h"

#define skip_space() while(isspace(sgetc()));

/* data structures to control file access */
#define NUM_FILES 4     /* the maximum recursion of "%fasl" operations */
#define BUF_LENGTH 4096 /* buffer length for fasl files */
#define READ 0          /* file access code for "read" */
#define CTL_Z 0x1A      /* MS-DOS end-of-file character */
static char *buffer;    /* character string buffer */
static int ch = 0;      /* the current character */
static int file_no = -1; /* the current file number */
static char *file_buffer[NUM_FILES] = {0,0,0,0}; /* character buffers */
static int file_handle[NUM_FILES] = {0,0,0,0}; /* handles for open files */
static char *file_pos[NUM_FILES] = {0,0,0,0}; /* current position in buffer */
static int file_end[NUM_FILES] = {0,0,0,0}; /* end of buffer */

static char *f_pos = NULL;
static char *f_end = NULL;

static int zeros[3] = {0,0,0};

char *getmem();         /* Lattice C's memory allocation routine */

/************************************************************************/
/*              Read In a Fast Load Format Object Module                */
/************************************************************************/
fasl(reg)
int reg[2];             /* parameter register */
 {
  char lcl_buffer[256]; /* character string buffer */
  int codebytes;        /* the number of codebytes in a code block */
  int constants;        /* the number of constants in a code block */
  int disp;             /* displacement portion of a pointer */
  int i;                /* index variable */
  int len;              /* length of a character string or symbol */
  int page;             /* page number portion of a pointer */
  int retstat = 0;      /* the return code */
  int type;             /* type code for parameter pointer */
  int dummy;            /* dummy variable for zopen - dbs */

  static max_nest[2] = {NUM_FILES, SPECFIX*2};  /* maximum FSL nesting level */

  ENTER (sfasl);

  buffer = lcl_buffer;
  page = CORRPAGE(reg[C_PAGE]);
  disp = reg[C_DISP];
  type = ptype[page];

  if (type == STRTYPE*2)
   {
    if(file_no >= NUM_FILES-1)
     {
      set_error(1, "FAST-LOAD nesting too deep. Maximum is", max_nest);
      retstat = -1;
      goto return_eof;
     }
    len = get_word(page,disp+1);
    if (len < 0)
      len = len + BLK_OVHD;
    else
      len = len - BLK_OVHD;
    get_str(buffer, page, disp);
    buffer[len] = '\0';
    file_no++;
    if ((i = zopen(&file_handle[file_no], buffer, READ, &dummy, &dummy)))
     {
      /* alloc_string(tmp_reg, buffer);				  */
      /* set_error(0, "Unable to open FAST-LOAD file", tmp_reg);  */
      /* retstat = -1;						  */
      /* file_no--;						  */
      /* goto return_eof;					  */

      /* Call to dos_err will not return */
      i += IO_ERROR_START;
      alloc_string(tmp_reg, buffer);
      dos_err(1,i,tmp_reg);
     }
    if (!(file_pos[file_no] = (file_buffer[file_no] =
                                getmem(BUF_LENGTH)))) getmem_error(rtn_name);
    file_end[file_no] = 0;
   }

  f_pos = file_pos[file_no];
  f_end = file_buffer[file_no] + (file_end[file_no]);

ASSERT (file_no >= 0 /* make sure file exists */);

  /* read and validate fasl program header; get # constants and codebytes */
  skip_space();
  while (ch == '#')
   {
    for (i = 0; i < 11; i++)
      if (sgetc() != "!fast-load "[i]) goto invalid_fasl;
    while (sgetc() != '\n') /* do nothing */ ;
    skip_space();
   }
  if (ch == EOF || ch == CTL_Z) goto close_file;
  if (ch != 'h') goto invalid_fasl;
  constants = next_word();
  codebytes = next_word();

  /* allocate and zero the code block */
  alloc_block(reg, CODETYPE, constants*PTRSIZE + PTRSIZE + codebytes);
  page = CORRPAGE(reg[C_PAGE]);
  disp = reg[C_DISP];
  zero_blk(page, disp);
  disp += BLK_OVHD;

  /* insert the entry point offset */
  put_ptr(page, disp, SPECFIX*2, constants*PTRSIZE + PTRSIZE +BLK_OVHD);

  /* process the constants list entries */
  disp = PTRSIZE + BLK_OVHD;
  while (constants--)
   {
    if (read_constant()) goto invalid_fasl;
    put_ptr(CORRPAGE(reg[C_PAGE]), reg[C_DISP]+disp, tmp_page, tmp_disp);
    disp += PTRSIZE;
   } /* end:  while (constants--) */

  /* validate the "text" portion header and read in bytecodes */
  skip_space();
  if (ch != 't') goto invalid_fasl;
  zap_chars(reg, disp, codebytes);

  /* validate the fasl module trailer */
  skip_space();
  if (ch == 'z')
   {
    file_pos[file_no] = f_pos;
    goto end_of_function;
   }

invalid_fasl:
  set_error(0, "Invalid FAST-LOAD module", nil_reg);
  retstat = -1;

close_file:
  zclose(file_handle[file_no]);
  if (rlsmem(file_buffer[file_no], BUF_LENGTH))
    rlsmem_error(rtn_name);
  file_no--;
return_eof:
  reg[C_PAGE] = EOF_PAGE*2;
  reg[C_DISP] = EOF_DISP;

end_of_function:
  return(retstat);
 } /* end of function:  fasl(reg) */

/************************************************************************/
/*                      Read In a Constant Entry                        */
/************************************************************************/
read_constant()
 {
  int disp;             /* displacement component of a pointer */
  int i;                /* index variable */
  int len;              /* length of a string or symbol */
  int lpage=0;          /* page number for a list cell */
  int page;             /* page number component of a pointer */

  double next_flonum(); /* reads a flonum from the input file */

tail_recursion:
  skip_space();
  switch(ch)
   {
    case 'x':  /* symbol */
               len = next_byte();
               for (i = 0; i < len; i++) buffer[i] = sgetc();
               intern(tmp_reg, buffer, len);
               break;

    case 'i':  /* short integer constant */
               tmp_page = SPECFIX*2;
               tmp_disp = next_word();
               break;

    case 'l':  /* list cell */
               if (nextcell[listpage] != END_LIST)
               {
                 tmp_page = ADJPAGE(listpage);
                 tmp_disp = nextcell[listpage];
                 nextcell[listpage] = get_word(listpage, tmp_disp+1);
               }
               else
                 alloc_list_cell(tmp_reg);
               toblock(tmp_reg, 0, zeros, LISTSIZE);
               if (lpage)
                { /* we're building a linked list-- update previous cdr */
                 C_pop(tm2_reg);
                 put_ptr((lpage=CORRPAGE(tm2_page)), tm2_disp+3, tmp_page,
                          tmp_disp);
                }
               else
                { /* starting a list-- preserve list header pointer */
                 C_push(tmp_reg);
                }
               C_push(tmp_reg);  /* record this list cell's location */
               if (stkspc() < 64)
                {
stk_ovfl:
 printf("\n[VM ERROR encountered!] PC stack overflow during FAST-LOAD\n%s%s",
                        "Attempting to execute SCHEME-RESET\n",
                        "[Returning to top level]\n");
                 force_reset();
                }
               read_constant();
/*********
               C_pop(tm2_reg);  /* restore current list cell pointer */
               C_push(tm2_reg); /* save for next iteration */
**********/
               put_ptr((lpage=CORRPAGE((int) S_stack[TOS])),
                       *((int *) (S_stack+TOS+1)), tmp_page, tmp_disp);
               goto tail_recursion;

    case 'n':  /* null pointer */
               tmp_page = tmp_disp = 0;
               break;

    case 's':  /* string constant */
               len = next_word();
               alloc_block(tmp_reg, STRTYPE, len);
               zap_chars(tmp_reg, 3, len);
               break;

    case 'c':  /* character constant */
               tmp_page = SPECCHAR*2;
               tmp_disp = next_byte();
               break;

    case 'b':  /* bignum constant */
               len = next_byte();
               alloc_block(tmp_reg, BIGTYPE, len+len+1);
               page = CORRPAGE(tmp_page);
               disp = tmp_disp + BLK_OVHD;
               put_byte(page, disp++, next_byte());
               while (len--)
                {
                 put_word(page, disp, next_word());
                 disp += WORDINCR;
                }
               break;

    case 'f':  /* flonum constant */
               alloc_flonum(tmp_reg, next_flonum());
               break;

    case 'v':  /* vector */
               len = next_word();
               alloc_block(tm2_reg, VECTTYPE, len+len+len);
               zero_blk(CORRPAGE(tm2_page),tm2_disp);
               if (stkspc() < 64) goto stk_ovfl;
               for (i = 0, disp = BLK_OVHD; i < len; i++, disp += PTRSIZE)
                {
                 C_push(tm2_reg);       /* save pointer to vector object */
                 read_constant();       /* read next vector entry */
                 C_pop(tm2_reg);        /* restore pointer to vector object */
                 put_ptr(CORRPAGE(tm2_page), tm2_disp+disp, tmp_page, tmp_disp);
                } /* end:  for (i = 0, etc.) */
               mov_reg(tmp_reg, tm2_reg);
               break;

    default:   /* error-- unexpected constant tag */
           /*  printf("read_constant:  invalid constant tag '%c'\n", ch);  */
               return(1);

   } /* end:  switch(ch) */

  /* if we're filling in the last cdr field of a linked list, fix it up */
  if (lpage)
   {
    C_pop(tm2_reg);
    put_ptr(CORRPAGE(tm2_page), tm2_disp+3, tmp_page, tmp_disp);
    C_pop(tmp_reg);     /* restore list header pointer */
   }
  return(0);
 } /* end of function:  read_constant() */


/************************************************************************/
/*                      Read In a Hexadecimal Byte                      */
/************************************************************************/
static int  low_digit[23] = {0x00,0x01,0x02,0x03,0x04,0x05,0x06,0x07,0x08,0x09,
                             -1,-1,-1,-1,-1,-1,-1,
                             0x0A,0x0B,0x0C,0x0D,0x0E,0x0F};
static int high_digit[23] = {0x00,0x10,0x20,0x30,0x40,0x50,0x60,0x70,0x80,0x90,
                             -1,-1,-1,-1,-1,-1,-1,
                             0xA0,0xB0,0xC0,0xD0,0xE0,0xF0};
next_byte()
 {
  int value;
  skip_space();
  return(high_digit[ch - '0'] + low_digit[sgetc() - '0']);
 } /* end of function:  next_byte() */

/************************************************************************/
/*                      Read In a Hexadecimal Word                      */
/************************************************************************/
next_word()
 {
  int value;
  value = next_byte() << 8;
  return(value | next_byte());
 } /* end of function:  next_word() */

/************************************************************************/
/*                      Read In a Floating Point Value                  */
/************************************************************************/
double next_flonum()
 {
  int flo_parts[4];     /* "words" comprising a floating point value */
  int i;                /* index variable */

  /* read in the four words comprising a floating point constant */
  for (i = 0; i < 4; i++)
    flo_parts[i] = next_word();

  /* convert "parts" of floating point value to a true floating point number */
  return(*((double *) flo_parts));
 } /* end of function:  next_flonum() */

/************************************************************************/
/*               Read Character From Current Input File                 */
/************************************************************************/
sgetc()
 {
  int stat;             /* status returned from read */
  if (f_pos >= f_end)
   {
    file_end[file_no] = BUF_LENGTH;
    if((stat = zread(file_handle[file_no], file_buffer[file_no],
         &file_end[file_no])))
     {
      printf("[VM INTERNAL ERROR] sfasl: read error status=%d\n", stat);
     }
    if ((f_pos = file_buffer[file_no]) >= (f_end = f_pos + file_end[file_no]))
     {
      return((ch = EOF));
     }
   }
  return((ch = *f_pos++));
 } /* end of function:  sgetc() */

/************************************************************************/
/*      Copy Block of Characters from Input Buffer to Scheme Block      */
/************************************************************************/
zap_chars(ptr, offset, len)
int ptr[2];             /* register holding pointer to Scheme Block */
int offset;             /* beginning offset into the Scheme Block */
int len;                /* the number of characters to transfer */
 {
  int actual;           /* the number of characters transfered in one move */

  while (len)
   {
    if (f_pos >= f_end)
     {
      sgetc();
      f_pos--;
     }
    actual = f_end - f_pos;
    if (len < actual) actual = len;
    toblock(ptr, offset, f_pos, actual);
    len -= actual;
    offset += actual;
    f_pos += actual;
   } /* end:  while (len) */
 } /* end of function:  zap_chars(ptr, offset, len) */

/************************************************************************/
/*                      Reset Fasl Data Structures                      */
/************************************************************************/
reset_fasl()
 {
  while (file_no >= 0)
   {
  zclose(file_handle[file_no]);
  if (rlsmem(file_buffer[file_no], BUF_LENGTH))
    rlsmem_error("reset_fasl");
  file_no--;
   } /* end:  while (file_no >= 0) */
 } /* end of function:  reset_fasl() */
