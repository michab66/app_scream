/*                                                      =====> MAKE_FSL.C    */
/* TIPC Scheme Runtime Support - Conversion To Fast Load Format
   (C) Copyright 1984,1985,1987 by Texas Instruments Incorporated.
   All rights reserved.

   Author:  John C. Jensen
   Installation:  Texas Instruments Incorporated, Dallas, Texas
   Division:  Central Research Laboratories
   Cost Center:  Computer Science Laboratory
   Project:  Computer Architecture Branch
   Date Written:  27 December 1984
   Last Modification:  17 October 1985

   Purpose:  To convert PC Scheme object programs into a fast-load format.

   Description:  Object programs are read in by the make_fsl utility and an
                 equivalent fast-load representation is output.  This utility
                 preforms similarly to the Scheme reader, but it understands
                 only enough about s-expressions to read in valid object
                 programs in a standard format.  Any error condition will
                 abort the exectution of this utility.

                 The format of the object module which can be read by
                 this utility is:

     (execute (quote (pcs-code-block #consts #codebytes (constants) (code))))

                where pcs-code-block is a compiler generated tag indicating
                                 that this is an object module.
                      #consts    is an integer value indicating the number of
                                 source contants in the compiled program.
                                 This is the number of entries in the
                                 "constants" list (see below).
                      #codebytes is an integer value indicating the number
                                 of bytes of executable code in the compiled
                                 program.  This is the number of entries in
                                 the "code" list (see below).
                      (constants) is a list of program constants.  Each
                                 constant is a Scheme s-expression in its
                                 ASCII source representation.
                      (code)     is a list of integer values representing the
                                 executable code of the compiled program.

   Invocation:  The MAKE_FSL utility is executed by entering the following
                command to MS-DOS:

                        MAKE_FSL  PROGRAM.SO  PROGRAM.FSL {/copyright}

                where PROGRAM.SO  is a valid MS-DOS file name of a file
                                  containing the object program (the output
                                  of the PC Scheme compiler),

                      PROGRAM.FSL is a valid MS-DOS file name of the file
                                  which is to contain the fast-load
                                  representation produced by this utility.

                      /copyright  is an optional flag which indicates that,
                                  instead of the standard fast-load module
                                  header record, a TI copyright notice is
                                  to be inserted in the output file.

   Note:  The /copyright option is an undocumented feature designed to
          add some measure of protection for TI proprietary software.  This
          capability is not intended to be visible to the general user
          community and is not documented in any of the messages produced
          by this utility.

   Modification History:
          02 Feb 86 - Made two changes which corrected problems in reading
                      symbols containing backslash characters. According to
                      the Common Lisp standard, a backslash is an escape
                      character, which causes the character following it to
                      be accepted as part of the symbol.
    rb    15 Oct 86 - changed %x to %X; fasl expects uppercase and Lattice C
                      now distinguishes the two printf formats
*/
#include "stdio.h"
#include "ctype.h"

#include "schars.h"     /* copy in special character definitions */

#define MAX_STRING_LENGTH 8192  /* maximum string length */

#define TRUE 1
#define FALSE 0
#define skip_space() ch=fgetc(in_file); while(isspace(ch))ch=fgetc(in_file);

static FILE *in_file, *out_file;        /* input/output file pointers */
static int count = 0;           /* count of characters in current record */

static char *month[12] = {"Jan","Feb","Mar","Apr","May","Jun",
                          "Jul","Aug","Sep","Oct","Nov","Dec"};

int _stack = 12288;     /* Increase Lattice C's default stack size */

int _fmode = 0;         /* default files to text mode translation */

char *getmem();         /* Lattice C's memory allocation routine */

main(argc, argv)
int argc;               /* number of arguments */
char *argv[];           /* array of ptrs to arg strings */
 {
  int cflag = FALSE;            /* copyright notice flag */
  int ch;                       /* current character */
  int codebytes = 0;            /* number of codebytes */
  int constants = 0;            /* number of constants */
  int date[3];                  /* array to receive current date */
  int module = 0;               /* module count */
  int n;                        /* number of fields read */
  int time[4];                  /* array to receive current time */
  static char *invalid_module = "***Error-- Invalid Object Module***\n%s\n";

  /* Print Welcome to Scheme */
  printf("\nFast-Load Conversion Utility for PC Scheme\n%s%s%s\n",
           "         Version 3.03  7 June 88\n",
           " (C) Copyright 1987 by Texas Instruments\n",
           "           All Rights Reserved.\n");

  /* test for generation of copyright notice */
  if (argc == 4)
   {
    if (!strcmp(argv[3], "/copyright"))
     {
      cflag = TRUE;
      argc = 3;
     }
   }

  /* test for proper number of arguments */
  if (argc != 3)
   {
    printf("***Error-- Wrong Number of Arguments to MAKE_FSL***\n");
describe:
    printf("\nMAKE_FSL expects two file names as follows:\n\n%s%s%s%s%s",
   "     MAKE_FSL  PROGRAM.SO  PROGRAM.FSL\n\n",
   "where PROGRAM.SO  is the output of the 'compile-file' function of",
   "                  the PC Scheme Compiler\n",
   "      PROGRAM.FSL is the name of the file which is to contain the\n",
   "                  output of the MAKE_FSL utility.\n");
    exit();
   }

  /* open input file */
  _fmode = 0x8000;      /* make files use binary (untranslated) output */
  if (!(in_file = fopen(argv[1], "r")))
   {
    printf("***Error-- Unable to Open Input File '%s'***\n", argv[1]);
    goto describe;
   }

  /* open output file */
  if (!(out_file = fopen(argv[2], "w")))
   {
    printf("***Error-- Unable to Open Output File '%s'***\n", argv[2]);
    goto describe;
   }
  _fmode = 0;           /* reset to text mode translation */

  /* obtain current time and date */
  get_date(date);

  /* Output header record(s) to output file */
  if (cflag)
   {
    /* Output TI Copyright notice */
    fprintf(out_file,
        "#!fast-load  (C) Copyright %4d by Texas Instruments\r\n%s",date[2],
        "#!fast-load            All Rights Reserved.\r\n");
   }
  else
   {
    /* Output module header to output file */
    get_time(time);
fprintf(out_file, "#!fast-load  Version 3.0  %2d %s %2d  %02d:%02d:%02d\r\n",
        date[1], month[date[0]-1], date[2]-1900, time[0], time[1], time[2]);
   }

  /* read 'til end-of-file */
  skip_space();
  while (ch != EOF && ch != '\032')
   {
    /* read (skip over) object module "header" */
    n = -1;
    if (ch != '(') goto oops_header;
    ch = fgetc(in_file);
    if (ch == '%') ch = fgetc(in_file);  /* "optional" % in front of execute */
    for (n = 0; n < 30; n++)
     {
      ch = tolower(ch);
      if (ch != "execute (quote (pcs-code-block "[n])
       {
oops_header:
        printf(invalid_module, "Bad Module Header");
        printf("n = %d    ch = '%c'\n", n, ch);
        exit();
       }
      ch = fgetc(in_file);
     }

    /* read number of constants, number of code bytes */
    fscanf(in_file, "%d %d", &constants, &codebytes);
    fprintf(out_file, "h%04X %04X\r\n", constants, codebytes);

    /* skip over '(' which begins constants list */
    skip_space();
    if (ch != '(')
     {
      if (constants || ch != 'n' || (ch = fgetc(in_file)) != 'i' ||
                                    (ch = fgetc(in_file)) != 'l')
       {
        printf (invalid_module, "Bad Start of Constants List");
        printf("ch = '%c'\n", ch);
        exit();
       }
      else goto good_const;
     }

    /* read and output constants */
    for (n = 0; n < constants; n++)
     {
      count = 0;
      process_constant();
      fprintf(out_file, "\r\n");
     }

    /* make sure constants list was exhausted */
    skip_space();
    if (ch != ')')
     {
      printf (invalid_module, "Bad End of Constants List");
      printf("ch = '%c'\n", ch);
      exit();
     }
good_const:
    /* output start of code bytes */
    fputc('t', out_file);

    /* read and format code bytes */
    skip_space();
    if (ch != '(')
     {
      printf(invalid_module,"Bad Start of Codebytes");
      printf("Looking For '('\n");
      exit();
     }

    for (count = n = 0; n < codebytes; n++)
     {
      fscanf(in_file, "%d", &ch);
      fprintf(out_file, "%c", (ch & 0x00ff));
count++;
/*    if (count++ == 34)
       {
        fprintf(out_file, "\r\n");
        count = 0;
       } */
     }
    if (count) fprintf(out_file, "\r\n");  /* complete last text record */

    /* flush closing parenthesis from program's end */
    for (n = 0; n < 4; n++)
     {
      skip_space();
      if (ch != ')')
       {
        printf(invalid_module,"Closing Parenthesis");
        exit();
       }
     }

    /* write trailer record for object program */
    fprintf(out_file, "z\r\n");
    printf("module %d complete\n", module++);

    /* skip over white space at end of program */
    skip_space();
  }

  /* close the input and output files */
  if (fclose(in_file)) printf("***Error Closing Input File***\n");
  if (fclose(out_file)) printf("***Error Closing Output File***\n");

 } /* end of function:  main(argc, argv) */

/************************************************************************/
/*                      Process Constant List Entry                     */
/************************************************************************/
process_constant()
 {
  int big_parts[2];     /* the two words of a 32-bit bignum value */
  char buffer[300];     /* input buffer */
  int ch;               /* the current character */
  char *ch_buffer;      /* character string buffer (dynamic) */
  double flonum;        /* a floating point value */
  int flo_parts[4];     /* the four words of a floating point value */
  int i,j;              /* our old favorite index variables */
  int n;                /* character count */
  int sign;             /* sign flag for integer values */
  long value;           /* accumulator for integer values */
  int  vect_count;      /* number of entries in a vector */
  long vect_end;        /* ending offset in out_file for a vector */
  long vect_start;      /* starting offset in out_file for a vector */

  long ftell();         /* returns the current position of a file */

  /* if current constant entry is "long enough", break this record */
  if (count > 72)
   {
    fprintf(out_file, "\r\n");
    count = 0;
   }

  /* skip over white space in front of constant */
  skip_space();

  /* make an initial attempt to decide the constant's type */
  switch (ch)
   {
    case '"':  /* the beginnings of a character string */
               if (!(ch_buffer = getmem(MAX_STRING_LENGTH)))
                {
                 printf("***Error-- Memory Exhausted***\n");
                 exit();
                }
               n = 0;
               do {
                   ch = fgetc(in_file);
                   if (ch != '\r')
                    {
                     if (ch == '\\')
                      {
                       ch_buffer[n++] = fgetc(in_file);
                      }
                     else
                      {
                       ch_buffer[n++] = ch;
                      }
                    }
                  } while (ch != '"');
               n--; /* decrement to remove the closing '"' */
               if (n > MAX_STRING_LENGTH)
                {
                 printf("***Error-- String Too Long***\n%s",
                        "MAKE_FSL limits strings to %d characters\n",
                        MAX_STRING_LENGTH);
                 exit();
                }
               fprintf(out_file, "s%04X", n);
               for (i = 0; i < n; i++) fputc(ch_buffer[i], out_file);
               count += (n + 3);
               if (rlsmem(ch_buffer, MAX_STRING_LENGTH))
                {
                 printf("***Warning-- Release Memory Error***\n");
                }
               break;

    case '(':  /* begin a list */
               ch = fgetc(in_file);
               if (ch == ')')
                { /* empty list found-- output "nil" */
                 fputc('n', out_file);
                 count++;
                 goto blow_this_place;
                }
               else
                {
nuther_list:
                 ungetc(ch, in_file);
                 fputc('l', out_file);
                 count++;
                 process_constant();
                 skip_space();
                 if (ch == ')')
                  {
                   fputc('n', out_file);
                   count++;
                   goto blow_this_place;
                  }
                 else
                  {
                   if (ch == '.')
                    {
                     ch = fgetc(in_file);
                     if (!isspace(ch))
                      {
                       /* character after '.'-- it's a symbol */
                       ungetc(ch, in_file);
                       ch = '.';
                       goto nuther_list;
                      }
                     process_constant();
                     skip_space();
                     if (ch != ')')
                      {
                       printf("***Error-- Invalid List Constant (cdr)***\n");
                       exit();
                      }
                     goto blow_this_place;
                    }
                   else goto nuther_list;
                  }
                }


    case '|':  /* a special slashified symbol */
               n = 0;
               do {
                   ch = fgetc(in_file);
                   if (ch == '\\') ch = fgetc(in_file);    /*** 2/14/86 ***/
                   if (ch != '\r')
                    {
                     if (ch == '|')
                      {
                       if (n > 0x00ff)
                        {
                         printf("***Error-- Symbol Too Long***\n%s",
                                "MAKE_FSL limits symbols to 255 characters\n");
                         exit();
                        }
                       fprintf(out_file, "x%02X", n);
                       for (i = 0; i < n; i++) fputc(buffer[i], out_file);
                       count += (n+3);
                       goto blow_this_place;
                      }
                     buffer[n++] = ch;
                    }
                  } while (TRUE);

    case '#':  /* special constants (we hope) */
               n = 0;
               buffer[n++] = ch;
               ch = fgetc(in_file);
               switch(ch)
                {
                 case '\\':  /* character constant */
                             buffer[n++] = ch;  /* store '\' */
                             ch = fgetc(in_file);
                             buffer[n++] = ch;  /* store 1st character */
                             ch = fgetc(in_file);
                             while ((!isspace(ch)) && ch != ')')
                              {
                               buffer[n++] = ch;
                               ch = fgetc(in_file);
                              }
                             if (ch == ')') ungetc(ch, in_file);
                             if (n == 3)
                              {
                               fprintf(out_file, "c%02X", buffer[2]);
                               count += 3;
                              }
                             else
                              { /* test for a multi-character character */
                               buffer[n] = '\0';
                               for (j = 0; j < test_num; j++)
                                {
                                 if (!strcmp(buffer+2, test_string[j]))
                                  {
                                   fprintf(out_file, "c%02X", test_char[j]);
                                   count += 3;
                                   break;
                                  }
                                 if (j == test_num-1)
                                  {
                                   printf("Invalid Character Constant\n");
                                   exit();
                                  }
                                } /* end: for (j = 0; j < test_num; j++) */
                              }
                             goto blow_this_place;

                 case '(':   /* vector constant */
                             vect_start = ftell(out_file);
                             vect_count = 0;
                             fprintf(out_file, "vxxxx");
                             skip_space();
                             while(ch != ')')
                              {
                               ungetc(ch, in_file);
                               process_constant();
                               vect_count++;
                               skip_space();
                              } /* end:  while(ch != ')') */
                             vect_end = ftell(out_file);
                             if (fseek(out_file, vect_start, 0))
                              {
                               printf("file seek error-- vector start\n");
                               exit();
                              }
                             fprintf(out_file, "v%04X", vect_count);
                             if (fseek(out_file, vect_end, 0))
                              {
                               printf("file seek error-- vector end\n");
                               exit();
                              }
                             goto blow_this_place;

                 default:    /* other special constant */
                             goto right_here;  /* treat as a symbol */
                }

    default:   /* must look at symbol to see what it is */
               n = 0;
right_here:
               while (!isspace(ch))
                {
                 if (ch == ')')
                  {
                   ungetc(ch, in_file);
                   break;
                  }
                 else
                  {
                   if (n >= sizeof(buffer)) goto too_long;
                   buffer[n++] = ch;
                   ch = fgetc(in_file);
                  }
                }
               i = 0;
               if (buffer[0] == '-' && n > 1) i++;
               for (i; i < n; i++)
                {
                 if (!isdigit(buffer[i]))
                  {
                   if (buffer[i] == '.' && (i > 0 || isdigit(buffer[i+1])))
                    {
                     buffer[n++] = '\0';
                     sscanf(buffer, "%lf", &flonum);
                     get_parts(flo_parts, flonum);
                     fprintf(out_file, "f%04X%04X%04X%04X", flo_parts[0],
                        flo_parts[1], flo_parts[2], flo_parts[3]);
                     count += 17;
                     goto blow_this_place;
                    }
                   else
                    { /* this here's got to be a symbol */
                     if (n > 0x00ff)
                      {
too_long:
                       printf("***Error-- Symbol Too Long***\n%s",
                              "MAKE_FSL limits symbols to 255 characters\n");
                       exit();
                      }
                     /* special fixup to remove escaping \'s */
                     for (i=0; i<n; i++)
                     {
                       if (buffer[i] == '\\')
                       {
                         for (j=i; j < n-1; j++) buffer[j] = buffer[j+1];
                         n--;
                       }
                     }
                     fprintf(out_file, "x%02X", n);
                     for (i = 0; i < n; i++) fputc(toupper(buffer[i]),
                                                                out_file);
                     count += (n + 3);
                     goto blow_this_place;
                    }
                  } /* end:  if (!isdigit(buffer[i]))) */
                } /* end:  for (i; i < n; i++) */
               /* if loop exits, we've got a fixnum */
               i = sign = 0;
               value = 0L;
               if (buffer[0] == '-')
                {
                 sign = 1;
                 i++;
                }
               for (i; i < n; i++)
                {
                 j = (buffer[i] - '0');
                 if (value > ((0x7fffffff - j)/10))
                  {
                   printf("***Error-- Integer Too Big***\n%s",
"MAKE_FSL requires integers be between -2,147,483,647 and 2,147,483,647\n");
                   exit();
                  }
                 value = (value * 10) + j;
                }
               if (sign) value = -value;
               if (value <= 16383 && value >= -16384)
                {
                 i = value & 0x00007fff;
                 fprintf(out_file, "i%04X", i);
                 count += 5;
                 goto blow_this_place;
                }
               else
                {
                 if (sign) value = -value;
                 get_pieces(big_parts, value);
                 fprintf(out_file, "b02%02X%04X%04X", (sign ? 1 : 0),
                                big_parts[0], big_parts[1]);
                 count += 13;
                }
   } /* end:  switch (ch) */

blow_this_place:
 } /* end of function:  process_constant() */

/************************************************************************/
/*              Coerce a floating point value to "words"                */
/************************************************************************/
get_parts(flo_parts, flonum)
double *flo_parts, flonum;
 {
  *flo_parts = flonum;
 }

/************************************************************************/
/*                    Coerce a bignum value to "words"                  */
/************************************************************************/
get_pieces(big_parts, value)
long *big_parts, value;
 {
  *big_parts = value;
 }
