/*                                                      =====> SPRINTF.C     */
/* TIPC Scheme '84 Runtime Support - C Compatible I/O Routines
   (C) Copyright 1984,1985 by Texas Instruments Incorporated.
   All rights reserved.

   Author:  John C. Jensen
   Installation:  Texas Instruments Incorporated, Dallas, Texas
   Division:  Central Research Laboratories
   Cost Center:  Computer Science Laboratory
   Project:  Computer Architecture Branch
   Date Written:  7 December 1984
   Last Modification:  10 September 1985 by Rusty Haddock
*/
#include "scheme.h"

/************************************************************************/
/*                      Main Print Driver - printf                      */
/*                                                                      */
/* Acknowledgement:  This routines perform formatted output functions   */
/*                      similar to that of the Lattice C compiler.      */
/*                      Some of the following descriptions are          */
/*                      excerpted from the Lattice 8086/8088 C Compiler */
/*                      manual.                                         */
/*                                                                      */
/* Description:  The control string (format statement) contains         */
/*                      ordinary characters, which are sent without     */
/*                      modification to the standard output port, and   */
/*                      format specifiers of the form:                  */
/*                                                                      */
/*                                      %-m.plX                         */
/*                                                                      */
/*                      where (1) the optional "-" indicates the field  */
/*                      is to be left justified (right justified is the */
/*                      default); (2) the optional "m" field is a       */
/*                      decimal number specifying a minimum field width;*/
/*                      (3) the optional ".p" field is the character '.'*/
/*                      followed by a decimal number specifying the     */
/*                      precision of a floating point image or the      */
/*                      maximum number of characters to be printed from */
/*                      a string; (4) the optional "l" (letter ell)     */
/*                      indicates that the item to be formatted is      */
/*                      "long"; and (5) "X" is one of the format type   */
/*                      indicators from the following list:             */
/*                                                                      */
/*                         d -- decimal signed integer                  */
/*                         u -- decimal unsigned integer                */
/*                         x -- hexadecimal integer                     */
/*                         o -- octal integer                           */
/*                         s -- character string                        */
/*                         c -- single character                        */
/*                         f -- fixed decimal floating point            */
/*                         e -- exponential floating point              */
/*                         g -- use "e" or "f", whichever is shorter    */
/*                                                                      */
/*                      The format type must be specified in lower case.*/
/*                      Characters in the control string which are not  */
/*                      part of a format specified are sent to the      */
/*                      output port; a % may be sent by using the       */
/*                      sequence %%.  See the Kernighan and Ritchie     */
/*                      text for a more detailed explanation of the     */
/*                      formatted output functions.                     */
/************************************************************************/

  /* working variables visible to all output routines in this module */
  static int auto_CR = TRUE; /* flag for inserting a "carriage return"
                                   whenever a "line feed" is encountered */
  static int field_width = 0;/* width (number of characters) of format field */
  static int leading_zeros = FALSE;  /* indicates if leading zeros desired */
  static int left_justified = FALSE; /* indicates if field left justified */
  static int long_object = FALSE;    /* indicates if a data item is "long" */
  static int precision = 0;  /* the precision of a floating point image, or
                                the maximum number of character to be printed
                                from a string */

  static char digit[16] = {'0','1','2','3','4','5','6','7','8','9',
                           'A','B','C','D','E','F'};

  char *concat_str();           /* concatenate two character strings */
  char *copy_str();             /* make copy of a character string */
  char *fmt_hex();              /* format a hexadecimal value */
  char *fmt_int();              /* format a signed integer value */
  char *fmt_long();             /* format a long signed integer value */
  char *fmt_unsigned();         /* format an unsigned integer value */
  char *getmem();               /* Lattice C's memory allocation routine */

printf(fmt, data)
char *fmt;              /* the "format statement" describing the I/O */
int  data;              /* the first data object to be printed */
 {
  char *buffer;         /* temporary output buffer for converted values */
  int *next;            /* pointer to the next object to be printed */
  double *next_float;   /* pointer to the next floating point object */
  long *next_long;      /* pointer to the next long object to be printed */

  ENTER(printf);

  /* set the default port address for the I/O operation */
  ssetadr(ADJPAGE(OUT_PAGE), OUT_DISP);

  next = &data;         /* create a pointer to the next object to print */

  /* Continue output until format is exhausted */
  while (*fmt)
   {
    if (*fmt == '%')
     {
      buffer = NULL;
      leading_zeros = FALSE;
      left_justified = FALSE;
      long_object = FALSE;
      field_width = 0;
      precision = 0;
      fmt++;            /* advance pointer to next character in format */

      /* test if field is to be left justified */
      if (*fmt == '-')
       {
        left_justified = TRUE;
        fmt++;
       }

      /* test for request for leading zeros ('0' follows the '%') */
      if (*fmt == '0')
       {
        leading_zeros = TRUE;
        fmt++;
       }

      /* determine the field width, if specified */
      while (isdigit(*fmt))
       {
        field_width = (field_width * 10) + *fmt - '0';
        fmt++;
       }

      /* test for a "precision" field */
      if (*fmt == '.')
       {
        fmt++;
        while (isdigit(*fmt))
         {
          precision = (precision * 10) + *fmt - '0';
          fmt++;
         }
       } /* end:  if (*fmt == '.') */

      /* test for a "long" object */
      if (*fmt == 'l')
       {
        long_object = TRUE;
        fmt++;
       }

      /* decode the format specifier */
      switch (*fmt)
       {
        case 'c': /* single character */
                  prtf_character(*next);
                  next++;
                  break;

        case 'd': /* decimal signed integer */
                  if (long_object)
                   {
                    next_long = (long *) next;
                    buffer = fmt_long(*next_long);
                    next++;
                   }
                  else
                   {
                    buffer = fmt_int(*next);
                   }
                  next++;
                  break;

        case 'e':
        case 'f':
        case 'g': /* Floating-point numbers */
                  next_float = (double *) next;
                  fmt_float(*next_float,*fmt);
                  next += 4;
                  break;

/***** WATCH OUT!!!  Lattice C allows nested comments!!!
        case 'o': /* octal integer */
                  prtf_string("<%o not implemented>");
                  next++;
                  break;
*****/
        case 's': /* character string */
                  prtf_string(*next);
                  next++;
                  break;

        case 'u': /* decimal unsigned integer */
                  if (long_object)
                   {
                    prtf_string("<long objects not implemented>");
                    next++;
                   }
                  else
                   {
                    buffer = fmt_unsigned(*next);
                   }
                  next++;
                  break;

        case 'x': /* hexadecimal integer */
                  if (long_object)
                   {
                    prtf_string("<long objects not implemented>");
                    next++;
                   }
                  else
                   {
                    buffer = fmt_hex(*next);
                   }
                  next++;
                  break;

        case '%': /* the percent sign itself */
                  outchar('%');
                  break;

        case '\0': /* unexpected end of format specification */
                  prtf_string("<unexpected end of format>\n");
                  fmt--;
                  break;

        default:  /* unexpected format specifier */
                  outchar('%');
                  outchar(*fmt);
                  prtf_string("< invalid format specifier>", 0, 0, 0);
                  break;
       } /* end:  switch (*fmt) */

      if (buffer)
       {
        prtf_string(buffer);
        rlsstr(buffer);
       }
     }
    else /* just a character to print (not a % formatting directive) */
     {
      outchar(*fmt);
     }

    fmt++;              /* advance pointer to next character in format */
   } /* end:  while (*fmt) */

  /* reset format control variables to permit calls to sub-entry points */
  leading_zeros = FALSE;
  left_justified = FALSE;
  long_object = FALSE;
  field_width = 0;
  precision = 0;
 }

/************************************************************************/
/*                      Print a single character                        */
/************************************************************************/
prtf_character(ch)
char ch;
 {
/*%%int i;                /* our old favorite index variable */*/

  if (field_width)
   {
    if (left_justified)
     {
      outchar(ch);
      pad_with_blanks(field_width - 1);
     }
    else /* right justified */
     {
      pad_with_blanks(field_width - 1);
      outchar(ch);
     }
   }
  else outchar(ch);    /* just print the single character */

 } /* end of function:  prtf_character(ch) */

/************************************************************************/
/*                      Print a character string                        */
/************************************************************************/
prtf_string(string)
char *string;
 {
  int i;                /* our old favorite index variable */
  int len = 0;          /* the actual length of the character string */

  /* determine string length (search for '\0' end-of-string mark) */
  while (string[len]) len++;

  /* if precision field specified, reduce "len" if too long */
  if (precision && precision < len) len = precision;

  /* output string with appropriate justification */
  if (left_justified)
   {
    for (i = 0; i < len; i++) outchar(string[i]);
    pad_with_blanks(field_width - len);
   }
  else /* right justified */
   {
    pad_with_blanks(field_width - len);
    for (i = 0; i < len; i++) outchar(string[i]);
   }
 }


/************************************************************************/
/*                      Format a floating point number                  */
/************************************************************************/
fmt_float(value,type)
double value;
char type;
 {
  char buf[32];
  int siz;
  siz = ((type=='g')? (outrange(value)) : (type=='e'));
  siz = makeflo(value,buf,precision,siz);
  buf[siz] = '\0';
  prtf_string(buf);
 } /* end of function:  fmt_float(value,type) */


/************************************************************************/
/*                      Format a signed decimal integer                 */
/************************************************************************/
char *fmt_int(value)
int value;
 {
  long lvalue;
  lvalue = value;
  return(fmt_long(lvalue));
/*****
  char buffer[100];     /* buffer to hold characters of converted value */
  int digit_count = 0;  /* count of significant digits */
  int i,j;              /* index variables */
  int negative = FALSE; /* flag to indicate sign of number */

  /* test the sign of the integer-- if negative, record that fact */
  if (value < 0)
   {
    negative = TRUE;
    value = -value;
    field_width--;
   }

  /* convert the integer to ASCII */
  i = sizeof(buffer) - 1;
  buffer[i--] = '\0';           /* insert end of string indicator */
  do {
      buffer[i--] = digit[value % 10];
      value /= 10;
      digit_count++;
     } while (value);

  /* if leading zeros are required, insert said */
  if (leading_zeros)
   {
    for (j = digit_count; j < field_width; j++)
      buffer[i--] = '0';
   }

  /* if negative, insert a '-' sign */
  if (negative)
   {
    buffer[i--] = '-';
   }

  /* return the formatted integer */
  return(copy_str(buffer+i+1));
*****/
 } /* end of function:  char *fmt_int(value) */


/************************************************************************/
/*              Format a long signed decimal integer                    */
/************************************************************************/
char *fmt_long(value)
long value;
 {
  char buffer[100];     /* buffer to hold characters of converted value */
  int digit_count = 0;  /* count of significant digits */
  int i,j;              /* index variables */
  int negative = FALSE; /* flag to indicate sign of number */

  /* test the sign of the integer-- if negative, record that fact */
  if (value < 0)
   {
    negative = TRUE;
    value = -value;
    field_width--;
   }

  /* convert the integer to ASCII */
  i = sizeof(buffer) - 1;
  buffer[i--] = '\0';           /* insert end of string indicator */
  do {
      buffer[i--] = digit[value % 10];
      value /= 10;
      digit_count++;
     } while (value);

  /* if leading zeros are required, insert said */
  if (leading_zeros)
   {
    for (j = digit_count; j < field_width; j++)
      buffer[i--] = '0';
   }

  /* if negative, insert a '-' sign */
  if (negative)
   {
    buffer[i--] = '-';
   }

  /* return the formatted integer */
  return(copy_str(buffer+i+1));

 } /* end of function:  char *fmt_long(value) */


/************************************************************************/
/*                      Format an unsigned decimal integer              */
/************************************************************************/
char *fmt_unsigned(value)
unsigned value;
 {
  int i,j;              /* index variables */
  char buffer[100];     /* buffer to hold characters of converted value */
  int digit_count = 0;  /* count of significant digits */

  /* convert the integer to ASCII */
  i = sizeof(buffer) - 1;
  buffer[i--] = '\0';           /* insert end of string indicator */
  do {
      buffer[i--] = digit[value % 10];
      value /= 10;
      digit_count++;
     } while (value);

  /* if leading zeros are required, insert said */
  if (leading_zeros)
   {
    for (j = digit_count; j < field_width; j++)
      buffer[i--] = '0';
   }

  /* return the formatted unsigned integer */
  return(copy_str(buffer+i+1));

 } /* end of function:  char *fmt_unsigned(value) */


/************************************************************************/
/*                      Format a hexadecimal integer                    */
/************************************************************************/
char *fmt_hex(value)
unsigned value;
 {
  int i,j;              /* index variables */
  char buffer[100];     /* buffer to hold characters of converted value */
  int digit_count = 0;  /* count of significant digits */

  /* convert the integer to ASCII */
  i = sizeof(buffer) - 1;
  buffer[i--] = '\0';           /* insert end of string indicator */
  do {
      buffer[i--] = digit[value & 0xf];
      value = value >> 4;
      digit_count++;
     } while (value);

  /* if leading zeros are required, insert said */
  if (leading_zeros)
   {
    for (j = digit_count; j < field_width; j++)
      buffer[i--] = '0';
   }

  /* return the hexadecimal integer value */
  return(copy_str(buffer+i+1));

 } /* end of function:  char *fmt_hex(value) */


/************************************************************************/
/*                            Pad with blanks                           */
/************************************************************************/
pad_with_blanks(number)
int number;             /* the number of blanks to output */
 {
  int i;                /* index variable */

  /* spew out the appropriate number of blanks */
  for (i = 0; i < number; i++) outchar(' ');

 } /* end of function:  pad_with_blanks */


/************************************************************************/
/*                      Create copy of a character string               */
/************************************************************************/
char *copy_str(string)
char *string;
 {
  char *ret_str;
  ENTER(copy_str);
  if (!(ret_str = getmem(strlen(string)+1))) getmem_error(rtn_name);
  else
   {
    strcpy(ret_str, string);
   }
  return(ret_str);
 }


/************************************************************************/
/*           Create concatenation of two character strings              */
/************************************************************************/
char *concat_str(str1,str2)
char *str1,*str2;
 {
  ENTER(concat_str);
  char *ret_str;
  if (!(ret_str = getmem(strlen(str1) + strlen(str2) + 1)))
    getmem_error(rtn_name);
  else
   {
    strcpy(ret_str, str1);
    strcat(ret_str, str2);
   }
  return(ret_str);
 }
