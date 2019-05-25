/*                                                      =====> ZCIO.C     */
/* TIPC Scheme Runtime Support - Window Input/Output Support
   (C) Copyright 1985 by Texas Instruments Incorporated.
   All rights reserved.

   Author:  John C. Jensen
   Installation:  Texas Instruments Incorporated, Dallas, Texas
   Division:  Central Research Laboratories
   Cost Center:  Computer Science Laboratory
   Project:  Computer Architecture Branch
   Date Written:  1 February 1985
   Last Modification:
     11 Feb 86 - Fixed READ-CHAR for "read from string".
               - added a call to "force-reset" after an error was encountered
                 in routine "takechar".
               - modified routine "takechar" to correctly report an error
                 when an invalid status is returned from a write. Added the
                 message "Write error: DOS error code X".
     April 86  - Most codes are moved to Assembly language. The modules
                 left are: clear_window, gc_on, gc_off
*/
#include "scheme.h"
#include "sport.h"
extern unsigned GC_ING;         /* Garbage collecting indicator */



/********************** Following Code commented out
#include "slist.h"

int prn_handle = 0;             /* current printer handle */
static int handle = 0;          /* current port file's handle */
static int port_disp = 0;       /* current port displacement */
static int port_page = 0;       /* current port page number */
static int port_reg[2] = {0,0}; /* current port pointer */
static int window_p = FALSE;    /* flag indicating if port is a window */
static int string_p = FALSE;    /* flag indicating string I/O */

#define NUM_FIELDS 12
static int defaults[NUM_FIELDS] = {0,   /* cursor line number */
                                   0,   /* cursor column number */
                                   0,   /* upper left corner line number */
                                   0,   /* upper left corner column number */
                                  25,   /* number of lines */
                                  80,   /* number of columns */
                                  -1,   /* no border */
                                  15,   /* text high intensity, enable */
                                   1,   /* wrap enabled */
                                   0,   /* current buffer position */
                                   0,   /* current buffer end */
TRANSCRIPT+BINARY+WINDOW+OPEN+READ_WRITE}; /* port attributes */
static int map_attr[NUM_FIELDS] = {10,12,14,16,18,20,22,24,26,28,30,6};

/* Extended Character Definitions */
#define F3 0x003d
#define F5 0x003f
#define LEFT_ARROW 0x004b
#define RIGHT_ARROW 0x004d
#define ENTER_KEY 0x000d
#define INSERT 0x0052
#define DELETE 0x0053

char *getmem();         /* Lattice C's memory allocation support */

/************************************************************************/
/*                      Allocate a Window                               */
/************************************************************************/
make_window(reg)
int reg[2];             /* register to receive pointer to window object */
 {
  int disp;             /* displacement component of a pointer */
  int i;                /* index variable */
  int page;             /* page number component of a pointer */
  int retstat = 0;      /* the return status */

  ENTER(make_window);

  page = CORRPAGE(reg[C_PAGE]);
  if (ptype[page] == STRTYPE*2 || (!page))
   {
    /* allocate window object (port data type) */
    mov_reg(tmp_reg, reg);      /* save pointer to window label */
    alloc_block(reg, PORTTYPE, WINDSIZE+BUFFSIZE);
    page = CORRPAGE(reg[C_PAGE]);
    disp = reg[C_DISP];
    zero_blk(page,disp);

    /* initialize fields of window object */
    for (i = 0; i < NUM_FIELDS; i++)
      put_word(page, disp+map_attr[i], defaults[i]);

    /* store window label pointer into window object */
    put_ptr(page, disp+STR_PTR, tmp_page, tmp_disp);
   }
  else
   {
    set_src_err("MAKE-WINDOW", 1, reg);
    retstat = -1;
   }

  return(retstat);
 } /* end of function:  make_window(reg) */
************************************************/



/************************************************************************/
/*                              Clear Window                            */
/************************************************************************/
clear_window(reg)
int reg[2];             /* register containing port pointer */
 {
  int b_attrib;         /* border attributes */
  int disp;             /* displacement component of a pointer */
/*%%int i;                /* index variable */*/
/*%%int len;              /* label length (in characters) */*/
  int n_cols;           /* number of columns in the window */
  int n_lines;          /* number of lines in the window */
  int page;             /* page number component of a pointer */
  int retstat = 0;      /* the return status */
  char *string;         /* buffer pointer for label's text */
  int t_attrib;         /* text attributes */
  int ul_col;           /* upper left corner's column number */
  int ul_line;          /* upper left corner's line number */

  char *string_asciz(); /* fetches characters of a string */

  ENTER(clear_window);

  get_port(reg,0);
  page = CORRPAGE(tmp_page);
  disp = tmp_disp;
  if (ptype[page] == PORTTYPE*2 &&
      get_byte(page, disp+P_FLAGS) & WINDOW)
   {
    pt_flds4(tmp_reg, &ul_line, &ul_col, &n_lines, &n_cols);
    t_attrib = get_word(page, disp+T_ATTRIB);
    b_attrib = get_word(page, disp+B_ATTRIB);
    zclear(ul_line, ul_col, n_lines, n_cols, t_attrib);
    if (b_attrib != -1)
     {
      tmp_page = get_byte(page, disp+STR_PTR);
      tmp_disp = get_word(page, disp+STR_PTR+1);
      string = string_asciz(tmp_reg);
      zborder(ul_line, ul_col, n_lines, n_cols, b_attrib, string);
      rlsstr(string);
     }
    /* put the cursor in the "home" position (upper left hand corner) */
    put_word(page, disp+CUR_LINE, 0);
    put_word(page, disp+CUR_COL, 0);
   }
  else
   {
    set_src_err("WINDOW-CLEAR", 1, reg);
    retstat = -1;
   }
  return(retstat);
 } /* end of function:  clear_window(reg) */

/************
/************************************************************************/
/*                      Get Window Attribute                            */
/************************************************************************/
get_window_attribute(reg, attr)
int reg[2];             /* register containing port pointer */
int attr[2];            /* attribute number */
 {
  int attribute;        /* index of the attribute desired */
  int disp;             /* displacement component of a pointer */
  int page;             /* page number component of a pointer */
  int retstat = 0;      /* the return status */

  ENTER(get_window_attribute);

  get_port(reg, 1);
  page = CORRPAGE(tmp_page);
  disp = tmp_disp;
  if (ptype[page] == PORTTYPE*2 &&
      attr[C_PAGE] == SPECFIX*2 &&
      (attribute = get_fix(CORRPAGE(attr[C_PAGE]),attr[C_DISP])) >= 0 &&
      attribute < NUM_FIELDS)
   {
    reg[C_PAGE] = SPECFIX*2;
    reg[C_DISP] = get_word(page, disp+map_attr[attribute]) & 0x7fff;
   }
  else
   {
    set_src_err("%REIFY-PORT", 2, reg, attr);
    retstat = -1;
   }
  return(retstat);
 } /* end of function:  get_window_attribute(reg, attr) */

/************************************************************************/
/*                      Set Window Attribute                            */
/************************************************************************/
set_window_attribute(reg, attr, value)
int reg[2];             /* register containing port pointer */
int attr[2];            /* attribute number */
int value[2];           /* new value for attribute */
 {
  int attribute;        /* index of the attribute desired */
  int disp;             /* displacement component of a pointer */
  int page;             /* page number component of a pointer */
  int retstat = 0;      /* the return status */
  int v;                /* the new attribute value */
  int fld;              /* offset of lines/columns field */
  int maxnum;           /* maximum possible number of lines/columns */

  ENTER(set_window_attribute);

  get_port(reg, 1);
  page = CORRPAGE(tmp_page);
  disp = tmp_disp;
  if (ptype[page] == PORTTYPE*2 &&
      attr[C_PAGE] == SPECFIX*2 &&
      (attribute = get_fix(CORRPAGE(attr[C_PAGE]),attr[C_DISP])) >= 0 &&
      attribute < NUM_FIELDS &&
      value[C_PAGE] == SPECFIX*2)
   {
    v = get_fix(CORRPAGE(value[C_PAGE]), value[C_DISP]);
    switch (attribute)
     {
      case 0:  /* cursor line */
      case 1:  /* cursor column */
               if (v < 0) goto src_err;
               break;

      case 2:  /* upper left corner line */
               fld = N_LINES;  maxnum = MAX_LINES;
               goto FIT;
               /*
               v = fit_in_range(v, 0, MAX_LINES-1);
               if (get_word(page, disp+N_LINES) > MAX_LINES - v)
                 put_word(page, disp+N_LINES, MAX_LINES - v);
               break;
               */

      case 3:  /* upper left corner column */
               fld = N_COLS;  maxnum = MAX_COLUMNS;
FIT:
               v = fit_in_range(v, 0, maxnum-1);
               if (get_word(page, disp+fld) > maxnum-v)
                 put_word(page, disp+fld, maxnum-v);
               break;
               /*
               v = fit_in_range(v, 0, MAX_COLUMNS-1);
               if (get_word(page, disp+N_COLS) > MAX_COLUMNS - v)
                 put_word(page, disp+N_COLS, MAX_COLUMNS - v);
               break;
               */

      case 4:  /* number of lines */
               v = fit_in_range(v, 1, MAX_LINES-get_word(page,disp+UL_LINE));
               break;

      case 5:  /* number of columns */
               if (!(get_word(page,disp+P_FLAGS) & WINDOW)) break;
               v = fit_in_range(v, 1, MAX_COLUMNS-get_word(page,disp+UL_COL));
               break;

      default:  goto src_err;

      case 6:  /* border color/attributes */
      case 7:  /* text color/attributes */
      case 8:  /* flags */
      case 9:  /* buffer position */
      case 10: /* buffer end */
      case 11: /* port flags */
     }
    put_word(page, disp+map_attr[attribute], v); /* make the change */
   }
  else
   {
src_err:
    set_src_err("%REIFY-PORT!", 3, reg, attr, value);
    retstat = -1;
   }
  return(retstat);
 } /* end of function:  set_window_attribute(reg, attr, value) */

/************************************************************************/
/*                      Save Window Contents                            */
/************************************************************************/
save_window(reg)
int reg[2];             /* pointer to window object */
 {
  int page,disp;        /* pointer components */
  int n_cols;           /* number of columns in the window */
  int n_lines;          /* number of lines in the window */
  int retstat = 0;      /* the return status */
  int ul_col;           /* upper left hand corner's column number */
  int ul_line;          /* upper left hand corner's line number */

  ENTER(save_window);

  get_port(reg,0);
  page = CORRPAGE(tmp_page);
  disp = tmp_disp;
  if (ptype[page] == PORTTYPE*2 &&
      get_word(page, disp+P_FLAGS) & WINDOW)
   {
    pt_flds4(tmp_reg, &ul_line, &ul_col, &n_lines, &n_cols);
    if (/* bordered? */ get_word(page, disp+B_ATTRIB) != -1)
     {
      adj4bord(&ul_line, &n_lines, &ul_col, &n_cols);
      /*
      if (ul_line) { ul_line--; n_lines++; }
      if (ul_col) { ul_col--; n_cols++; }
      if (ul_line + n_lines < MAX_LINES) n_lines++;
      if (ul_col + n_cols < MAX_COLUMNS) n_cols++;
      */
     }
    alloc_block(reg, STRTYPE, ((n_lines * n_cols) * 2) + 2);
    save_scr(reg, ul_line, ul_col, n_lines, n_cols);
   }
  else
   {
    set_src_err("WINDOW-SAVE-CONTENTS", 1, reg);
    retstat = -1;
   }
  return(retstat);
 } /* end of function:  save_window(reg) */

/************************************************************************/
/*                      Restore Window Contents                         */
/************************************************************************/
rest_window(reg,data)
int reg[2];             /* pointer to window object */
int data[2];            /* data to be restored */
 {
  int page,disp;        /* pointer components */
  int n_cols;           /* number of columns in the window */
  int n_lines;          /* number of lines in the window */
  int retstat = 0;      /* the return status */
  int ul_col;           /* upper left hand corner's column number */
  int ul_line;          /* upper left hand corner's line number */

  ENTER(rest_window);

  get_port(reg,0);
  page = CORRPAGE(tmp_page);
  disp = tmp_disp;
  if (ptype[CORRPAGE(data[C_PAGE])] == STRTYPE*2 &&
      ptype[page] == PORTTYPE*2 &&
      get_word(page, disp+P_FLAGS) & WINDOW)
   {
    pt_flds4(tmp_reg, &ul_line, &ul_col, &n_lines, &n_cols);
    if (/* bordered? */ get_word(page, disp+B_ATTRIB) != -1)
     {
      adj4bord(&ul_line, &n_lines, &ul_col, &n_cols);
      /*
      if (ul_line) { ul_line--; n_lines++; }
      if (ul_col) { ul_col--; n_cols++; }
      if (ul_line + n_lines < MAX_LINES) n_lines++;
      if (ul_col + n_cols < MAX_COLUMNS) n_cols++;
      */
     }
    rest_scr(data, ul_line, ul_col, n_lines, n_cols);
   }
  else
   {
    set_src_err("WINDOW-RESTORE-CONTENTS", 2, reg, data);
    retstat = -1;
   }
  return(retstat);
 } /* end of function:  rest_window(reg, data) */

/************************************************************************/
/*                      Output Character to Window                      */
/*                                                                      */
/* Description:  This routine writes a character to the current cursor  */
/*              position, then increments the cursor location.          */
/*              If the current cursor position is now within the bounds */
/*              of the window, the character is output in the first     */
/*              column of the next line, scrolling the window, if       */
/*              necessary.  The current text attributes are used to     */
/*              write the character.                                    */
/************************************************************************/
putc_window(window, ch)
int window[2];          /* register containing the window pointer */
int ch;                 /* the character to be written to the window */
 {
  int cur_col;          /* current cursor position column number */
  int cur_line;         /* current cursor position line number */
  int n_cols;           /* number of columns in the window */
  int n_lines;          /* number of lines in the window */
  int page, disp;       /* page/displacement components of window pointer */
  int t_attrib;         /* window's text character attributes */
  int ul_col;           /* upper left hand corner column number */
  int ul_line;          /* upper left hand corner line number */

  page = CORRPAGE(window[C_PAGE]);
  disp = window[C_DISP];
  if (ptype[page] == PORTTYPE*2)
   {
    if (get_byte(page, disp+P_FLAGS) & OPEN)
     {
      pt_flds6(window, &cur_line, &cur_col,
               &ul_line, &ul_col, &n_lines, &n_cols);
      t_attrib = get_word(page, disp+T_ATTRIB);
      switch(ch)
       {
        case '\b':  /* process backspace character */
                    cur_col--;
                    if (cur_col < 0) cur_col = 0;

        case '\0':  /* null character-- do nothing */
                    break;

        case '\007':  /* bell character-- sound the alarm */
                    zbell();
                    break;

        case '\t':  /* process tab character */
                    cur_col += (8 - (cur_col % 8));
                    break;

        case '\r':  /* process carriage return */
                    cur_col = 0;
                    break;

        case '\n':  /* process newline */
                    cur_col = 0;
                    cur_line++;
                    if (cur_line >= n_lines)
                     {
                      zscroll(ul_line, ul_col, n_lines, n_cols, t_attrib);
                      cur_line = n_lines-1;
                     }
                    break;

        default:    if (cur_col >= n_cols)
                     {
                      if (get_word(page, disp+W_FLAGS) & WRAP)
                       {
                        cur_line++;
                        cur_col=0;
                       }
                      else
                       {
                        cur_col++;
                        break;
                       }
                     }
                    if (cur_line >= n_lines)
                     {
                      zscroll(ul_line, ul_col, n_lines, n_cols, t_attrib);
                      cur_line = n_lines-1;
                      cur_col = 0;
                     }
                    zputc(cur_line+ul_line, cur_col+ul_col, ch, t_attrib);
                    cur_col++;
       } /* end:  switch(ch) */
      /* record new cursor position (next character position) */
      put_word(page, disp+CUR_LINE, cur_line);
      put_word(page, disp+CUR_COL, cur_col);
     }
   }
  else
   {
    printf("[VM INTERNAL ERROR] Bad port for window output\n");
    force_debug();
   }
 }

/************************************************************************/
/*                      Read a "record" from Window                     */
/************************************************************************/
static int *column;     /* column coordinate vector */
static int cur_col;     /* current cursor position- column number */
static int cur_line;    /* current cursor position- line number */
static int i;           /* index into character buffer */
static int insert_mode; /* insert mode flag */
static int len;         /* local copy of ln */
static int n_cols;      /* number of columns in the window */
static int n_lines;     /* number of lines in the window */
static int page, disp;  /* page/displacement components of window pointer */
static int *row;        /* row coordinate vector */
static int sh_ptr;      /* shadow buffer pointer */
static int t_attrib;    /* window's text character attributes */
static int ul_col;      /* upper left hand corner column number */
static int ul_line;     /* upper left hand corner line number */
static char *string;    /* input buffer pointer */

static int sh_length = 0; /* number of characters in the shadow buffer */
static char *sh_buffer; /* shadow buffer */

read_window(window, str, ln)
int window[2];          /* register containing window pointer */
char *str;              /* character buffer in which to return data read */
int *ln;                /* maximum number of characters to read */
 {
  int ch;               /* character just read */
  int j;                /* index variable */
  int len2;             /* length of input buffer times two (2) */

  ENTER(read_window);

  string = str;
  sh_ptr = i = 0;
  insert_mode = FALSE;
  len2 = (*ln) + (*ln);
  len = (*ln) - 3;

  if (!(row = (int *) getmem(len2)) || !(column = (int *) getmem(len2)))
    getmem_error(rtn_name);

  page = CORRPAGE(window[C_PAGE]);
  disp = window[C_DISP];
  if (ptype[page] == PORTTYPE*2)
   {
    pt_flds6(window, &cur_line, &cur_col,
             &ul_line, &ul_col, &n_lines, &n_cols);
    t_attrib = get_word(page, disp+T_ATTRIB);

    zcuron();
    do
     {
      if (cur_line >= n_lines)
       {
        zscroll(ul_line, ul_col, n_lines, n_cols, t_attrib);
        cur_line = n_lines-1;
        cur_col = 0;
       }
      zputcur(ul_line + cur_line, ul_col + cur_col);
      ch = getch();
      switch(ch)
       {
        case '\0':  /* process extended key sequence */
                    ch = getch();
                    switch (ch)
                     {
                      case LEFT_ARROW:  goto backspace;

                      case RIGHT_ARROW:  insert_mode = FALSE;
                                         if (sh_ptr < sh_length)
                                          {
                                           ch = sh_buffer[sh_ptr];
                                           goto one_char;
                                          }
                                         break;

                      case F3:  insert_mode = FALSE;
                                while (i < len && sh_ptr < sh_length)
                                  echo_char(sh_buffer[sh_ptr]);
                                break;

                      case F5:  if (i)
                                 {
                                  rlsstr(sh_buffer);
                                  if (!(sh_buffer = getmem(i+1)))
                                    getmem_error(rtn_name);
                                  str2str(sh_buffer, string, i);
                                  sh_buffer[i] = '\0';
                                  sh_length = i;
                                  for (j = i-1; j >= 0; j--)
                                   {
                                    if (row[j] < 0) break;
                                    zputc(ul_line + (cur_line = row[j]),
                                          ul_col + (cur_col = column[j]),
                                          ' ', t_attrib);
                                   }
                                  sh_ptr = i = 0;
                                 }
                                insert_mode = FALSE;
                                break;

                      case INSERT:  insert_mode = TRUE;
                                    break;

                      case DELETE:  if (sh_ptr < sh_length) sh_ptr++;
                                    insert_mode = FALSE;
                                    break;

                      case ENTER_KEY:  goto carriage_return;

                     } /* end:  switch (ch) */
                    break;

        case '\b':  /* process backspace key */
backspace:
                    if (i <= 0 || row[i-1] < 0) zbell();
                    else
                     {
                      i--;
                      if (sh_ptr) sh_ptr--;
                      cur_col = column[i];
                      cur_line = row[i];
                      zputc(ul_line + cur_line, ul_col + cur_col, ' ',
                                t_attrib);
                     }
                    insert_mode = FALSE;
                    break;

        case '\r':  /* process return key */
carriage_return:
                    string[i++] = '\r';  /* insert carriage return in buffer */
                    string[i++] = '\n';  /* insert line feed in buffer */
                    cur_line++;
                    cur_col = 0;
                    if (cur_line >= n_lines)
                     {
                      zscroll(ul_line, ul_col, n_lines, n_cols, t_attrib);
                      cur_line = n_lines-1;
                     }
                    rlsstr(sh_buffer);
                    if (!(sh_buffer = getmem(i))) getmem_error(rtn_name);
                    str2str(sh_buffer, string, i);
                    sh_buffer[i-1] = '\0';
                    sh_length = i-2;
                    break;

        case '\n':  /* ignore line feed key */
                    break;
one_char:
        default:  if (i >= len)
                    zbell();
                  else echo_char(ch);
       } /* end:  switch(ch) */
     } while (ch != '\r');
    zcuroff();
    put_word(page, disp+CUR_LINE, cur_line);
    put_word(page, disp+CUR_COL, cur_col);
    *ln = i;
   }
  else
   {
    printf("[VM INTERNAL ERROR] Bad port for window input\n");
    force_debug();
   }
  if (rlsmem(row, len2) || rlsmem(column, len2))
    rlsmem_error(rtn_name);
 } /* end of function:  char *read_window(window) */

/************************************************************************/
/*                         Echo Single Character                        */
/************************************************************************/
echo_char(ch)
int ch;
 {
  int j;                /* index variable */
  string[i++] = ch;
  if (!insert_mode) sh_ptr++;
  if (cur_col >= n_cols)
   {
    cur_line++;
    cur_col = 0;
   }
  if (cur_line >= n_lines)
   {
    zscroll(ul_line, ul_col, n_lines, n_cols, t_attrib);
    cur_line = n_lines-1;
    cur_col = 0;
    for (j = 0; j < i; j++) row[j]--;
   }
  row[i-1] = cur_line;
  column[i-1] = cur_col;
  if (ch == '\t')
   {
    if((cur_col += (8 - (cur_col % 8)))>n_cols) cur_col = n_cols;
    if (ul_col + cur_col >= 80) zputcur(cur_line, 79);
   }
  else
   {
    zputc(ul_line + cur_line, ul_col + cur_col, ch, t_attrib);
    cur_col++;
   }
 } /* end of function:  out_char(ch) */

/************************************************************************/
/*                         Force Value Into Range                       */
/*                                                                      */
/* Purpose:  To test a value to determine if it falls within a range    */
/*              of values, as specified by an lower and upper bound.    */
/*              If the value is within the range, the value is returned */
/*              unchanged.  If it is outside the range, the value of the*/
/*              endpoint nearest its value is returned.                 */
/************************************************************************/
fit_in_range(value, lower, upper)
int value;              /* value to be tested */
int lower;              /* lower bound of range */
int upper;              /* upper bound of range */
 {
  return(value < lower ? lower : (value > upper ? upper : value));
 } /* end of function:  fit_in_range(value, lower, upper) */

/************************************************************************/
/*                   Support for "read-char-ready?"                     */
/************************************************************************/
rd_ch_rdy(port)
int port[2];            /* register containing port designation */
 {
  int b_pos;            /* current buffer position */
  int ch;               /* character read from a file */
  int flags;            /* port flags */
  int page;             /* port's page number */
  int retstat = 0;      /* the return status */

  if (!get_port(port, 0))
   {
    port[C_PAGE] = SPECCHAR*2;  /* prepare to return a character */
    page = CORRPAGE(tmp_page);
    if ((b_pos = get_word(page, tmp_disp+BUF_POS)) <
         get_word(page, tmp_disp+BUF_END))
     {
      ch = get_byte(page, tmp_disp+BUFR+b_pos);
return_T:
      if (ch == '\032' && !(get_word(page,tmp_disp+P_FLAGS) & BINARY))
        goto return_eof;
      port[C_DISP] = ch;
      goto end_of_routine;
     }
    else /* no character in input buffer */
     {
      if ((flags = get_byte(page, tmp_disp+P_FLAGS)) & WINDOW)
       {
        if ((ch = zch_rdy()))
         {
          ch = ch & 0x00ff;
          goto return_T;
         }
       }
      else /* not a window */
       {
        if (flags & OPEN)
         {
          ssetadr(tmp_page, tmp_disp);
          ch = take_ch();
          if (ch != 256)
           {
            pushchar();
            port[C_DISP] = ch;
           } /* end:  if (ch != 256) */
          else
           {
return_eof:
            port[C_PAGE] = EOF_PAGE*2;
            port[C_DISP] = EOF_DISP;
           } /* end:  else */
          goto end_of_routine;
         } /* end:  if (flags & OPEN) */
       } /* end:  else /* not a window */  */
     } /* end:  else /* no character in input buffer */  */
   } /* end:  if (!get_port(port, 0)) */
  else
   {
    set_src_err("CHAR-READY?", 1, port);
    retstat = -1;
   }
  /* no character available-- return '() */
  port[C_PAGE] = port[C_DISP] = 0;
end_of_routine:
  return(retstat);
 } /* end of function:  rd_ch_rdy(port) */

/************************************************************************/
/*                       Support for "read-char"                        */
/************************************************************************/
read_char(port)
int port[2];
 {
  int i;                /* temporary */                            /* 2-11-86 */
  int page;             /* port's page number */
  int retstat = 0;      /* the return status */

  if (!get_port(port, 0))
   {
    page = CORRPAGE(tmp_page);
    port[C_PAGE] = SPECCHAR*2;
    if ((i = get_byte(page, tmp_disp+P_FLAGS)) & WINDOW &&        /* 2-11-86 */
        !(i & STRSRC) &&
        get_word(page, tmp_disp+BUF_POS) >= get_word(page, tmp_disp+BUF_END))
     {
      zputcur(get_word(page,tmp_disp+UL_LINE)+get_word(page,tmp_disp+CUR_LINE),
              get_word(page,tmp_disp+UL_COL)+get_word(page,tmp_disp+CUR_COL));
      zcuron();
      port[C_DISP] = getch();
      zcuroff();
      put_byte(page, tmp_disp+BUFR, port[C_DISP]);
      put_word(page, tmp_disp+BUF_POS, 1);
      put_word(page, tmp_disp+BUF_END, 1);
     }
    else
     {
      ssetadr(tmp_page,tmp_disp);
      if ((port[C_DISP] = take_ch()) == 256)
       {
        port[C_PAGE] = EOF_PAGE*2;
        port[C_DISP] = EOF_DISP;
       }
     }
   }
  else
   {
    set_src_err("READ-CHAR", 1, port);
    retstat = -1;
   }
  return(retstat);
 } /* end of function:  read_char(port) */

/************************************************************************/
/*                           Set Port Address                           */
/************************************************************************/
setadr(pg, ds, direction)
int pg;         /* the port's page number */
int ds;         /* the port's displacement */
int direction;  /* read/write flag */
 {
  if (ptype[pg] == PORTTYPE*2)
   {
    port_reg[C_PAGE] = ADJPAGE((port_page = pg));
    port_reg[C_DISP] = port_disp = ds;
    handle = get_word(pg, ds+HANDLE);
    direction = get_word(pg, ds+P_FLAGS);
    window_p = direction & WINDOW;
    string_p = direction & STRSRC;
   }
  else
   {
    printf("[VM INTERNAL ERROR] setadr: bad port\n");
    force_debug();
    return(1);
   }
  return(0);
 } /* end of function:  setadr(pg, ds, direction) */

/************************************************************************/
/*                      Output a Single Character                       */
/************************************************************************/
givechar(ch)
int ch;         /* the character to be output */
 {
  int cur_col;  /* the current column */
  int n_cols;   /* number of columns  */
  int length;   /* character string length for output */
  int stat;     /* status returned from zwrite */               /* 2-11-86 */
  int sav_page,sav_disp;

/*if (ch == '\n') ch = '\r';           Don't change!    *** JHAO ***/

  /* If transcript file "on", check this port for transcript-enable */
  if (TRNS_pag && (get_word(port_page, port_disp+P_FLAGS) & TRANSCRIPT))
   {
    sav_page = port_page;
    sav_disp = port_disp;
    setadr(CORRPAGE(TRNS_pag), TRNS_dis, 1);
    givechar(ch);
    setadr(sav_page, sav_disp, 1);
   } /* end:  if (TRNS_pag) */

  if (window_p)
   {
    if (!string_p)
     {
      if (ch == '\r') ch = '\n';
      putc_window(port_reg, ch);
     }
   }
  else
   {
    n_cols  = get_word(port_page, port_disp+N_COLS);
    cur_col = get_word(port_page, port_disp+CUR_COL);
    length = 1;
    if (ch == '\n')
    {
      ch = '\r';
      if ((stat = zwrite(handle, &ch, &length)))                /* 2-11-86 */
        goto zerror;
      ch = '\n';
      length = 1;
      if ((stat = zwrite(handle, &ch, &length)))                /* 2-11-86 */
        goto zerror;
    }
    else
    {
      if ((stat = zwrite(handle, &ch, &length)))                /* 2-11-86 */
        goto zerror;
      if (ch == '\r')
      {
        if (handle != prn_handle)
        {                              /* write to file CR = CRLF */
          ch = '\n';
          length = 1;
          if ((stat = zwrite(handle, &ch, &length)))            /* 2-11-86 */
            goto zerror;
        }
      }
    }
    switch (ch)
    {
      case '\b':
                 cur_col--;
                 if (cur_col < 0) cur_col = 0;
                 break;
      case '\t':
                 cur_col += (8 - (cur_col % 8));
                 break;
      case '\r':
      case '\n':
                 cur_col = 0;
                 break;
      default:
                 if (cur_col >= n_cols)
                   cur_col = 0;
                 else cur_col++;
    }
    put_word(port_page, port_disp+CUR_COL, cur_col);
   }
  return(0);

 zerror:                                                        /* 2-11-86 */
   printf("[VM ERROR encountered!] Write error: DOS error code %d\n%s",
          stat,"Attempting to execute SCHEME-RESET\n[Returning to top level]");
   force_reset();
  return(0);


 } /* end of function:  givechar(ch) */

/************************************************************************/
/*                      Input a Single Character                        */
/*                                                                      */
/* Description:  Accepts the next character from the currently active   */
/*              port.                                                   */
/*                                                                      */
/* Calling Sequence:  ch = takechar();                                  */
/*              Where ch -- the character read or EOF (0x0100)          */
/*                                                                      */
/* Note:  This routine reads from the port data object previously       */
/*              defined by a call to "SETADR".                          */
/************************************************************************/
takechar()
 {
  char buffer[BUFFSIZE];/* local buffer for sequential read request */
  int ch;               /* the character to be returned */
  int cur_chr;          /* buffer position of the current character */
  int i;                /* index variable */
  int length;           /* buffer length; number of characters read */
  int sav_page,sav_disp;/* port pointer save area */
  int stat;             /* status variable for read requests */

  cur_chr = get_word(port_page, port_disp+BUF_POS);
  if (cur_chr >= get_word(port_page, port_disp+BUF_END))
   { /* buffer empty-- fill it up */
    length = BUFFSIZE;
    if (window_p)
     {
      if (string_p)
       {
        if (stringrd(port_page, port_disp, buffer, &length))
          printf("[VM INTERNAL ERROR] takechar: source not a string\n");
       }
       else
       {
        read_window(port_reg, buffer, &length);
        /* If transcript file "on", check this file for transcript-enable */
        if (TRNS_pag && (get_word(port_page, port_disp+P_FLAGS) & TRANSCRIPT))
         {
          sav_page = port_page;
          sav_disp = port_disp;
          setadr(CORRPAGE(TRNS_pag), TRNS_dis, 1);
          printstr(buffer, length - 1);
          setadr(sav_page, sav_disp, 1);
         } /* end:  if (TRNS_pag ...) */
       }
     }
    else
     {
      if ((stat = zread(handle, buffer, &length)))
       {
        printf("[VM ERROR encountered!] Read error: DOS error code %d\n%s",stat,
               "Attempting to execute SCHEME-RESET\n[Returning to top level]");
        force_reset();
       }
     }
    put_word(port_page, port_disp+BUF_END, length);
    if (length == 0)
     {
      put_word(port_page, port_disp+BUF_POS, 0);
      return(256);
     }
    toblock(port_reg, BUFR, buffer, length);
    cur_chr = 0;
   }

  /* return the next character from the input buffer */
  put_word(port_page, port_disp+BUF_POS, cur_chr+1);
  ch = get_byte(port_page, port_disp+BUFR+cur_chr);

  /* test for a control-Z-- if so, and we've got a text file, return EOF */
  if (ch == '\032' && !(get_word(port_page, port_disp+P_FLAGS) & BINARY))
    ch = 256;

  return(ch);
 } /* end of function:  takechar() */

/************************************************************************/
/*         Push a Single Character Back into the Input Buffer           */
/************************************************************************/
pushchar()
 {
  int cur_chr;          /* buffer position of the current character */

  cur_chr = get_word(port_page, port_disp+BUF_POS);
  if (cur_chr > 0)
   {
    cur_chr--;
    put_word(port_page, port_disp+BUF_POS, cur_chr);
   }
  else
   {
    printf("[VM INTERNAL ERROR] pushchar: failed\n");
    force_debug();
   }
 } /* end of function:  pushchar() */


/************************************************************************/
/*          Return Number of Spaces Remaining on Current Line           */
/*                                                                      */
/* Description:  The routine interrogates a port data object and        */
/*              returns the number of print positions remaining on the  */
/*              current line.  If no line length is associated with the */
/*              port (i.e., the line length is zero), the value 0x7fff  */
/*              is returned.  This routine works for both file and      */
/*              window port data objects.                               */
/*                                                                      */
/* Calling Sequence:  setadr(pg, ds);                                   */
/*                    space = currspc();                                */
/*                                                                      */
/*              Where pg ---- the port's page number                    */
/*                    ds ---- the port's displacement                   */
/*                    space - the number of positions remaining on the  */
/*                            current print line                        */
/************************************************************************/
currspc()
 {
  int cur_col;
  int line_length;      /* the port's line length */

  cur_col = get_word(port_page, port_disp+CUR_COL);
  line_length = get_word(port_page, port_disp+N_COLS);
  return(line_length ? line_length - cur_col : 0x7fff);
 } /* end of function:  currspc() */

/************************************************************************/
/*                      Return Current Column                           */
/************************************************************************/
curr_col()
 {
  return(get_word(port_page, port_disp+CUR_COL));
 }
 /* end of function:  curr_col() */
******************************************/

/************ The following module is not called by anyone
/************************************************************************/
/*              Return Line Length of a Port/Window                     */
/************************************************************************/
lnlen(pg,ds)
int pg,ds;
 {
  return(ptype[pg] == PORTTYPE*2 ? get_word(pg,ds+N_COLS) : -1);
 }
****************************************************/

/***** Code turned off 17 May 1985 *****
/************************************************************************/
/*              Return Line Length of a Port/Window                     */
/************************************************************************/
set_lnlen(pg,ds,len)
int pg,ds,len;
 {
  if (ptype[pg] == PORTTYPE*2)
   {
    if (len >= 0) put_word(pg,ds+N_COLS,len);
   }
  else
   {
    printf("set_lnlen:  bad port\n");
    force_debug();
   }
 } /* end of function:  set_lnlen(pg,ds,len) */
***** Code turned off 17 May 1985 *****/
/**************
/************************************************************************/
/*                   Modify Transcript File Status                      */
/************************************************************************/
trns_chg(reg)
int reg[2];
 {
  int mode;
  int page;

  if (ptype[(page = CORRPAGE(reg[C_PAGE]))] == PORTTYPE*2 &&
     (mode = get_word(page, reg[C_DISP] + P_FLAGS)) & OPEN &&
     mode & 0x03)
   {
    TRNS_pag = reg[C_PAGE];
    TRNS_dis = reg[C_DISP];
   }
  else
   {
    TRNS_pag = TRNS_dis = 0;
   }
 } /* end of function:  trns_chg(reg) */

/************************************************************************/
/*                   Write Message to the who-line                      */
/************************************************************************/
who_write(str)
char *str;
 {
  extern int port_r[2];
  int ds,pg;            /* page and displacement components of active port */
  int ppg;

  /* save the current port */
  ds = port_r[C_DISP];
  pg = CORRPAGE(port_r[C_PAGE]);
  ppg = port_r[C_PAGE];

  /* write message to the "who line" */
  ssetadr(ADJPAGE(WHO_PAGE), WHO_DISP);
  printstr(str, strlen(str));

  /* restore the port which was in effect when we started */
  if (ptype[pg] == PORTTYPE*2 && get_byte(pg,ds) == PORTTYPE)
    ssetadr(ppg, ds);
 } /* end of function:  who_write(str) */
**************************************************/

/************************************************************************/
/*                 Write "GC On"Message to the who-line                 */
/************************************************************************/
gc_on()
 {
  int lcl_reg[2];
  char *text;
  char *string_asciz();

  GC_ING = 1;

  intern(lcl_reg, "PCS-GC-MESSAGE", 14);
  if (sym_lookup (lcl_reg, GNV_reg) &&
      (text = string_asciz(lcl_reg)))
   {
    who_write("\n");
    who_write(text);
    rlsstr(text);
   }
  else
   {
    who_write("\n * Garbage Collecting *");
   }
 } /* end of function:  gc_on() */

/************************************************************************/
/*               Un-Write "GC On"Message to the who-line                */
/************************************************************************/
gc_off()
 {

  GC_ING = 0;

  who_clear();
 } /* end of function:  gc_off() */
