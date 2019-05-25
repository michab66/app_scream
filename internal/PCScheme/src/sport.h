/*							-----> SPORT.H	    */
/* TIPC Scheme Runtime Support - I/O Control Structure Definition
   Copyright 1985 by Texas Instruments Incorporated.
   All Rights Reserved.

   Author:  John C. Jensen
   Installation:  Texas Instruments Incorporated, Dallas, Texas
   Division:  Corporate Research Laboratories
   Cost Center:  Computer Science Laboratory
   Project:  Computer Architecture Branch
   Date Written:  1 February 1985
   Last Modification:  18 July 1985  by Mark Meyer
		       22 Jan  1987  by dbs - added random i/o
*/

/*  The format of a window data object is:

	     +--------+--------+--------+
	   0 |tag=port| length in bytes |
	     +--------+-----------------+
	   3 |	       pointer		|
	     +--------+--------+--------+--------+
	   6 |	 port flags    |     handle	 |
	     +-----------------+-----------------+
	  10 |	 cursor line   |  cursor column  |
	     +-----------------+-----------------+
	  14 | upper left line |upper left column|
	     +-----------------+-----------------+
	  18 | number of lines |number of columns|
	     +-----------------+-----------------+
	  22 |border attributes| text attributes |
	     +-----------------+-----------------+
	  26 |	window flags   | buffer position |
	     +-----------------+-----------------+
	  30 |	 buffer end    |
	     +--------+--------+--------+--------+----... -----+
	  32 |		    input buffer		      |
	     +--------+--------+-----------------+-------...---+
	     |		     window label		       |
	     +--------+--------+-----------------+---------...-+

		      7 6 5 4 3 2 1 0
		     +-+-+-+-+-+-+---+
	port flags:  | |s|b|t|o|w|mod|
		     +-+-+-+-+-+-+---+

		mod - mode:  0=read
			     1=write
			     2=read and write
		w - window/file:  0=file
				  1=window
		o - open/closed:  0=closed
				  1=open
		t - transcript:   0=disabled
				  1=enabled
		b - binary flag:  0=text file/window
				  1=binary file/window
		s - string I/O:   0=file or window
				  1=I/O from/to string

			7 6 5 4 3 2 1 0
		       +-----------+-+-+
	window flags:  |	   |e|w|
		       +-----------+-+-+

		w - wrap/clip:	0=clip
				1=wrap
		e - exposed:	0=exposed
				1=(partially) covered

    The format of a file data object is:

	     +--------+--------+--------+
	   0 |tag=port| length in bytes |
	     +--------+-----------------+
	   3 |		 null		|
	     +--------+--------+--------+--------+
	   6 |	 port flags    |     handle	 |
	     +-----------------+-----------------+
	  10 | pathname offset | current column  |
	     +-----------------+-----------------+
	  14 |	  chunk #      |    (reserved)	 |
	     +-----------------+-----------------+
	  18 |file size (high) |number of columns|
	     +-----------------+-----------------+
	  22 | file size (low) |    (reserved)	 |
	     +-----------------+-----------------+
	  26 |	 (reserved)    | buffer position |
	     +-----------------+-----------------+
	  30 |	 buffer end    |
	     +--------+--------+--------+--------+----... -----+
	  32 |		  input/output buffer ...	       |
	     +--------+--------+-----------------+-------... --+
	     |		    file pathname ...		       |
	     +--------+--------+-----------------+---------... +

*/
#define READ 0x00
#define WRITE 0x01
#define APPEND 0x02
#define READ_WRITE 0x02
#define WINDOW 0x04
#define OPEN 0x08
#define TRANSCRIPT 0x10
#define BINARY 0x20
#define STRSRC 0x40

#define WRAP 0x01

#define MAX_LINES 25		/* number of lines on the VDT */
#define MAX_COLUMNS 80		/* number of columns on the VDT */
#define WINDSIZE 32-BLK_OVHD
#define BUFFSIZE 256		/* input/output buffer size (bytes) */

#define STR_PTR   3		/* pointer to source string, if any */
#define P_FLAGS   6		/* port flags */
#define HANDLE	  8		/* file/device handle */
#define CUR_LINE 10		/* current line/record number */
#define CUR_COL  12		/* current column/record number */
#define UL_LINE  14		/* window:  upper left corner's line number */
				/* file: chunk # */
#define UL_COL	 16		/* window:  upper left corner's column number */
#define N_LINES  18		/* window:  number of lines */
				/* file: high word of file size */
#define N_COLS	 20		/* line length */
#define B_ATTRIB 22		/* window:  border attributes */
				/* file: low word of file size */
#define T_ATTRIB 24		/* window:  text attributes */
#define W_FLAGS  26		/* window:  flags */
#define BUF_POS  28		/* current buffer position */
#define BUF_END  30		/* current end of buffer offset */

#define BUFR	 32		/* input/output buffer */
#define LABEL	 32+BUFFSIZE	/* window label field */
#define PATHNAME 32+BUFFSIZE	/* file pathname field */
