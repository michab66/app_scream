/************************************************************************/
/*		 Scheme Special Character Declarations			*/
/*									*/
/*  Copyright 1985 by Texas Instruments Incorporated.			*/
/*  All Rights Reserved.						*/
/************************************************************************/
#define test_num 8	/* the number of "special" characters */

/*	Text Representations for Special Characters	*/
static char *test_string[test_num] = {"NEWLINE", "SPACE", "RUBOUT",
				      "PAGE",    "TAB",   "BACKSPACE",
				      "RETURN",  "ESCAPE"};

/*	Values for Special Characters		*/
static char test_char[test_num] =    {'\n',      ' ',     '\177',
				      '\f',      '\t',    '\b',
				      '\r',      '\033'};
