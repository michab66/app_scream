/************************************************************************/
/*		C Equivalents for Scheme List Operations		*/
/*									*/
/*  Copyright 1985 by Texas Instruments Incorporated.			*/
/*  All Rights Reserved.						*/
/*									*/
/*  Date Written:  29 March 1985					*/
/*  Last Modification:  1 April 1985					*/
/************************************************************************/

/* copy contents of one "register" to another */
#define mov_reg(dest,src) dest[C_PAGE]=src[C_PAGE]; dest[C_DISP]=src[C_DISP]

/* test equality (eq? -ness) of two registers */
#define eq(r1,r2) (r1[C_DISP] == r2[C_DISP] && r1[C_PAGE] == r2[C_PAGE])

/* take caar of a "register" */
#define take_caar(reg) take_car(reg); take_car(reg)

/* take cadr of a "register" */
#define take_cadr(reg) take_cdr(reg); take_car(reg)

/* take cddr of a "register" */
#define take_cddr(reg) take_cdr(reg); take_cdr(reg)
