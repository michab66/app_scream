/* TIPC Scheme '84 Runtime Support - Arithmetic Support
   (C) Copyright 1984,1985 by Texas Instruments Incorporated.
   All rights reserved.

   Author:  John C. Jensen
   Installation:  Texas Instruments Incorporated, Dallas, Texas
   Division:  Central Research Laboratories
   Cost Center:  Computer Science Laboratory
   Project:  Computer Architecture Branch
   Date Written:  9 May 1984
   Last Modification:
	  11 February 1986 - Change error message text "MOD" to
			     "REMAINDER"
*/
#include "scheme.h"

#include "slist.h"

int CXFERR_status = 0;		/* floating point error status code */

static int *resreg = NULL;	   /* Result regsiter */

/****************************************************************/
/*     Support of unary arithmetic operations on values other	*/
/*     than fixnums.						*/
/****************************************************************/
arith1(op, reg)
int op;
int reg[2];
 {
  int disp,page;
  int siz1;
  char *big1;
  double flo;

  double get_flo();
  char *getmem();

  /* fetch operand page number and displacement */
  setabort();
  resreg = reg;
  disp = reg[0];
  page = CORRPAGE(reg[1]);
  switch (ptype[page]) /* dispatch on pointer type */
   {
    case (FLOTYPE*2):  flo = get_flo(page, disp);
		       switch (op)
			{
			 case MINUS_OP:  if (flo == 0.0) goto get_out;
					 else flo = - flo;
					 break;
			 case ZERO_OP:	return(flo == 0.0);
			 case NEG_OP:  return(flo < 0.0);
			 case POS_OP:  return(flo > 0.0);
			 case ABS_OP:  if (flo >= 0.0) goto get_out;
				       else flo = -flo;
				       break;
			} /* end:  switch (op) */
		       alloc_flonum(reg, flo);
		       break;

    case (BIGTYPE*2):  siz1 = get_word(page,disp+1)+2;
		       if (!(big1 = getmem(siz1)))
			 abort(HEAPERR);
		       copybig(page,disp,big1);
		       switch(op)
			{
			 case MINUS_OP: big1[2] ^= 1; break;
			 case ZERO_OP: page = FALSE;
				       goto ret_page;
			 case POS_OP: page = !(big1[2] & 1);
				      goto ret_page;
			 case NEG_OP: page = big1[2] & 1;
ret_page:
				     rlsmem(big1,siz1);
				     return(page);
			 case ABS_OP: big1[2] &= '\376'; break;
			}
		       alloc_int(reg,big1);
		       rlsmem(big1,siz1);
		       break;

    default:	       /* Invalid operand to arithmetic function */
		       not_numb(op, reg);
		       return(-1);
   } /* end:  switch (ptype[page]) */
get_out:
  return(0);
 } /* end of function:	arith1(op, reg) */


/****************************************************************/
/*     Support of binary arithmetic operations on values other	*/
/*     than fixnums (+, -, *, /, mod)				*/
/****************************************************************/
arith2(op, reg1, reg2)
int op;
int reg1[2], reg2[2];
 {
  int disp1,disp2;		/* displacements of input operands */
  int page1,page2;		/* page numbers of input operands */
  int type1,type2;		/* input operand types */
  int fix1, fix2;		/* values of fixnum operands */
  int mag;			/* magnitude test result for bignums */
  int new_type; 		/* type used with mixed mode operands */
  char *getmem();		/* memory-grabbing function */
  char *big1,*big2,*big3;	/* bignum buffers */
  int siz1,siz2,siz3;		/* sizes of bignum buffers */
  double flo1, flo2;		/* values of flonum operands */

  double get_flo();

  setabort();			big1 = big2 = 0;
overs:	/* "re-entry" point in case bignum divide produces factional result */
  resreg = reg1;
  /* localize operand information */
  disp1 = reg1[0];		disp2 = reg2[0];
  page1 = CORRPAGE(reg1[1]);	page2 = CORRPAGE(reg2[1]);
  type1 = ptype[page1]; 	type2 = ptype[page2];

  /* validate and fetch 1st operand */
  switch (type1)
   {
    case (FIXTYPE*2):  fix1 = get_fix(page1, disp1);  break;
    case (FLOTYPE*2):  flo1 = get_flo(page1, disp1);  break;
    case (BIGTYPE*2):  siz1 = get_word(page1, disp1+1) + 4;
		       if (!(big1 = getmem(siz1)))
			 abort(HEAPERR);
		       copybig(page1,disp1,big1);
		       break;
    default:	       goto non_numeric;
   } /* end:  switch (type1) */

  /* validate and fetch 2nd operand */
  switch (type2)
   {
    case (FIXTYPE*2):  fix2 = get_fix(page2, disp2);  break;
    case (FLOTYPE*2):  flo2 = get_flo(page2, disp2);  break;
    case (BIGTYPE*2):  siz2 = get_word(page2, disp2+1) + 2;
		       if (!(big2 = getmem(siz2)))
			{
			 if (big1)
			   rlsmem(big1,siz1);
			 abort(HEAPERR);
			}
		       copybig(page2,disp2,big2);
		       break;
    default:	       goto non_numeric;
   } /* end:  switch (type1) */

  /* if types don't match, convert one of the operands */
  new_type = type1;
  if (type1 != type2)
   {
    switch (type1)
     {
      case (FIXTYPE*2):  switch (type2)
			  {
			   case (FLOTYPE*2):  new_type = FLOTYPE*2;
					      flo1 = fix1;
					      break;
			   case (BIGTYPE*2):  new_type = BIGTYPE*2;
					      siz1 = 7;
					      if (!(big1 = getmem(7)))
					       {
						rlsmem(big2,siz2);
						abort(HEAPERR);
					       }
					      fix2big(fix1,big1);
					      /* convert fixnum to bignum */
					      break;
			  } /* end:  switch (type2) */
			 break;

      case (FLOTYPE*2):  switch(type2)
			  {
			   case (FIXTYPE*2):  flo2 = fix2;
					      break;
			   case (BIGTYPE*2):  if (big2flo(big2,&flo2))
					       {
						rlsmem(big2,siz2);
				     BIGnum:    
					  /* No return from dos_err           */
				          dos_err(1,FLONUM_OVERFLOW_ERROR,reg2);
					   /*	reg1[0] = OVR_DISP;           */
					   /*	reg1[1] = ADJPAGE(OVR_PAGE);  */
					   /*	abort(OVERERR);		      */
					       }
					      rlsmem(big2,siz2);
					      break;
			  } /* end:  switch(type2) */
			 break;

      case (BIGTYPE*2):  switch(type2)
			  {
			   case (FIXTYPE*2):  siz2 = 7;
					      if (!(big2 = getmem(7)))
					       {
						rlsmem(big1,siz1);
						abort(HEAPERR);
					       }
					      fix2big(fix2,big2);
					      break;
			   case (FLOTYPE*2):  new_type = FLOTYPE*2;
					      if (big2flo(big1,&flo1))
					       {
						rlsmem(big1,siz1);
					  /* No return from dos_err           */
				          dos_err(1,FLONUM_OVERFLOW_ERROR,reg1);
						/* goto BIGnum; */
					       }
					      rlsmem(big1,siz1);
					      break;
			  } /* end:  switch(type2) */
			 break;
     } /* end:	switch (type1) */
   } /* end:  if (type1 != type2) */

  /* Preform the operation */
  switch (new_type)
   {
    case (FLOTYPE*2):  CXFERR_status = 0;
		       switch (op)
			{
			  case ADD_OP:	flo1 += flo2;  break;
			  case SUB_OP:	flo1 -= flo2;  break;
			  case MUL_OP:	flo1 *= flo2;  break;
			  case DIV_OP:	flo1 /= flo2;  break;
			  case QUOT_OP:
					set_src_err("QUOTIENT", 2, reg1, reg2);
					goto error_return;
			  case MOD_OP:
					set_src_err("REMAINDER", 2, reg1, reg2);
					goto error_return;
			  case AND_OP:
bad_and:
					set_src_err("LOGAND",2,reg1,reg2);
					goto error_return;
			  case OR_OP:
bad_or:
					set_src_err("LOGIOR",2,reg1,reg2);
					goto error_return;
			  case XOR_OP:
bad_xor:
					set_src_err("LOGXOR",2,reg1,reg2);
					goto error_return;
			  case EQ_OP:	return(flo1 == flo2);
			  case NE_OP:	return(flo1 != flo2);
			  case LT_OP:	return(flo1 < flo2);
			  case GT_OP:	return(flo1 > flo2);
			  case LE_OP:	return(flo1 <= flo2);
			  case GE_OP:	return(flo1 >= flo2);
			} /* end:  switch (op) */
		       if (CXFERR_status)
			{
			 float_overflow(op, reg1, reg2);
			 goto error_return;
			}
		       else
			{
			 if (reg1[C_PAGE] != SPECSYM*2)
			   alloc_flonum(reg1, flo1);
			}
		       break;

    case (BIGTYPE*2):  mag = magcomp(big1,big2) & 0x00ff;
		       switch (op)
			{
			 case SUB_OP: big2[2] ^= 1; /* Negate & fall thru */
				      mag ^= 16;
			 case ADD_OP: if (mag & 16) /* Same signs */
					if (mag & 2) /* |BIG1| greater? */
					 {
					  bigadd(big2,big1);
					  goto alloc1;
					 }
					 else
					 {
					  bigadd(big1,big2);
					  goto alloc2;
					 }
				       else
				       {
					if (mag & 2) /* |BIG1| greater? */
					 {
					  bigsub(big2,big1);
			      alloc1:	  alloc_int(reg1,big1);
					 }
					 else
					 {
					  bigsub(big1,big2);
			      alloc2:	  alloc_int(reg1,big2);
					 }
				       }
				      break;
			 case MUL_OP: /* if reg1 is zero, we're done */
				      if (type1 == FIXTYPE*2 && !fix1) break;
				      /* if reg2 is zero, set result to zero */
				      if (type2 == FIXTYPE*2 && !fix2)
				       {
					alloc_fixnum(reg1,0);
					break;
				       }
				      /* must perform multiply */
				      siz3 = (siz1 + siz2) - 3;
				      if (!(big3=getmem(siz3)))
				       {
				rls12:	rlsmem(big1,siz1);
					rlsmem(big2,siz2);
					abort(HEAPERR);
				       }
				      bigmul(big1,big2,big3);
			     alloc3:  alloc_int(reg1,big3);
			     rls3:    rlsmem(big3,siz3);
				      break;
			 case DIV_OP: if (mag & 1)  goto float_it;
				      siz3 = siz1 - siz2 + 5;
				      if (!(big3 = getmem(siz3)))
					goto rls12;
				      thefix(big1);
				      if (bigdiv(big1,big2,big3))
				       {
			      rls123:	rlsmem(big1,siz1);
					rlsmem(big2,siz2);
					rlsmem(big3,siz3);
					CXFERR_status = -2;
					float_over(op, reg1, reg2);
					goto error_return;
				       }
				      /* test for fractional result */
				      if (big1[3] || big1[4] ||
					  big1[1] || big1[0] != 1)
				       {
					rlsmem(big3,siz3);
float_it:
					rlsmem(big1,siz1);
					rlsmem(big2,siz2);
					sfloat(reg1); /* make reg1 flonum */
					goto overs; /* re-try as flonum */
				       }
				      goto alloc3;
			 case MOD_OP: if (!(mag & 1))
				       {
					siz3 = siz1 - siz2 + 5;
					if (!(big3 = getmem(siz3)))
					  goto rls12;
					thefix(big1);
					if (bigdiv(big1,big2,big3))
					  goto rls123;
					alloc_int(reg1,big1);
					goto rls3;
				       }
				      break;
			 case QUOT_OP: if (mag & 1)
					alloc_fixnum(reg1,0);
				       else
				       {
					siz3 = siz1 - siz2 + 5;
					if (!(big3 = getmem(siz3)))
					  goto rls12;
					thefix(big1);
					if (bigdiv(big1,big2,big3))
					  goto rls123;
					goto alloc3;
				       }
				      break;
			 case AND_OP:  goto bad_and;
			 case OR_OP:   goto bad_or;
			 case XOR_OP:  goto bad_xor;
			 case EQ_OP:
			 case NE_OP:
			 case LT_OP:
			 case GT_OP:
			 case LE_OP:
			 case GE_OP:
			   rlsmem(big1,siz1);
			   rlsmem(big2,siz2);
			   switch (op)
			    {
			     case EQ_OP: return(!(mag & 0x000f));
			     case NE_OP: return(mag & 0x000f);
			     case LT_OP: return(mag & 4);
			     case GT_OP: return(mag & 8);
			     case LE_OP: return(!(mag & 8));
			     case GE_OP: return(!(mag & 4));
			    }
			} /* end:  switch (op) */
		       rlsmem(big1,siz1);
		       rlsmem(big2,siz2);
		       break;
   } /* end:  switch (new_type) */
  goto end_arith2;
non_numeric:
  not_numb(op, reg1, reg2);
error_return:
  return(-1);
end_arith2:
  return(0);
 } /* end of function:	arith2(op, reg1, reg2) */


/****************************************************************/
/* float to integer conversion-- truncate (adjust toward zero)	*/
/****************************************************************/
truncate(reg)
int reg[2];			/* register holding value/result */
 {
  int disp,page;		/* pointer displacement, page number */
/*%%  long fix; 		    /* temp to hold result */*/

  disp = reg[C_DISP];
  page = CORRPAGE(reg[C_PAGE]);

  switch (ptype[page])
   {
    case FLOTYPE*2:  fixflo(reg,get_flo(page, disp)); /* fetch and
					    re-allocate as an integer */
					  /* falls through  to "break" */
    case BIGTYPE*2:  /* bignums and fixnums mutually exclusive */
    case FIXTYPE*2:  /* already a fixnum, so no action required */
		     break;
    default:	     not_numb(TRUNC_OP, reg); /* invalid type */
		     return(-1);
   } /* end:  switch (type) */
  return(0);
 } /* end of function:	truncate(reg) */

/****************************************************************/
/* float to integer-- floor (adjust toward -infinity)		*/
/****************************************************************/
floor(reg)
int reg[2];			/* register holding value/result */
 {
  int disp,page;		/* pointer displacement, page number */
/*%%  long fix; 		    /* temp to hold converted result */*/
  double flo;			/* temp to hold value to be converted */

  disp = reg[C_DISP];
  page = CORRPAGE(reg[C_PAGE]);

  switch (ptype[page])
   {
    case FLOTYPE*2:  flo = get_flo(page, disp);
		     if (flo < 0.0) flo -= 0.9999999999;
		     fixflo(reg, flo); /* re-allocate as an integer */
					/* falls through to "break" */
    case BIGTYPE*2:  /* bignums and fixnums mutually exclusive */
    case FIXTYPE*2:  /* already a fixnum, so no action required */
		     break;
    default:	     not_numb(FLOOR_OP, reg); /* invalid type */
		     return(-1);
   } /* end:  switch (type) */
  return(0);
 } /* end of function:	floor(reg) */

/****************************************************************/
/* float to integer-- ceiling (adjust toward +infinity) 	*/
/****************************************************************/
ceiling(reg)
int reg[2];			/* register holding value/result */
 {
  int disp,page;		/* pointer displacement, page number */
/*%%  long fix; 		    /* temp to hold converted result */*/
  double flo;			/* temp to hold value to be converted */

  disp = reg[C_DISP];
  page = CORRPAGE(reg[C_PAGE]);

  switch (ptype[page])
   {
    case FLOTYPE*2:  flo = get_flo(page, disp);
		     if (flo > 0.0) flo += 0.9999999999;
		     fixflo(reg,flo); /* re-allocate as an integer */
					/* falls through to "break" */
    case BIGTYPE*2:  /* bignums and fixnums mutually exclusive */
    case FIXTYPE*2:  /* already a fixnum, so no action required */
		     break;

    default:	     not_numb(CEIL_OP, reg); /* invalid type */
		     return(-1);
   } /* end:  switch (type) */
  return(0);
 } /* end of function:	ceiling(reg) */

/****************************************************************/
/* float to integer-- round (adjust toward nearest integer)	*/
/****************************************************************/
round(reg)
int reg[2];			/* register holding value/result */
 {
  int disp,page;		/* pointer displacement, page number */
/*%%  long fix; 		    /* temp to hold converted result */*/
  double flo;			/* floating point value */

  disp = reg[C_DISP];
  page = CORRPAGE(reg[C_PAGE]);

  switch (ptype[page])
   {
    case FLOTYPE*2:  flo = get_flo(page, disp);
		     flo += (flo<0 ? -0.5 : 0.5);
		     fixflo(reg,flo); /* re-allocate as an integer */
					/* falls through to "break" */
    case BIGTYPE*2:  /* bignums and fixnums mutually exclusive */
    case FIXTYPE*2:  /* already a fixnum, so no action required */
		     break;
    default:	     not_numb(ROUND_OP, reg); /* invalid type */
		     return(-1);
   } /* end:  switch (type) */
  return(0);
 } /* end of function:	round(reg) */

/****************************************************************/
/* Convert flonum to integer, which is stored in a register	*/
/****************************************************************/
fixflo(reg,flo)
int reg[2];
double flo;
 {
  int siz;
  char *getmem();
  char *bigbuf;
  setabort();
  if (flo==0.0) goto alloc_zero;
   else
   {
    if (siz=flosiz(flo))
     {
      if (!(bigbuf=getmem(siz)))  abort(HEAPERR);
      flotobig(flo,bigbuf);
      alloc_int(reg,bigbuf);
      rlsmem(bigbuf,siz);
     }
     else
     {
alloc_zero:
      alloc_fixnum(reg,0);
     }
   }
 } /* end of function: fixflo(reg,flo) */

/****************************************************************/
/* Convert value to floating point 				*/
/****************************************************************/
sfloat(reg)
int reg[2];			/* register containing value/result */
 {
  int disp,page,type;		/* pointer displacement, page number, type */
  int siz1;			/* size of working bignum */
  char *big1;			/* working bignum */
  double flo;			/* temp for floating point result */
  char *getmem();		/* memory-grabbing function */

  setabort();
  disp = reg[0];
  page = CORRPAGE(reg[1]);
  type = ptype[page];

  switch (type)
   {
    case FIXTYPE*2:  flo = get_fix(page, disp); /* fetch and convert value */
		     alloc_flo(reg, flo); /* re-allocate as flonum */
		     break;
    case BIGTYPE*2:  siz1 = get_word(page,disp+1)+2;
		     if (!(big1 = getmem(siz1)))
		       abort(HEAPERR);
		     copybig(page,disp,big1);
		     if (big2flo(big1, &flo))
		      {
		       rlsmem(big1,siz1);
		       /* Control does not return from dos_err */	
		       dos_err(1,FLONUM_OVERFLOW_ERROR, reg);	
		       /* reg[0] = OVR_DISP;		*/
		       /* reg[1] = ADJPAGE(OVR_PAGE);	*/
                       /* abort(OVERERR);		*/
		      }
		     rlsmem(big1, siz1);
		     alloc_flo(reg,flo);
		     break;
    case FLOTYPE*2:  /* already a flonum, so no action required */
		     break;
    default:	     not_numb(FLOAT_OP, reg);
		     return(-1);
   } /* end:  switch (type) */
  return(0);
 } /* end of function:	sfloat(reg) */


/* What to do when a fixnum result is too large to be fixnum */
enlarge(reg,i)
int reg[2];
long i;
 {
  alloc_block(reg, BIGTYPE, ((abs(i)>65535) ? 5 : 3));
  putlong(reg,i);
 }


/*     Arithmetic support error routines     */
/* Arithmetic Operations */
static char *operation[24]={"+","-",    "*",    "/",    "REMAINDER",
			"LOGAND","LOGIOR","MINUS","=?", "<>?",
			"<?",   ">?",   "<=?",  ">=?",  "ABS",
			"QUOTIENT","TRUNCATE","FLOOR","CEILING","ROUND",
			"FLOAT","ZERO?","POSITIVE?","NEGATIVE?"};
/* Note:  TRUE -> binary operation;  FALSE -> unary operation */
static char binary[24]={TRUE,	TRUE,	TRUE,	TRUE,	TRUE,
			TRUE,	TRUE,	FALSE,	TRUE,	TRUE,
			TRUE,	TRUE,	TRUE,	TRUE,	FALSE,
			TRUE,	FALSE,	FALSE,	FALSE,	FALSE,
			FALSE,	FALSE,	FALSE,	FALSE};
not_numb(op, reg1, reg2)
int op, reg1[2], reg2[2];
 {
  mov_reg(tmp_reg, nil_reg);
  if (binary[op]) cons(tmp_reg, reg2, tmp_reg);
  cons(reg1, reg1, tmp_reg);
  intern(tmp_reg, operation[op], strlen(operation[op]));
  cons(reg1, tmp_reg, reg1);
  set_numeric_error(1, NUMERIC_OPERAND_ERROR, reg1);
  reg1[C_DISP] = NTN_DISP;
  reg1[C_PAGE] = NTN_PAGE*2;
 }

float_overflow(op, reg1, reg2)
int op, reg1[2], reg2[2];
 {
  cons(tmp_reg, reg2, nil_reg);
  cons(reg1, reg1, tmp_reg);
  intern(tmp_reg, operation[op], strlen(operation[op]));
  cons(reg1, tmp_reg, reg1);
  set_numeric_error(1,
   (CXFERR_status == -1 ? FLONUM_OVERFLOW_ERROR : ZERO_DIVIDE_ERROR), reg1);
  reg1[C_DISP] = NTN_DISP;
  reg1[C_PAGE] = NTN_PAGE*2;
 }

/* What to do in the event of a floating-point exception */
CXFERR(code)
int code;
 {
  switch (code)
   {
    case 2: /* Overflow */
      set_numeric_error(1, FLONUM_OVERFLOW_ERROR, nil_reg);
      CXFERR_status = -1;
      break;
    case 3: /* Divide by zero */
      set_numeric_error(1, ZERO_DIVIDE_ERROR, nil_reg);
      CXFERR_status = -2;
      break;
   }
 }

/* Put the next number in the present pseudo-random sequence into REG */
/* For details on the generator KRANDOM, see the file STIMER.ASM      */
srandom(reg)
int reg[2];
 {
  alloc_fixnum(reg,krandom());
 }
