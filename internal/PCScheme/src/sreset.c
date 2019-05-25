/*							=====> SRESET.C      */
/* TIPC Scheme Runtime Support - Reset
   (C) Copyright 1984, 1985, 1986 by Texas Instruments Incorporated.
   All rights reserved.

   Author:  John C. Jensen
   Installation:  Texas Instruments Incorporated, Dallas, Texas
   Division:  Central Research Laboratories
   Cost Center:  Computer Science Laboratory
   Project:  Computer Architecture Branch
   Date Written:  18 December 1984
   Last Modification:  25 February 1986
*/
#include "ctype.h"
#include "scheme.h"

/************************************************************************/
/*				Scheme-Reset				*/
/************************************************************************/
scheme_reset()
 {
  int car_page, car_disp;
  int i;
  int page, disp;

  ENTER(scheme_reset);

  /* create a pointer to the symbol "scheme-top-level" */
  intern(tmp_reg, "SCHEME-TOP-LEVEL", 16);

  /* If first call to Scheme-reset, initialize state parameters */
  if (!FP_save)
   {
    FP_save = FP;
    page = CORRPAGE(FNV_save[C_PAGE] = FNV_pag);
    disp = FNV_save[C_DISP] = FNV_dis;

    /* find the binding for "scheme-top-level" */
    while (page)
     {
      car_page = CORRPAGE(get_byte(page, disp));
      car_disp = get_word(page, disp+1);
      if (tmp_disp == get_word(car_page, car_disp+1) &&
	  tmp_page == get_byte(car_page, car_disp))
       {
	STL_save[C_PAGE] = get_byte(car_page, car_disp+3);
	STL_save[C_DISP] = get_word(car_page, car_disp+4);
	break;
       }
      i = CORRPAGE(get_byte(page, disp+3));
      disp = get_word(page, disp+4);
      page = i;
     } /* end:	while (page) */

    if (!page) /* if "scheme-top-level" not in fluids, error */
     {
      print_and_exit(
"[VM FATAL ERROR] No fluid binding for SCHEME-TOP-LEVEL\n");
     }
   } /* end:  if (!FP_save) */

  else
   {
    /* Reset fluid environment */
    page = CORRPAGE(FNV_pag = FNV_save[C_PAGE]);
    disp = FNV_dis = FNV_save[C_DISP];

    /* find the binding for "scheme-top-level" */
    while (page)
     {
      car_page = CORRPAGE(get_byte(page, disp));
      car_disp = get_word(page, disp+1);
      if (tmp_disp == get_word(car_page, car_disp+1) &&
	  tmp_page == get_byte(car_page, car_disp))
       {
	put_ptr(car_page, car_disp+3, STL_save[C_PAGE], STL_save[C_DISP]);
	break;
       }
      i = CORRPAGE(get_byte(page, disp+3));
      disp = get_word(page, disp+4);
      page = i;
     } /* end:	while (page) */

    ASSERT (page /* make sure scheme-top-level updated */ );
   } /* end:  else */

 } /* end of function: scheme_reset() */
