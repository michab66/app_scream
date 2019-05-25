/* TIPC Scheme '84 Runtime Support - Error Messages
   (C) Copyright 1984,1985 by Texas Instruments Incorporated.
   All rights reserved.

   Author:  Mark E. Meyer
   Installation:  Texas Instruments Incorporated, Dallas, Texas
   Division:  Central Research Laboratories
   Cost Center:  Computer Science Laboratory
   Project:  Computer Architecture Branch
   Date Written:   3 July 1984
   Last Modification:  12 June 1985
*/

#include "scheme.h"


/**************************************************************/
/*			ERRMSG(code)			      */
/*	This simply prints whatever error message is called   */
/* for by CODE. 					      */
/**************************************************************/
errmsg(code)
int code;
 {
  switch (code)
   {
    case QUOTERR:
      printf("Bad quote form\n");
      break;
    case DOTERR:
      printf("Bad dot form\n");
      break;
    case RPARERR:
      printf(") before (\n");
      break;
    case PORTERR:
      printf("Wrong port direction\n");
      break;
    case FULLERR:
      printf("Disk full\n");
      break;
    case HEAPERR:
      printf("Heap space exhausted\n");
      printf("Press any key to return to Scheme toplevel.\n");  /*rb*/
      getch();							/*rb*/
      rbrk();							/*rb*/
      force_re();	/* we won't return */			/*rb*/
      break;
    case OVERERR:
      printf("Flonum overflow\n");
      break;
    case DIV0ERR:
      printf("Divide by zero\n");
      break;
    case EOFERR:
      /* Don't print a message for end-of-file */
      break;
    case SHARPERR:
      printf("#-macro error\n");
      break;
   }
 }
