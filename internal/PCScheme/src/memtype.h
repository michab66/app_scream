#ifdef REGMEM
  #define MIN_PAGESIZE 0x0C00
#endif

#ifdef EXPMEM
  #define MIN_PAGESIZE 0x4000
#endif

#ifdef EXTMEM
  #define MIN_PAGESIZE 0x4000
#endif

#ifdef PROMEM
  #define MIN_PAGESIZE 0x0C00
  #define MAX_PAGESIZE 0x7FF0
#endif
