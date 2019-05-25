;***************************************
;*          MS-DOS Utilities	       *
;*                                     *
;*    (C) Copyright 1984 by Texas      *
;*     Instruments Incorporated.       *
;*        All rights reserved.         *
;*                                     *
;* Date Written:  21 June 1984	       *
;* Last Modification:  21 June 1984    *
;***************************************

MSDOS	  equ	  021h		   ; MS-DOS interrupt number
GETDATE   equ	  02Ah		   ; "get_date" function request id
GETTIME	  equ	  02Ch		   ; "get_time" function request id

DGROUP    group   data
data      segment word public 'DATA'
          assume  DS:DGROUP
data      ends

PGROUP    group   prog
prog      segment byte public 'PROG'
          assume  CS:PGROUP

get_args  struc
	  dw	  ?		   ; caller's BP
	  dw	  ?		   ; return address
get_ary	  dw	  ?		   ; pointer to result array
get_args  ends

date_fmt  struc			   ; format of data returned by get_date()
dat_mon	  dw	  ?		   ; month
dat_day	  dw	  ?		   ; day
dat_year  dw	  ?		   ; year
date_fmt  ends

time_fmt  struc			   ; format of data returned by get_time()
tim_hour  dw	  ?		   ; hour
tim_min	  dw	  ?		   ; minute
tim_sec	  dw	  ?		   ; seconds
tim_hnds  dw	  ?		   ; hundredths
time_fmt  ends

	  public  get_date
get_date  proc    near
	  push	  BP		   ; save caller's BP
	  mov	  BP,SP		   ; establish operand addressability

	  mov	  AH,GETDATE	   ; load "get_date" service call id
	  int	  MSDOS		   ; request service from MS-DOS
	  mov	  BX,[BP].get_ary  ; load pointer to result array
	  xor	  AX,AX		   ; clear AX
	  mov	  AL,DH		   ; copy month, and
	  mov	  [BX].dat_mon,AX  ;  store into result array
	  xor	  DH,DH		   ; clear high order byte of DX
	  mov	  [BX].dat_day,DX  ; store day into result array
	  mov	  [BX].dat_year,CX ; store year into result array

	  pop	  BP		   ; restore caller's BP
	  ret
get_date  endp

	  public  get_time
get_time  proc	  near
	  push	  BP		   ; save caller's BP
	  mov	  BP,SP		   ; establish operand addressability

	  mov	  AH,GETTIME	   ; load "get_time" service call id
	  int	  MSDOS		   ; request service from MS-DOS
	  mov	  BX,[BP].get_ary  ; load pointer to result array
	  xor	  AX,AX		   ; clear AX
	  mov	  AL,CH		   ; copy hours
	  mov	  [BX].tim_hour,AX ;  and store into result array
	  mov	  AL,CL		   ; copy minutes
	  mov	  [BX].tim_min,AX  ;  and store into result array
	  mov	  AL,DH		   ; copy seconds
	  mov	  [BX].tim_sec,AX  ;  and store into result array
	  mov	  AL,DL		   ; copy hundredths
	  mov	  [BX].tim_hnds,AX ;  and store into result array

	  pop	  BP
	  ret
get_time  endp

prog      ends
          end

