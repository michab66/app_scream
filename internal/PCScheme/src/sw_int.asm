
DGROUP    group   data
data      segment word public 'DATA'
          assume  DS:DGROUP
data      ends

PGROUP    group   prog
prog      segment byte public 'PROG'
          assume  CS:PGROUP
	  public  isw_int,fsw_int,tsw_int,ssw_int

SWI_ARGS  struc
OLDBP	  DW	?
RET_ADDR  DW	?
INT_NUM	  DW    ?
AX_ARG	  DW	?
BX_ARG	  DW	?
CX_ARG	  DW	?
DX_ARG	  DW	?
SWI_ARGS  ends

sw_int    proc    near
isw_int:
fsw_int:
tsw_int:
ssw_int:
	  push	  bp		; Save Base Pointer
	  mov	  bp,sp		; Update with Stack Pointer

	  mov	  ax,[bp].INT_NUM  ; Get interrupt number
	  mov	  cs:int_no,al     ; Move to location in code

	  mov	  ax,[bp].AX_ARG   ; Load ax register with 1st arg
	  mov	  bx,[bp].BX_ARG   ; Load bx register with 2nd arg	
	  mov	  cx,[bp].CX_ARG   ; Load cx register with 3rd arg
	  mov	  dx,[bp].DX_ARG   ; Load dx register with 4th arg

	  db	  0CDh		; Byte code for INT instruction
int_no	  db	  070h		; Byte code for interrupt number
	  pop	  bp
	  ret			;  and go back to the caller.

sw_int    endp
prog      ends
          end
