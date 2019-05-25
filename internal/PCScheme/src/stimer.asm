;							=====> STIMER.ASM
;***************************************
;*    TIPC Scheme '84 Engine Timer     *
;*	       Utilities	       *
;*				       *
;*  (C) Copyright 1984,1985 by Texas   *
;*     Instruments Incorporated.       *
;*	  All rights reserved.	       *
;*				       *
;* Date Written:  June 1985	       *
;* Last Modification:  30 July 1985    *
;***************************************
	  include scheme.equ

DGROUP	  group   data
data	  segment word public 'DATA'
	  assume  DS:DGROUP
timer_int db	  58h		    ;40 Hz timer interrupt number
data	  ends

PGROUP	  group   prog
prog	  segment byte public 'PROG'
	  assume  CS:PGROUP

dos_func  equ	  21h		    ;DOS function call interrupt number
get_vec   equ	  35h		    ;DOS call to retrieve interrupt vector
set_vec   equ	  25h		    ;Call to set vector

	  public  tickstat
tickstat  db	  -1		    ;0=timeout, 1=engine running,
				    ;	-1=no engine running (normal)
clk_ptr   dw	  0,0		    ;Former timer vector
lo_time   dw	  0		    ;Timer ticks
hi_time   dw	  0

;     Start timer running
;	Calling sequence: set_timer(hi,lo)
;	      Where ---- hi,lo: upper,lower words of initial timer value
;     Returns nonzero iff the set was during normal VM running mode
set_args  struc
	  dw	  ?		   ;Caller's BP
	  dw	  ?		   ;Return address
hi	  dw	  ?		   ;High word
lo	  dw	  ?		   ;Low word
set_args  ends
	  public  settimer
settimer  proc	  near
	  cmp	  PC_MAKE,252	    ;Is computer an IBM variant?
	  jb	  nochange	    ;Jump if not
	  mov	  timer_int,1ch     ;Otherwise, set to IBM's vector
nochange: xor	  AX,AX 	    ;Clear AX
	  cmp	  CS:tickstat,-1    ;Check for normal run mode
	  jne	  no_set	    ;Abort if timeout or engine running
	  push	  BP
	  mov	  BP,SP
	  push	  ES		    ;Save ES
	  mov	  AH,get_vec	    ;Put present timer interrupt vector
	  mov	  AL,timer_int	    ;  into ES:BX
	  int	  dos_func
	  mov	  CS:clk_ptr,BX     ;Save vector
	  mov	  CS:clk_ptr+2,ES
	  pop	  ES		    ;Restore ES
	  mov	  AX,[BP].hi	    ;Set timer
	  mov	  CS:hi_time,AX
	  mov	  AX,[BP].lo
	  mov	  CS:lo_time,AX
	  push	  DS		    ;Save DS
	  mov	  AH,set_vec	    ;Set new interrupt vector
	  mov	  AL,timer_int
	  push	  CS		    ;Put vector segment number in DS
	  pop	  DS
	  mov	  DX,offset tick    ;Vector offset in DX
	  int	  dos_func
	  pop	  DS		    ;Restore DS
	  mov	  AL,1		    ;Denote engine running
	  mov	  CS:tickstat,AL
	  pop	  BP		    ;Restore BP
no_set:   ret
settimer  endp

;     Stop the timer
;	Calling sequence: rst_timer();
;     Returns the number in the counter at the time of reset
	  public  rsttimer
rsttimer  proc	  near
	  cmp	  CS:tickstat,1     ;Only if timeout or engine running
	  ja	  no_reset	    ;Otherwise forget it
	  mov	  AH,set_vec	    ;Prepare to reset timer interrupt
	  mov	  AL,timer_int
	  push	  DS		    ;Save DS
	  lds	  DX,dword ptr CS:clk_ptr  ;Put original vector into DS:DX
	  int	  dos_func
	  pop	  DS		    ;Restore DS
	  mov	  CS:tickstat,-1    ;Denote normal mode
no_reset: mov	  AX,CS:hi_time     ;Return 32-bit clock value
	  mov	  BX,CS:lo_time
	  ret
rsttimer  endp

;The new timer code
tick	  proc	  near
	  sti			    ;Re-enable interrupts
	  cmp	  CS:tickstat,0     ;If timeout, do nothing special
	  je	  norm_vec
	  sub	  CS:lo_time,1	    ;Otherwise decrement counter
	  sbb	  CS:hi_time,0
	  jnz	  norm_vec	    ;If not zero, jump ahead
	  cmp	  CS:lo_time,0
	  jnz	  norm_vec
	  mov	  CS:tickstat,0     ;Otherwise, record timeout event
	  C_call  force_ti	    ;Force a timeout condition
norm_vec: jmp	  dword ptr CS:clk_ptr	  ;Jump to original timer code
tick	  endp

prog	  ends
	  end

