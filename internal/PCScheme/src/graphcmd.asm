	  name	  graphics
	  title   PC Scheme Graphics
	  page	  60,132
;-----------------------------------------------------------------------------
;
;	TITLE: PC Scheme Graphics
;      AUTHOR: Medford W. Haddock II (Rusty)
;	 DATE: October 20, 1983
;    COMPUTER: Texas Instruments Professional Computer with 3-plane graphics
;	       IBM PC with Color, Enhanced, or Professional Graphics Adapters
;    ABSTRACT: These routines are designed to interface between PC Scheme
;	       and the color graphics board for both the IBM and TI PCs.
;   REVISIONS: ds - 9/25/86 - added support for the IBM EGA modes 14 and 16
;	       rb 11/7/86 - added point, line, box clipping (both TI and IBM)
;	       rb 11/24/86 - fix line drawn from p1 to p2 not same as
;			     line drawn from p2 to p1
;	       mrm 4/15/87 - modified set-mode! to run w/o screen flicker
;			     modified set-palette! to save EGA colors
;	       rb  6/13/87 - use CR for EGA mode 16 for illegal mode values
;
;-----------------------------------------------------------------------------

	  include pcmake.equ

	  page
;-----------------------------------------------------------------------------
; The "intersect" macro.                in: none
;					out: AX=intersect value
;					destroys: AX,BX,CX,DX,SI
; usage: intersect L,y2,x2,x1,y1   (be careful of the funny ordering)
;
; Given a line that crosses a clipping edge, determine the point of
; intersection:  one of the coordinates is that of the clipping edge,
; and this macro calculates the other coordinate.
;
; The equation pattern is:  new-y = y1 + (y2 - y1) * (L - x1) / (x2 - x1).
;-----------------------------------------------------------------------------
intersect macro   L,y2,x2,x1,y1
	  mov	  AX,L
	  mov	  BX,y2
	  mov	  CX,x2
	  mov	  DX,x1
	  mov	  SI,y1
	  sub	  BX,SI 	   ;; y2 - y1
	  sub	  CX,DX 	   ;; x2 - x1
	  sub	  AX,DX 	   ;; L - x1
	  imul	  BX		   ;; (y2 - y1) * (L - x1) = q
	  idiv	  CX		   ;; q / (x2 - x1)
	  add	  AX,SI 	   ;; y1 + q / (x2 - x1)
	  endm

;-----------------------------------------------------------------------------
; The "overlap" macro.                  in: none
;					out: none (look at Z flag)
;					destroys AX,BX,CX
; usage:  overlap contained,disjoint
;
; Compares the two rectangles:
;	  (Curr_X,Curr_Y) - (Stop_X,Stop_Y)   and
;	  (Clip_left,Clip-top) - (Clip_right,Clip-bottom)
; and returns status on their intersection.
;
; If the Curr/Stop rectangle is totally contained in the clipping rectangle,
; jump to label "contained" with the Z flag on.  If they are disjoint, jump
; to label "disjoint" with the Z flag off.  Otherwise, they intersect, so
; fall through.  Both jumps are short relative jumps.
;-----------------------------------------------------------------------------
overlap   macro   contained,disjoint
	  mov	  AX,Curr_X
	  mov	  BX,Curr_Y
	  call	  Encode_XY
	  mov	  CH,CL
	  mov	  AX,Stop_X
	  mov	  BX,Stop_Y
	  call	  Encode_XY
	  cmp	  CX,0
	  jz	  contained	   ;;jump if Curr/Stop totally contained in CR
	  test	  CH,CL
	  jnz	  disjoint	   ;;jump if they're disjoint
	  endm

	  page
;-----------------------------------------------------------------------------

TI_CRT	  equ	  49h
IBM_CRT   equ	  10h
DOS_FUN   equ	  21h

	  page
XGROUP	  group   PROGX
DGROUP	  group   DATA
DATA	  segment byte public 'DATA'
	  assume  DS:DGROUP
	  public  VID_MODE
	  extrn   PC_MAKE:word
	  extrn   char_hgt:word
;------------------------------------------------------------------------------
;	Some TIPC system constants.
;------------------------------------------------------------------------------
X_MAX	  equ	  720		   ; Horizontal resolution
Y_MAX	  equ	  300		   ; Vertical resolution
Num_Colors equ	  8		   ; Number of colors displayable by TIPC
Bytes_per_Line	  equ	  92	   ; (720-displayed + extra word)/ 8-bits/byte
;-----------------------------------------------------------------------------
;	These are the default values of the palette & misc. output latches.
;-----------------------------------------------------------------------------
DEF_RED   equ	  0AAh
DEF_GRN   equ	  0CCh
DEF_BLU   equ	  0F0h
TEXT_ON   equ	  040h		   ; This value is needed for bit-twiddling
TEXT_OFF  equ	   00h
YES_GRPH  equ	  0FFh
NO_GRAPH  equ	   00h
TRUE	  equ	  0FFh
FALSE	  equ	   00h
;-----------------------------------------------------------------------------
;	Local variable storage.
;-----------------------------------------------------------------------------
Curr_X	  dw	  ?		   ; Current x-coordinate
Curr_Y	  dw	  ?		   ; Current y-coordinate
Stop_X	  dw	  ?		   ; Second endpoint x-coordinate for drawing
Stop_Y	  dw	  ?		   ; Second endpoint y-coordinate for drawing
clip_left   dw	  ?		   ; Clipping rectangle (in screen coordinates)
clip_top    dw	  ?
clip_right  dw	  ?
clip_bottom dw	  ?
px	  dw	  ?		   ; Points to the independent variable
py	  dw	  ?		   ; Points to the dependent variable
Delta_X   dw	  ?		   ; = Stop_X - Start_X
Delta_Y   dw	  ?		   ; = Stop_Y - Start_Y
X_Dir	  dw	  ?		   ; -1,0,+1 : step of independent variable
Y_Dir	  dw	  ?		   ; -1,0,+1 : step of dependent variable
Xend	  dw	  ?		   ; End value of independent variable
Incr1	  dw	  ?		   ; Step for using pnt below desired value
Incr2	  dw	  ?		   ; Step for using pnt above desired value
GRAFIX_ON dw	  YES_GRPH	   ; TI Graphics are initially enabled
VID_MODE  dw	  3		   ; Current video mode for TI (text & grafx on)
Box_Hite  dw	  ?		   ; Box is this number of pixels high
Box_Width dw	  ?		   ; Number of bytes the box's width occupies
Left_Offset dw	  ?	     ; Byte offset into graphx planes of upper left box
Left_End  dw	  ?		   ; Bit pattern of left end of solid box
Left_Side dw	  ?		   ; Bit pattern of left side of hollow box
Right_End dw	  ?		   ; Bit pattern of right end of solid box
Right_Side dw	  ?		   ; Bit pattern of right side of hollow box
Fill_Fig  db	  ?		   ; True if box is to be filled
func	 db	?		    ; EGA function 0 or 18h
f_code	 db	7		    ; and or xor function
st_word  dw	?		    ; start sceen offset
st_bit	 dw	?		    ; start bit offset
ed_word  dw	?		    ; ending word offset
ed_bit	 dw	?		    ; ending bit offset
w_p_row  dw	40		    ; # of words per row
b_p_wrds db	16		    ; 16 bits per word
two	 dw	2		    ; two
pix_c	 dw	?		    ; pixel color
gra_ram  dw	0a000h		    ; EGA graphics ram address
y_val	 dw	?

;-----------------------------------------------------------------------------
;	Local constants storage.
;-----------------------------------------------------------------------------
X_Resolution	  dw	  X_MAX
Y_Resolution	  dw	  Y_MAX
Bits_per_Byte	  dw	  8
Color_Cycle	  db	  8
;-----------------------------------------------------------------------------
;	Stored here will be the current values for the latches should
;	the (ab)user decide to change them later with (set-palette!).
;-----------------------------------------------------------------------------
RED_Latch	  db	  DEF_RED
GRN_Latch	  db	  DEF_GRN
BLU_Latch	  db	  DEF_BLU
;-----------------------------------------------------------------------------
;	A table of zeroes for clearing the palettes before a mode change on
;	the EGA.
;	A table of current values for the EGA colors.  The table will be
;	modified by each set-palette! command for the EGA.  These values
;	will be used to restore the colors after a mode change on the EGA.
;-----------------------------------------------------------------------------
clear_pal	  db	  17 dup(0)
save_pal	  db	  0,1,2,3,4,5,6,7,38h,39h,3ah,3bh,3ch,3dh,3eh,3fh,0
;-----------------------------------------------------------------------------
;	These are the segments for the three graphics bit-planes
;	in the TIPC color graphics board. The order below is
;	important - see XPCINIT
;-----------------------------------------------------------------------------
Bank_A		  dw	  0C000h
Bank_B		  dw	  0C800h
Bank_C		  dw	  0D000h
Misc_Latch	  dw	  0DF82h
;-----------------------------------------------------------------------------
;	These are the segments of the Red, Green, Blue palette latches.
;-----------------------------------------------------------------------------
RED_Palette	  dw	  0DF01h
GRN_Palette	  dw	  0DF02h
BLU_Palette	  dw	  0DF03h
;-----------------------------------------------------------------------------
;	Color to palette bits translation
;-----------------------------------------------------------------------------
Palette_Trans	  label byte
	  db	  00000001b
	  db	  00000010b
	  db	  00010000b
	  db	  00100000b
	  db	  00000100b
	  db	  00001000b
	  db	  01000000b
	  db	  10000000b
;-----------------------------------------------------------------------------
;	Single-bit-on words for setting individual pixels
;-----------------------------------------------------------------------------
Bit_Table label   byte
;		  01234567   (Pixel numbering - not bit numbering)
	  db	  10000000b
	  db	  01000000b
	  db	  00100000b
	  db	  00010000b
	  db	  00001000b
	  db	  00000100b
	  db	  00000010b
	  db	  00000001b
;-----------------------------------------------------------------------------
;	Gradual bit filled bytes for the "left-side" of horizontal lines
;-----------------------------------------------------------------------------
Start_Line label   byte
;		  01234567   (Pixel numbering - not bit numbering)
	  db	  11111111b
	  db	  01111111b
	  db	  00111111b
	  db	  00011111b
	  db	  00001111b
	  db	  00000111b
	  db	  00000011b
	  db	  00000001b
;-----------------------------------------------------------------------------
;	Gradual bit filled bytes for the "right-side" of horizontal lines
;-----------------------------------------------------------------------------
End_Line  label   byte
;		  01234567   (Pixel numbering - not bit numbering)
	  db	  10000000b
	  db	  11000000b
	  db	  11100000b
	  db	  11110000b
	  db	  11111000b
	  db	  11111100b
	  db	  11111110b
	  db	  11111111b
;-----------------------------------------------------------------------------
;	  Clipping masks
;-----------------------------------------------------------------------------
;		      LTRB   (left, top, right, bottom)
left_mask   db	  00001000b
top_mask    db	  00000100b
right_mask  db	  00000010b
bottom_mask db	  00000001b
;-----------------------------------------------------------------------------
;	  Screen resolution table (for the different IBM video modes)
;-----------------------------------------------------------------------------
; The table contains the maximum *plottable* X,Y value for the mode.
Res_Table_IBM label word
	  dw	  0,0		   ;mode  0 not a graphics mode
	  dw	  0,0		   ;mode  1 not a graphics mode
	  dw	  0,0		   ;mode  2 not a graphics mode
	  dw	  0,0		   ;mode  3 not a graphics mode
	  dw	  319,199	   ;mode  4  is a graphics mode
	  dw	  319,199	   ;mode  5  is a graphics mode
	  dw	  639,199	   ;mode  6  is a graphics mode
	  dw	  0,0		   ;mode  7 not a graphics mode
	  dw	  0,0		   ;mode  8 PCjr only
	  dw	  0,0		   ;mode  9 PCjr only
	  dw	  0,0		   ;mode 10 PCjr only
	  dw	  0,0		   ;mode 11 EGA internal mode
	  dw	  0,0		   ;mode 12 EGA internal mode
	  dw	  319,199	   ;mode 13  is a graphics mode
	  dw	  639,199	   ;mode 14  is a graphics mode
	  dw	  639,349	   ;mode 15  is a graphics mode
	  dw	  639,349	   ;mode 16  is a graphics mode
;-----------------------------------------------------------------------------
;	  Jump table for graphit() based on op_code
;-----------------------------------------------------------------------------
OP_CODE   dw	  SET_MODE
	  dw	  SETP
	  dw	  SET_PAL	   ; This used to be RESETP
	  dw	  LINE
	  dw	  GETP
	  dw	  VIDEO_MODE
	  dw	  BOX
	  dw	  FILLD_BX
	  dw	  SET_CLIP_RECT
table_len equ	  $ - OP_CODE
DATA	  ends
;-----------------------------------------------------------------------------
;-----------------------------------------------------------------------------
	  page
PROGX	  segment byte public 'PROGX'
	  assume  CS:XGROUP,DS:DGROUP
;-----------------------------------------------------------------------------
;-----------------------------------------------------------------------------
; name		GRAPHIT -- Scheme interface to Rusty's graphics routines
;
; synopsis	graphit(op, arg1, arg2, arg3, arg4, arg5, arg6);
;
; description	call the appropriate graphics routine based on the "op"
;		argument:
;		  0 - (set-video-mode! mode)
;		  1 - (setp x y color)
;		  2 - (set-palette! curr-color-id new-color-id)
;		  3 - (line x1 y1 x2 y2 color)
;		  4 - (point x y)
;		  5 - (get-video-mode)
;		  6 - (box x-ul y-ul x-len y-len color)
;		  7 - (filled_box x-ul y-ul x-len y-len color xor)
;		  8 - (set-clipping-rectangle! left top right bottom)
;
gr_args   struc
	  dw	  ?		   ; caller's BP
	  dd	  ?		   ; return address
arg6	  dw	  ?		   ; argument 6    -- dbs 10/10/86
arg5	  dw	  ?		   ; argument 5
arg4	  dw	  ?		   ; argument 4
arg3	  dw	  ?		   ; argument 3
arg2	  dw	  ?		   ; argument 2
arg1	  dw	  ?		   ; argument 1
opcode	  dw	  ?		   ; sub operation code
gr_args   ends

	  public  graphit
graphit   proc	  far
	  push	  BP		   ; save caller's BP
	  mov	  BP,SP

;     Load sub opcode
	  mov	  BX,[BP].opcode   ; load sub operation code
	  add	  BX,BX 	   ; adjust for index into jump table
	  cmp	  BX,table_len	   ; bad op_code?
	  jae	  bad_op
;	  cmp	  BX,0		   ; "jae" serves as well
;	  jl	  bad_op

;     Call desired graphics function
	  call	  OP_CODE[BX]
	  jmp	  short gr_end

bad_op:   mov	  AX,-1

;     Return to caller
gr_end:   mov	  SP,BP 	   ; dump arguments off TIPC's stack
	  pop	  BP		   ; restore caller's BP
	  ret			   ; return to caller
graphit   endp
	  page
;-----------------------------------------------------------------------------
;-----------------------------------------------------------------------------
;
; name		SET_MODE - graphics initialize
;
; synopsis	(set-video-mode! mode_number)
;
; description	TIPC			  |  IBM-PC
;		MODE  ACTION		  |  MODE  ACTION(same as AH=0,INT 10H)
;		--------------------------+---------------------------
;		  0   Clear graphics	  |   0    40x25 BW	4   320x200 Col
;		  1   Text Enable	  |   1    40x25 Color	5   320x200 BW
;		  2   Graphics Enable	  |   2    80x25 BW	6   640x200 BW
;		  3   Text & Graphics Ena |   3    80x25 Color
;					  +---------------------------
;					  | extra EGA modes:
;					  |  13  320x200 16col 40x25 8x8cbox
;					  |  14  640x200 16col 80x25 8x8cbox
;					  |  15  640x350  4col 80x25 8x14cbox
;					  |  16  640x350 16col 80x25 8x14cbox
;
; returns	nothing
;
SET_MODE  proc	  near
	  push	  BP
	  push	  ES
	  mov	  AX,[BP].arg1	   ; Get mode-number
	  push	  AX		   ; Save mode number for later
	  cmp	  PC_MAKE,TIPC	   ; set IBM mode?
	  je	  ti_mode

ibm_mode  label   near

comment %					;;; Protected Mode

  Commented out 8/3/87 by TC

	  mov	  AH,12H	   ; Test for presence of EGA
	  mov	  BX,10H
	  int	  IBM_CRT	   ; IBM's video BIOS interrupt
	  cmp	  CX,0		   ; Is there an EGA here ?
	  je	  ibm_cga	   ; Apparently not; assume CGA
	  push	  DS
	  pop	  ES
	  mov	  DX,offset clear_pal
	  mov	  AX,1002H	   ; Set EGA palettes to black for mode
	  int	  IBM_CRT	   ;   change without screen flicker
	  pop	  AX
	  push	  AX
	  xor	  AH,AH 	   ; Set video I/O mode (AH=0) (AL=MODE)
	  int	  IBM_CRT	   ; IBM's video BIOS interrupt
	  call	  Reset_CR_IBM	   ; reset clipping rectangle to full screen
				     Initialize a delay loop
	  mov	  AH,2CH	   ; Get time
	  int	  DOS_FUN	   ; DOS function request
	  inc	  DH		   ; Add 1 second delay to start time
	  mov	  BX,DX 	   ; Save the ending time
	  cmp	  BH,59 	   ; Test for 59 seconds (impossible limit)
	  jl	  tm_loop	   ; OK
	  mov	  BH,0		   ; Set it = 0 to avoid a long delay
tm_loop:  mov	  AH,2CH	   ; Get time
	  int	  DOS_FUN	   ; DOS function request
	  cmp	  DX,BX 	   ; Enough time yet ?
	  jle	  tm_loop	   ; No, loop again

	  mov	  DX,offset save_pal
	  mov	  AX,1002H	   ; Set EGA palettes to saved colors
	  int	  IBM_CRT	   ; IBM's video BIOS interrupt
	  jmp	  short mode_end
%

ibm_cga   label   near
	  pop	  AX
	  push	  AX
	  xor	  AH,AH 	   ; Set video I/O mode (AH=0) (AL=MODE)
	  int	  IBM_CRT	   ; IBM's video BIOS interrupt
	  call	  Reset_CR_IBM	   ; reset clipping rectangle to full screen
	  jmp	  short mode_end

ti_mode:  call	  Reset_CR_TI	   ; reset clipping rectangle to full screen
	  cmp	  AL,0		   ; Clear TI graphics and re-init palette
	  je	  clr_grfx
	  cmp	  AL,1		   ; Turn off Graphics and Text on
	  je	  textonly
	  cmp	  AL,2		   ; Turn on Graphics and Text off
	  je	  grfxonly
	  cmp	  AL,3		   ; Turn on both Graphics and Text
	  jne	  ti_err
	  jmp	  all_on
ti_err:
	  pop	  AX
	  xor	  AX,AX 	   ; Bad op-code
	  not	  AX		   ; AX = -1
	  jmp	  short err_ret

mode_end: pop	  AX
	  mov	  VID_MODE,AX	   ; Save VID-MODE for (get-video-mode)[TI-only]
	  xor	  AX,AX 	   ; Return something nice(?)

	  mov	  char_hgt,8
	  cmp	  vid_mode,14
	  jle	  err_ret
	  mov	  char_hgt,14

err_ret:  pop	  ES		   ; Get the heck outta here
	  pop	  BP
	  ret

clr_grfx: 

	  IFDEF	  PROMEM			;;; Protected Mode
reg_block struc			   ; register block
	  dw	  ?		   ; AX
	  dw	  ?		   ; BX
    	  dw	  ?		   ; CX
       	  dw	  ?		   ; DX
reg_block ends

	  push	  AX
	  push	  BX
	  push	  CX
	  push	  DX
	  
	  mov	  DX,SP
	  

	  mov	  AH,0C4h	   ; Issue Real Interrupt
	  mov	  AL,TI_CRT	   ; TI CRT interrupt number
	  int	  DOS_FUN	   ; (extended dos function for protected mode)  
	  ELSE
	  mov	  AH,14h	   ; Clear graphics planes
	  int	  TI_CRT	   ; Send command to CRT device driver
	  ENDIF

	  mov	  RED_Latch,DEF_RED    ; Reset palettes to default values
	  mov	  GRN_Latch,DEF_GRN
	  mov	  BLU_Latch,DEF_BLU
	  cmp	  byte ptr GRAFIX_ON,YES_GRPH
	  jne	  short mode_end
	  mov	  AL,RED_Latch	   ; if graphics are enabled reset the palettes
	  mov	  BL,GRN_Latch
	  mov	  CL,BLU_Latch
	  mov	  DL,YES_GRPH
	  call	  pal_set	   ; Set the graphics palettes on
	  jmp	  short mode_end

grfxonly  label   near
	  mov	  AL,RED_Latch
	  mov	  BL,GRN_Latch
	  mov	  CL,BLU_Latch
	  mov	  DL,YES_GRPH
	  call	  pal_set	   ; Set the graphics palettes on
	  mov	  AL,TEXT_OFF
	  call	  txt_set	   ; Turn text off
	  jmp	  short mode_end

textonly  label   near
	  xor	  AL,AL
	  mov	  BL,AL
	  mov	  CL,AL
	  mov	  DL,NO_GRAPH
	  call	  pal_set	   ; Set the graphics palettes off
	  mov	  AL,TEXT_ON
	  call	  txt_set	   ; Turn text on
	  jmp	  short mode_end

all_on	  label   near
	  mov	  AL,RED_Latch
	  mov	  BL,GRN_Latch
	  mov	  CL,BLU_Latch
	  mov	  DL,YES_GRPH
	  call	  pal_set	   ; Set the graphics palettes on
	  mov	  AL,TEXT_ON
	  call	  txt_set	   ; Turn text on
	  jmp	  mode_end

pal_set   label   near
	  xor	  BP,BP 	   ; Zero offset from palette segments
	  mov	  ES,RED_Palette
	  mov	  byte ptr ES:[BP],AL	       ; Set red palette
	  mov	  byte ptr ES:[BP]+16,BL       ; Set green palette
	  mov	  byte ptr ES:[BP]+32,CL       ; Set blue palette
	  mov	  byte ptr GRAFIX_ON,DL        ; if graphics are on or not
	  ret

txt_set   label   near
	  xor	  BP,BP
	  mov	  ES,Misc_Latch
	  mov	  byte ptr ES:[BP],AL
	  ret
SET_MODE  endp

;-----------------------------------------------------------------------------
;-----------------------------------------------------------------------------
	  page
;-----------------------------------------------------------------------------
;-----------------------------------------------------------------------------
;
; name		SETP -- turn on a pixel at the given coordinates with
;			the specified color.
;
; synopsis	(setp x y color)
;
; description	Turn on the pixel at (x,y) [origin at upper left] with
;		one of 8 colors specified by 'color'.
;		Point clipping is done; ignore the ";;" comments.
;
;;		The arguments
;;		need not be in their proper range (i.e. a MOD of x, y,
;;		and color will be done with their proper values to get
;;		them into the correct range. [0 <= x <= 719, 0 <= y <= 299,
;;		0 <= color <= 7]).  This will give a "wrap-around" effect.
;;		On the IBM-PC with graphics adapter no range checking is done
;;		on either the (x,y) coordinates or the color.
;
; returns	nothing
;
SETP	  proc	  near
	  push	  BP
	  push	  DI
	  push	  ES
;
	  mov	  AX,[BP].arg1	   ; Get `x'
	  mov	  BX,[BP].arg2	   ; Get `y'
;	  call	  Fix_XY	   ; Force x and y into their proper ranges
	  call	  Encode_XY	   ; Encode point's visibility
	  cmp	  CL,0		   ; is it visible?
	  jnz	  Set_exit	   ; no, jump
	  mov	  CX,[BP].arg6	   ; xor code
	  mov	  f_code,CL
	  mov	  CX,[BP].arg3	   ; Get `color'
	  call	  LCL_SETP	   ; Display pixel
Set_exit: xor	  AX,AX 	   ; Return code of zero
	  pop	  ES
	  pop	  DI
	  pop	  BP
	  ret
SETP	  endp			   ; End of SETP(,,)
;-----------------------------------------------------------------------------
;-----------------------------------------------------------------------------
	  page
;-----------------------------------------------------------------------------
;-----------------------------------------------------------------------------
;
; name		SET_PAL -- Modify the current palette according to PC_MAKE
;
; synopsis	(set-palette! curr-color-id new-color-id)
;
; description	If PC_MAKE == TIPC then set-palette twiddles the TIPC
;		graphics palette latches according to the colors specified.
;
;		If PC_MAKE == [PC,XT,jr,AT] then use the IBM video I/O
;		interrupt (10h), function 11, set color palette;
;		or function 16, set palette registers if EGA is present.
;
; returns	nothing
;
SET_PAL   proc	  near
	  push	  BP
	  push	  ES
	  mov	  BX,[BP].arg1	   ; Get current-color-id
	  mov	  CX,[BP].arg2	   ; Get new-color-id
; **** WARNING **** Fix the IBM side of this swapping of A,BX <=> B,CX
;
	  cmp	  PC_MAKE,TIPC
	  jne	  ibm_pal
	  and	  BX,7		   ; use only lower three bits
	  mov	  AL,Palette_Trans[BX]	  ; convert BL to 1-in-8 bits
	  mov	  AH,AL
	  not	  AH		   ; AH = 7-in-8 mask
	  mov	  BL,RED_Latch
	  call	  twiddle
	  mov	  RED_Latch,BL
	  mov	  BL,BLU_Latch
	  call	  twiddle
	  mov	  BLU_Latch,BL
	  mov	  BL,GRN_Latch
	  call	  twiddle
	  mov	  GRN_Latch,BL
	  cmp	  byte ptr GRAFIX_ON,YES_GRPH  ; are graphics enabled?
	  jne	  pal_ret
	  mov	  AL,RED_Latch	   ; if yes, then update display palettes
	  mov	  CL,BLU_Latch
	  mov	  DL,YES_GRPH
	  call	  pal_set	   ; Set the graphics palettes on
	  jmp	  short pal_ret

twiddle   label   near
	  sar	  CL,1		   ; Do we turn the bit on or off
	  jnc	  turn_off
	  or	  BL,AL 	   ; Turn it on
	  ret
turn_off: and	  BL,AH 	   ; Turn it off
	  ret

ibm_pal:  mov	  AH,15 	   ; Get current video mode
	  int	  IBM_CRT	   ; IBM video I/O interrupt
	  cmp	  AL,4		   ; Is mode = 4 ?
	  jne	  pal_ega	   ; No, jump
			    ; CGA palette
	  mov	  BH,BL 	   ; BH = palette color id being set
	  mov	  BL,CL 	   ; BL = color value
	  mov	  AH,11 	   ; Set CGA color palette
	  int	  IBM_CRT	   ; IBM video I/O interrupt
	  jmp	  short pal_ret
			    ; EGA palette
pal_ega:  mov	  BH,CL 	   ; BL = palette color id being set
				   ; BH = color value
	  cmp	  BL,16 	   ; Is color id reasonable ?
	  jge	  pal_ret	   ; No, forget it
	  mov	  AX,1000H	   ; Set EGA color palette
	  int	  IBM_CRT	   ; IBM video I/O interrupt
	  mov	  BH,0		   ; Use palette color id (BL) as index
	  mov	  DS:save_pal[BX],CL   ; Save color value in palette table

pal_ret:  xor	  AX,AX 	   ; Return code of zero
	  pop	  ES
	  pop	  BP
	  ret
SET_PAL   endp			   ; End of (set-palette!...)
;-----------------------------------------------------------------------------
;-----------------------------------------------------------------------------
	  page
;-----------------------------------------------------------------------------
;-----------------------------------------------------------------------------
;
; name		VIDEO_MODE - return the current video mode
;
; synopsis	(get-video-mode)
;
; description	Returns the video mode number for the appropriate PC.
;
; returns	video mode number
;
	  public    VIDEO_MODE
VIDEO_MODE proc    near
	  cmp	  PC_MAKE,TIPC
	  je	  get_ti_m
	  mov	  AH,15 	   ; IBM's get current video state
	  int	  IBM_CRT
	  cbw			   ; Convert to full word.
	  ret

get_ti_m: mov	  AX,VID_MODE	   ; This was squirreled away by SET_MODE (TI)
	  ret
VIDEO_MODE endp
;-----------------------------------------------------------------------------
;-----------------------------------------------------------------------------
	  page
;-----------------------------------------------------------------------------
;-----------------------------------------------------------------------------
;
; name		LINE -- draw a line between the two sets of coordinates
;			given with the specified color.
;
; synopsis	(line x1 y1 x2 y2 color)
;
; description	Draw a line between (x1,y1) and (x2,y2) with one of the 8
;		colors specified by 'color'.  The line is clipped.
;
;		This routine is based upon Bresenham's Line Algorithm
;		from page 435 in "Fundamentals of Interactive Computer
;		Graphics" by Foley and Van Dam.
;
;		The clipping algorithm is Cohen and Sutherland's.
;		See pages 65-67, "Principles of Interactive Computer Graphics"
;		(2nd edition) by Newman and Sproull.
;
; returns	nothing
;
LINE	  proc	  near
	  push	  DI
	  push	  SI
	  push	  ES

; Clip line

	  mov	  AX,[BP].arg1	   ; Get x1
	  mov	  BX,[BP].arg2	   ; Get y1
	  mov	  CX,[BP].arg3	   ; Get x2
	  mov	  DX,[BP].arg4	   ; Get y2
	  cmp	  AX,CX 	   ; is x1 <= x2?
	  jle	  x1_first	   ; yes, jump
	  ; always draw from p1 to p2; otherwise the same line drawn
	  ; in the opposite direction may not exactly overlay it
	  xchg	  AX,CX 	   ; no, interchange the two points
	  xchg	  BX,DX
x1_first: mov	  Curr_X,AX
	  mov	  Curr_Y,BX
	  mov	  Stop_X,CX
	  mov	  Stop_Y,DX
	  call	  Clip_line
	  jz	  Do_line	   ; jump if line is visible
	  jmp	  Line_exit	   ; jump if line is invisible

; Line drawing proper

Do_line:  mov	  px,offset Curr_X ; px = address of Curr_X
	  mov	  py,offset Curr_y ; py = address of Curr_Y
;
	  mov	  BX,[BP].arg6	   ; get xored or not
	  mov	  f_code,BL
;
	  mov	  AX,Stop_X
	  mov	  BX,Stop_Y
	  mov	  Xend,AX	   ; Independent var's end-value unless swapped

	  sub	  BX,Curr_Y	   ; Delta_Y = y2 - y1
	  mov	  Delta_Y,BX
	  sub	  AX,Curr_X	   ; Delta_X = x2 - x1
	  mov	  Delta_X,AX
	  xchg	  AX,BX 	   ;	   Put Delta_Y into ax; Delta_X into bx
;
	  jz	  Swap_Things	   ; Is Delta_X == 0 ?
	  cwd			   ;	   Ready dx for division
	  idiv	  BX
	  neg	  AX
	  jge	  Test_Slope
	  neg	  AX		   ; slope = ax = ABS(INT(dy/dx))
Test_Slope	  label near
	  cmp	  AX,1		   ; IF slope >= 1 THEN
	  jl	  Get_X_Increment
;
Swap_Things	  label near
	  xchg	  Delta_Y,BX
	  mov	  Delta_X,BX	   ;	   swap(dx,dy)
	  mov	  CX,px
	  xchg	  py,CX
	  mov	  px,CX 	   ;	   swap(px,py)
	  mov	  CX,Stop_Y
	  mov	  Xend,CX	   ;	   Xend = Stop_Y since variables'
				   ;		  dependence was swapped.
				   ; ENDIF
Get_X_Increment   label near
	  or	  BX,BX 	   ; X_Dir = sgn(Delta_X)
	  jz	  Save_X_Dir	   ;	   IF it's zero THEN we're done
	  mov	  BX,1		   ;	   ELSE force bx = 1
	  jg	  Save_X_Dir	   ;		IF Delta_X was < zero THEN
	  neg	  BX		   ;		     bx = -1
Save_X_Dir	  label near
	  mov	  X_Dir,BX
;
	  mov	  BX,Delta_Y
	  or	  BX,BX 	   ; Y_Dir = sgn(Delta_Y)
	  jz	  Save_Y_Dir	   ;	   IF it's zero THEN we're done
	  mov	  BX,1		   ;	   ELSE force bx = 1
	  jg	  Save_Y_Dir	   ;		IF Delta_X was < zero THEN
	  neg	  BX		   ;		     bx = -1
Save_Y_Dir	  label   near
	  mov	  Y_Dir,BX
;
	  mov	  AX,Delta_X	   ; Delta_X = ABS(Delta_X)
	  neg	  AX
	  jge	  Save_ABS_Dx
	  neg	  AX
Save_ABS_Dx	  label   near
	  mov	  Delta_X,AX
;
	  mov	  BX,Delta_Y	   ; Delta_Y = ABS(Delta_Y)
	  neg	  BX
	  jge	  Save_ABS_Dy
	  neg	  BX
Save_ABS_Dy	  label   near
	  mov	  Delta_Y,BX
;
	  shl	  BX,1
	  mov	  Incr1,BX	   ; Incr1 = Delta_Y * 2
	  sub	  BX,AX
	  push	  BX		   ; d = Delta_Y * 2 - Delta_X
	  sub	  BX,AX
	  mov	  incr2,BX	   ; Incr2 = (Delta_Y - Delta_X) * 2
;
	  mov	  CX,[BP].arg5	       ; Push `color' for call to SETP
	  mov	  BX,Curr_Y	       ; Push `y'
	  mov	  AX,Curr_X	       ; Push `x'
	  call	  LCL_SETP	   ; Plot beginning point
;
	  mov	  DI,px 	   ; Get pointer to independent variable
	  mov	  SI,py 	   ; Get pointer to dependent variable
	  mov	  AX,X_Dir
	  mov	  BX,Y_Dir
	  mov	  CX,Xend
	  pop	  DX		   ;	   get D from stack
;
While	  label   near
	  cmp	  CX,DS:[DI]	   ; While (px->start != xend) {
	  je	  While_End
	  add	  DS:[DI],AX	   ;	   Px->start += X_Dir
	  or	  DX,DX 	   ;	   IF (D < 0) THEN
	  jge	  Inc_Dependent
	  add	  DX,Incr1	   ;		   D += Incr1
	  jmp	  short End_If
Inc_Dependent	  label near	   ;	   ELSE
	  add	  [SI],BX	   ;		   Py->start += Y_Dir
	  add	  DX,Incr2	   ;		   D += Incr2
End_If	  label   near		   ;	   ENDIF
	  push	  AX		   ;		   Save X_Dir
	  push	  BX		   ;		   Save Y_Dir
	  push	  CX		   ;		   Save Xend
	  push	  DX		   ;		   Save D
	  push	  DI
;
	  mov	  CX,[BP].arg5	       ; Push `color' for call to SETP
	  mov	  BX,Curr_Y	       ; Push `y'
	  mov	  AX,Curr_X	       ; Push `x'
	  call	  LCL_SETP	   ; Plot beginning point
;
	  pop	  DI
	  pop	  DX
	  pop	  CX
	  pop	  BX
	  pop	  AX
	  jmp	  short While
;
While_End label   near
Line_exit label   near
	  xor	  AX,AX 	   ; Return code of zero
	  pop	  ES
	  pop	  SI
	  pop	  DI
	  ret
LINE	  endp			   ; End of LINE(,,,,)
;-----------------------------------------------------------------------------
;-----------------------------------------------------------------------------
	  page
;-----------------------------------------------------------------------------
;-----------------------------------------------------------------------------
;
; name		GETP -- return the attribute (color) at the specified
;			coordinates.
;
; synopsis	(getp x y)
;
; description	Return the pixel value (0 - 7) at the coordinates given
;		as arguments.  The coordinates are clipped.
;
; returns	An unsigned integer in the range 0 to 7 , inclusive,
;		if the pixel lies inside the clipping rectangle.
;		The first bit-plane starting at 0C0000h will have its
;		bit represented by the lsb of the returned word.  The
;		last bit-plane starting at 0D0000h will have its bit
;		represented by bit number 2 (lsb = bit 0) of the returned
;		word.
;
;		If the pixel lies outside the clipping rectangle, return -1.
;
GETP	  proc	  near
	  push	  BP
	  push	  DI
	  push	  ES
;
	  mov	  AX,[BP].arg1	   ; Get `x'
	  mov	  BX,[BP].arg2	   ; Get `y'
;	  call	  Fix_XY	   ; Force x and y into their proper ranges
	  call	  Encode_XY	   ; Encode point's visibility in the CR
	  cmp	  CL,0		   ; is point visible in the CR?
	  mov	  AX,-1
	  jne	  IBM_Ret_Clr	   ; no, jump (return -1 in AX)
	  mov	  AX,[BP].arg1	   ; restore AX to 'x'

	  cmp	  PC_MAKE,TIPC
	  je	  ti_getp
;
	  mov	  dx,bx 	   ; Do it the IBM way (ugh!)
	  mov	  cx,ax
	  mov	  ah,13
	  int	  IBM_CRT	   ; IBM Video BIOS
	  xor	  ah,ah 	   ; Color is in AL
	  mov	  dx,ax
	  jmp	  short IBM_Ret_Clr


ti_getp   label   near
	  call	  GM_Offset	   ; Convert (x,y) to linear offset
;
;     Read the specified bit in each of the graphics memory banks.
;
	  xor	  DX,DX 	   ; Clear value to be returned
	  mov	  ES,Bank_C	   ; Get segment of 3rd bank
	  mov	  BH,ES:[DI]	   ; Copy the selected byte in graphics memory
	  and	  BH,AH 	   ; Was the bit on ?
	  jz	  short Test_Bank_B
	  inc	  DX
;
Test_Bank_B	  label near
	  shl	  DX,1
	  mov	  ES,Bank_B	   ; Get segment of 2nd bank
	  mov	  BH,ES:[DI]	   ; Copy the selected byte in graphics memory
	  and	  BH,AH 	   ; Was the bit on ?
	  jz	  short Test_Bank_A
	  inc	  DX
;
Test_Bank_A	  label near
	  shl	  DX,1
	  mov	  ES,Bank_A	   ; Get segment of 1st bank
	  mov	  BH,ES:[DI]	   ; Copy the selected byte in graphics memory
	  and	  BH,AH 	   ; Was the bit on ?
	  jz	  short Return_Color
	  inc	  DX
;
Return_Color	  label near
	  mov	  AX,DX 	   ; Put returning value into ax

IBM_Ret_Clr	  label near
	  pop	  ES
	  pop	  DI
	  pop	  BP
	  ret
GETP	  endp			   ; End of GETP(,)
;-----------------------------------------------------------------------------
;-----------------------------------------------------------------------------
	  page
;-----------------------------------------------------------------------------
;	  Encode_XY		   in: AX=X, BX=Y
;				   out: CL=code
;				   destroyed: CL
;
; Encode X,Y into a 4-bit code indicating its visibility in the clipping rectangle.
; The code is returned in CL:  CL =0: point is visible
;			       CL<>0: point is invisible.
;-----------------------------------------------------------------------------
Encode_XY proc	  near
	  mov	  CL,0		   ; clear CL; code is constructed here
	  cmp	  AX,clip_left	   ; x >= clip_left?
	  jge	  Enc_1 	   ; yes, jump
	  or	  CL,left_mask	   ; no, set bit
Enc_1:	  cmp	  BX,clip_top	   ; y >= clip_top?
	  jge	  Enc_2 	   ; yes, jump
	  or	  CL,top_mask	   ; no, set bit
Enc_2:	  cmp	  AX,clip_right    ; x <= clip_right?
	  jle	  Enc_3 	   ; yes, jump
	  or	  CL,right_mask    ; no, set bit
Enc_3:	  cmp	  BX,clip_bottom   ; y <= clip_bottom?
	  jle	  Enc_4 	   ; yes, jump
	  or	  CL,bottom_mask   ; no, set bit
Enc_4:	  ret
Encode_XY endp

	  page
;-----------------------------------------------------------------------------
;	  Clip_line		   in: none
;				   out: none (Z flag)
;				   destroyed: AX,BX,CX,DX,SI,DI
;
; The line between (Curr_X, Curr_Y) and (Stop_X, Stop_Y) is clipped.
; The two points' coordinates are possibly modified during the process.
; On exit:   Z=0 if line is visible (onscreen); the final coordinates
;		 are in the Curr and Stop memory locations
;	     Z=1 if line is invisible (offscreen)
;-----------------------------------------------------------------------------
Clip_line proc
	  mov	  DI,offset Stop_X
	  overlap Cli_exit,Cli_exit ; if line's extents rectangle lies wholly
				    ; inside or wholly outside clipping rectangle,
				    ; exit immediately

	  jmp	  short Cli_loop    ; else start clipping

; At this point AX=new X and BX=new Y.
; (Note this is executed *after* the loop.  It's rearranged to
; get all the relative branches within range.)

Cli_join:
	  mov	  [DI],AX	   ; store X back into memory
	  mov	  [DI+2],BX	   ; ditto for Y
	  pop	  CX		   ; restore codes
	  call	  Encode_XY	   ; get code for new X and Y

	  cmp	  CX,0		   ; is combined code zero?
	  jz	  Cli_exit	   ; yes, jump; line totally visible at last
	  test	  CH,CL 	   ; do any encoded bits line up?
	  jz	  Cli_loop	   ; no, jump; some part of line is visible.
				   ; if fall thru, line was invisible after all
Cli_exit: ret

; We have to clip the line.

Cli_loop: cmp	  CL,0		   ; is this point visible?
	  jnz	  Cli_1 	   ; no, jump
	  xchg	  CH,CL 	   ; yes, go work on other point
	  sub	  DI,4		   ; set pointer to other point
Cli_1:	  push	  CX		   ; tempsave the codes
	  test	  CL,left_mask	   ; is point off left side?
	  jz	  Cli_2 	   ; no, jump
	  ; The endpoint is to the left of the clipping rectangle.
	  intersect clip_left,Stop_Y,Stop_X,Curr_X,Curr_Y
	  mov	  BX,AX 	   ; new Y
	  mov	  AX,clip_left	   ; new X
	  jmp	  Cli_join
Cli_2:	  test	  CL,top_mask	   ; is point off top side?
	  jz	  Cli_3 	   ; no, jump
	  ; The endpoint is above the top of the clipping rectangle.
	  intersect clip_top,Stop_X,Stop_Y,Curr_Y,Curr_X
				   ; AX contains new X already
	  mov	  BX,clip_top	   ; new Y
	  jmp	  Cli_join
Cli_3:	  test	  CL,right_mask    ; is point off right side?
	  jz	  Cli_4 	   ; no, jump
	  ; The endpoint is to the right of the clipping rectangle.
	  intersect clip_right,Stop_Y,Stop_X,Curr_X,Curr_Y
	  mov	  BX,AX 	   ; new Y
	  mov	  AX,clip_right    ; new X
	  jmp	  Cli_join
Cli_4:				   ; no need for more tests
	  ; The endpoint is below the bottom of the clipping rectangle.
	  intersect clip_bottom,Stop_X,Stop_Y,Curr_Y,Curr_X
				   ; AX contains new X already
	  mov	  BX,clip_bottom   ; new Y
	  jmp	  Cli_join

Clip_line endp

	  page
;-----------------------------------------------------------------------------
;	  Clip_box		   in: none
;				   out: none
;				   destroyed: AX
;
; The box with corners (Curr_X, Curr_Y) and (Stop_X, Stop_Y) is clipped.
; (The corners should be (left,top) and (right,bottom) respectively.)
; The two points' coordinates are possibly modified during the process.
;-----------------------------------------------------------------------------
Clip_box  proc
	  mov	  AX,clip_left
	  cmp	  Curr_X,AX
	  jge	  CB_1
	  mov	  Curr_X,AX
CB_1:	  mov	  AX,clip_top
	  cmp	  Curr_Y,AX
	  jge	  CB_2
	  mov	  Curr_Y,AX
CB_2:	  mov	  AX,clip_right
	  cmp	  Stop_X,AX
	  jle	  CB_3
	  mov	  Stop_X,AX
CB_3:	  mov	  AX,clip_bottom
	  cmp	  Stop_Y,AX
	  jle	  CB_4
	  mov	  Stop_Y,AX
CB_4:	  ret
Clip_box  endp

	  page
;-----------------------------------------------------------------------------

	  comment ~

; NOTE:  This routine is no longer called.  Clipping is done instead.  - rb

Fix_XY	  proc	  near		   ; Force x and y into their proper values
	  cmp	  PC_MAKE,TIPC
	  jne	  ibm_dsnt	   ; IBM doesn't do range checking, Y should I?
				   ; On IBM, the ranges will vary with the mode
				   ; On entry ax = `x', bx = `y'
				   ; On exit  ax = ax MOD 720, bx = bx MOD 300
				   ;	      cx & dx = <changed>
				   ; Get `x';fix to proper range (already in ax)
	  xor	  DX,DX 	   ;	   Clear DX - unsigned dbl-word
	  div	  X_Resolution	   ;	   ax = INT(x / 720), dx = (x MOD 720)
	  mov	  CX,DX 	   ;	   I want the MOD function....
				   ;
	  mov	  AX,BX 	   ; Get `y' and fix to proper range
	  xor	  DX,DX 	   ;	   Clear DX - unsigned dbl-word
	  div	  Y_Resolution	   ;	   ax = INT(y / 300), dx = (y MOD 300)
				   ;	   I want the MOD function....
	  mov	  BX,DX
	  mov	  AX,CX 	   ; Put `x' back
ibm_dsnt: ret
Fix_XY	  endp
;-----------------------------------------------------------------------------
;-----------------------------------------------------------------------------
	  ~	  ;end comment
	page
;-----------------------------------------------------------------------------
;-----------------------------------------------------------------------------
GM_Offset proc	  near
;
;	Determine which word needs modifying and which bit to set.
;		byte_offset = (Curr_Y * 736-bits/y_pixel * 1-byte/8-bits)
;			      + INT(Curr_X * 1-byte/8-x_pixels)
;		bit-in-byte = Curr_X MOD 8   [0-msb, 8-lsb in byte]
;
				   ; On entry ax = `x', bx = `y'
				   ; On exit
				   ;	   ah = bit-in-byte, bx = <changed>
				   ;	   cx = <unchanged>, dx = <changed>
				   ;	   di = byte-addr into graphics memory
	  xchg	  AX,BX 	   ; now ax = `y' & bx = `x'
;	  neg	  AX		   ; Translate y=0 to bottom of screen
;	  add	  AX,Y_MAX-1	   ;	   y_new = 299 - (y_old MOD 300)
;	  mul	  Bytes_per_Line   ;	   Curr_Y * 736/8-bytes/y_pixel
	  shl	  AX,1		   ;		   2-clocks
	  shl	  AX,1		   ;		   2-clocks
	  mov	  DX,AX 	   ;		   2-clocks
	  shl	  AX,1		   ;		   2-clocks
	  add	  AX,DX 	   ;		   3-clocks
	  neg	  DX		   ;		   3-clocks
	  shl	  AX,1		   ;		   2-clocks
	  shl	  AX,1		   ;		   2-clocks
	  shl	  AX,1		   ;		   2-clocks
	  add	  AX,DX 	   ;		   3-clocks
				   ; TOTAL  =	  23-clocks
				   ; MUL    =  (128-143)+EA
	  xchg	  AX,BX 	   ;	   ........save partial sum
				   ;	   and get `x' into accumulator
;	  xor	  DX,DX 	   ;	   Clear DX - unsigned dbl-word
;	  div	  Bits_per_Byte    ;   ax = word offset from beginning of line
				   ;   dx = bit-in-byte (x MOD 8)
	  mov	  DX,7		   ;   mask all bits 'cept lower 3
				   ;		   4-clocks
	  and	  DX,AX 	   ;		   3-clocks
	  shr	  AX,1		   ;		   2-clocks
	  shr	  AX,1		   ;		   2-clocks
	  shr	  AX,1		   ;		   2-clocks
				   ; TOTAL  =	  13-clocks
				   ; DIV    =  (154-172)+EA
	  add	  AX,BX 	   ; Ax = byte # offset into graphics bank
	  xor	  AL,1		   ;	  fix byte offset address to jive with
				   ;	  Intel's screwy byte ordering!!!
	  mov	  DI,AX 	   ;	  move for addressing graphics memory
	  mov	  BX,DX 	   ; Saves on number of memory accesses
	  mov	  AH,Bit_Table[bx] ;	  Ax = bit-pattern
	  mov	  AL,AH
	  not	  AL		   ; al = NOT ah - for turning bits off
	  ret
GM_Offset endp
;-----------------------------------------------------------------------------
;-----------------------------------------------------------------------------
;
	  public  LCL_SETP
LCL_SETP  proc	  near
	  cmp	  PC_MAKE,TIPC
	  je	  ti_setp
	  cmp	  VID_MODE,14
	  jl	  ibm_setp
	  cmp	  f_code,1
	  jne	  ibm_setp
	  or	  CL,080h	   ; set xor flag on

ibm_setp: mov	  DX,BX 	   ; Move arguments around for IBM
	  xchg	  CX,AX
	  xor	  BH,BH 	   ; video plane
	  mov	  AH,12 	   ; write dot
	  int	  IBM_CRT
	  ret

ti_setp   label   near
	  call	  GM_Offset	   ; Convert (x,y) to byte offset
;
;     Determine which graphics memory banks get their bits twiddled.
;

Set_Byte: mov	  ES,Bank_A	       ; Get segment of 1st bank
	  call	  set_pixel
;
	  mov	  ES,Bank_B
	  call	  set_pixel	       ;    Turn on the proper bit
;
	  mov	  ES,Bank_C
	  call	  set_pixel	       ;    Turn on the proper bit
;
Quit_n_Quit label near		       ; Save the current X & Y and return
	  ret
LCL_SETP  endp

set_pixel   proc    near
	 shr	 CL,1		      ; Do we turn on/off bit in this bank?
	 jnc	 short this_bank      ; If bit was on (one) then

	 cmp	f_code,1		; is the bit xored?
	 jne	set_01

	 mov	BL,ES:[DI]		; get current value
	 xor	BL,AH			; xor with mask
	 mov	ES:[DI],BL		; replace value
	 ret

set_01:  or	ES:[DI],AH
	 ret

this_bank  label  near
	 cmp	f_code,1		; xored?
	 je	set_02
	 and	ES:[DI],AL		; Turn off the proper bit
set_02:  ret
set_pixel   endp

;-----------------------------------------------------------------------------
;-----------------------------------------------------------------------------
	  page
;-----------------------------------------------------------------------------
;-----------------------------------------------------------------------------
;
; name		BOX -- Draw a box in the graphics plane with the
;		       specified color.
;
; synopsis	(box x-ul y-ul x-lr y-lr color)
;
; description	Draw a box with graphics (not text characters).  The
;		upper left-hand corner is specified by (x-ul,y-ul)
;		and the lower right-hand is specified by (x-lr,y-lr).
;		Color indicates the pixel values that will make up the
;		box.   The interior will not be filled nor modified
;		in any way.  The box is clipped.
;		Edges that are clipped are "shrunk inwards" to fit
;		snug against the corresponding edges of the clipping
;		rectangle.  The result is another box and not just
;		some line segments as you'd might expect.
;
; returns	nothing
;
BOX	  proc	  near
	  mov	  Fill_Fig,FALSE   ; This box ain't getting filled
BOX_2ND   label   near		   ; A secondary entry point for FILLED_BOX
	  push	  SI
	  mov	  AX,[BP].arg1	   ; Get x upper-left
	  mov	  BX,[BP].arg2	   ; Get y upper-left
;	  call	  Fix_XY	   ; Force x-ul and y-ul into correct ranges
	  mov	  Curr_X,AX
	  mov	  Curr_Y,BX
	  mov	  AX,[BP].arg3	   ; Get x lower-right
	  mov	  BX,[BP].arg4	   ; Get y lower-right
;	  call	  Fix_XY	   ; Force x-lr and y-lr into correct ranges
;
	  cmp	  AX,Curr_X
	  jg	  check_y	   ; Swap if x-lr < x-ul
	  xchg	  AX,Curr_X
check_y:  cmp	  BX,Curr_Y
	  jg	  goodargs	   ; Swap if y-lr < y-ul (origin at top-left)
	  xchg	  BX,Curr_Y
;
goodargs: mov	  Stop_X,AX	   ; (var. Stop used during clipping only)
	  mov	  Stop_Y,BX
	  overlap box_1,box_done_1 ; if box totally inside CR, no need to clip
				   ; if box totally outside, skip it
	  call	  Clip_box	   ; else clip box to the clipping rectangle
box_1:	  mov	  AX,Stop_X
	  mov	  BX,Stop_Y
;
	  sub	  BX,Curr_Y
	  inc	  BX		   ; BX = the height of the box (min=1-pixel)
	  mov	  Box_Hite,BX
	  mov	  BX,[BP].arg6	   ; get function code
	  mov	  f_code,BL
	  mov	  BX,[BP].arg5	   ; get the color
	  mov	  pix_c,BX
;
	  cmp	  PC_MAKE,TIPC	   ; All the "common" material taken care of
	  je	  BOX_TI
	  jmp	  BOX_IBM
;
box_done_1: jmp   Box_done	   ; rel. branch not long enough
;

BOX_TI:   mov	  BX,Curr_Y	   ; find upper right-hand corner address
	  mov	  y_val,BX
	  call	  GM_Offset
	  push	  DI		   ; save offset into graphics mem
	  push	  BX		   ; save bit-number
;
	  mov	  AX,Curr_X	   ; find upper left-hand corner address
	  mov	  BX,Curr_Y
	  call	  GM_Offset
	  mov	  Left_Offset,DI   ; Offset into graphics plane of upper-left
	  xor	  DI,1		   ; This craziness is due to *^&$%! Intel
;
	  mov	  AH,Start_Line[BX] ; Get left-end bit pattern
	  mov	  AL,AH
	  not	  AL		    ; Need (0ffh - AH) for Set_Byte()
	  mov	  DH,Bit_Table[BX]  ; Get left-siding bit pattern
	  mov	  DL,DH
	  not	  DL		    ; Need (0ffh - DH) for Set_Byte()
	  mov	  Left_End,AX
	  mov	  Left_Side,DX
;
	  pop	  BX
	  mov	  AH,End_Line[BX]  ; Get right-end bit pattern
	  mov	  AL,AH
	  not	  AL		   ; Need (0ffh - AH) for Set_Byte()
	  mov	  DH,Bit_Table[BX] ; Get right-siding bit pattern
	  mov	  DL,DH
	  not	  DL		    ; Need (0ffh - DH) for Set_Byte()
	  mov	  Right_End,AX
	  mov	  Right_Side,DX
;
	  pop	  CX
	  xor	  CL,1		   ; This craziness due to #$%&! Intel
	  sub	  CX,DI
	  inc	  CL
	  mov	  Box_Width,CX	   ; CX = Number of bytes in box's width
	  cmp	  CL,1		   ; are bits in the same byte? (narrow box)
	  jg	  wide_box	   ; jump if not
;
				   ; We need to combine the "ends" and "sides"
				   ; for a byte-wide box.
	  and	  AH,byte ptr Left_End+1    ; combine left-end and right-end
	  mov	  AL,AH
	  not	  AL
	  or	  DH,byte ptr Left_Side+1   ; combine left-side and right-side
	  mov	  DL,DH
	  not	  DL
	  mov	  Left_End,AX
	  mov	  Left_Side,DX
;
wide_box  label   near
	  mov	  DL,CL 	   ; CL = width in bytes
	  mov	  DH,byte ptr [BP].arg5 ; get color of box
	  xor	  DI,1		   ; This squirreliness is due to $%^&*! Intel
	  call	  Solid_Line	   ; Draw top of BOX
	  dec	  Box_Hite
Box_Loop: jz	  Box_Done
;
	  add	  Left_Offset,Bytes_per_Line ; goto next scan line
	  mov	  DI,Left_Offset
	  mov	  DL,byte ptr Box_Width    ; just get the lower byte
	  cmp	  Box_Hite,1
	  je	  Bottom_Line
;
	  cmp	  Fill_Fig,TRUE    ; Do we draw a filled or hollow box?
	  jne	  Do_Hollow
;
	  call	  Solid_Line	   ;	Draw a solid horizontal line
	  jmp	  short BLoop_End
;
Do_Hollow: call   Just_Ends	   ;	Draw just the side points
BLoop_End: dec	   Box_Hite
	  jmp	  short Box_Loop
;
;
Bottom_Line: mov    AX,stop_y
	  mov	  y_val,AX
	  call	  Solid_Line	   ; Draw the bottom of the box (if >1 high)
Box_Done: xor	  AX,AX 	   ; Return a value of zero
	  pop	  SI
	  ret
;
;
Solid_Line label  near
	  cmp	f_code,1
	  jne	S_Line_1
	  push	DX
	  call	ti_xxset
	  pop	DX
	  ret

S_Line_1: mov	  AX,Left_End	   ; Get the bit pattern for left-most byte
	  mov	  CL,DH 	   ; Get the color
	  call	  Set_Byte
	  xor	  DI,1		   ;	This craziness is due to %$&*@! INTEL!
	  dec	  DL
	  jz	  SLine_Done
S_Loop:   cmp	  DL,1
	  je	  Last_Byte	   ; Jump if we need the right side
;
	  mov	  AX,0FF00h	   ; Get the solid (FFh) pattern
	  mov	  CL,DH
	  inc	  DI		   ; Point to next byte in graphics memory
	  xor	  DI,1		   ;	This craziness is due to %$&*@! INTEL!
	  call	  Set_byte
	  xor	  DI,1		   ;	This craziness is due to %$&*@! INTEL!
	  dec	  DL
	  jmp	  S_Loop
;
Last_Byte: mov	  AX,Right_End	   ; Get the bit pattern for right-most byte
	  inc	  DI
	  mov	  CL,DH 	   ; Get color
	  xor	  DI,1		   ;	This craziness is due to %$&*@! INTEL!
	  call	  Set_Byte
SLine_Done: ret
;
Just_Ends label   near
	  mov	  AX,Left_Side
	  mov	  CL,DH
	  call	  Set_Byte
	  xor	  DI,1		   ;	This craziness is due to %$&*@! INTEL!
	  dec	  DL
	  jz	  Hollow_End
	  add	  DI,Box_Width
	  dec	  DI		   ; Went one too far with addition
	  mov	  AX,Right_Side
	  mov	  CL,DH
	  xor	  DI,1		   ;	This craziness is due to %$&*@! INTEL!
	  call	  Set_Byte
Hollow_End: ret
;
;
;  IBM (ugh!) version of draw box (sorry, but to maintain compatability
;  among all the IBM video modes I've used the write-dot function (slow).
;
;     modified - 10/10/86 for EGA
;
BOX_IBM   label   near
	  sub	  AX,Curr_X
	  inc	  AX		   ; Box_Width (number of pixels to draw line)
	  mov	  Box_Width,AX
	  call	  IBM_Solid	   ; Draw the top line of box
	  inc	  Curr_Y
	  dec	  Box_Hite
	  jz	  Box_Done
IBM_while: cmp	  Box_Hite,1
	  je	  IBM_botm	   ; Go draw bottom line
	  cmp	  Fill_Fig,TRUE    ;	Is box to be filled or not?
	  jne	  IBM_nofill
	  call	  IBM_Solid
	  jmp	  short IBM_fi
;
IBM_nofill: call  IBM_epts	   ; Draw the side points for current scan line
IBM_fi:   inc	  Curr_Y	   ; end of "if"
	  dec	  Box_Hite
	  jmp	  IBM_while

IBM_botm: call	  IBM_Solid	   ; Draw bottom line (needs to be solid)
	  jmp	  Box_Done
;
IBM_Solid label   near		   ; Draw a solid horizontal line

	  mov	  DI,Box_Width	   ; sounds more like a room freshener :-)
	  mov	  DX,Curr_Y
	  mov	  CX,Curr_X

	 cmp	vid_mode,14
	 jge	ega_box

	  mov	  BL,byte ptr [BP].arg5    ; Get the color
I_Sloop:  mov	  AH,0Ch	   ; write-dot function
	  mov	  AL,BL 	   ; copy the color
	  int	  IBM_CRT	   ; WRITE-DOT(x,y,color)
	  inc	  CX
	  dec	  DI
	  jnz	  I_Sloop
	  ret
;
IBM_epts  label   near		   ; Draw the end points of a horizontal line
	  mov	  DX,Curr_Y
	  mov	  CX,Curr_X
	  mov	  BL,byte ptr [BP].arg5    ; Get the color
	  call	  epts
	  cmp	  Box_Width,1	   ; Do we need to do the other end?
	  je	  I_eend
	  add	  CX,Box_Width
	  dec	  CX		   ; We added 1 too many
	  call	  epts
I_eend:   ret

epts	  proc	  near
	  mov	  AH,0Ch	   ; write-dot function
	  mov	  AL,BL
	  cmp	  f_code,1
	  jne	  epts_01
	  or	  AL,080h	   ; set xor bit
epts_01:  int	  IBM_CRT	   ; Write Left dot
	  ret
epts	  endp

;********************************************************************
;*								    *
;*    EGA_BOX will draw a solid line on the EGA screen. This method *
;*    is used in preference to write dot since write dot is so slow.*
;*								    *
;*    DX = start row						    *
;*    CX = start col						    *
;*    DI = length						    *
;*								    *
;********************************************************************

ega_box: mov	AX,CX		; put start col into AX
	 add	AX,DI		; AX is not the ending column
	 dec	AX		; added one too many
	 call	xxset
	 ret
BOX	 endp

	 public     xxset
XXSET	PROC	  NEAR

	PUSH	  ES
	PUSH	  DX
	PUSH	  DX
	PUSH	  AX

	MOV	  FUNC,0	    ; DEFAULT TO DATA UNMODIFIED
	CMP	  F_CODE,0	    ; IS THIS An xor'ed box?
	JE	  AND_TYPE
	MOV	  FUNC,18H	    ; SET TO XOR
AND_TYPE:
	MOV	  AX,CX 	    ; PUT THE START COLUMN IN
	MOV	  BX,DX 	    ; PUT THE ROW IN
	CALL	  GET_OFFSET	    ; CALCULATE START ADDR, OFFSET
	CMP	  BX,8		    ; ON A WORD BOUNDARY?
	JL	  BYTE_01	    ; YES, THEN CONTINBUE
	INC	  AX		    ; BUMP THE WORD OFFSET
	SUB	  BX,8		    ; ADJUST FOR NEW BYTE ADDRESS
BYTE_01:
	MOV	  ST_WORD,AX	    ; SAVE START ADDRESS AND
	MOV	  ST_BIT,BX	    ; BIT OFFSET

	POP	  AX		    ; RESET THE END COLUMN
	POP	  BX		    ; POP DX INTO BX - ROW
	CALL	  GET_OFFSET	    ; CALCULATE END ADDR, OFFSET
	CMP	  BX,8		    ; ON A WORD BOUNDARY?
	JL	  BYTE_02	    ; YES, THEN CONTINBUE
	INC	  AX		    ; BUMP THE WORD OFFSET
	SUB	  BX,8		    ; ADJUST FOR NEW BYTE ADDRESS
BYTE_02:
	MOV	  ED_WORD,AX	    ; SAVE START ADDRESS AND
	MOV	  ED_BIT,BX	    ; BIT OFFSET

; Now to set up the addresses and masks and write to the planes
	MOV	  DI,ST_WORD	    ; SET THE STARTING OFFSET

XOR_LOOP:
	MOV	  AL,-1
	CMP	  DI,ST_WORD	    ; STARTING OFFSET?
	JNE	  END_OFF	    ; IF NOT, THEN CHECK FOR ENDING OFFSET
	MOV	  CX,ST_BIT	    ; SUBTRACT THE STARTING BIT OFFSET
	SHR	  AL,CL 	    ; SET UP THE CORRECT MASK FOR START
END_OFF:    ; End of offset processing
	CMP	  DI,ED_WORD	    ; IS THIS THE LAST BYTE TO PROCESS?
	JNE	  DO_XOR	    ; NO, THEN XOR THE DATA AND UPDATE
	MOV	  AH,-1 	    ; INITIALIZE THE MASK
	MOV	  CX,7
	SUB	  CX,ED_BIT	    ; SUBTRACT THE # OF ENDING OFFSET
	SHL	  AH,CL 	    ; WANT TO SAVE ALL BUT BITS PAST END
	AND	  AL,AH 	    ; AND OFF ALL USELESS BITS
DO_XOR:

 ; Latch up the current mask
	PUSH	AX
	MOV	DX,3CEH 		; LATCH PORT
	MOV	AL,8			; BIT MASK = on
	OUT	DX,AL
	INC	DX
	POP	AX			; RESTORE THE CURRENT MASK
	OUT	DX,AL

	CMP	FUNC,18H
	JNE	WRT_ZEROS		; IF XOR, THE ONLY DO 1'S

; Set to XOR function
	DEC	DX
	MOV	AL,3			; DATA ROTATE REGISTER
	OUT	DX,AL			; WRITE IT
	MOV	AL,FUNC 		; SET THE XOR OPERATOR
	INC	DX			; to or everything on to the planes
	OUT	DX,AL
	JMP	WRT_ONES

WRT_ZEROS:
; Write the one to the planes that are set

	MOV	DX,3C4H 		; SEQUENCER ADDRESS
	MOV	AL,2			;
	OUT	DX,AL

	MOV	AX,PIX_C		; SET THE COLOR INTO THE AL
	XOR	AL,0FH			; SET THE ZERO PLANES TO ON
	INC	DX
	OUT	DX,AL			; ENABLE THIS PLANE
	MOV	ES,gra_ram		; GRAPHICS RAM ADDRESS

	MOV	AL,ES:[DI]		; LATCH UP THE EXISTING DATA
	XOR	AL,AL			; WRITE ZEROES
	MOV	ES:[DI],AL		; OR WORD IN GRAPHICS PLANE.

; Now write to the planes that are ONESes

WRT_ONES:
	MOV	DX,3C4H 		; SEQUENCER ADDRESS
	MOV	AL,2			;
	OUT	DX,AL

	MOV	AX,PIX_C		; SET THE COLOR INTO THE AL
	INC	DX
	OUT	DX,AL			; ENABLE THIS PLANE
	MOV	ES,GRA_RAM		; GRAPHICS RAM ADDRESS

	MOV	AL,ES:[DI]		; LATCH UP THE EXISTING DATA
	MOV	AL,0FFH 		; WRITE ONES
	MOV	ES:[DI],AL		; OR WORD IN GRAPHICS PLANE.

; Now ready to update the pointers and continue

NEXT_BYTE:

	CMP	DI,ED_WORD		; PROCESSED LAST ONE?
	JE	XOR_EXIT
	INC	DI			; NEXT WORD IN THE GRAPHICS PLANES
	JMP	XOR_LOOP		; DO NEXT BYTE

XOR_EXIT:

	MOV	DX,3C4H 		; SEQUENCER ADDRESS
	MOV	AL,2			;
	OUT	DX,AL

	MOV	AL,0FFH 		; ENABLE ALL BAMNK
	INC	DX
	OUT	DX,AL			; ENABLE THIS PLANE

	MOV	DX,3CEH 		; SEQUENCER ADDRESS
	MOV	AL,3			;
	OUT	DX,AL

	MOV	AL,0			; NORMAL WRITES
	INC	DX
	OUT	DX,AL			; ENABLE THIS PLANE

	DEC	DX
	MOV	AL,8			;
	OUT	DX,AL

	MOV	AL,0FFH 		; ALL BITS
	INC	DX
	OUT	DX,AL			; ENABLE THIS PLANE

	POP	DX
	POP	ES
	RET
;
XXSET	ENDP


get_offset proc  near

; AX has the pixel column number
; BX has the pixel row number

	div	b_p_wrds		; divide by bits per word
	push	AX			; save the bit offset
	mov	AX,BX			; get the pixel row
	mul	w_p_row 		; row * 46 words per row
	pop	BX			; get words and bit within row
	push	BX			; save it again
	xor	BH,BH			; get rid of bit
	add	AX,BX			; bump to absolute offset
	mul	two			; byte offset!
	pop	BX
	mov	BL,BH			; shift bit count to bl
	xor	BH,BH
	ret
 ; return - ax=word offset ; bx=bit offset

get_offset endp

;-----------------------------------------------------------------------------
;-----------------------------------------------------------------------------
	  page

;***************************************************************************
; XXSET - PUT A LINE ON THE SCREEN AT THE START, END LOC AND OF LENGTH L
;	  AX=START COL, BX=START ROW , CX=END COL
;	  COLOR = COLOR
;***************************************************************************
	 public ti_xxset
ti_xxset   proc      near
	push	  ES
;
	 mov	AX,curr_x
	 mov	BX,y_val
	 mov	CX,stop_x
;
	push	  BX			; save the start row
	call	  get_offset		; convert row/col to word/bit offset

	mov	  st_word,AX		; save the start row offset
	mov	  st_bit,BX		; save the start bit offset
	pop	  BX			; restore the start row
	mov	  AX,CX 		; get the ending col
	call	  get_offset		; convert to word/bit offset

	mov	ed_word,AX		; save the ending word offset
	mov	ed_bit,BX		; save the ending bit offset
; Determine the starting word mask
	mov	BX,st_word		; get the starting word offset
ti_xloop:
	mov	DX,-1
	cmp	BX,st_word
	jne	ti_endoff
	mov	CX,st_bit		; starting bit offset
	shr	DX,CL			; shift off one bits until mask gotten
ti_endoff:
	cmp	BX,ed_word		; last byte to process?
	jne	ti_xor			; no. then xor and update
	push	DX			; save mask
	mov	DX,-1			; initialize mask
	mov	CX,0fh
	sub	CX,ed_bit		;subtract the # of ending offset
	shl	DX,CL			; want to save allbut bits past end
	pop	AX			; and off all useless bits
	and	DX,AX

ti_xor: mov	  CX,pix_c		; get the color
	call	  ti_xor_word
	cmp	BX,ed_word
	je	ti_exit

	add	BX,2			; bump the offset to next word
	jmp	ti_xloop		; do next word
ti_exit:
	pop	  ES
	inc	  y_val
	ret
;
ti_xxset   endp

;-----------------------------------------------------------------------------
;-----------------------------------------------------------------------------
	  page

;*****************************************************************************
; XOR_WORD - XOR THE MASK IN THE DX INTO THE 3 GRAPHICS PLANES AT OFFSET
; XOR THE DATA INTO THE THREE GRAPHICS PLANES
; BX = WORD OFFSET , DX=MASK , CX=COLOR
;****************************************************************************

ti_xor_word proc     near

	test	  CX,01h		; xor this plane only if bit set
	jz	  xor_b 		; no, then go to b plane
	mov	  ES,bank_a		; get the seg addr of the a plane
	call	  doit
;
xor_b:
	test	  CX,02h		; xor this plane only if bit set
	jz	  xor_c 		; no, then go to c plane
	mov	  ES,bank_b		; get the seg addr of the b plane
	call	  doit

xor_c:
	test	  CX,04h		; xor this plane only if bit set
	jz	  xor_end		; no, then go bump the offset
	mov	  ES,bank_c		; get the seg addr of the c plane
	call	  doit

xor_end:
	ret
ti_xor_word endp

doit	 proc	near
	 mov	AX,ES:[BX]	      ; get the word from a plane
	 xor	AX,DX		      ; xor the word
	 mov	ES:[BX],AX	      ; put it back
	 ret
doit	 endp

;-----------------------------------------------------------------------------
;-----------------------------------------------------------------------------
	  page
;-----------------------------------------------------------------------------
;-----------------------------------------------------------------------------
;
; name		FILLED_BX -- Draw a solid box in the graphics plane with the
;			     specified color.
;
; synopsis	(filled_box x-ul y-ul x-lr y-lr color)
;
; description	Draw a filled box with graphics (not text characters).
;		The upper left-hand corner is specified by (x-ul,y-ul)
;		and the lower right-hand is specified by (x-lr,y-lr).
;		Color indicates the pixel values that will make up the
;		box.   The interior will be filled with the same color
;		as the box.   The box is clipped.
;
; returns	nothing
;
FILLD_BX  proc	  near
	  mov	  Fill_Fig,TRUE
	  call	  BOX_2ND	   ; Call BOX at a second entry point
	  ret
FILLD_BX  endp
;-----------------------------------------------------------------------------
;-----------------------------------------------------------------------------
	  page
;-----------------------------------------------------------------------------
; name		SET_CLIP_RECT - Set the clipping rectangle.
;
; synopsis	(set-clipping-rectangle! left top right bottom)
;
; description	This routine sets the clipping rectangle for the screen.
;		The coordinate values can be any signed integer.  The
;		intersection of the clipping rectangle and the screen is
;		used as the final clipping rectangle.  If this would be nil,
;		the clipping rectangle is set to the full screen; we never
;		let it become invisible.
;
; returns	nothing
;
;				   in: no registers
;				   out: no registers
;				   destroyed: AX,BX,CX,DX
;-----------------------------------------------------------------------------
SET_CLIP_RECT proc near
	  cmp	  PC_MAKE,TIPC
	  je	  SCR_TI
	  call	  Reset_CR_IBM	   ; set CR to screen's full size
	  jmp	  short SCR_join
SCR_TI:   call	  Reset_CR_TI	   ; set CR to screen's full size
SCR_join: mov	  AX,[BP].arg1
	  mov	  BX,[BP].arg2
	  mov	  CX,[BP].arg3
	  mov	  DX,[BP].arg4
	  ; rearrange coordinates so first point is upper left hand corner
	  cmp	  CX,AX 	   ; swap if x-lr < x-ul
	  jg	  SCR_1
	  xchg	  CX,AX
SCR_1:	  cmp	  DX,BX 	   ; swap if y-lr < y-ul (origin at top left)
	  jg	  SCR_2
	  xchg	  DX,BX
	  ; now we can continue
SCR_2:	  mov	  Curr_X,AX	   ; store for the overlap check
	  mov	  Curr_Y,BX
	  mov	  Stop_X,CX
	  mov	  Stop_Y,DX
	  overlap SCR_3,SCR_4	   ; check how screen and CR overlap
	  call	  Clip_box	   ; they overlap, clip
SCR_3:	  mov	  AX,Curr_X	   ; move new coords to be final CR
	  mov	  clip_left,AX
	  mov	  BX,Curr_Y
	  mov	  clip_top,BX
	  mov	  AX,Stop_X
	  mov	  clip_right,AX
	  mov	  BX,Stop_Y
	  mov	  clip_bottom,BX
SCR_4:	  ret
SET_CLIP_RECT endp

	  page
;-----------------------------------------------------------------------------
; Reset the clipping rectangle to the full size of the screen for IBM modes.
; Destroys AX and BX.
;-----------------------------------------------------------------------------
Reset_CR_IBM proc near
	  mov	  AH,15 	   ; get the current video mode
	  int	  IBM_CRT
	  cmp	  al,16 	   ; cmp with max video mode (EGA 16)
	  jbe	  RCI_1
	  mov	  al,16 	   ; map out-of-range values to EGA 16
RCI_1:	  cbw
	  shl	  AX,1		   ; multiply by 4
	  shl	  AX,1
	  mov	  BX,AX
	  mov	  clip_left,0	   ; set the clipping rectangle accordingly
	  mov	  clip_top,0
	  mov	  AX,Res_Table_IBM[BX]
	  mov	  clip_right,AX
	  mov	  AX,Res_Table_IBM+2[BX]
	  mov	  clip_bottom,AX
	  ret
Reset_CR_IBM endp

;-----------------------------------------------------------------------------
; Reset the clipping rectangle to the full size of the screen for TIPC.
; No registers are affected.
;-----------------------------------------------------------------------------
Reset_CR_TI proc near
	  mov	  clip_left,0
	  mov	  clip_top,0
	  mov	  clip_right,X_max-1
	  mov	  clip_bottom,Y_max-1
	  ret
Reset_CR_TI endp

	  page
;-----------------------------------------------------------------------------
;-----------------------------------------------------------------------------
;
; name		XPCINIT - Any special initialization required for a
;			  particular type PC (e.g. IBM)
;
; synopsis	call  far xpcinit   (from PGROUP)
;
; description	A C callable routine (well, almost) that should be used
;		internally to PCS for any special initialization that may
;		be needed for a particular PC.
;
; returns	nothing ('cept personal satisfaction)
;
	  public  XPCINIT
XPCINIT   proc	  far
	  cmp	  PC_MAKE,TIPC
	  jne	  not_ti

	  IFDEF   PROMEM			;;; Protected Mode
	  lea	  DI,Bank_A	; Segment Address of Bank A
	  mov	  SI,4
	  cld
INISEG:   mov	  BX,[DI]	; Get real mode segment address
	  xor	  CX,CX
	  mov	  DX,0FFFFh	; Length of segment
	  mov	  AH,0C0H	; Create Real Data Window
	  int	  DOS_FUN	; (extended Dos function for protected mode)
	  stosw	    		; Save Segment Selector	to memory address
	  dec	  SI
	  jnz	  iniseg
	  ENDIF

	  mov	  w_p_row,46
	  mov	  AX,offset XGROUP:endinit ; THIS IS REALLY UGLY!!!
	  push	  AX		   ; push return address (return from all_on)
	  push	  BP
	  push	  ES
	  push	  VID_MODE
	  jmp	  all_on	   ; Turn on TEXT, init & clear graphics
;
not_ti:
COMMENT %
	  IFDEF	  PROMEM			;;; Protected Mode
	  mov	  BX,GRA_RAM	; Segment Address of EGA GRAPHICS RAM
	  xor	  CX,CX
	  mov	  DX,0FFFFh	; Length of segment
	  mov	  AL,0C0H	; Create Real Data Window
	  int	  DOS_FUN	; Extended Dos function for protected mode
	  mov	  GRA_RAM,AX	; Save Segment Selector
	  ENDIF
%
	  cmp	  PC_MAKE,0FCh
	  jl	  not_ibm
	  mov	  AX,0500h	   ; Set active display page (for alpha modes)
	  int	  IBM_CRT	   ; should I check for graphics mode??? Nah!

	 mov	AH,15		    ; get current video mode
	 int	IBM_CRT
	 xor	AH,AH		    ; clear AH
	 mov	VID_MODE,AX	    ; save videomode

	 mov	w_p_row,40

	 cmp	AX,16
	 jne	short endinit
	 mov	char_hgt,14

	  jmp	  short endinit
;
not_ibm   label   near		   ; Could there be a Zenith Z-100 out there?
				   ; Not for now.
endinit:  ret
XPCINIT   endp

PROGX	  ends
	  end
