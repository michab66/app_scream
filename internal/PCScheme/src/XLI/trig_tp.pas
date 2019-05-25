PROGRAM math_test;

{
  (This program has been tested under Turbo Pascal, version 3.0.)

  This program is designed to show what needs to be done in order to interface
  a TURBO Pascal program with TI Scheme via Scheme's XLI interface.  This
  program simply takes two arguments and performs the requested math operation
  on the args and then returns the computed value.  The lines that may need
  to be changed by you will be marked with a @@ in the following comments.

  Note:  Real numbers, in TURBO Pascal, are stored internally in a six byte
  format that is not compatiable with Scheme's internal format.  This causes
  a problem with exchanging real numbers.  Also Pascal strings start with
  a one byte length count.  This poses a small problem.  Since you pass to
  XLI the address of where the string starts and it length you will need to
  make sure that you add one byte to the address so that XLI will not get
  length of the string incorporated as the first character of the string.

  NOTE:  When compiling any TURBO Pascal program to work with XLI you need
	 to set the "mAximum free dynamic memory" under the "compiler Option"
	 to be less than 0A000.  The smaller you make this number the more
	 room TI Scheme has for its heap space.  For example this program
	 has that number set at 0400 paragraphs which is more than enough.
}

CONST
  F_NEAR = $0001; {Near data pointers. USED}
  F_INT  = $0002; {integers fit into 16 bits. USED}
  F_REL  = $0004; {release of the env block is done by the program. NOT-USED}
  F_PAD  = $0008; {leave args unpacked in parm block.  USED}

  RT_INTEGER = 0; { Return type of integer.  USED}
  RT_BOOLEAN = 1; { Return type of boolean.  NOT-USED}
  RT_STRING  = 2; { Return type of string.   NOT-USED}
  RT_FLOAT   = 3; { Return type of real num. NOT-USED}

TYPE
  regPack = record
    case Byte of
     1 : (AX,BX,CX,DX,BP,SI,DI,DS,ES,Flags : Integer);
     2 : (AL,AH,BL,BH,CL,CH,DL,DH :Byte);
    end;

  cmdTable_ptr = ^cmdTable;
  xliFile_ptr = ^xliFileStruct;
  xliRoutine_ptr = ^xliRoutineStruct;

  cmdTable = string[50]; { @@ Allocate the needed length for the cmd table.}

  xliFileStruct = record
      id	: integer;
      flags	: integer;
      table	: cmdTable_ptr;
      parmBlock : xliRoutine_ptr;
      reserved	: array[1..8] of integer;
  end;

  xliRoutineStruct = record
	    select : integer;
    specialService : integer;
	    ssArgs : array[1..8] of integer;
	  reserved : array[1..8] of integer;
	returnType : integer;
       returnValue : integer;
	    dummy1 : array[1..3] of integer; { @@ Add args as needed. }
	      arg1 : integer;
	    dummy2 : array[1..3] of integer;
	      arg2 : integer;
  end;


VAR
  psp,progSize	: integer;
  xliWaitRtn	: integer;
  regs		: regPack;
  fileBlock	: xliFileStruct;
  parmBlock	: xliRoutineStruct;
  table 	: cmdTable;
  fileBlock_ptr : xliFile_ptr;
  xwait, xbye	: array[1..2] of integer;


FUNCTION CalcProgSize:Integer;
{ Calculate the size of this program by
  1) Obtain a pointer to the paragraph of memory following this program.
  2) Obtain a pointer to this program's PSP address.
  3) Take the difference between steps 1 and 2.
}
Var
  top : integer;
Begin
  Regs.AX := $4800; {AX is set to the Allocate Memory function request ID.}
  Regs.BX := $FFFF; {BX is set to all of memory, in paragraphs, which is
			65535 * 16 or 1,048,560 bytes.	Regardless of the
			amount of memory you have, at least for MSDos 3.12,
			this request will fail because there is not that
			much memory available.	But BX, after the DOS call
			will contain the amount of memory, in paragraphs,
			we are able to allocate.}

  MsDos(Regs); {Issue the Allocate Memory call.}

  Regs.AX := $4800; {AX gets clobbered by the previous call so reset it.}
		    {BX is set to the max block size allowed, by the
			previous function request, so let's get the pointer to
			the beginning of that block. It will be in AX.}

  MsDos(Regs); {Issue the Allocate Memory call again.}

  top := Regs.AX - 1; {AX now contains the pointer/segment address, in
			  paragraphs, of the available memory just past the
			  end of this program. We need to subtract 1 from
			  this to account for the Arena. Now we have the
			  address of the top of memory for our program.}
  Regs.ES := Regs.AX; {Set ES to be equal to the block of memory that we
			   allocated so that we can release that memory.}
  Regs.AX := $4900;   {The release memory function request ID}

  MsDos(Regs); {Issue the Release Memory call.}

  CalcProgSize := top - psp;
End;


PROCEDURE xli_wait ;
Begin
  inline
    ($FF/$36/psp/
     $FF/$36/progSize/
     $FF/$1E/xwait/
     $58/
     $58/
     $A3/xliWaitRtn);
End;


PROCEDURE xli_bye ;
Begin
  inline
    ($FF/$1E/xbye);
End;

		{ ..............  MAIN starts here  ............. }
BEGIN
  psp := Cseg; {At the start of all .COM files Cseg will point to the PSP.}
  progSize := CalcProgSize;

  {Note: Because of the above mentioned problems with Pascal strings we
	 need to, in essence, throw away the first element of the table
	 since it has the string length as its starting character. Consequently
	 XLI would never be able to find the first name in the table.  To
	 prevent this from being a problem we just start the table off with
	 a slash and then down in the CASE stmt instead of starting with the
	 number zero we start with one. }
  table := '/pplus/mminus/ttimes//';

  fileBlock.id := $4252;
  fileBlock.flags := F_NEAR + F_INT + F_PAD;  { @@ Set the flags as you need.}
  fileBlock.table := Addr(table);
  fileBlock.parmBlock := Addr(parmBlock);

  fileBlock_ptr := Addr(fileBlock);

  MemW[psp:$5c] := Ofs(fileBlock_ptr^); { Set into the PSP the offest and }
  MemW[psp:$5e] := Seg(fileBlock_ptr^); { segment address of the file block.}

  xwait[1] := MemW[psp:$a];  { Store into XWAIT the offset and }
  xwait[2] := MemW[psp:$c];  { segment address of DOS's terminate routine.}

  xbye[1] := xwait[1];	{ Copy the termination offset and }
  xbye[2] := xwait[2];	{ segment address, from above, into here.}

  xwait[1] := xwait[1] + 3; { Increment by 3 for normal call. }
  xbye[1]  := xbye[1]  + 6; { Increment by 6 for termination. }

  xli_wait;  { Make the initial call to XWAIT. }

  while xliWaitRtn <> 0 do
    begin

      case parmBlock.select of
	1 :  parmBlock.returnValue := parmBlock.arg1 + parmBlock.arg2;
	2 :  parmBlock.returnValue := parmBlock.arg1 - parmBlock.arg2;
	3 :  parmBlock.returnValue := parmBlock.arg1 * parmBlock.arg2;
    end;
    parmBlock.returnType := RT_INTEGER;
    xli_wait;
  end;

  xli_bye;

END.
