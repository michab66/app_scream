:							=====> SCHBUILD.BAT

rem	SCHBUILD.BAT - Get all the PC SCHEME source files in
rem		       the proper places for the build.

cd \build
path = \tools;\pcs;\

rem
rem	Copy source code
rem
copy a:\tools          \tools
copy \tools\*.lib      \lib
copy \tools\*.obj      \lib		
if "%1" == "protected" goto getrest
copy a:\edwin          \build\edwin
copy a:\scoops         \build\scoops
copy \tools\pboot.fsl  \pcs
:getrest
copy a:*.*
rem
rem	Remove the PC Scheme source diskette #1 from drive A: and
rem	replace it with source diskette #2.
rem
pause
if "%1" == "protected" goto getrest2
copy a:\sources \build\sources
copy a:\xli     \build\xli
copy a:\newpcs  \build\newpcs
:getrest2
copy a:*.*
rem
rem	Remove the PC Scheme source diskette #2 from drive A: and
rem	replace it with source diskette #3.
rem
pause
copy a:*.*
rem
rem	Remove the PC Scheme source diskette #3 from drive A: and
rem	replace it with source diskette #4.
rem
pause
copy a:*.*
if "%1" == "skip"  goto skip
if "%1" == "skip2" goto skip2
rem
rem	Remove the PC Scheme source diskette #4 from drive A: and
rem	replace it with the Microsoft Macro Assembler, version 4.0 diskette.
rem
pause
CD \TOOLS
COPY A:MASM.EXE /V
COPY A:LINK.EXE /V
COPY A:MAKE.EXE /V
COPY A:LIB.EXE  /v
rem
rem	Remove the Macro Assembler diskette from drive A: and
rem	replace it with the Lattice C compiler, version 3.0, diskette #1.
pause
COPY A:LC.EXE  /V
COPY A:LC1.EXE /V
COPY A:LC2.EXE /V
CD \TOOLS
rem
rem	Remove the Lattice C compiler, diskette #1, from drive A: and
rem	replace it with the Lattice C compiler version 3.0, diskette #3.
rem
pause
CD \LIB
COPY A:LCS.LIB LC.LIB /V
COPY A:LCMS.LIB LCM.LIB /V
COPY A:CS.OBJ C.OBJ /V
CD \TOOLS
rem
rem	Remove the Lattice C compiler, diskette #3, from drive A: and
rem	replace it with the Dater diskette.
rem
pause
copy a:dater.com /v
:
:skip
:
rem
rem	Remove the Dater diskette from drive A: 
if "%1" == "protected" goto skip3
rem	and replace it with the PC Scheme version 3.02 Installation 
rem	diskette. Any notices about "unable to create directory" can
rem     be ignored.
rem
pause
:
command /c a:install e: \pcs W
:
:skip2
:
rem
rem	Remove any diskettes that may be in the drives.
rem
pause
:skip3
rem
rem	All files are now in their proper places.
rem
rem	Press the RETURN key to start the build proper.
pause
cd \build
schbuil2 %1
