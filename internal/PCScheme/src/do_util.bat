:						=====> DO_UTIL.BAT
:
: command dir: \TOOLS (assumed in path)
: source dir : \BUILD (the current directory)
: output dir : \EXEC

CD \BUILD
PATH = \TOOLS;\PCS;\

rem
rem
rem  Machine type utility
rem
rem
masm machtype;
link machtype,\exec\machtype;
del machtype.obj

if "%1" == "protected" goto end
rem
rem
rem   MAKE_FSL utility
rem
rem
masm MSDOS1;
lc1 make_fsl
lc2 make_fsl
link \TOOLS\C+MAKE_FSL+MSDOS1,\EXEC\MAKE_FSL,\EXEC\MAKE_FSL/M,\TOOLS\LCM+\TOOLS\LC
del make_fsl.obj
del msdos1.obj

rem
rem
rem   NEWTRIG - XLI interface to PCS 3.0 transcendental functions
rem
rem
masm glue;
lc1 newtrig
lc2 newtrig
link \TOOLS\C+NEWTRIG+GLUE,\EXEC\NEWTRIG,\EXEC\NEWTRIG/M,\TOOLS\LCM+\TOOLS\LC
del newtrig.obj
: don't delete glue.obj yet

rem
rem
rem  Memory utility
rem
rem
masm memtype;
link memtype,\exec\memtype;
del memtype.obj

rem
rem
rem   XLI utilities
rem
rem

CD \BUILD\XLI

: note these .EXE's stay in the XLI directory
: (XCALL "exec" ...)
copy \build\dos.h
lc1 exec
lc2 exec
link \tools\c+exec+\build\glue,exec,,\tools\lc
del exec.map
del exec.obj
del dos.h

: (XCALL "sound" ...)
masm sound;
link sound;
del sound.obj

CD \BUILD

: now you can delete glue.obj
del glue.obj

:end