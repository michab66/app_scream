:						=====> DO_AUTO.BAT
:
: command dir: \TOOLS,\PCS (assumed in path)
: source dir : \NEWPCS (the current directory)
: output dir : \EXEC


CD \BUILD\NEWPCS
PATH = \TOOLS;\PCS;\

rem
rem
rem Compile the autoload files
rem
rem
PCS COMPILE.ALL /AUTO
rem
rem
rem Fasl the autoload files
rem
rem
make_fsl edit.so edit.fsl         /copyright
make_fsl padvise.so padvise.fsl   /copyright
make_fsl pdefstr.so pdefstr.fsl   /copyright
make_fsl pdos.so pdos.fsl         /copyright
make_fsl pfunarg.so pfunarg.fsl   /copyright
make_fsl pgr.so pgr.fsl           /copyright
make_fsl pinspect.so pinspect.fsl /copyright
make_fsl pmath.so pmath.fsl       /copyright
make_fsl pnum2s.so pnum2s.fsl     /copyright
make_fsl pp.so pp.fsl             /copyright
make_fsl psort.so psort.fsl       /copyright
make_fsl pwindows.so pwindows.fsl /copyright
make_fsl pboot.so pboot.fsl       /copyright
make_fsl oldpmath.so oldpmath.fsl /copyright
copy pboot.fsl \exec\misc
del pboot.fsl
copy *.fsl \exec
del *.so
del *.fsl