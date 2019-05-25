:						=====> DO_PCS.BAT
:
: command dir: \TOOLS,\PCS (assumed in path)
: source dir : \NEWPCS (the current directory)
: output dir : \EXEC

CD \BUILD\NEWPCS
PATH = \TOOLS;\PCS;\

rem
rem
rem  Compile the Scheme compiler
rem
rem
: make certain we don't accidentally use any COMPILER.APP in current directory
del compiler.app
PCS COMPILE.ALL /SRC
rem
rem
rem  Fasl (create the fast-load format of) the compiler
rem
rem
copy pmacros.so+pme.so+psimp.so+pca.so+pgencode.so+ppeep.so+pasm.so+pcomp.so c1.so /v
copy pstd.so+pstd2.so+pio.so+popcodes.so+pdebug.so+pchreq.so+pauto_c.so+pauto_r.so+pstl.so c2.so /v
copy c1.so+c2.so compiler.so /v
MAKE_FSL COMPILER.SO COMPILER.APP /copyright
copy compiler.app \exec
: make same precaution on COMPILER.APP as before
del compiler.app
rem
rem
rem  Compile the Scheme runtime compiler
rem
rem
PCS COMPILE.ALL /RT
rem
rem
rem  Fasl the runtime compiler
rem
rem
del runtime.app
copy pstd.rto+pstd2.rto+pio.rto+pdebug.so+pchreq.rto+primops.rto rt1.so  /v
copy rt1.so+pauto_r.so+autoprim.rto+pstl.so rt.so /v
MAKE_FSL RT.SO RUNTIME.APP /copyright
copy runtime.app \exec
rem
rem  Build the autoloadable compiler
rem
rem
copy pmacros.so+pme.so+psimp.so+pca.so+pgencode.so+ppeep.so+pasm.so+pcomp.so c3.so /v
copy c3.so+pauto_c.so+popcodes.so compiler.so
MAKE_FSL COMPILER.SO  COMPILER.FSL /copyright
MAKE_FSL PRIMOPS.RTO  PRIMOPS.FSL  /copyright
MAKE_FSL AUTOCOMP.SO  AUTOCOMP.FSL /copyright
MAKE_FSL AUTOPRIM.RTO AUTOPRIM.FSL /copyright
copy compiler.fsl \exec\misc
copy primops.fsl  \exec\misc
copy autocomp.fsl \exec\misc
copy autoprim.fsl \exec\misc
del *.so
del *.app
del *.fsl