cls
rem
rem     This batch copies the modified source files to the PC Scheme
rem     source diskettes #1 through #4. This batch requires that this
rem     batch be run from drive B: and that the source diskettes are
rem     loaded into drive A: (which must be a High Density Floppy 
rem     Drive). If this is not the case, please halt the batch via
rem     typing CONTROL C, otherwise hit any key to continue
pause
cls
rem
rem	Place the PC Scheme source diskette #1 into drive A:
rem
pause
copy b:readme.* a: /v
copy b:*.bat a: /v
rem
rem     Remove PC Scheme source diskette #1 from drive A: and
rem     replace it with PC Scheme source diskette # 2
pause
copy b:pro2real.asm a: /v
rem
rem     Remove PC Scheme source diskette #2 from drive A: and
rem     replace it with PC Scheme source diskette # 3
pause
copy b:version.h a: /v
rem
rem     Remove PC Scheme source diskette #3 from drive A: and
rem     replace it with PC Scheme source diskette # 4
pause
copy b:smain.c a: /v
rem
rem     Remove PC Scheme source diskette #4 from drive A:
pause
rem
rem     The source diskettes have now been modified. You can now
rem     begin the Build Procedure as specified in the README.PRO
rem	file.
