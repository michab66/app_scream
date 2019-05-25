:
: PC Scheme installation batch stream, part 2
: call from part 1:  install2 <f|f2|w> <dir> <memtype>
:
: then-parts of next 2 lines are never executed by a:install
if "%3" == "EXP" install2 %1 %2 exp
if "%3" == "EXT" install2 %1 %2 ext
if "%1" == "f2" goto floppy
echo If installing from 5 1/4" floppy, remove the PC Scheme Installation 
echo disk from drive A and replace it with the PC Scheme Autoload disk. 
echo If installing from a 3 1/2" diskette for PS2's, just press a key to
echo proceed.
pause
if "%1" == "w" goto windisk
if "%1" == "f" a:install2 f2 %2 %3
:
:floppy
:
: we are in the midst of creating the Boot diskette
a:pkxarc -r a:pkdisk2 make_fsl.exe scoops.fsl edit.fsl dummy.fsl
rename dummy.fsl edwin0.fsl
a:pkxarc -r a:pkdisk2 p*.fsl oldpmath.fsl
echo .
echo Remove the disk from drive B.
if "%3" == ""    echo Label it "PCS Boot Diskette for Conventional Memory".
if "%3" == "exp" echo Label it "PCS Boot Diskette for Expanded Memory".
if "%3" == "ext" echo Label it "PCS Boot Diskette for Extended Memory".
echo Replace it with a blank, formatted diskette.
pause
echo --------------------  Creating Autoload diskette  -----------------------
md %2
cd %2
a:pkxarc -r a:pkdisk2
del dummy.fsl
echo .
echo Remove the disk from drive B and label it "PCS Autoload Diskette".
echo Replace it with a blank, formatted diskette.
pause
echo --------------------   Creating Sources diskette  -----------------------
md %2
cd %2
md xli
md sources
cd xli
a:pkxarc -r a:pkxli
cd ..\sources
a:pkxarc -r a:pksrc
cd ..
a:
cd \
echo .
echo Remove the disk from drive B and label it "PCS Sources Diskette".
pause
goto exit
:
:windisk
:
md xli
md sources
a:pkxarc -r a:pkdisk2
del dummy.fsl
cd xli
a:pkxarc -r a:pkxli
cd ..\sources
a:pkxarc -r a:pksrc
cd ..
:
:exit
:
echo *************** Installation of PC Scheme is complete ***************
