:
: PC Scheme installation batch stream
: For help, do A:INSTALL ?
:
echo off
echo *********************** Installing PC Scheme ************************
if "%1" == "" goto error
if "%2" == "" goto error
if "%3" == "f" goto floppy
if "%3" == "F" goto floppy
if "%3" == "w" goto windisk
if "%3" == "W" goto windisk
goto error
:
:floppy
:
echo ***********************  onto Floppy Disks   ************************
cd a:\
%1
echo To install PC Scheme onto floppies, 3 blank formatted disks are needed.
echo If you haven't prepared these yet, please do a Control-C now to exit
echo this installation procedure and format the disks.  Then rerun the
echo installation procedure.
pause
echo If you have not already done so, please insert a blank disk into drive B.
pause
md %2
cd %2
copy a:read.me
echo -------------------- Creating Boot diskette for ---------------------
if "%4" == "" goto con
goto %4
:
:con
:
echo ----------------------- Conventional Memory  ------------------------
a:pkxarc -r a:pkdisk1 pcs.exe
goto merge1
:
:ext
:
echo -----------------------   Extended Memory    ------------------------
a:pkxarc -r a:pkdisk1 pcsext.exe
goto merge1
:
:exp
:
echo -----------------------   Expanded Memory    ------------------------
a:pkxarc -r a:pkdisk1 pcsexp.exe
goto merge1
:
:merge1
:
a:pkxarc -r a:pkdisk1 compiler.app newtrig.exe
: overwrite the following
copy a:install2.bat
copy a:memory.bat
copy a:memtype.exe
install2 f %2 %4
:
:windisk
:
echo ***********************  onto a Winchester   ************************
cd a:\
%1
md %2
cd %2
copy a:read.me
: install all memory versions
a:pkxarc -r a:pkdisk1 pcs.exe pcsext.exe pcsexp.exe compiler.app newtrig.exe
: overwrite the following
copy a:install2.bat
copy a:memory.bat
copy a:memtype.exe
install2 w %2 %4
:
:error
:
echo -----------------------        Error!        ------------------------
echo You must specify a drive and directory on which PC Scheme is to be
echo installed; note a space separates the two.  The directory need not exist.
echo You also specify Winchester (W) vs. 2-floppy (F) installation.  W
echo installation copies all PCS memory versions to disk.  For F, you can
echo specify the memory version--there is room for only one on the Boot disk
echo that is created.  F requires 3 blank, formatted disks.
echo -----------------------       Examples       ------------------------
echo A:INSTALL B: \ F      installs conventional-memory PCS in root
echo                       directory of floppy in drive B:
echo A:INSTALL B: \ F EXT  as above but uses extended-memory PCS
echo A:INSTALL B: \ F EXP  as above but uses expanded-memory PCS
echo A:INSTALL E: \PCS W   installs all memory versions of PCS into
echo                       directory \PCS on a TIPC Winchester
echo A:INSTALL C: \MYDIR W installs all memory versions of PCS into
echo                       directory \MYDIR on an IBM Winchester
echo ************** Installation of PC Scheme was not done ***************
echo on
