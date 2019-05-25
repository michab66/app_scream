:						=====> SCHBUIL2.BAT

cd \build
if "%1" == "protected" goto buildpro
PATH = \TOOLS;\PCS;\

rem
rem
rem     Build conventional memory PCS
rem
rem

\TOOLS\MAKE PCS.MAK

rem
rem
rem     Build protected memory PCS.
rem
rem
:buildpro
\TOOLS\MAKE PCSPRO.MAK
if "%1" == "protected" goto proutil
rem
rem
rem     Build expanded memory PCS.
rem
rem

\TOOLS\MAKE PCSEXP.MAK

rem
rem
rem     Build extended memory PCS.
rem
rem

\TOOLS\MAKE PCSEXT.MAK

rem
rem
rem	Build utility .EXE's.
rem
rem
:proutil
COMMAND /C \BUILD\DO_UTIL %1
if "%1" == "protected" goto copybuild
rem
rem
rem	Build Scheme compiler 
rem	(source compiler, autoloading compiler, runtime)
rem
rem

COMMAND /C \BUILD\DO_PCS

rem
rem
rem	Build Scheme autoload files
rem
rem

COMMAND /C \BUILD\DO_AUTO

rem
rem
rem	Build SCOOPS (there will be no prompts for 10-15 minutes)
rem
rem

COMMAND /C \BUILD\DO_SCOOPS

rem
rem
rem	Build EDWIN (3 phases)
rem
rem

COMMAND /C \BUILD\DO_EDWIN

:copybuild
rem
rem 	Copy everything else to \EXEC directory
rem

cd \build
if "%1" == "protected" goto copypro
COPY read.me                  \exec /v
COPY install.bat              \exec /v
COPY install2.bat             \exec /v
COPY memory.bat               \exec /v
COPY \tools\pkxarc.com        \exec /v
COPY \exec\misc\compiler.fsl  \exec /v
COPY \exec\misc\primops.fsl   \exec /v
COPY \exec\misc\autocomp.fsl  \exec /v
COPY \exec\misc\autoprim.fsl  \exec /v
COPY \build\newpcs\edwin.ini  \exec /v
COPY \build\scoops\scpsdemo.s \exec /v
COPY \build\newpcs\kldscope.s \exec /v
COPY \build\newpcs\help.s     \exec /v
COPY \build\newpcs\graphics.s \exec /v
:copypro
COPY instpro.bat              \exec /v
COPY proread.me		      \exec /v
COPY \tools\os.286            \exec /v
COPY \tools\vers8042.com      \exec /v	


rem
rem	Get today's date on everything
rem

cd \exec
dater *.*
if "%1" == "protected" goto createpro
cd \build\sources
dater *.*
cd \build\xli
dater *.*

rem
rem	Create .ARC files
rem

cd \exec
pkarc a pkdisk1 compiler.app pcs.exe pcsex?.exe newtrig.exe
pkarc a pkdisk2 *.s *.fsl runtime.app make_fsl.exe edwin.ini
pkarc a pksrc \build\sources\*.*
pkarc a pkxli \build\xli\*.*
dater *.arc

rem
rem	Create the installation diskettes
rem

rem Please put blank 360KB floppy disk into drive B:.
pause

copy read.me      b:
copy install.bat  b:
copy install2.bat b:
copy memory.bat   b:
copy memtype.exe  b:
copy pkdisk1.arc  b:
copy pkxarc.com   b:

rem Remove the floppy disk from drive B: and label it
rem "PC Scheme Installation Diskette".
rem Put blank 360KB floppy disk into drive B:.
pause

copy install2.bat b:
copy pkdisk2.arc  b:
copy pksrc.arc    b:
copy pkxli.arc    b:
copy pkxarc.com   b:

rem Remove the floppy disk from drive B: and label it
rem "PC Scheme Autoload Diskette."
:createpro
rem Put a blank 360KB floppy disk into drive B:.
pause

copy proread.me   b:
copy instpro.bat  b:
copy pcspro.exe   b:
copy machtype.exe b:
copy os.286       b:
copy realio.exe   b:
copy graphics.exe b:
copy realschm.exe b:
copy vers8042.com b:

if "%1" == "" goto createreg
rem - 
rem -  Remove the floppy disk from drive B: and label it
rem -  "PC Scheme Protected Mode Installation Diskette."
rem - 
rem -         Protected Mode build complete
rem -
goto finished


:createreg
ECHO OFF
cls
ECHO - 
ECHO -  Remove the floppy disk from drive B: and label it
ECHO -  "PC Scheme Protected Mode Installation Diskette."
ECHO - 
ECHO -  Take the two diskettes: 
ECHO - 
ECHO -    "PC Scheme Installation Diskette" p/n 2537903-1610 and
ECHO -    "PC Scheme Autoload Diskette"     p/n 2537903-1611
ECHO -  
ECHO -  along with a formatted 3 1/2" diskette to a system which
ECHO -  contains both a 5 1/4" inch floppy drive and a 3 1/2"
ECHO -  drive and do the following.
ECHO - 
ECHO -	  1. Place the 3 1/2" diskette into the 3 1/2" drive
ECHO -	  2. Place the INSTALLATION DISKETTE into the floppy drive
ECHO -	  3. COPY *.* the INSTALLATION DISKETTE to the 3 1/2" drive
ECHO -	  4. Place the AUTOLOAD DISKETTE into the floppy drive
ECHO -	  5. COPY *.* the AUTOLOAD DISKETTE to the 3 1/2" drive
ECHO -	  6. Remove the 3 1/2" diskette and label:
ECHO -	       "PC Scheme 3 1/2" Installation Diskette"
ECHO - 	  
ECHO - 	After completing the above procedure, the PC Scheme build
ECHO -  will be complete.  
:finished