:
: PC Scheme installation batch stream, part 2
: call from part 1:  install2 <f|f2|w> <dir> <memtype>
:
: then-parts of next 2 lines are never executed by a:install
if "%3" == "EXP" install2 %1 %2 exp
if "%3" == "EXT" install2 %1 %2 ext
if "%1" == "f2" goto floppy
: Next 3 lines commented out for installations from 3.5" diskette
: echo Please remove the PC Scheme Installation disk from drive A
: echo and replace it with the PC Scheme Autoload disk.
: pause
if "%1" == "w" goto windisk
if "%1" =