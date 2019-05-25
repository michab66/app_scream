ECHO OFF
CLS
a:
IF %1x==x INSTPRO C:
IF %2y==y INSTPRO %1 \
ECHO ------------------------------------------------------------------------
ECHO -
ECHO - Installing Protected Mode Scheme on disk %1 directory %2
ECHO - 
ECHO - If after installation you encounter problems getting
ECHO - the protected mode application running, read PROREAD.ME
ECHO - for assistance.
ECHO -
ECHO ------------------------------------------------------------------------
PAUSE
CLS

IF EXIST %1%2 (pause, warning, will overwrite old files)
ECHO Creating the %2 directory structure on drive %1.
IF NOT EXIST %1%2      MKDIR %1%2

COPY a:\PROREAD.ME %1%2
COPY a:\MACHTYPE.EXE %1%2

ECHO Installing Protected Mode files in %1%2
COPY a:OS.286 %1%2
COPY a:PCSPRO.EXE %1%2
COPY a:REALSCHM.EXE %1%2
COPY a:REALIO.EXE %1%2
COPY a:GRAPHICS.EXE %1%2


if not exist %1\CONFIG.286 goto build_config
ECHO - config.286 already exists, new one will NOT be created.
GOTO config_ret

:build_config
ECHO Copying CONFIG.286  to %1\
ECHO
ECHO You may need to edit CONFIG.286 for your particular machine.
ECHO See %1%2\PROREAD.ME for details.
ECHO
MACHTYPE

IF NOT ERRORLEVEL 3 GOTO chk_newat
REM  PS2 model 50,60, or 80 - note as such in config.286
ECHO ps2=1 >%1\config.286
ECHO shutdown=a >>%1\config.286
ECHO keyboardwait=1 >>%1\config.286
GOTO done_config

:chk_newat
IF NOT ERRORLEVEL 2 GOTO chk_oldat
REM  newer at/bios, use fastest values in config.286
ECHO shutdown=a >%1\config.286
ECHO keyboardwait=1 >>%1\config.286
GOTO done_config

:chk_oldat
IF NOT ERRORLEVEL 1 GOTO chk_known
REM  older at/bios, use relatively safe values in config.286
ECHO shutdown=9 >%1\config.286
ECHO keyboardwait=1 >>%1\config.286
GOTO done_config

:chk_known
IF NOT ERRORLEVEL 0 GOTO chk_error
REM  unknown machine, create default values in config.286
ECHO shutdown=9 >%1\config.286
ECHO keyboardwait=200 >>%1\config.286

:done_config
REM  append location of os286 kernel to config.286 file
ECHO kernel=%1%2\os.286 >>%1\config.286
GOTO config_ret

:chk_error
ECHO
ECHO Machine does not support extended memory and therefore doesn't
ECHO support protected mode applications 
ECHO
GOTO config_ret

:config_ret
%1:
CD %2
ECHO - End of Protected Mode Scheme installation.
