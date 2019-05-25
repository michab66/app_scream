ECHO OFF
MEMTYPE
IF NOT ERRORLEVEL 3 GOTO NEXT1
ECHO Your computer contains both expanded and extended memory.
GOTO END
:NEXT1
IF NOT ERRORLEVEL 2 GOTO NEXT2
ECHO Your computer contains expanded memory.
GOTO END
:NEXT2
IF NOT ERRORLEVEL 1 GOTO NEXT3
ECHO Your computer contains extended memory.
GOTO END
:NEXT3
ECHO Your computer contains only conventional memory.
:END
