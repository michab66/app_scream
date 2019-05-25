# PC Scheme 4.0 make file:  Protected Memory Scheme

#
# command-line macro variables
#
redo=                         # if nonempty, force make to do everything here
debug=			      # if nonempty, generate symbol file

#
# directories
#

tools=\tools		    # read-only
lib=\lib		    # read-only
src=\build		    # read-only
obj=\objectp		    # read-write
exec=\exec		    # read-write

#
# rules
#

.asm.obj:
  $(tools)\masm /DPROMEM $*,$@;

.c.obj:
  $(tools)\lc -dPROMEM -ms -ccdswum -o$@ $*

#
# make-specific initialization
#

# if redo on command line specified, build the entire system
:
  if not "$(redo)"=="" del $(obj)\*.obj
  if not "$(redo)"=="" del $(exec)\pcspro.*

#
# application-specific initialization
#

:
  cd $(src)
  path $(tools)
				  

#
# assembly language files
#

$(src)\schemed.equ:  $(src)\memtype.equ
  $(tools)\touch schemed.equ

$(src)\scheme.equ:   $(src)\schemed.equ $(src)\schemed.ref $(src)\schemed.mac \
		     $(src)\smmu.mac
  $(tools)\touch scheme.equ

$(obj)\alink.obj:      $(src)\alink.asm
  $(tools)\masm /DPROMEM $*,$@;

$(obj)\block.obj:     $(src)\block.asm $(src)\scheme.equ $(src)\memtype.equ
  $(tools)\masm /DPROMEM $*,$@;

$(obj)\flo2hex.obj:    $(src)\flo2hex.asm
  $(tools)\masm /DPROMEM $*,$@;

$(obj)\get_path.obj:   $(src)\get_path.asm $(src)\scheme.equ
  $(tools)\masm /DPROMEM $*,$@;

$(obj)\prointrp.obj:     $(src)\prointrp.asm $(src)\dos.mac $(src)\pcmake.equ
  $(tools)\masm /DPROMEM $*,$@;
  REM **** REMEMBER TO CHECK INTRUP.ASM FOR REAL MODE UPDATES ALSO ****

$(obj)\msdos.obj:      $(src)\msdos.asm
  $(tools)\masm /DPROMEM $*,$@;

$(obj)\saprop.obj:     $(src)\saprop.asm $(src)\scheme.equ
  $(tools)\masm /DPROMEM $*,$@;

$(obj)\probid.obj:       $(src)\probid.asm
  $(tools)\masm /DPROMEM $*,$@;
  REM **** REMEMBER TO CHECK SBID.ASM FOR REAL MODE UPDATES ALSO ****

$(obj)\sbigmath.obj:   $(src)\sbigmath.asm $(src)\scheme.equ
  $(tools)\masm /DPROMEM $*,$@;

$(obj)\sc.obj:	       $(src)\sc.asm $(src)\dos.mac
  $(tools)\masm /DPROMEM $*,$@;

$(obj)\scannum.obj:    $(src)\scannum.asm
  $(tools)\masm /DPROMEM $*,$@;

$(obj)\scar_cdr.obj:  $(src)\scar_cdr.asm $(src)\scheme.equ $(src)\sinterp.mac \
		      $(src)\sinterp.arg
  $(tools)\masm /DPROMEM $*,$@;

$(obj)\schemed.obj:    $(src)\schemed.asm $(src)\schemed.equ $(src)\sasm.mac
  $(tools)\masm /DPROMEM $*,$@;

$(obj)\senv.obj:       $(src)\senv.asm	$(src)\scheme.equ $(src)\sinterp.mac \
		       $(src)\sinterp.arg $(src)\stackf.equ
  $(tools)\masm /DPROMEM $*,$@;

$(obj)\sexec.obj:      $(src)\sexec.asm $(src)\scheme.equ
  $(tools)\masm /DPROMEM $*,$@;

$(obj)\sgcmark.obj:    $(src)\sgcmark.asm $(src)\scheme.equ
  $(tools)\masm /DPROMEM $*,$@;

$(obj)\sgcsweep.obj:   $(src)\sgcsweep.asm $(src)\scheme.equ
  $(tools)\masm /DPROMEM $*,$@;

$(obj)\sinterp.obj:   $(src)\sinterp.asm $(src)\schemed.equ $(src)\schemed.ref \
		      $(src)\schemed.mac $(src)\pcmake.equ $(src)\stackf.equ   \
		      $(src)\smmu.mac $(src)\sinterp.mac $(src)\sinterp.arg
  $(tools)\masm /DPROMEM $*,$@;

$(obj)\prosmmu.obj:       $(src)\prosmmu.asm $(src)\schemed.equ $(src)\schemed.ref
  $(tools)\masm /DPROMEM $*,$@;

$(obj)\sobjhash.obj:   $(src)\sobjhash.asm $(src)\scheme.equ
  $(tools)\masm /DPROMEM $*,$@;

$(obj)\squish.obj:     $(src)\squish.asm $(src)\scheme.equ
  $(tools)\masm /DPROMEM $*,$@;

$(obj)\srch_str.obj:   $(src)\srch_str.asm $(src)\scheme.equ $(src)\pcmake.equ \
		       $(src)\sinterp.arg
  $(tools)\masm /DPROMEM $*,$@;

$(obj)\srelocat.obj:   $(src)\srelocat.asm $(src)\scheme.equ
  $(tools)\masm /DPROMEM $*,$@;

$(obj)\sstack.obj:     $(src)\sstack.asm $(src)\scheme.equ $(src)\sinterp.mac \
		       $(src)\sinterp.arg $(src)\stackf.equ
  $(tools)\masm /DPROMEM $*,$@;

$(obj)\sstring.obj:    $(src)\sstring.asm $(src)\scheme.equ $(src)\sinterp.mac \
		       $(src)\sinterp.arg
  $(tools)\masm /DPROMEM $*,$@;

# stimer exists within sinterp.asm for protected mode
#(obj)\stimer.obj:     $(src)\stimer.asm $(src)\scheme.equ
# $(tools)\masm /DPROMEM $*,$@;

$(obj)\strmlnrs.obj:   $(src)\strmlnrs.asm $(src)\scheme.equ
  $(tools)\masm /DPROMEM $*,$@;

$(obj)\sutil.obj:      $(src)\sutil.asm $(src)\scheme.equ
  $(tools)\masm /DPROMEM $*,$@;
  REM **** REMEMBER TO CHECK FOR REAL MODE UPDATES ALSO ****

$(obj)\svars.obj:      $(src)\svars.asm $(src)\scheme.equ $(src)\sinterp.mac \
		       $(src)\sinterp.arg
  $(tools)\masm /DPROMEM $*,$@;

# sw_int now exists within PRO2REAL.ASM
#$(obj)\sw_int.obj:     $(src)\sw_int.asm
#  $(tools)\masm /DPROMEM $*,$@;

$(obj)\pro2real.obj:   $(src)\pro2real.asm $(src)\scheme.equ $(src)\xli.equ \
		       $(src)\xli_pro.mac $(src)\rpc.equ $(src)\sinterp.arg
  $(tools)\masm /DPROMEM $*,$@;
  REM **** REMEMBER TO CHECK XLI.ASM FOR REAL MODE UPDATES ALSO ****



#
# C source files
#

$(src)\scheme.h:       $(src)\memtype.h $(src)\schmdefs.h
  $(tools)\touch scheme.h	

$(obj)\asm_link.obj:   $(src)\asm_link.c $(src)\scheme.h
# $(tools)\lc -ms -ccdswum -o$@ $*
  $(tools)\lc1 -dPROMEM -ms -ccdswum -o$(obj)\$*.q $*
  $(tools)\lc2 -o -v$@ $(obj)\$*.q

$(obj)\freesp.obj:     $(src)\freesp.c $(src)\scheme.h $(src)\ctype.h
# $(tools)\lc -ms -ccdswum -o$@ $*
  $(tools)\lc1 -dPROMEM -ms -ccdswum -o$(obj)\$*.q $*
  $(tools)\lc2 -o -v$@ $(obj)\$*.q

$(obj)\sarith.obj:     $(src)\sarith.c $(src)\scheme.h $(src)\slist.h
# $(tools)\lc -ms -ccdswum -o$@ $*
  $(tools)\lc1 -dPROMEM -ms -ccdswum -o$(obj)\$*.q $*
  $(tools)\lc2 -o -v$@ $(obj)\$*.q

$(obj)\sbigmem.obj:    $(src)\sbigmem.c $(src)\scheme.h
# $(tools)\lc -ms -ccdswum -o$@ $*
  $(tools)\lc1 -dPROMEM -ms -ccdswum -o$(obj)\$*.q $*
  $(tools)\lc2 -o -v$@ $(obj)\$*.q

$(obj)\sdebug.obj:     $(src)\sdebug.c $(src)\scheme.h $(src)\ctype.h
# $(tools)\lc -ms -ccdswum -o$@ $*
  $(tools)\lc1 -dPROMEM -ms -ccdswum -o$(obj)\$*.q $*
  $(tools)\lc2 -o -v$@ $(obj)\$*.q

$(obj)\sdump.obj:      $(src)\sdump.c $(src)\scheme.h $(src)\ctype.h \
		       $(src)\schars.h $(src)\slist.h
# $(tools)\lc -ms -ccdswum -o$@ $*
  $(tools)\lc1 -dPROMEM -ms -ccdswum -o$(obj)\$*.q $*
  $(tools)\lc2 -o -v$@ $(obj)\$*.q

$(obj)\serrmsg.obj:    $(src)\serrmsg.c $(src)\scheme.h
# $(tools)\lc -ms -ccdswum -o$@ $*
  $(tools)\lc1 -dPROMEM -ms -ccdswum -o$(obj)\$*.q $*
  $(tools)\lc2 -o -v$@ $(obj)\$*.q

$(obj)\serror.obj:     $(src)\serror.c $(src)\scheme.h $(src)\ctype.h \
		       $(src)\slist.h
# $(tools)\lc -ms -ccdswum -o$@ $*
  $(tools)\lc1 -dPROMEM -ms -ccdswum -o$(obj)\$*.q $*
  $(tools)\lc2 -o -v$@ $(obj)\$*.q

$(obj)\sfasl.obj:      $(src)\sfasl.c $(src)\scheme.h $(src)\stdio.h \
		       $(src)\slist.h
# $(tools)\lc -ms -ccdswum -o$@ $*
  $(tools)\lc1 -dPROMEM -ms -ccdswum -o$(obj)\$*.q $*
  $(tools)\lc2 -o -v$@ $(obj)\$*.q

$(obj)\shash.obj:      $(src)\shash.c $(src)\scheme.h $(src)\ctype.h
# $(tools)\lc -ms -ccdswum -o$@ $*
  $(tools)\lc1 -dPROMEM -ms -ccdswum -o$(obj)\$*.q $*
  $(tools)\lc2 -o -v$@ $(obj)\$*.q

$(obj)\slink.obj:      $(src)\slink.c $(src)\ctype.h $(src)\slink.h
# $(tools)\lc -ms -ccdswum -o$@ $*
  $(tools)\lc1 -dPROMEM -ms -ccdswum -o$(obj)\$*.q $*
  $(tools)\lc2 -o -v$@ $(obj)\$*.q

$(obj)\smain.obj:      $(src)\smain.c $(src)\version.h $(src)\scheme.h \
		       $(src)\sport.h $(src)\pcmake.h
# $(tools)\lc -ms -ccdswum -o$@ $*
  $(tools)\lc1 -dPROMEM -ms -ccdswum -o$(obj)\$*.q $*
  $(tools)\lc2 -o -v$@ $(obj)\$*.q

$(obj)\smemory.obj:    $(src)\smemory.c $(src)\scheme.h
# $(tools)\lc -ms -ccdswum -o$@ $*
  $(tools)\lc1 -dPROMEM -ms -ccdswum -o$(obj)\$*.q $*
  $(tools)\lc2 -o -v$@ $(obj)\$*.q

$(obj)\sprintf.obj:    $(src)\sprintf.c $(src)\scheme.h
# $(tools)\lc -ms -ccdswum -o$@ $*
  $(tools)\lc1 -dPROMEM -ms -ccdswum -o$(obj)\$*.q $*
  $(tools)\lc2 -o -v$@ $(obj)\$*.q

$(obj)\sprop.obj:      $(src)\sprop.c $(src)\scheme.h $(src)\ctype.h \
		       $(src)\slist.h
# $(tools)\lc -ms -ccdswum -o$@ $*
  $(tools)\lc1 -dPROMEM -ms -ccdswum -o$(obj)\$*.q $*
  $(tools)\lc2 -o -v$@ $(obj)\$*.q

$(obj)\sreify.obj:     $(src)\sreify.c $(src)\scheme.h
# $(tools)\lc -ms -ccdswum -o$@ $*
  $(tools)\lc1 -dPROMEM -ms -ccdswum -o$(obj)\$*.q $*
  $(tools)\lc2 -o -v$@ $(obj)\$*.q

$(obj)\sreset.obj:     $(src)\sreset.c $(src)\scheme.h $(src)\ctype.h
# $(tools)\lc -ms -ccdswum -o$@ $*
  $(tools)\lc1 -dPROMEM -ms -ccdswum -o$(obj)\$*.q $*
  $(tools)\lc2 -o -v$@ $(obj)\$*.q

$(obj)\strace.obj:     $(src)\strace.c $(src)\scheme.h
# $(tools)\lc -ms -ccdswum -o$@ $*
  $(tools)\lc1 -dPROMEM -ms -ccdswum -o$(obj)\$*.q $*
  $(tools)\lc2 -o -v$@ $(obj)\$*.q

$(obj)\support.obj:    $(src)\support.c $(src)\scheme.h $(src)\slist.h
# $(tools)\lc -ms -ccdswum -o$@ $*
  $(tools)\lc1 -dPROMEM -ms -ccdswum -o$(obj)\$*.q $*
  $(tools)\lc2 -o -v$@ $(obj)\$*.q

#
# I/O source files - All of protected mode scheme's I/O files have been 
# grouped together in one place so they can be easily recognized from 
# Real Mode Scheme's I/O files. There wasn't enough time to get all the I/O
# code merged, so there may be duplicate code between regular scheme and
# protected mode scheme. This means that if any code is modified in the
# following files, you should check the corresponding real mode files also
#

# Assembly language I/O files

$(obj)\proio.obj:  $(src)\proio.asm $(src)\scheme.equ $(src)\sinterp.arg
  $(tools)\masm /DPROMEM $*,$@;
  REM **** REMEMBER TO CHECK REAL MODE I/O FILES FOR UPDATES ALSO ****

$(obj)\proiosup.obj:  $(src)\proiosup.asm $(src)\scheme.equ $(src)\xli.equ \
		      $(src)\xli_pro.mac
  $(tools)\masm /DPROMEM $*,$@;
  REM **** REMEMBER TO CHECK REAL MODE I/O FILES FOR UPDATES ALSO ****

$(obj)\proread.obj:  $(src)\proread.asm $(src)\scheme.equ $(src)\sinterp.arg
  $(tools)\masm /DPROMEM $*,$@;
  REM **** REMEMBER TO CHECK REAL MODE I/O FILES FOR UPDATES ALSO ****

$(obj)\prosprin.obj:   $(src)\prosprin.asm $(src)\scheme.equ $(src)\sinterp.arg
  $(tools)\masm /DPROMEM $*,$@;
  REM **** REMEMBER TO CHECK REAL MODE I/O FILES FOR UPDATES ALSO ****

$(obj)\prosread.obj:    $(src)\prosread.asm $(src)\scheme.equ $(src)\sinterp.arg
  $(tools)\masm /DPROMEM $*,$@;
  REM **** REMEMBER TO CHECK REAL MODE I/O FILES FOR UPDATES ALSO ****

$(obj)\prowin.obj:    $(src)\prowin.asm $(src)\scheme.equ $(src)\xli.equ \
		      $(src)\xli_pro.mac
  $(tools)\masm /DPROMEM $*,$@;
  REM **** REMEMBER TO CHECK REAL MODE I/O FILES FOR UPDATES ALSO ****



# Lattice C I/O routines

$(obj)\prociosp.obj:   $(src)\prociosp.c $(src)\scheme.h $(src)\sport.h \
		       $(src)\slist.h
  $(tools)\lc1 -dPROMEM -ms -ccdswum -o$(obj)\$*.q $*
  $(tools)\lc2 -o -v$@ $(obj)\$*.q
  REM **** REMEMBER TO CHECK REAL MODE I/O FILES FOR UPDATES ALSO ****



# Real Mode Exe Files

$(obj)\realschm.obj:    $(src)\realschm.asm $(src)\xli.equ $(src)\xli.ref \
		        $(src)\xli_pro.mac
  $(tools)\masm /DPROMEM $*,$@;
  REM **** REMEMBER TO CHECK REAL MODE I/O FILES FOR UPDATES ALSO ****

$(obj)\realio.obj:    $(src)\realio.asm $(src)\schemed.equ
  $(tools)\masm /DPROMEM $*,$@;
  REM **** REMEMBER TO CHECK REAL MODE I/O FILES FOR UPDATES ALSO ****

$(obj)\graphics.obj:   $(src)\graphics.asm $(src)\pcmake.equ
  $(tools)\masm /DPROMEM $*,$@;
  REM **** REMEMBER TO CHECK REAL MODE I/O FILES FOR UPDATES ALSO ****


#
# Generate PCSPRO.EXE file
#

$(exec)\pcs.exe:    $(obj)\sc.obj \
		$(obj)\schemed.obj \
		$(obj)\sinterp.obj \
		$(obj)\smain.obj \
		$(obj)\sstring.obj \
		$(obj)\sgcmark.obj \
		$(obj)\sgcsweep.obj \
		$(obj)\sbigmem.obj \
		$(obj)\sexec.obj \
		$(obj)\support.obj \
		$(obj)\shash.obj \
		$(obj)\sstack.obj \
		$(obj)\sarith.obj \
		$(obj)\sutil.obj \
		$(obj)\serror.obj \
		$(obj)\sdebug.obj \
		$(obj)\sdump.obj \
		$(obj)\strace.obj \
		$(obj)\serrmsg.obj \
		$(obj)\sbigmath.obj \
		$(obj)\scar_cdr.obj \
		$(obj)\svars.obj \
		$(obj)\saprop.obj \
		$(obj)\sprop.obj \
		$(obj)\msdos.obj \
		$(obj)\sreset.obj \
		$(obj)\sfasl.obj \
		$(obj)\sreify.obj \
		$(obj)\senv.obj \
		$(obj)\sprintf.obj \
		$(obj)\scannum.obj \
		$(obj)\get_path.obj \
		$(obj)\sobjhash.obj \
		$(obj)\asm_link.obj \
		$(obj)\slink.obj \
		$(obj)\alink.obj \
		$(obj)\probid.obj \
		$(obj)\strmlnrs.obj \
		$(obj)\srch_str.obj \
		$(obj)\squish.obj \
		$(obj)\srelocat.obj \
		$(obj)\freesp.obj \
		$(obj)\flo2hex.obj \
                $(obj)\block.obj \
                $(obj)\prociosp.obj \
	        $(obj)\prointrp.obj \
		$(obj)\proio.obj \
                $(obj)\proiosup.obj \
                $(obj)\proread.obj \
	        $(obj)\prosmmu.obj \
                $(obj)\prosread.obj \
                $(obj)\prowin.obj \
                $(obj)\pro2real.obj \
                $(obj)\prosprin.obj
  cd $(obj)
  $(tools)\link @$(src)\pcspro.lnk,$(exec)\pcspro.exe,$(exec)\pcspro.map/map,$(lib)\lcm+$(lib)\lc;
  cd $(exec)
  if not "$(debug)"=="" $(tools)\mapsym $(exec)\pcspro.map
  $(tools)\express -msl pcspro
  $(tools)\bind -o $(exec)\pcspro.exe -i $(exec)\pcspro.exp -l $(tools)\tinyup.exe
  cd $(src)


#
# Generate REALSCHM.EXE file
#

$(exec)\realschm.exe:    $(obj)\realschm.obj
  cd $(obj)
  $(tools)\link $(obj)\realschm.obj,$(exec)\realschm.exe,$(exec)\realschm.map/map;
  cd $(src)

#
# Generate REALIO.EXE file
#

$(exec)\realio.exe:     $(obj)\realio.obj
  cd $(obj)
  $(tools)\link $(obj)\realio.obj,$(exec)\realio.exe,$(exec)\realio.map/map;
  cd $(src)

#
# Generate GRAPHICS.EXE file
#

$(exec)\graphics.exe:     $(obj)\graphics.obj
  cd $(obj)
  $(tools)\link $(obj)\graphics.obj,$(exec)\graphics.exe,$(exec)\graphics.map/map;
  cd $(src)

:
  cd $(src)

#
# make-specific wrapup
#

:
  echo MAKE is done.
