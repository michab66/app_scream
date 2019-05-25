# PC Scheme 4.0 make file:  Conventional memory PCS

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
obj=\object		    # read-write
exec=\exec		    # read-write

#
# rules
#

.asm.obj:
  $(tools)\masm /DREGMEM $*,$@;

.c.obj:
  $(tools)\lc -dREGMEM -ms -ccdswum -o$@ $*

#
# make-specific initialization
#

# if redo on command line specified, build the entire system
:
  if not "$(redo)"=="" del $(obj)\*.obj
  if not "$(redo)"=="" del $(exec)\pcs.*

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
  $(tools)\masm /DREGMEM $*,$@;

$(obj)\block.obj:     $(src)\block.asm $(src)\scheme.equ $(src)\memtype.equ
  $(tools)\masm /DREGMEM $*,$@;

$(obj)\flo2hex.obj:    $(src)\flo2hex.asm
  $(tools)\masm /DREGMEM $*,$@;

$(obj)\get_path.obj:   $(src)\get_path.asm $(src)\scheme.equ
  $(tools)\masm /DREGMEM $*,$@;

#$(obj)\graphcmd.obj:   $(src)\graphcmd.asm $(src)\pcmake.equ
#  $(tools)\masm /DREGMEM $*,$@;

# PCS 3.02 version:  graphics integrated into VM
$(obj)\graphcmd.obj:   $(src)\graphics.asm $(src)\pcmake.equ
  $(tools)\masm /DREGMEM $(src)\graphics,$@ /Dcombined /Dti /Dibm;

$(obj)\intrup.obj:     $(src)\intrup.asm $(src)\dos.mac $(src)\pcmake.equ
  $(tools)\masm /DREGMEM $*,$@;
  REM **** REMEMBER TO CHECK PROINTRP.ASM FOR PROTECTED MODE UPDATES ALSO ****

$(obj)\msdos.obj:      $(src)\msdos.asm
  $(tools)\masm /DREGMEM $*,$@;

$(obj)\saprop.obj:     $(src)\saprop.asm $(src)\scheme.equ
  $(tools)\masm /DREGMEM $*,$@;

$(obj)\sbid.obj:       $(src)\sbid.asm
  $(tools)\masm /DREGMEM $*,$@;
  REM **** REMEMBER TO CHECK PROBID.ASM FOR PROTECTED MODE UPDATES ALSO ****

$(obj)\sbigmath.obj:   $(src)\sbigmath.asm $(src)\scheme.equ
  $(tools)\masm /DREGMEM $*,$@;

$(obj)\sc.obj:	       $(src)\sc.asm $(src)\dos.mac
  $(tools)\masm /DREGMEM $*,$@;

$(obj)\scannum.obj:    $(src)\scannum.asm
  $(tools)\masm /DREGMEM $*,$@;

$(obj)\scar_cdr.obj:  $(src)\scar_cdr.asm $(src)\scheme.equ $(src)\sinterp.mac \
		      $(src)\sinterp.arg
  $(tools)\masm /DREGMEM $*,$@;

$(obj)\schemed.obj:    $(src)\schemed.asm $(src)\schemed.equ $(src)\sasm.mac
  $(tools)\masm /DREGMEM $*,$@;

$(obj)\senv.obj:       $(src)\senv.asm	$(src)\scheme.equ $(src)\sinterp.mac \
		       $(src)\sinterp.arg $(src)\stackf.equ
  $(tools)\masm /DREGMEM $*,$@;

$(obj)\sexec.obj:      $(src)\sexec.asm $(src)\scheme.equ
  $(tools)\masm /DREGMEM $*,$@;

$(obj)\sgcmark.obj:    $(src)\sgcmark.asm $(src)\scheme.equ
  $(tools)\masm /DREGMEM $*,$@;

$(obj)\sgcsweep.obj:   $(src)\sgcsweep.asm $(src)\scheme.equ
  $(tools)\masm /DREGMEM $*,$@;

$(obj)\sinterp.obj:   $(src)\sinterp.asm $(src)\schemed.equ $(src)\schemed.ref \
		      $(src)\schemed.mac $(src)\pcmake.equ $(src)\stackf.equ   \
		      $(src)\smmu.mac $(src)\sinterp.mac $(src)\sinterp.arg
  $(tools)\masm /DREGMEM $*,$@;

$(obj)\smmu.obj:       $(src)\smmu.asm $(src)\schemed.equ $(src)\schemed.ref
  $(tools)\masm /DREGMEM $*,$@;

$(obj)\sobjhash.obj:   $(src)\sobjhash.asm $(src)\scheme.equ
  $(tools)\masm /DREGMEM $*,$@;

$(obj)\squish.obj:     $(src)\squish.asm $(src)\scheme.equ
  $(tools)\masm /DREGMEM $*,$@;

$(obj)\srch_str.obj:   $(src)\srch_str.asm $(src)\scheme.equ $(src)\pcmake.equ \
		       $(src)\sinterp.arg
  $(tools)\masm /DREGMEM $*,$@;

$(obj)\srelocat.obj:   $(src)\srelocat.asm $(src)\scheme.equ
  $(tools)\masm /DREGMEM $*,$@;

$(obj)\sstack.obj:     $(src)\sstack.asm $(src)\scheme.equ $(src)\sinterp.mac \
		       $(src)\sinterp.arg $(src)\stackf.equ
  $(tools)\masm /DREGMEM $*,$@;

$(obj)\sstring.obj:    $(src)\sstring.asm $(src)\scheme.equ $(src)\sinterp.mac \
		       $(src)\sinterp.arg
  $(tools)\masm /DREGMEM $*,$@;

$(obj)\stimer.obj:     $(src)\stimer.asm $(src)\scheme.equ
  $(tools)\masm /DREGMEM $*,$@;
  echo **** REMEMBER TO CHECK SINTERP.ASM FOR PROTECTED MODE UPDATES ALSO ****

$(obj)\strmlnrs.obj:   $(src)\strmlnrs.asm $(src)\scheme.equ
  $(tools)\masm /DREGMEM $*,$@;

$(obj)\sutil.obj:      $(src)\sutil.asm $(src)\scheme.equ
  $(tools)\masm /DREGMEM $*,$@;
  REM **** REMEMBER TO CHECK FOR PROTECTED MODE UPDATES ALSO ****

$(obj)\svars.obj:      $(src)\svars.asm $(src)\scheme.equ $(src)\sinterp.mac \
		       $(src)\sinterp.arg
  $(tools)\masm /DREGMEM $*,$@;

$(obj)\sw_int.obj:     $(src)\sw_int.asm
  $(tools)\masm /DREGMEM $*,$@;

$(obj)\xli.obj:        $(src)\xli.asm $(src)\scheme.equ
  $(tools)\masm /DREGMEM $*,$@;
  REM **** REMEMBER TO CHECK PRO2REAL.ASM FOR PROTECTED MODE UPDATES ALSO ****



#
# C source files
#

$(src)\scheme.h:       $(src)\memtype.h $(src)\schmdefs.h
  $(tools)\touch scheme.h	

$(obj)\asm_link.obj:   $(src)\asm_link.c $(src)\scheme.h
# $(tools)\lc -ms -ccdswum -o$@ $*
  $(tools)\lc1 -dREGMEM -ms -ccdswum -o$(obj)\$*.q $*
  $(tools)\lc2 -o -v$@ $(obj)\$*.q

$(obj)\freesp.obj:     $(src)\freesp.c $(src)\scheme.h $(src)\ctype.h
# $(tools)\lc -ms -ccdswum -o$@ $*
  $(tools)\lc1 -dREGMEM -ms -ccdswum -o$(obj)\$*.q $*
  $(tools)\lc2 -o -v$@ $(obj)\$*.q

$(obj)\sarith.obj:     $(src)\sarith.c $(src)\scheme.h $(src)\slist.h
# $(tools)\lc -ms -ccdswum -o$@ $*
  $(tools)\lc1 -dREGMEM -ms -ccdswum -o$(obj)\$*.q $*
  $(tools)\lc2 -o -v$@ $(obj)\$*.q

$(obj)\sbigmem.obj:    $(src)\sbigmem.c $(src)\scheme.h
# $(tools)\lc -ms -ccdswum -o$@ $*
  $(tools)\lc1 -dREGMEM -ms -ccdswum -o$(obj)\$*.q $*
  $(tools)\lc2 -o -v$@ $(obj)\$*.q

$(obj)\sdebug.obj:     $(src)\sdebug.c $(src)\scheme.h $(src)\ctype.h
# $(tools)\lc -ms -ccdswum -o$@ $*
  $(tools)\lc1 -dREGMEM -ms -ccdswum -o$(obj)\$*.q $*
  $(tools)\lc2 -o -v$@ $(obj)\$*.q

$(obj)\sdump.obj:      $(src)\sdump.c $(src)\scheme.h $(src)\ctype.h \
		       $(src)\schars.h $(src)\slist.h
# $(tools)\lc -ms -ccdswum -o$@ $*
  $(tools)\lc1 -dREGMEM -ms -ccdswum -o$(obj)\$*.q $*
  $(tools)\lc2 -o -v$@ $(obj)\$*.q

$(obj)\serrmsg.obj:    $(src)\serrmsg.c $(src)\scheme.h
# $(tools)\lc -ms -ccdswum -o$@ $*
  $(tools)\lc1 -dREGMEM -ms -ccdswum -o$(obj)\$*.q $*
  $(tools)\lc2 -o -v$@ $(obj)\$*.q

$(obj)\serror.obj:     $(src)\serror.c $(src)\scheme.h $(src)\ctype.h \
		       $(src)\slist.h
# $(tools)\lc -ms -ccdswum -o$@ $*
  $(tools)\lc1 -dREGMEM -ms -ccdswum -o$(obj)\$*.q $*
  $(tools)\lc2 -o -v$@ $(obj)\$*.q

$(obj)\sfasl.obj:      $(src)\sfasl.c $(src)\scheme.h $(src)\stdio.h \
		       $(src)\slist.h
# $(tools)\lc -ms -ccdswum -o$@ $*
  $(tools)\lc1 -dREGMEM -ms -ccdswum -o$(obj)\$*.q $*
  $(tools)\lc2 -o -v$@ $(obj)\$*.q

$(obj)\shash.obj:      $(src)\shash.c $(src)\scheme.h $(src)\ctype.h
# $(tools)\lc -ms -ccdswum -o$@ $*
  $(tools)\lc1 -dREGMEM -ms -ccdswum -o$(obj)\$*.q $*
  $(tools)\lc2 -o -v$@ $(obj)\$*.q

$(obj)\slink.obj:      $(src)\slink.c $(src)\ctype.h $(src)\slink.h
# $(tools)\lc -ms -ccdswum -o$@ $*
  $(tools)\lc1 -dREGMEM -ms -ccdswum -o$(obj)\$*.q $*
  $(tools)\lc2 -o -v$@ $(obj)\$*.q

$(obj)\smain.obj:      $(src)\smain.c $(src)\version.h $(src)\scheme.h \
		       $(src)\sport.h $(src)\pcmake.h
# $(tools)\lc -ms -ccdswum -o$@ $*
  $(tools)\lc1 -dREGMEM -ms -ccdswum -o$(obj)\$*.q $*
  $(tools)\lc2 -o -v$@ $(obj)\$*.q

$(obj)\smemory.obj:    $(src)\smemory.c $(src)\scheme.h
# $(tools)\lc -ms -ccdswum -o$@ $*
  $(tools)\lc1 -dREGMEM -ms -ccdswum -o$(obj)\$*.q $*
  $(tools)\lc2 -o -v$@ $(obj)\$*.q

$(obj)\sprintf.obj:    $(src)\sprintf.c $(src)\scheme.h
# $(tools)\lc -ms -ccdswum -o$@ $*
  $(tools)\lc1 -dREGMEM -ms -ccdswum -o$(obj)\$*.q $*
  $(tools)\lc2 -o -v$@ $(obj)\$*.q

$(obj)\sprop.obj:      $(src)\sprop.c $(src)\scheme.h $(src)\ctype.h \
		       $(src)\slist.h
# $(tools)\lc -ms -ccdswum -o$@ $*
  $(tools)\lc1 -dREGMEM -ms -ccdswum -o$(obj)\$*.q $*
  $(tools)\lc2 -o -v$@ $(obj)\$*.q

$(obj)\sreify.obj:     $(src)\sreify.c $(src)\scheme.h
# $(tools)\lc -ms -ccdswum -o$@ $*
  $(tools)\lc1 -dREGMEM -ms -ccdswum -o$(obj)\$*.q $*
  $(tools)\lc2 -o -v$@ $(obj)\$*.q

$(obj)\sreset.obj:     $(src)\sreset.c $(src)\scheme.h $(src)\ctype.h
# $(tools)\lc -ms -ccdswum -o$@ $*
  $(tools)\lc1 -dREGMEM -ms -ccdswum -o$(obj)\$*.q $*
  $(tools)\lc2 -o -v$@ $(obj)\$*.q

$(obj)\strace.obj:     $(src)\strace.c $(src)\scheme.h
# $(tools)\lc -ms -ccdswum -o$@ $*
  $(tools)\lc1 -dREGMEM -ms -ccdswum -o$(obj)\$*.q $*
  $(tools)\lc2 -o -v$@ $(obj)\$*.q

$(obj)\support.obj:    $(src)\support.c $(src)\scheme.h $(src)\slist.h
# $(tools)\lc -ms -ccdswum -o$@ $*
  $(tools)\lc1 -dREGMEM -ms -ccdswum -o$(obj)\$*.q $*
  $(tools)\lc2 -o -v$@ $(obj)\$*.q

#
# I/O source files - All of regular scheme's I/O files have been grouped
# together in one place so they can be easily recognized from Protected
# Mode Scheme's I/O files. There wasn't enough time to get all the I/O
# code merged, so there may be duplicate code between regular scheme and
# protected mode scheme. This means that if any code is modified in the
# following files, you should check the corresponding protected mode
# files also.
#

# Assembly language I/O files

$(obj)\border.obj:     $(src)\border.asm $(src)\scheme.equ $(src)\pcmake.equ
  $(tools)\masm /DREGMEM $*,$@;
  REM **** REMEMBER TO CHECK PROTECTED MODE I/O FILES FOR UPDATES ALSO ****

$(obj)\cio.obj:        $(src)\cio.asm $(src)\scheme.equ $(src)\sinterp.arg
  $(tools)\masm /DREGMEM $*,$@;
  REM **** REMEMBER TO CHECK PROTECTED MODE I/O FILES FOR UPDATES ALSO ****

$(obj)\cprint.obj:     $(src)\cprint.asm $(src)\scheme.equ $(src)\sinterp.arg
  $(tools)\masm /DREGMEM $*,$@;
  REM **** REMEMBER TO CHECK PROTECTED MODE I/O FILES FOR UPDATES ALSO ****

$(obj)\cprint1.obj:    $(src)\cprint1.asm $(src)\scheme.equ
  $(tools)\masm /DREGMEM $*,$@;
  REM **** REMEMBER TO CHECK PROTECTED MODE I/O FILES FOR UPDATES ALSO ****

$(obj)\cread.obj:      $(src)\cread.asm $(src)\scheme.equ $(src)\sinterp.arg
  $(tools)\masm /DREGMEM $*,$@;
  REM **** REMEMBER TO CHECK PROTECTED MODE I/O FILES FOR UPDATES ALSO ****

$(obj)\cwindow.obj:    $(src)\cwindow.asm $(src)\scheme.equ $(src)\sinterp.arg
  $(tools)\masm /DREGMEM $*,$@;
  REM **** REMEMBER TO CHECK PROTECTED MODE I/O FILES FOR UPDATES ALSO ****

$(obj)\scroll.obj:     $(src)\scroll.asm $(src)\pcmake.equ
  $(tools)\masm /DREGMEM $*,$@;
  REM **** REMEMBER TO CHECK PROTECTED MODE I/O FILES FOR UPDATES ALSO ****

$(obj)\sio.obj:        $(src)\sio.asm $(src)\scheme.equ
  $(tools)\masm /DREGMEM $*,$@;
  REM **** REMEMBER TO CHECK PROTECTED MODE I/O FILES FOR UPDATES ALSO ****

$(obj)\zio.obj:        $(src)\zio.asm $(src)\scheme.equ $(src)\pcmake.equ
  $(tools)\masm /DREGMEM $*,$@;
  REM **** REMEMBER TO CHECK PROTECTED MODE I/O FILES FOR UPDATES ALSO ****

# Lattice C I/O files

$(obj)\get_port.obj:   $(src)\get_port.c $(src)\scheme.h $(src)\ctype.h
  $(tools)\lc1 -dREGMEM -ms -ccdswum -o$(obj)\$*.q $*
  $(tools)\lc2 -o -v$@ $(obj)\$*.q
  REM **** REMEMBER TO CHECK PROTECTED MODE I/O FILES FOR UPDATES ALSO ****

$(obj)\sin_out.obj:    $(src)\sin_out.c $(src)\scheme.h $(src)\sport.h \
		       $(src)\slist.h
  $(tools)\lc1 -dREGMEM -ms -ccdswum -o$(obj)\$*.q $*
  $(tools)\lc2 -o -v$@ $(obj)\$*.q
  REM **** REMEMBER TO CHECK PROTECTED MODE I/O FILES FOR UPDATES ALSO ****

$(obj)\sprint.obj:     $(src)\sprint.c $(src)\scheme.h $(src)\schars.h
  $(tools)\lc1 -dREGMEM -ms -ccdswum -o$(obj)\$*.q $*
  $(tools)\lc2 -o -v$@ $(obj)\$*.q
  REM **** REMEMBER TO CHECK PROTECTED MODE I/O FILES FOR UPDATES ALSO ****

$(obj)\sread.obj:      $(src)\sread.c $(src)\scheme.h $(src)\schars.h
  $(tools)\lc1 -dREGMEM -ms -ccdswum -o$(obj)\$*.q $*
  $(tools)\lc2 -o -v$@ $(obj)\$*.q
  REM **** REMEMBER TO CHECK PROTECTED MODE I/O FILES FOR UPDATES ALSO ****

$(obj)\zcio.obj:       $(src)\zcio.c $(src)\scheme.h $(src)\sport.h \
		       $(src)\slist.h
  $(tools)\lc1 -dREGMEM -ms -ccdswum -o$(obj)\$*.q $*
  $(tools)\lc2 -o -v$@ $(obj)\$*.q
  REM **** REMEMBER TO CHECK PROTECTED MODE I/O FILES FOR UPDATES ALSO ****

#
# Generate .EXE file
#

$(exec)\pcs.exe:    $(obj)\sc.obj \
		$(obj)\schemed.obj \
		$(obj)\sinterp.obj \
		$(obj)\smain.obj \
		$(obj)\intrup.obj \
		$(obj)\sstring.obj \
		$(obj)\smemory.obj \
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
		$(obj)\sprint.obj \
		$(obj)\cprint.obj \
		$(obj)\cprint1.obj \
		$(obj)\serrmsg.obj \
		$(obj)\sio.obj \
		$(obj)\sbigmath.obj \
		$(obj)\sread.obj \
		$(obj)\cread.obj \
		$(obj)\zcio.obj \
		$(obj)\sin_out.obj \
		$(obj)\cio.obj \
		$(obj)\cwindow.obj \
		$(obj)\scar_cdr.obj \
		$(obj)\svars.obj \
		$(obj)\saprop.obj \
		$(obj)\sprop.obj \
		$(obj)\msdos.obj \
		$(obj)\sreset.obj \
		$(obj)\get_port.obj \
		$(obj)\sfasl.obj \
		$(obj)\zio.obj \
		$(obj)\sreify.obj \
		$(obj)\senv.obj \
		$(obj)\sprintf.obj \
		$(obj)\scannum.obj \
		$(obj)\stimer.obj \
		$(obj)\get_path.obj \
		$(obj)\sobjhash.obj \
		$(obj)\asm_link.obj \
		$(obj)\graphcmd.obj \
		$(obj)\border.obj \
	        $(obj)\block.obj \
		$(obj)\slink.obj \
		$(obj)\alink.obj \
		$(obj)\sbid.obj \
		$(obj)\strmlnrs.obj \
		$(obj)\srch_str.obj \
		$(obj)\squish.obj \
		$(obj)\srelocat.obj \
		$(obj)\freesp.obj \
		$(obj)\scroll.obj \
		$(obj)\sw_int.obj \
		$(obj)\flo2hex.obj \
		$(obj)\smmu.obj \
		$(obj)\xli.obj
  cd $(obj)
  $(tools)\link @$(src)\pcs.lnk,$(exec)\pcs.exe,$(exec)\pcs.map/map,$(lib)\lcm+$(lib)\lc
  cd $(src)

#
# Write debug info if specified
#

:  
  if not "$(debug)"=="" cd $(exec)
  if not "$(debug)"=="" $(tools)\mapsym $(exec)\pcs.map

#
# make-specific wrapup
#

:
  echo MAKE is done.
