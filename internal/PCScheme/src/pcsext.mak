# PC Scheme 4.0 make file:  Extended memory PCS

#
# command-line macro variables
#

redo=                         # if nonempty, force make to do everything here
debug=			      # if nonempty, generate symbol file


#
# drives and directories
#

tools=\tools		    # read-only
lib=\lib		    # read-only
src=\build		    # read-only
obj=\objectx		    # read-write
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

# this records the current status of generated files onto a log file
:
  if not "$(redo)"=="" del $(obj)\*.obj
  if not "$(redo)"=="" del $(exec)\pcsext.*

#
# application-specific initialization
#

:
  cd $(src)
  path $(tools)

#
# assembly language files
#

$(obj)\intrup.obj:     $(src)\intrup.asm $(src)\dos.mac $(src)\pcmake.equ
  $(tools)\masm /DEXTMEM $*,$@;

$(obj)\sbigmath.obj:   $(src)\sbigmath.asm $(src)\scheme.equ
  $(tools)\masm /DEXTMEM $*,$@;

$(obj)\msdos.obj:      $(src)\msdos.asm
  $(tools)\masm /DEXTMEM $*,$@;

$(obj)\scannum.obj:    $(src)\scannum.asm
  $(tools)\masm /DEXTMEM $*,$@;

$(obj)\stimer.obj:     $(src)\stimer.asm $(src)\scheme.equ
  $(tools)\masm /DEXTMEM $*,$@;

$(obj)\get_path.obj:   $(src)\get_path.asm $(src)\scheme.equ
  $(tools)\masm /DEXTMEM $*,$@;

# $(obj)\graphcmd.obj:   $(src)\graphcmd.asm $(src)\pcmake.equ
#   $(tools)\masm /DEXTMEM $*,$@;

# PCS 3.02 version:  graphics integrated into VM
$(obj)\graphcmd.obj:   $(src)\graphics.asm $(src)\pcmake.equ
  $(tools)\masm /DEXTMEM $(src)\graphics,$@ /Dcombined /Dti /Dibm;

$(obj)\alink.obj:      $(src)\alink.asm
  $(tools)\masm /DEXTMEM $*,$@;

$(obj)\sbid.obj:       $(src)\sbid.asm
  $(tools)\masm /DEXTMEM $*,$@;

$(obj)\scroll.obj:     $(src)\scroll.asm $(src)\pcmake.equ
  $(tools)\masm /DEXTMEM $*,$@;

$(obj)\sw_int.obj:     $(src)\sw_int.asm
  $(tools)\masm /DEXTMEM $*,$@;

$(obj)\flo2hex.obj:    $(src)\flo2hex.asm
  $(tools)\masm /DEXTMEM $*,$@;

$(obj)\extsc.obj:         $(src)\sc.asm $(src)\dos.mac $(src)\smmu.mac
  $(tools)\masm /DEXTMEM $(src)\sc.asm,$(obj)\extsc.obj;

$(obj)\extinter.obj:    $(src)\sinterp.asm $(src)\schemed.equ $(src)\schemed.ref $(src)\schemed.mac \
                          $(src)\pcmake.equ $(src)\stackf.equ $(src)\smmu.mac \
                          $(src)\sinterp.mac $(src)\sinterp.arg
  $(tools)\masm /DEXTMEM $(src)\sinterp.asm,$(obj)\extinter.obj;

$(obj)\schemed.obj:    $(src)\schemed.asm $(src)\schemed.equ $(src)\sasm.mac
  $(tools)\masm /DEXTMEM $*,$@;

$(obj)\extsweep.obj:   $(src)\sgcsweep.asm $(src)\scheme.equ
  $(tools)\masm /DEXTMEM $(src)\sgcsweep.asm,$(obj)\extsweep.obj;

$(obj)\sgcmark.obj:    $(src)\sgcmark.asm $(src)\scheme.equ
  $(tools)\masm /DEXTMEM $*,$@;

$(obj)\extsquis.obj:     $(src)\squish.asm $(src)\scheme.equ
  $(tools)\masm /DEXTMEM $(src)\squish.asm,$(obj)\extsquis.obj;

$(obj)\extreloc.obj:   $(src)\srelocat.asm $(src)\scheme.equ
  $(tools)\masm /DEXTMEM $(src)\srelocat.asm,$(obj)\extreloc.obj;

$(obj)\border.obj:     $(src)\border.asm $(src)\scheme.equ $(src)\pcmake.equ
  $(tools)\masm /DEXTMEM $*,$@;

$(obj)\scar_cdr.obj:   $(src)\scar_cdr.asm $(src)\scheme.equ $(src)\sinterp.mac $(src)\sinterp.arg
  $(tools)\masm /DEXTMEM $*,$@;

$(obj)\senv.obj:       $(src)\senv.asm  $(src)\scheme.equ $(src)\sinterp.mac $(src)\sinterp.arg $(src)\stackf.equ
  $(tools)\masm /DEXTMEM $*,$@;

$(obj)\sexec.obj:      $(src)\sexec.asm $(src)\scheme.equ
  $(tools)\masm /DEXTMEM $*,$@;

$(obj)\sio.obj:        $(src)\sio.asm $(src)\scheme.equ
  $(tools)\masm /DEXTMEM $*,$@;

$(obj)\sobjhash.obj:   $(src)\sobjhash.asm $(src)\scheme.equ
  $(tools)\masm /DEXTMEM $*,$@;

$(obj)\srch_str.obj:   $(src)\srch_str.asm $(src)\scheme.equ $(src)\pcmake.equ $(src)\sinterp.arg
  $(tools)\masm /DEXTMEM $*,$@;

$(obj)\sstack.obj:     $(src)\sstack.asm $(src)\scheme.equ $(src)\sinterp.mac $(src)\sinterp.arg $(src)\stackf.equ
  $(tools)\masm /DEXTMEM $*,$@;

$(obj)\extsmmu.obj:    $(src)\extsmmu.asm $(src)\schemed.equ $(src)\schemed.ref  $(src)\schemed.mac
  $(tools)\masm /DEXTMEM $*,$@;

$(obj)\block.obj:      $(src)\block.asm $(src)\scheme.equ
  $(tools)\masm /DEXTMEM $*,$@;

$(obj)\sstring.obj:    $(src)\sstring.asm $(src)\scheme.equ $(src)\sinterp.mac $(src)\sinterp.arg
  $(tools)\masm /DEXTMEM $*,$@;

$(obj)\strmlnrs.obj:   $(src)\strmlnrs.asm $(src)\scheme.equ
  $(tools)\masm /DEXTMEM $*,$@;

$(obj)\sutil.obj:      $(src)\sutil.asm $(src)\scheme.equ $(src)\pcmake.equ
  $(tools)\masm /DEXTMEM $*,$@;

$(obj)\svars.obj:      $(src)\svars.asm $(src)\scheme.equ $(src)\sinterp.mac $(src)\sinterp.arg
  $(tools)\masm /DEXTMEM $*,$@;

$(obj)\extzio.obj:        $(src)\zio.asm $(src)\scheme.equ $(src)\pcmake.equ
  $(tools)\masm /DEXTMEM $(src)\zio.asm,$(obj)\extzio.obj;

$(obj)\cwindow.obj:    $(src)\cwindow.asm $(src)\scheme.equ $(src)\sinterp.arg
  $(tools)\masm /DEXTMEM $*,$@;

$(obj)\cprint.obj:     $(src)\cprint.asm $(src)\scheme.equ $(src)\sinterp.arg
  $(tools)\masm /DEXTMEM $*,$@;

$(obj)\cread.obj:      $(src)\cread.asm $(src)\scheme.equ $(src)\sinterp.arg
  $(tools)\masm /DEXTMEM $*,$@;

$(obj)\cio.obj:        $(src)\cio.asm $(src)\scheme.equ $(src)\sinterp.arg
  $(tools)\masm /DEXTMEM $*,$@;

$(obj)\cprint1.obj:    $(src)\cprint1.asm $(src)\scheme.equ
  $(tools)\masm /DEXTMEM $*,$@;

$(obj)\saprop.obj:     $(src)\saprop.asm $(src)\scheme.equ
  $(tools)\masm /DEXTMEM $*,$@;

$(obj)\xli.obj:        $(src)\xli.asm $(src)\scheme.equ
  $(tools)\masm /DEXTMEM $*,$@;

#
# C source files
#

$(obj)\extmain.obj:      $(src)\smain.c $(src)\version.h $(src)\scheme.h $(src)\sport.h $(src)\pcmake.h
# $(tools)\lc -ms -ccdswum -o$@ $*
  $(tools)\lc1 -dEXTMEM -ms -ccdswum -o$(obj)\$*.q smain.c
  $(tools)\lc2 -v  -o$@ $(obj)\$*.q

$(obj)\freesp.obj:     $(src)\freesp.c $(src)\scheme.h $(src)\ctype.h
# $(tools)\lc -ms -ccdswum -o$@ $*
  $(tools)\lc1 -dEXTMEM -ms -ccdswum -o$(obj)\$*.q $*
  $(tools)\lc2 -v  -o$@ $(obj)\$*.q

$(obj)\asm_link.obj:   $(src)\asm_link.c $(src)\scheme.h
# $(tools)\lc -ms -ccdswum -o$@ $*
  $(tools)\lc1 -dEXTMEM -ms -ccdswum -o$(obj)\$*.q $*
  $(tools)\lc2 -v  -o$@ $(obj)\$*.q

$(obj)\get_port.obj:   $(src)\get_port.c $(src)\scheme.h $(src)\ctype.h
# $(tools)\lc -ms -ccdswum -o$@ $*
  $(tools)\lc1 -dEXTMEM -ms -ccdswum -o$(obj)\$*.q $*
  $(tools)\lc2 -v  -o$@ $(obj)\$*.q

$(obj)\sarith.obj:     $(src)\sarith.c $(src)\scheme.h $(src)\slist.h
# $(tools)\lc -ms -ccdswum -o$@ $*
  $(tools)\lc1 -dEXTMEM -ms -ccdswum -o$(obj)\$*.q $*
  $(tools)\lc2 -v  -o$@ $(obj)\$*.q

$(obj)\sbigmxt.obj:    $(src)\sbigmxt.c $(src)\scheme.h
# $(tools)\lc -ms -ccdswum -o$@ $*
  $(tools)\lc1 -dEXTMEM -ms -ccdswum -o$(obj)\$*.q $*
  $(tools)\lc2 -v  -o$@ $(obj)\$*.q

$(obj)\serrmsg.obj:    $(src)\serrmsg.c $(src)\scheme.h
# $(tools)\lc -ms -ccdswum -o$@ $*
  $(tools)\lc1 -dEXTMEM -ms -ccdswum -o$(obj)\$*.q $*
  $(tools)\lc2 -v  -o$@ $(obj)\$*.q

$(obj)\sdebug.obj:     $(src)\sdebug.c $(src)\scheme.h $(src)\ctype.h
# $(tools)\lc -ms -ccdswum -o$@ $*
  $(tools)\lc1 -dEXTMEM -ms -ccdswum -o$(obj)\$*.q $*
  $(tools)\lc2 -v  -o$@ $(obj)\$*.q

$(obj)\sdump.obj:      $(src)\sdump.c $(src)\scheme.h $(src)\ctype.h $(src)\schars.h $(src)\slist.h
# $(tools)\lc -ms -ccdswum -o$@ $*
  $(tools)\lc1 -dEXTMEM -ms -ccdswum -o$(obj)\$*.q $*
  $(tools)\lc2 -v  -o$@ $(obj)\$*.q

$(obj)\serror.obj:     $(src)\serror.c $(src)\scheme.h $(src)\ctype.h $(src)\slist.h
# $(tools)\lc -ms -ccdswum -o$@ $*
  $(tools)\lc1 -dEXTMEM -ms -ccdswum -o$(obj)\$*.q $*
  $(tools)\lc2 -v  -o$@ $(obj)\$*.q

$(obj)\sfasl.obj:      $(src)\sfasl.c $(src)\scheme.h $(src)\stdio.h $(src)\slist.h
# $(tools)\lc -ms -ccdswum -o$@ $*
  $(tools)\lc1 -dEXTMEM -ms -ccdswum -o$(obj)\$*.q $*
  $(tools)\lc2 -v  -o$@ $(obj)\$*.q

$(obj)\shash.obj:      $(src)\shash.c $(src)\scheme.h $(src)\ctype.h
# $(tools)\lc -ms -ccdswum -o$@ $*
  $(tools)\lc1 -dEXTMEM -ms -ccdswum -o$(obj)\$*.q $*
  $(tools)\lc2 -v  -o$@ $(obj)\$*.q

$(obj)\sin_out.obj:    $(src)\sin_out.c $(src)\scheme.h $(src)\sport.h $(src)\slist.h
# $(tools)\lc -ms -ccdswum -o$@ $*
  $(tools)\lc1 -dEXTMEM -ms -ccdswum -o$(obj)\$*.q $*
  $(tools)\lc2 -v  -o$@ $(obj)\$*.q

$(obj)\slink.obj:      $(src)\slink.c $(src)\ctype.h $(src)\slink.h
# $(tools)\lc -ms -ccdswum -o$@ $*
  $(tools)\lc1 -dEXTMEM -ms -ccdswum -o$(obj)\$*.q $*
  $(tools)\lc2 -v  -o$@ $(obj)\$*.q

$(obj)\smemory.obj:    $(src)\smemory.c $(src)\scheme.h
# $(tools)\lc -ms -ccdswum -o$@ $*
  $(tools)\lc1 -dEXTMEM -ms -ccdswum -o$(obj)\$*.q $(src)\smemory.c
  $(tools)\lc2 -v  -o$@ $(obj)\$*.q

$(obj)\sprint.obj:     $(src)\sprint.c $(src)\scheme.h
# $(tools)\lc -ms -ccdswum -o$@ $*
  $(tools)\lc1 -dEXTMEM -ms -ccdswum -o$(obj)\$*.q $*
  $(tools)\lc2 -v  -o$@ $(obj)\$*.q

$(obj)\sprintf.obj:    $(src)\sprintf.c $(src)\scheme.h
# $(tools)\lc -ms -ccdswum -o$@ $*
  $(tools)\lc1 -dEXTMEM -ms -ccdswum -o$(obj)\$*.q $*
  $(tools)\lc2 -v  -o$@ $(obj)\$*.q

$(obj)\sprop.obj:      $(src)\sprop.c $(src)\scheme.h $(src)\ctype.h $(src)\slist.h
# $(tools)\lc -ms -ccdswum -o$@ $*
  $(tools)\lc1 -dEXTMEM -ms -ccdswum -o$(obj)\$*.q $*
  $(tools)\lc2 -v  -o$@ $(obj)\$*.q

$(obj)\sread.obj:      $(src)\sread.c $(src)\scheme.h $(src)\schars.h
# $(tools)\lc -ms -ccdswum -o$@ $*
  $(tools)\lc1 -dEXTMEM -ms -ccdswum -o$(obj)\$*.q $*
  $(tools)\lc2 -v  -o$@ $(obj)\$*.q

$(obj)\sreify.obj:     $(src)\sreify.c $(src)\scheme.h
# $(tools)\lc -ms -ccdswum -o$@ $*
  $(tools)\lc1 -dEXTMEM -ms -ccdswum -o$(obj)\$*.q $*
  $(tools)\lc2 -v  -o$@ $(obj)\$*.q

$(obj)\sreset.obj:     $(src)\sreset.c $(src)\scheme.h $(src)\ctype.h
# $(tools)\lc -ms -ccdswum -o$@ $*
  $(tools)\lc1 -dEXTMEM -ms -ccdswum -o$(obj)\$*.q $*
  $(tools)\lc2 -v  -o$@ $(obj)\$*.q

$(obj)\strace.obj:     $(src)\strace.c $(src)\scheme.h
# $(tools)\lc -ms -ccdswum -o$@ $*
  $(tools)\lc1 -dEXTMEM -ms -ccdswum -o$(obj)\$*.q $*
  $(tools)\lc2 -v  -o$@ $(obj)\$*.q

$(obj)\support.obj:    $(src)\support.c $(src)\scheme.h $(src)\slist.h
# $(tools)\lc -ms -ccdswum -o$@ $*
  $(tools)\lc1 -dEXTMEM -ms -ccdswum -o$(obj)\$*.q $*
  $(tools)\lc2 -v  -o$@ $(obj)\$*.q

$(obj)\zcio.obj:       $(src)\zcio.c $(src)\scheme.h $(src)\sport.h
# $(tools)\lc -ms -ccdswum -o$@ $*
  $(tools)\lc1 -dEXTMEM -ms -ccdswum -o$(obj)\$*.q $*
  $(tools)\lc2 -v  -o$@ $(obj)\$*.q

#
# Generate .EXE file
#

$(exec)\pcsext.exe:    $(obj)\extsc.obj \
                $(obj)\schemed.obj \
                $(obj)\extinter.obj \
                $(obj)\extmain.obj \
                $(obj)\intrup.obj \
                $(obj)\sstring.obj \
                $(obj)\smemory.obj \
                $(obj)\sgcmark.obj \
                $(obj)\extsweep.obj \
                $(obj)\sbigmxt.obj \
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
                $(obj)\extzio.obj \
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
                $(obj)\slink.obj \
                $(obj)\alink.obj \
                $(obj)\sbid.obj \
                $(obj)\strmlnrs.obj \
                $(obj)\srch_str.obj \
                $(obj)\extsquis.obj \
                $(obj)\extreloc.obj \
                $(obj)\freesp.obj \
                $(obj)\scroll.obj \
                $(obj)\sw_int.obj \
                $(obj)\flo2hex.obj \
                $(obj)\extsmmu.obj \
		$(obj)\block.obj \
                $(obj)\xli.obj
  cd $(obj)
  $(tools)\link @$(src)\pcsext.lnk,$(exec)\pcsext.exe,$(exec)\pcsext.map/map,$(lib)\lcm+$(lib)\lc
  cd $(src)

#
# Write debug info if specified
#

:  
  if not "$(debug)"=="" cd $(exec)
  if not "$(debug)"=="" $(tools)\mapsym $(exec)\pcsext.map

#
# make-specific wrapup
#

:
  echo MAKE is done.
