:						=====> DO_EDWIN.BAT
:
: command dir: \TOOLS,\PCS (assumed in path)
: source dir : \BUILD\EDWIN (the current directory)
: output dir : \EXEC

CD \BUILD\EDWIN
PATH = \TOOLS;\PCS;\

rem
rem
rem  Build EDWIN (3 phases)
rem
rem  (1st phase)
rem
pcs doedwin1.scm
rem
make_fsl charset.so edwin11.fsl /copyright
rem
rem  (2nd phase)
rem
pcs doedwin2.scm
rem
rem
rem  (phase 2a)
rem
pcs doedwi2a.scm
rem
copy comfun.so+dwind.so+ldchset.so+strcomp.so+nstring.so temp1.so
copy struct.so+regops.so+comtabv.so+initmac.so+initkey.so temp2.so
copy buffer.so+bufset.so+ring.so+motion.so temp3.so
copy main.so+curr.so+redisp1.so+redisp2.so+insert80.so temp35.so
copy messages.so+modeln.so+argred.so+toplevel.so temp4.so
copy allcoms1.so+allcoms2.so+allcoms3.so temp5.so
copy marks.so+io.so+search1.so+things.so+parens.so+autoload.so+edinit.so temp6.so
copy temp1.so+temp2.so+temp3.so+temp35.so+temp4.so+temp5.so+temp6.so edwin0.so
del temp1.so
del temp2.so
del temp3.so
del temp35.so
del temp4.so
del temp5.so
del temp6.so
make_fsl edwin0.so edwin0.fsl    /copyright
make_fsl argredp.so edwin1.fsl   /copyright
make_fsl bufsetp.so edwin2.fsl   /copyright
make_fsl transpos.so edwin3.fsl  /copyright
make_fsl kill1.so edwin4.fsl     /copyright
make_fsl kill2.so edwin5.fsl     /copyright
make_fsl lisp.so edwin6.fsl      /copyright
make_fsl incser.so edwin7.fsl    /copyright
make_fsl words.so edwin8.fsl     /copyright
make_fsl search2.so edwin9.fsl   /copyright
make_fsl sentence.so edwin10.fsl /copyright
rem
rem  (3rd phase)
rem
pcs doedwin3.scm
rem
make_fsl ldall.so edwin.fsl /copyright
make_fsl dummy.so dummy.fsl /copyright
copy *.fsl \exec
del *.so
del *.fsl
