:						=====> DO_SCOOPS.BAT
:
: command dir: \TOOLS,\PCS (assumed in path)
: source dir : \BUILD\SCOOPS (the current directory)
: output dir : \EXEC

CD \BUILD\SCOOPS
PATH = \TOOLS;\PCS;\

PCS COSCOOPS.SCM
copy class.so+methods.so+meth2.so+instance.so+inht.so temp1.so /v
copy temp1.so+interf.so+send.so+scsend.so+utl.so+expand.so+ldscoop.so scoops.so /v
make_fsl scoops.so scoops.fsl     /copyright
copy scoops.fsl \exec
rem
rem  Compile and build the SCOOPS tutorial
rem
pcs compile.dem
copy tutorial.so+frame.so+demstart.so scpsdemo.so
make_fsl scpsdemo.so tutorial.fsl /copyright
copy tutorial.fsl \exec
del *.so
del *.fsl
