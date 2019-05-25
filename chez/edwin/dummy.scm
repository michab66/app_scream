;;; Dummy Module to replace EDWIN0.FSL on Diskette 1 of a dual floppy
;;; installation.
;;;
;;; Note:  this module must be compiled using the pcs-compile-file
;;;	   procedure found in PBOOT.FSL as follows:
;;;
;;;		(load "pboot.fsl")	; if not already loaded
;;;		(pcs-compile-file "dummy.s" "dummy.so")

(begin
  (newline)
  (display "Replace PC Scheme diskette 1 with PC Scheme")
  (newline)
  (display "diskette 2 and re-enter (EDWIN) command . . .")
  (newline)
  (display #\space)
  (reset))
