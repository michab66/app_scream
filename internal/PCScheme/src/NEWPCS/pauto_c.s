;      -*- Mode: Lisp -*-			      Filename:  compauto.s

;--------------------------------------------------------------------------;
;									   ;
;			  TI SCHEME -- PCS Compiler			   ;
;		   Copyright 1985 (c) Texas Instruments 		   ;
;									   ;
;			       Terry Caudill				   ;
;									   ;
;		   Autoload definitions for COMPILER.APP		   ;
;									   ;
;--------------------------------------------------------------------------;

; Revision history:
;  6/02/87 tc - Removed from PSTL.S so that compiler and runtime versions  
;		can more easily be built.

;;;
;;; Set up the standard autoload files. RUNAUTO.S also has autoload
;;; definitions for runtime version. Both COMPAUTO.S and RUNAUTO.S
;;; should be included in COMPILER.APP.

(autoload-from-file (%system-file-name "SCOOPS.FSL")    ; SCOOPS
	'(load-scoops)
	user-global-environment)

(autoload-from-file (%system-file-name "PINSPECT.FSL")  ; INSPECTOR
	'(%inspect %inspector)
	user-global-environment)

(autoload-from-file (%system-file-name "PDEFSTR.FSL")   ; DEFINE-STRUCTURE
	'(%define-structure %make-structure %structure-predicate)
	user-global-environment)

(autoload-from-file (%system-file-name "EDIT.FSL")      ; STRUCTURE EDITOR
       '(edit)
       user-global-environment)

(autoload-from-file (%system-file-name "PADVISE.FSL")	; PADVISE
       '(advise-entry advise-exit break break-both break-entry break-exit
	 trace trace-both trace-entry trace-exit unadvise unadvise-entry
	 unadvise-exit unbreak unbreak-entry unbreak-exit untrace untrace-entry
	 untrace-exit *args* *proc* *result*)
       user-global-environment)

