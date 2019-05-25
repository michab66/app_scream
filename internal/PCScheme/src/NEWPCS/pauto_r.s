;      -*- Mode: Lisp -*-			      Filename:  runauto.s

;--------------------------------------------------------------------------;
;									   ;
;			  TI SCHEME -- PCS Compiler			   ;
;		   Copyright 1985 (c) Texas Instruments 		   ;
;									   ;
;			       Terry Caudill				   ;
;									   ;
;		   Autoload definitions for Runtime version		   ;
;									   ;
;--------------------------------------------------------------------------;

; Revision history:
;  6/02/87 tc - Removed from PSTL.S so that runtime version can more
;		easily be built.

;;;
;;; Set up the standard autoload files. COMPAUTO.S also has autoload
;;; definitions for compiler version. Both COMPAUTO.S and RUNAUTO.S
;;; should be included in COMPILER.APP.

(autoload-from-file (%system-file-name "PWINDOWS.FSL")  ; windows
  '(make-window window-clear window-delete
    window-get-position window-set-position!
    window-get-size window-set-size! window-get-cursor
    window-set-cursor! window-popup window-popup-delete
    window-get-attribute window-set-attribute!)
  user-global-environment)

(autoload-from-file (%system-file-name "PMATH.FSL")     ; real arithmetic
  '(acos asin atan cos exact? exp expt inexact?
    log pi sin sqrt tan)
  user-global-environment)

(autoload-from-file (%system-file-name "PP.FSL")        ; pretty printer
  '(pp %pretty-printer %pp-me)
  user-global-environment)

(autoload-from-file (%system-file-name "PDOS.FSL")      ; DOS facilities
  '(dos-dir dos-call sw-int dos-delete dos-file-copy
    dos-rename dos-file-size dos-chdir
    dos-change-drive)
  user-global-environment)

(autoload-from-file (%system-file-name "PSORT.FSL")     ; Sort package
  '(sort! %sort-less?)
  user-global-environment)

(autoload-from-file (%system-file-name "PNUM2S.FSL")    ; Number->String
  '(number->string integer->string string->number)
  user-global-environment)

(autoload-from-file (%system-file-name "PFUNARG.FSL")
  '(* + - / append append! char-ready? display list list* make-vector
    make-string max min newline prin1 princ print read-line read-atom read-char
    vector write write-char %xesc)
  user-global-environment)

(autoload-from-file (%system-file-name "PGR.FSL")
  '(clear-graphics draw-point clear-point is-point-on? position-pen
    draw-line-to set-pen-color! *graphics-colors*
    set-video-mode! get-video-mode set-palette!
    draw-box-to draw-filled-box-to
    get-pen-color get-pen-position point-color set-clipping-rectangle!
    ;; the following are experimental in PCS 3.0
    graphics-window current-graphics-window *character-boxes*)
  user-global-environment)

