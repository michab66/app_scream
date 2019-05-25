;;;
;;;	Copyright (c) 1985 Massachusetts Institute of Technology
;;;
;;;	This material was developed by the Scheme project at the
;;;	Massachusetts Institute of Technology, Department of
;;;	Electrical Engineering and Computer Science.  Permission to
;;;	copy this software, to redistribute it, and to use it for any
;;;	purpose is granted, subject to the following restrictions and
;;;	understandings.
;;;
;;;	1. Any copy made of this software must include this copyright
;;;	notice in full.
;;;
;;;	2. Users of this software agree to make their best efforts (a)
;;;	to return to the MIT Scheme project any improvements or
;;;	extensions that they make, so that these may be included in
;;;	future releases; and (b) to inform MIT of noteworthy uses of
;;;	this software.
;;;
;;;	3.  All materials developed as a consequence of the use of
;;;	this software shall duly acknowledge such use, in accordance
;;;	with the usual standards of acknowledging credit in academic
;;;	research.
;;;
;;;	4. MIT has made no warrantee or representation that the
;;;	operation of this software will be error-free, and MIT is
;;;	under no obligation to provide any services, by way of
;;;	maintenance, update, or otherwise.
;;;
;;;	5.  In conjunction with products arising from the use of this
;;;	material, there shall be no use of the name of the
;;;	Massachusetts Institute of Technology nor of any adaptation
;;;	thereof in any advertising, promotional, or sales literature
;;;	without prior written consent from MIT in each case.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;	Modified by Texas Instruments Inc 8/15/85
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(autoload-from-file
	  (%system-file-name "edwin1.fsl")
	  '(%edwin-autoargument
	    command-argument-increment-multiplier-exponent!
	    update-argument-prompt!
	    command-argument-accumulate-digit!
	    set-command-argument-radix! command-argument-negate!
	    command-argument-radix
	    set-command-argument-multiplier-base!
	    enter-autoargument-mode! command-argument-multiplier-only?
	    command-argument-negative-only? command-argument-beginning?
	   )
	  edwin-environment
)

(autoload-from-file
	  (%system-file-name "edwin2.fsl")
	  '(
	   bufferset-select-buffer! bufferset-find-buffer
	   bufferset-create-buffer
	   bufferset-find-or-create-buffer
	   bufferset-kill-buffer!
	   bufferset-rename-buffer
	   )
	  edwin-environment
)

(autoload-from-file
	  (%system-file-name "edwin3.fsl")
	  '(
	    twiddle-characters
	    %edwin-transpose-characters
	   )
	  edwin-environment
)

(autoload-from-file
	  (%system-file-name "edwin4.fsl")
	  '(
	    append-next-kill-tag  delete-region kill-region
	    %kill-region un-kill-region
	   )
	  edwin-environment
)

(autoload-from-file
	  (%system-file-name "edwin5.fsl")
	  '(
	    copy-region un-kill-region-reversed %edwin-un-kill-pop
	   )
	  edwin-environment
)

(autoload-from-file
	  (%system-file-name "edwin6.fsl")
	  '(
	    lisp-indent-line lisp-indent-sexp
	    forward-sexp backward-sexp
	    forward-list backward-list forward-down-list
	    backward-down-list forward-up-list backward-up-list
	    backward-up-one-list
	   )
	  edwin-environment
)

(autoload-from-file
	  (%system-file-name "edwin7.fsl")
	  '(
	    incremental-search
	   )
	  edwin-environment
)

(autoload-from-file
	  (%system-file-name "edwin8.fsl")
	  '(
	    forward-word backward-word
	   )
	  edwin-environment
)

(autoload-from-file
	  (%system-file-name "edwin9.fsl")
	  '(
	    match-next-strings match-next-string
	    match-previous-strings match-previous-string
	    match-next-substring match-previous-substring
	    match-next-char match-previous-char
	    match-next-char-in-set match-previous-char-in-set
	   )
	  edwin-environment
)

(autoload-from-file
	  (%system-file-name "edwin10.fsl")
	  '(
	    forward-sentence backward-sentence
	    forward-paragraph backward-paragraph
	   )
	  edwin-environment
)

