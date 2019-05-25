

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                 ;;;
;;;                     S c o o p s                                 ;;;
;;;                                                                 ;;;
;;;      (c) Copyright 1985 Texas Instruments Incorporated          ;;;
;;;                  All Rights Reserved                            ;;;
;;;                                                                 ;;;
;;;               File updated : 5/16/85                            ;;;
;;;                                                                 ;;;
;;;                   File : scsend.scm                             ;;;
;;;                                                                 ;;;
;;;                 Amitabh Srivastava                              ;;;
;;;                                                                 ;;;
;;;    This file contains send routines coded in assembler          ;;;
;;;    for speed.                                                   ;;;
;;;                                                                 ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; send for various arguments

;;; 0 args

(%execute (quote (pcs-code-block 1 30
   (scoop-send-handler-0)
   (   1    4   0            ; load-constant      r1,c0
      60    4   7   0   2    ; close              r1,label, 2 args
      31    4   0            ; define!
       1    4   0            ; load-constant      r1,c0
      59                     ; exit
;label
       0   12   8            ; load               r3,r2
     225   12                ; %sge               r3,r3
      25   12                ; push               r3
      52    4   0            ; call-closure       r1, 0 args
      24    8                ; pop                r2
     225    8                ; %sge r2,r2
      59))))

;;; 1 args


(%execute (quote (pcs-code-block 1 30
   (scoop-send-handler-1)
   (   1    4   0            ; load-constant      r1,c0
      60    4   7   0   3    ; close              r1,label, 3 args
      31    4   0            ; define!
       1    4   0            ; load-constant      r1,c0
      59                     ; exit
;label
       0   16  12            ; load               r4,r3
     225   16                ; %sge               r4,r4
      25   16                ; push               r4
      52    8   1            ; call-closure       r2, 1 args
      24   12                ; pop                r3
     225   12                ; %sge r3,r3
      59))))


;;; 2 args


(%execute (quote (pcs-code-block 1 30
   (scoop-send-handler-2)
   (   1    4   0            ; load-constant      r1,c0
      60    4   7   0   4    ; close              r1,label, 4 args
      31    4   0            ; define!
       1    4   0            ; load-constant      r1,c0
      59                     ; exit
;label
       0   20  16            ; load               r5,r4
     225   20                ; %sge               r5,r5
      25   20                ; push               r5
      52   12   2            ; call-closure       r3, 2 args
      24   16                ; pop                r4
     225   16                ; %sge r4,r4
      59))))

;;; 3 args


(%execute (quote (pcs-code-block 1 30
   (scoop-send-handler-3)
   (   1    4   0            ; load-constant      r1,c0
      60    4   7   0   5    ; close              r1,label, 5 args
      31    4   0            ; define!
       1    4   0            ; load-constant      r1,c0
      59                     ; exit
;label
       0   24  20            ; load               r6,r5
     225   24                ; %sge               r6,r6
      25   24                ; push               r6
      52   16   3            ; call-closure       r4, 3 args
      24   20                ; pop                r5
     225   20                ; %sge r5,r5
      59))))

;;; 4 args


(%execute (quote (pcs-code-block 1 30
   (scoop-send-handler-4)
   (   1    4   0            ; load-constant      r1,c0
      60    4   7   0   6    ; close              r1,label, 6 args
      31    4   0            ; define!
       1    4   0            ; load-constant      r1,c0
      59                     ; exit
;label
       0   28  24            ; load               r7,r6
     225   28                ; %sge               r7,r7
      25   28                ; push               r7
      52   20   4            ; call-closure       r5, 4 args
      24   24                ; pop                r6
     225   24                ; %sge r6,r6
      59))))



;;; 5 args


(%execute (quote (pcs-code-block 1 30
   (scoop-send-handler-5)
   (   1    4   0            ; load-constant      r1,c0
      60    4   7   0   7    ; close              r1,label, 7 args
      31    4   0            ; define!
       1    4   0            ; load-constant      r1,c0
      59                     ; exit
;label
       0   32  28            ; load               r8,r7
     225   32                ; %sge               r8,r8
      25   32                ; push               r8
      52   24   5            ; call-closure       r6, 5 args
      24   28                ; pop                r7
     225   28                ; %sge r7,r7
      59))))

;;; 6 args


(%execute (quote (pcs-code-block 1 30
   (scoop-send-handler-6)
   (   1    4   0            ; load-constant      r1,c0
      60    4   7   0   8    ; close              r1,label, 8 args
      31    4   0            ; define!
       1    4   0            ; load-constant      r1,c0
      59                     ; exit
;label
       0   36  32            ; load               r9,r8
     225   36                ; %sge               r9,r9
      25   36                ; push               r9
      52   28   6            ; call-closure       r7, 6 args
      24   32                ; pop                r8
     225   32                ; %sge r8,r8
      59))))

;;; 7 args


(%execute (quote (pcs-code-block 1 30
   (scoop-send-handler-7)
   (   1    4   0            ; load-constant      r1,c0
      60    4   7   0   9    ; close              r1,label, 9 args
      31    4   0            ; define!
       1    4   0            ; load-constant      r1,c0
      59                     ; exit
;label
       0   40  36            ; load               r10,r9
     225   40                ; %sge               r10,r10
      25   40                ; push               r10
      52   32   7            ; call-closure       r8, 7 args
      24   36                ; pop                r9
     225   36                ; %sge r9,r9
      59))))


;;; 8 args


(%execute (quote (pcs-code-block 1 30
   (scoop-send-handler-8)
   (   1    4   0            ; load-constant      r1,c0
      60    4   7   0   10    ; close              r1,label, 10 args
      31    4   0            ; define!
       1    4   0            ; load-constant      r1,c0
      59                     ; exit
;label
       0   44  40            ; load               r11,r10
     225   44                ; %sge               r11,r11
      25   44                ; push               r11
      52   36   8            ; call-closure       r9, 8 args
      24   40                ; pop                r10
     225   40                ; %sge r10,r10
      59))))

;;; 9 args


(%execute (quote (pcs-code-block 1 30
   (scoop-send-handler-9)
   (   1    4   0            ; load-constant      r1,c0
      60    4   7   0   11    ; close              r1,label, 11 args
      31    4   0            ; define!
       1    4   0            ; load-constant      r1,c0
      59                     ; exit
;label
       0   48  40            ; load               r12,r11
     225   48                ; %sge               r12,r12
      25   48                ; push               r12
      52   40   9            ; call-closure       r10, 9 args
      24   44                ; pop                r11
     225   44                ; %sge r11,r11
      59))))

;;; 10 args


(%execute (quote (pcs-code-block 1 30
   (scoop-send-handler-10)
   (   1    4   0            ; load-constant      r1,c0
      60    4   7   0   12    ; close              r1,label, 12 args
      31    4   0            ; define!
       1    4   0            ; load-constant      r1,c0
      59                     ; exit
;label
       0   52  44            ; load               r13,r12
     225   52                ; %sge               r13,r13
      25   52                ; push               r13
      52   44  10            ; call-closure       r11, 10 args
      24   48                ; pop                r12
     225   48                ; %sge r12,r12
      59))))




