;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;     Copyright Texas Instruments Inc 8/15/85
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                 ;;;
;;;                    Dynamic Wind                                 ;;;
;;;                                                                 ;;;
;;;              File Updated : May 23, 1985                        ;;;
;;;                                                                 ;;;
;;;      This file contains the code to implement dynamic           ;;;
;;;      wind. User interacts by using dynamic-wind and             ;;;
;;;      call/cc-dw.                                                ;;;
;;;                                                                 ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;

;;; macros for states

(macro make-new-state
  (lambda (e)
    (cons 'vector (cdr e))))

(macro %in-out-flag
  (lambda (e)
    (list 'vector-ref (cadr e) 0)))

(macro %before
  (lambda (e)
    (list 'vector-ref (cadr e) 1)))

(macro %after
  (lambda (e)
    (list 'vector-ref (cadr e) 2)))

(macro %next
  (lambda (e)
    (list 'vector-ref (cadr e) 3)))

(macro %set-next
  (lambda (e)
    (list 'vector-set! (cadr e) 3 (caddr e))))

(alias %in? %in-out-flag)

;;;
;;; State Space - routines
;;;

(define dynamic-wind '())
(define call/cc-dw '())

(letrec

  ((%state-space (vector #!TRUE nil nil nil))

   (extend-state-space
     (lambda (state)
       (%set-next %state-space state)
       (set! %state-space state)))

   (execute-at-new-state
     (lambda (state)
       (letrec 
         ((loop
            (lambda (previous current)
              (if (not (null? (%next current)))
                  (loop current (%next current)))
              (%set-next current previous)
              (if (%in? current)
                  ((%after current))
                  ((%before current)))))

          (reroot-state-space
            (lambda ()
              (loop state (%next state))
              (%set-next state nil)
              (set! %state-space state)))

          (recompute-new-state
            (lambda ()
              (if (not (%in? state))
                  ((%before state))))))

         (if (not (eq? state %state-space))
             (begin
               (reroot-state-space)
               (recompute-new-state)))))))
   


;;;

  (set! call/cc-dw
    (lambda (f)
      (call/cc
        (lambda (k)
          (let ((state %state-space))
            (let ((cob 
                    (lambda (v)
                      (execute-at-new-state state)
                      (k v))))
              (f cob)))))))


  (set! dynamic-wind
    (lambda (before body after)
      (let ((state %state-space))
        (extend-state-space
          (make-new-state #!TRUE before after nil))
        (before)
        (begin0
          (body)
          (execute-at-new-state state))))))

 (define catch call/cc-dw)

