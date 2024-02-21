;
; Scream @ https://github.com/urschleim/scream
;
; Copyright © 2001-2024 Michael G. Binz
;

;
;
;
(define (get-class string)
  ((make-object java.lang.Class) (forName string)))

;
;
;
(define (make-interface . rest)
  (if (null? rest)
    (error "NOT_ENOUGH_ARGUMENTS" 1))
  (let*
    (
      (class-list (map get-class rest))
      (class-adapter (make-object de.michab.scream.binding.JavaClassAdapter))
      (proxy (class-adapter (createObject (list->vector class-list))))
    )
    (proxy (instanciateInterface))))

(define (find-method class-obj method-name . argument-class-list)
  (class-obj (getMethod method-name (list->vector argument-class-list))))
