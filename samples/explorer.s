;; $Id: explorer.s,v 1.1 2001/11/11 20:33:23 michab66 Exp $
;;
;; Scream / samples
;;
;; A type explorer.
;;
;; Released under Gnu Public License
;; Copyright (c) 2001 Michael G. Binz



;;
;; Returns the class object for the passed class name.
;;
(define (class-for-name name)
  (let
    (
      (classAdapter (make-object de.michab.scream.JavaClassAdapter))
      (class ((make-object java.lang.Class) (forName name)))
    )

    (classAdapter (createObject class))))



;;
;;
;;
(let
  (
    (splitpane-class (make-object javax.swing.JSplitPane))
    (borderLayout-class (make-object java.awt.BorderLayout))
  )
(let*
  (
    (toolbar (make-object (javax.swing.JToolBar)))
    (textfield (make-object (javax.swing.JTextField)))
    (listener (make-object (de.michab.scream.AwtListener)))
    (method-list (make-object (javax.swing.JList)))
    (attrib-list (make-object (javax.swing.JList)))
    (method-list-scroll (make-object (javax.swing.JScrollPane method-list)))
    (attrib-list-scroll (make-object (javax.swing.JScrollPane attrib-list)))
    (splitpane (make-object (javax.swing.JSplitPane)))
    (frame (make-object (javax.swing.JFrame "Scream type explorer")))
  )

  ((frame (getContentPane)) (add toolbar (borderLayout-class NORTH)))
  ((frame (getContentPane)) (add splitpane (borderLayout-class CENTER)))

  (toolbar (add textfield))

  (splitpane (setTopComponent method-list-scroll))
  (splitpane (setBottomComponent attrib-list-scroll))

  (textfield (addActionListener listener))

  (listener (setListener (listener actionPerformedC)
    (lambda (x)
      (let*
        (
          (name (textfield (getText)))
          (clazz (class-for-name name))
        )
        (method-list (setListData (clazz (getMethods))))
        (attrib-list (setListData (clazz (getFields))))

        (frame (pack))
        (frame (setVisible #t))))))

  (frame (pack))
  (frame (setVisible #t))
))


