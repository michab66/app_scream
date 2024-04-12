; $Id: shell.s 8 2008-09-14 14:23:20Z binzm $
;
; Scream / schemeRuntime
;
; Released under Gnu Public License
; Copyright (c) 2001 Michael G. Binz


;;
;; A factory for console windows.  Delivers console windows that are ready to
;; use out of the box as a side effect.  The return value is undefined.
;;
(define makeConsole
  ; This defines the outer closure that holds the symbol bindings that are
  ; common for all of the created console windows.
  (let (
        ; The number of open console windows.  If this drops to zero, the
        ; application terminates.
        (consoleCount 0)
        ; The single common event listener.
        (masterListener (make-object (de.michab.scream.binding.AwtListener))))

    ; Create and register a window listener responsible for shutting down the
    ; system when the number of open console windows reaches zero.
    (masterListener (setListener (masterListener windowClosingC)
      (lambda (event)
        (set! consoleCount (- consoleCount 1))
        (if (< consoleCount 1)
          ((make-object java.lang.System) (exit 0))))))

    ; Create and register an action listener that is responsible for creating
    ; new console windows.
    (masterListener (setListener (masterListener actionPerformedC)
      (lambda (event)
        (makeConsole))))

    ; We don't want to see debug logs.
    (masterListener (setVerbose #f))

    ; Create the actual factory procedure based on the outer closure.
    (lambda ()
      ; This is an inner closure that exists for each console window.
      (let*
        (
          ; Create a frame window.
          (frame (make-object (javax.swing.JFrame "Scream console")))
          ; This application's console.
          (console (make-object (de.michab.scream.ScreamConsole)))
          ; Set up a reference to the interpreter instance.
          (interpreterInstance
            (%%interpreter%%
              (createSchemeInterpreter (console (getReader))
                                       (console (getWriter)))))
          ; Button
          (button (make-object (javax.swing.JButton "New console")))
          ; Make a listener responsible for cleanup.
          (slaveListener (make-object (de.michab.scream.binding.AwtListener)))

          ;; Shortcuts to the layout constraints.
          (NORTH ((make-object java.awt.BorderLayout) NORTH))
          (CENTER ((make-object java.awt.BorderLayout) CENTER))
        )

        ; Only if we are on jdk 1.3 or better activate the command line
        ; completion.  See definition of (connect-completion).
        (if (string>=? %jdk-version "1.3")
          (connect-completion console interpreterInstance))

        ;; Add a new exit command to the interpreter.
        (let (
          (close-procedure (lambda () (close-frame frame)))
          (environment (interpreterInstance (getTopLevelEnvironment)))
          )

          ((object environment) (set 'exit close-procedure)))

        ;; Set up the slave listener responsible for cleanup.
        (slaveListener (setListener (slaveListener windowClosingC)
          (lambda (event)
;;            (interpreterInstance (dispose))
            ;; The listener removes are important.  The listeners hold backward
            ;; references to the interpreter and user interface elements in
            ;; their environments, so only if we break the circle gc will
            ;; remove all the freed objects.
            (frame (removeWindowListener slaveListener))
            (frame (removeWindowListener masterListener))
            (frame (dispose)))))
        (slaveListener (setVerbose #f))

        ;; ... and add that also to the frame.
        (frame (addWindowListener slaveListener))
        ;; Connect the master listener to the frame.
        (frame (addWindowListener masterListener))


        ;; Count this frame.
        (set! consoleCount (+ consoleCount 1))

        ;; Add the factory procedure to the button.
        (button (addActionListener masterListener))

        ;; Set up all the gui elements.
        ((frame (getContentPane)) (add console CENTER))
        ((frame (getContentPane)) (add button NORTH))
        (frame (pack))
        (frame (setSize 400 200))
        (frame (setVisible #t))
        (console (requestFocus))))))



;;
;; This procedure closes the passed frame as if the user had pressed the close
;; gadget.  This is even in Java black art.  The universal algorithm is to
;  create a window closing event for the specific frame to close and to post
;; that on the system event queue.  This will trigger the machinery driving
;; the window listeners.
;; It is not possible to pass the event to the frame's dispatchEvent() method.
;; This results in an exception saying 'Disposal interrupted'.  Reason seems to
;; be that this is delivered synchronously but on the wrong thread (but the
;; probability for that being true is only about 75%).
;;
(define (close-frame frame)
  (let*
    (
    ; Build a close event.
    (close-id ((make-object java.awt.event.WindowEvent) WINDOW_CLOSING))
    (close-event (make-object (java.awt.event.WindowEvent frame close-id)))

    ; Get a reference to the system event queue (Don't try this at home!)
    (toolkit ((make-object java.awt.Toolkit) (getDefaultToolkit)))
    (event-queue (toolkit (getSystemEventQueue)))
    )
    (event-queue (postEvent close-event))))



;;
;; Add a completer to the JConsole.  The actual completer is implemented in
;; Java and forwards the call to SchemeInterpreter.complete().
;;
(define (connect-completion console interpreter)
  (let* ((name-completion-itf (make-interface "de.michab.scream.ScreamConsole$NameCompletion"))
        (completion-method (find-method (name-completion-itf (getClass)) "completeName" (make-object java.lang.String)))
        (completion-handler (lambda (string) (interpreter (complete string)))))
    (name-completion-itf (defineOperation completion-method completion-handler))
    (console (setNameCompletion name-completion-itf))))



;;
;; Triggers opening of the initial console window.
;;
(makeConsole)
