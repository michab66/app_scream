;; $Id: editor.s,v 1.4 2002/05/24 20:42:11 michab66 Exp $
;;
;; Scream / samples
;;
;; Editor written in pidgin scheme.  Used to test the scheme->java interface
;; and to get some feeling how my new programming language wears.
;;
;;
;; Released under Gnu Public License
;; Copyright (c) 1998 Michael G. Binz

;;
;; Simple editor.
;;
(define (editor)

  (let*
    (
      ;; This applications frame window
      (frame (make-object (java.awt.Frame "Test Frame from Scream...")))

      ;; This applications file dialog.
      (filedlg (make-object (java.awt.FileDialog frame)))

      ;; This application's menu bar
      (menuBar (make-object (java.awt.MenuBar)))

      ;; The editor's text area
      (textArea (make-object (java.awt.TextArea)))

      ;; This applications command handler specification
      (commandHandlers ())

      ;; The current file name
      (currentFileName "")
    )

    ;; Event procedures
    (letrec
      (
        ;;
        ;;
        ;;
        (menuListener (make-object (de.michab.scream.binding.AwtListener)))

        ;;
        ;; Creates a java menu item and sets its title and label.
        ;;
        (createMenuItem
          (lambda (title label handler listener)
            (let
              (
                (menuItem (make-object (java.awt.MenuItem title)))
                (commandHandlerPair (list label handler))
              )
              (set! commandHandlers (append commandHandlers (list commandHandlerPair)))
              (display commandHandlers) (display #\newline)
              (menuItem (setActionCommand label))
              (menuItem (addActionListener listener))
              menuItem
            ) ;; let-end
          ) ;; lambda-end
        ) ;; createMenuItem-end

        ;;
        ;; The reader.
        ;;
        ;; Reads the given file into a string that is returned.
        ;;
        (reader
          (lambda (filename)
            (let*
              (
                (jreader (make-object (java.io.LineNumberReader
                                       (make-object (java.io.FileReader filename)))))
                (newline (string #\newline))
              )

              (do
                (
                  (readLine (jreader (readLine)) (jreader (readLine)))
                  (text #f)
                )
                (
                  (null? readLine) text
                )
                (set! text (if text
                               (string-append text newline readLine)
                               readLine))
              ) ; do-end
            ) ; let-end
          ) ; lambda-end
        ) ; reader-end


        ;;
        ;;
        ;;
        (writer
          (lambda (filename contents)
            (let*
              (
                (jwriter (make-object (java.io.BufferedWriter
                                      (make-object (java.io.FileWriter filename)))))
              )
              (jwriter (write contents 0 (string-length contents)))
              (jwriter (close))
            ) ; let-end
          ) ; lambda-end
        ) ; writer-end

        ;;
        ;;
        ;;
        (fileOpenHandler
          (lambda ()
            (display "fileOpenHandler") (display #\newline)
            (let
              (
                (somethingSelected
                  (begin
                    (filedlg (setTitle "Open..."))
;;                  (filedlg (setMode (access-class "java.awt.FileDialog" "LOAD")))
                    (filedlg (setMode ((make-object java.awt.FileDialog) LOAD)))
                    (filedlg (show))
                    (filedlg (getFile))))
              )
              (if (not (null? somethingSelected))
                (begin
                  (set! currentFileName (string-append (filedlg (getDirectory)) (filedlg (getFile))))
                  (frame (setTitle currentFileName))
                  (textArea (setText (reader currentFileName)))
                ) ; end-begin
              ) ; end-if
            ) ; end-let
          ) ; end-lambda
        ) ; end-fileOpenHandler


        ;;
        ;;
        ;;
        (fileSaveHandler
          (lambda ()
            (display "fileSaveHandler") (display #\newline)
            (writer currentFileName (textArea (getText)))
          ) ; lambda-end
        ) ; fileSaveHandler-end


        ;;
        ;;
        ;;
        (fileSaveAsHandler
          (lambda ()
            (display "fileSaveAsHandler") (display #\newline)
            (let
              (
                (somethingSelected
                  (begin
                    (filedlg (setTitle "Save as..."))
                    (filedlg (setMode ((make-object java.awt.FileDialog) SAVE)))
                    (filedlg (show))
                    (filedlg (getFile))))
              )
              (if (not (null? somethingSelected))
                (begin
                  (set! currentFileName (string-append (filedlg (getDirectory)) (filedlg (getFile))))
                  (frame (setTitle currentFileName))
                  (writer currentFileName (textArea (getText)))
                ) ; end-begin
              ) ; end-if
            ) ; end-let
          ) ; end-lambda
        ) ; end-fileOpenHandler



        ;;
        ;;
        ;;
        (fileCloseHandler
          (lambda ()
            (display "fileCloseHandler") (display #\newline)
            (frame (setVisible #f))
            (frame (dispose))
          ) ; end-lambda
        ) ; end-fileCloseHandler

      )

;;
;; Build the menu
;;
    ;;;
    ;;; Setup the menu listener
    ;;;
    (menuListener
      (setListener ((make-object de.michab.scream.binding.AwtListener) actionPerformedC)
                     (lambda (x)
                       (let*
                         (
                           (commandId (x (getActionCommand)))
                           (command (assoc commandId commandHandlers))
                         )
                         (display command) (display #\newline)
                         (display x) (display #\newline)
                         (if command
                           ((cadr command))
                           (begin
                             (display "Command not found: ")
                             (display commandId)
                             (display #\newline)
                           ) ; begin-end
                         ) ; if-end
                       ) ; let-end
                     ) ; lambda-end
      ) ; set-listener-end
    ) ; menu-listener-end


      (define fileMenu (make-object (java.awt.Menu "File")))
      (fileMenu
        (add (createMenuItem "Open..."
                             "FILE_OPEN"
                             fileOpenHandler
                             menuListener)
        )
      ) ; end-fileMenu

      (fileMenu
        (add (createMenuItem "Save"
                             "FILE_SAVE"
                             fileSaveHandler
                             menuListener)
        )
      ) ; end-fileMenu

      (fileMenu
        (add (createMenuItem "Save As..."
                             "FILE_SAVE_AS"
                             fileSaveAsHandler
                             menuListener)
        )
      ) ; end-fileMenu

      (fileMenu
        (add (createMenuItem "Exit"
                             "FILE_EXIT"
                             fileCloseHandler
                             menuListener)
        )
      )

      (menuBar (add fileMenu))
      (menuBar (add (make-object (java.awt.Menu "Edit"))))
      (menuBar (add (make-object (java.awt.Menu "Help"))))

      (frame (setSize (make-object (java.awt.Dimension 400 200))))
      (frame (setMenuBar menuBar))

      (frame (add textArea))


      ;;
      ;; Listener object for handling window events
      ;;
      (define windowListener (make-object (de.michab.scream.binding.AwtListener)))

      ;; Add the close handler procedure.
      (let ((closeHandler (lambda (x) (fileCloseHandler)))
            (openHandler  (lambda (x) (frame (toFront)))))
        (windowListener
          (setListener
            ((make-object de.michab.scream.binding.AwtListener) windowClosingC)
           closeHandler))
        (windowListener
          (setListener
            ((make-object de.michab.scream.binding.AwtListener) windowOpenedC)
           openHandler))
)
      ;;
      ;; Now connect the listener to the appropriate objects
      ;;
      ;; The frame window feeds the window listener interface
      (frame (addWindowListener windowListener))


      ;;
      ;; At last make our frame visible.
      ;;
      (frame (show))
    )
  )
)

(editor)