;;; -*- mode: lisp; indent-tabs: nil -*-
;;; (require :info.read-eval-print.climacs.ext)

(in-package :info.read-eval-print.climacs.ext)

(defun test ()
  (format t "Hello World from new project info.read-eval-print.climacs.ext~%"))


(in-package :clim-internals)

(setq *trace-complete-input* t)

(defun ext%normalize-so-far (so-far)
  (reduce (lambda (acc x)
            (ppcre:regex-replace (car x) acc (cadr x)))
          `((".*//"  "/") (".*/~/" ,(namestring (user-homedir-pathname))))
          :initial-value so-far))
(assert (string= (ext%normalize-so-far "/tmp//a") "/a"))
(assert (string= (ext%normalize-so-far "/tmp/~/a") (q:str (namestring (user-homedir-pathname)) "a")))


(defun filename-completer (so-far mode)
  (let* ((so-far (ext%normalize-so-far so-far))
         (directory-prefix
          (if (eq :absolute (car (pathname-directory (pathname so-far))))
              ""
              (namestring #+sbcl *default-pathname-defaults*
                          #+cmu (ext:default-directory)
                          #-(or sbcl cmu) *default-pathname-defaults*)))
         (full-so-far (concatenate 'string directory-prefix so-far))
         (pathnames
          (loop with length = (length full-so-far)
                and wildcard = (format nil "~A*.*"
                                       (loop for start = 0 ; Replace * -> \*
                                             for occurence = (position #\* so-far :start start)
                                             until (= start (length so-far))
                                             until (null occurence)
                                             do (replace so-far "\\*" :start1 occurence)
                                                (setf start (+ occurence 2))
                                             finally (return so-far)))
                for path in
                #+(or sbcl cmu lispworks) (directory wildcard)
                #+openmcl (directory wildcard :directories t)
                #+allegro (directory wildcard :directories-are-files nil)
                #+cormanlisp (nconc (directory wildcard)
                                    (cl::directory-subdirs dirname))
                #-(or sbcl cmu lispworks openmcl allegro cormanlisp)
                (directory wildcard)
                when (let ((mismatch (mismatch (namestring path) full-so-far)))
                       (or (null mismatch) (= mismatch length)))
                  collect path))
         (strings (mapcar #'namestring pathnames))
         (first-string (car strings))
         (length-common-prefix nil)
         (completed-string nil)
         (full-completed-string nil)
         (input-is-directory-p (when (plusp (length so-far))
                                 (char= (aref so-far (1- (length so-far))) #\/))))
    (unless (null pathnames)
      (setf length-common-prefix
            (loop with length = (length first-string)
                  for string in (cdr strings)
                  do (setf length (min length (or (mismatch string first-string) length)))
                  finally (return length))))
    (unless (null pathnames)
      (setf completed-string
            (subseq first-string (length directory-prefix)
                    (if (null (cdr pathnames)) nil length-common-prefix)))
      (setf full-completed-string
            (concatenate 'string directory-prefix completed-string)))
    (case mode
      ((:complete-limited :complete-maximal)
       (cond ((null pathnames)
              (values so-far nil nil 0 nil))
             ((null (cdr pathnames))
              (values completed-string (plusp (length so-far)) (car pathnames) 1 nil))
             (input-is-directory-p
              (values completed-string t (parse-namestring so-far) (length pathnames) nil))
             (t
              (values completed-string nil nil (length pathnames) nil))))
      (:complete
       ;; This is reached when input is activated, if we did
       ;; completion, that would mean that an input of "foo" would
       ;; be expanded to "foobar" if "foobar" exists, even if the
       ;; user actually *wants* the "foo" pathname (to create the
       ;; file, for example).
       (values so-far t so-far 1 nil))
      (:possibilities
       (values nil nil nil (length pathnames)
               (loop with length = (length directory-prefix)
                     for name in pathnames
                     collect (list (subseq (namestring name) length nil)
                                   name)))))))


#|
C-i でファイル名補完の件。

stream-input.lisp

(defun event-matches-gesture-name-p (event gesture-name)
  ;; Just to be nice, we special-case literal characters here.
  ;; We also special-case literal 'physical' gesture specs of
  ;; the form (type device-name modifier-state).
  ;; The CLIM spec requires neither of these things.
  (let ((gesture-entry
         (typecase gesture-name
           (character (list (multiple-value-list (realize-gesture-spec :keyboard gesture-name))))
           (cons (list gesture-name)) ;; Literal physical gesture
           (t (gethash gesture-name *gesture-names*)))))    
    (loop for (type device-name modifier-state) in gesture-entry
	  do (when (%event-matches-gesture event
					   type
					   device-name
					   modifier-state)
	       (return-from event-matches-gesture-name-p t))
	  finally (return nil))))



input-edting.lisp
これが 1 回目の C-i を返してくれない。
(defun read-completion-gesture (stream
				partial-completers
				help-displays-possibilities)
  (flet ((possibilitiesp (gesture)
	   (or (gesture-match gesture *possibilities-gestures*)
	       (and help-displays-possibilities
		    (gesture-match gesture *help-gestures*)))))
    (let ((*completion-possibilities-continuation*
	   #'(lambda ()
	       (return-from read-completion-gesture
		 (values nil :possibilities)))))
      (handler-bind ((accelerator-gesture
		      #'(lambda (c)
			  (let ((gesture (accelerator-gesture-event c)))
			    (when (possibilitiesp gesture)
				(return-from read-completion-gesture
				  (values nil :possibilities)))))))
	(let ((gesture (read-gesture :stream stream)))
	  (values gesture
		  (cond ((possibilitiesp gesture)
			 :possibilities)
			((gesture-match gesture partial-completers)
			 :complete-limited)
			((gesture-match gesture *completion-gestures*)
			 :complete-maximal)
			((complete-gesture-p gesture)
			 :complete)
			(t nil))))))))

read-gesture で 1 回目の C-i が返って来ないみたい。


  0: (CLIM:STREAM-READ-GESTURE #<CLIM:STANDARD-INPUT-EDITING-STREAM {10036221B1}> :TIMEOUT NIL :PEEK-P NIL :INPUT-WAIT-TEST #<FUNCTION CLIM-INTERNALS::INPUT-CONTEXT-WAIT-TEST> :INPUT-WAIT-HANDLER #<FUNCTION CLIM-INTERNALS::INPUT-CONTEXT-EVENT-HANDLER> :POINTER-BUTTON-PRESS-HANDLER #<FUNCTION CLIM-INTERNALS::INPUT-CONTEXT-BUTTON-PRESS-HANDLER>)
    1: (CLIM:STREAM-READ-GESTURE #<CLIMACS-GUI::CLIMACS-MINIBUFFER-PANE ESA:MINIBUFFER {1004E0FC81}> :TIMEOUT NIL :INPUT-WAIT-TEST #<FUNCTION CLIM-INTERNALS::INPUT-CONTEXT-WAIT-TEST> :INPUT-WAIT-HANDLER #<FUNCTION CLIM-INTERNALS::INPUT-CONTEXT-EVENT-HANDLER> :POINTER-BUTTON-PRESS-HANDLER #<FUNCTION CLIM-INTERNALS::INPUT-CONTEXT-BUTTON-PRESS-HANDLER>)
; No value
CL-USER> 
; No value
CL-USER> 
; No value
CL-USER> 
; No value
CL-USER> 
; No value
CL-USER> 
; No value
CL-USER> 

:AROUND #<KEY-PRESS-EVENT {1005A7F051}> is an instance of type KEY-PRESS-EVENT
   it has the following slots:
           TIMESTAMP: 7078973
               SHEET: #<CLIMACS-GUI::CLIMACS-MINIBUFFER-PANE ESA:MINIBUFFER {1004E0FC81}>
      MODIFIER-STATE: 512
                   X: 150
                   Y: 90
             GRAFT-X: 1115
             GRAFT-Y: 694
            KEY-NAME: :CONTROL-LEFT
       KEY-CHARACTER: NIL
NIL is of type NULL
   it has a value of NIL
   it is in the #<PACKAGE "COMMON-LISP"> package
   it has a property list of NIL
    1: CLIM:STREAM-READ-GESTURE returned #<CLIM:KEY-PRESS-EVENT {1005A7F051}>
    1: (ESA:PROPER-GESTURE-P #<CLIM:KEY-PRESS-EVENT {1005A7F051}>)
    1: ESA:PROPER-GESTURE-P returned NIL
    1: (ESA:PROCESS-GESTURES-OR-COMMAND #<DREI:DREI-AREA TEXTUAL-DREI-SYNTAX-VIEW {1003623641}>)
      2: (ESA::ESA-READ-GESTURE :COMMAND-PROCESSOR #<DREI:DREI-AREA TEXTUAL-DREI-SYNTAX-VIEW {1003623641}>)
        3: (CLIM:STREAM-READ-GESTURE #<CLIMACS-GUI::CLIMACS-MINIBUFFER-PANE ESA:MINIBUFFER {1004E0FC81}> :TIMEOUT NIL :PEEK-P NIL :INPUT-WAIT-TEST #<FUNCTION CLIM-INTERNALS::INPUT-CONTEXT-WAIT-TEST> :INPUT-WAIT-HANDLER #<FUNCTION CLIM-INTERNALS::INPUT-CONTEXT-EVENT-HANDLER> :POINTER-BUTTON-PRESS-HANDLER #<FUNCTION CLIM-INTERNALS::INPUT-CONTEXT-BUTTON-PRESS-HANDLER>)

:AROUND #<KEY-PRESS-EVENT {1005A7FE51}> is an instance of type KEY-PRESS-EVENT
   it has the following slots:
           TIMESTAMP: 7081433
               SHEET: #<CLIMACS-GUI::CLIMACS-MINIBUFFER-PANE ESA:MINIBUFFER {1004E0FC81}>
      MODIFIER-STATE: 512
                   X: 150
                   Y: 90
             GRAFT-X: 1115
             GRAFT-Y: 694
            KEY-NAME: :|i|
       KEY-CHARACTER: #\i
NIL is of type NULL
   it has a value of NIL
   it is in the #<PACKAGE "COMMON-LISP"> package
   it has a property list of NIL
        3: CLIM:STREAM-READ-GESTURE returned
             #<CLIM:KEY-PRESS-EVENT {1005A7FE51}>
        3: (ESA:PROPER-GESTURE-P #<CLIM:KEY-PRESS-EVENT {1005A7FE51}>)
        3: ESA:PROPER-GESTURE-P returned #\i
      2: ESA::ESA-READ-GESTURE returned #<CLIM:KEY-PRESS-EVENT {1005A7FE51}>
      2: (ESA:PROCESS-GESTURE #<DREI:DREI-AREA TEXTUAL-DREI-SYNTAX-VIEW {1003623641}> #<CLIM:KEY-PRESS-EVENT {1005A7FE51}>)
      2: ESA:PROCESS-GESTURE returned NIL
    1: ESA:PROCESS-GESTURES-OR-COMMAND returned NIL
    1: (CLIM:STREAM-READ-GESTURE #<CLIMACS-GUI::CLIMACS-MINIBUFFER-PANE ESA:MINIBUFFER {1004E0FC81}> :TIMEOUT NIL :INPUT-WAIT-TEST #<FUNCTION CLIM-INTERNALS::INPUT-CONTEXT-WAIT-TEST> :INPUT-WAIT-HANDLER #<FUNCTION CLIM-INTERNALS::INPUT-CONTEXT-EVENT-HANDLER> :POINTER-BUTTON-PRESS-HANDLER #<FUNCTION CLIM-INTERNALS::INPUT-CONTEXT-BUTTON-PRESS-HANDLER>)

:AROUND #<KEY-PRESS-EVENT {1005E09001}> is an instance of type KEY-PRESS-EVENT
   it has the following slots:
           TIMESTAMP: 7082235
               SHEET: #<CLIMACS-GUI::CLIMACS-MINIBUFFER-PANE ESA:MINIBUFFER {1004E0FC81}>
      MODIFIER-STATE: 512
                   X: 150
                   Y: 90
             GRAFT-X: 1115
             GRAFT-Y: 694
            KEY-NAME: :|i|
       KEY-CHARACTER: #\i
NIL is of type NULL
   it has a value of NIL
   it is in the #<PACKAGE "COMMON-LISP"> package
   it has a property list of NIL
    1: CLIM:STREAM-READ-GESTURE returned #<CLIM:KEY-PRESS-EVENT {1005E09001}>
  0: CLIM:STREAM-READ-GESTURE returned #<CLIM:KEY-PRESS-EVENT {1005E09001}>
  0: (CLIM:STREAM-READ-GESTURE #<CLIM:STANDARD-INPUT-EDITING-STREAM {10036221B1}> :TIMEOUT NIL :PEEK-P NIL :INPUT-WAIT-TEST #<FUNCTION CLIM-INTERNALS::INPUT-CONTEXT-WAIT-TEST> :INPUT-WAIT-HANDLER #<FUNCTION CLIM-INTERNALS::INPUT-CONTEXT-EVENT-HANDLER> :POINTER-BUTTON-PRESS-HANDLER #<FUNCTION CLIM-INTERNALS::INPUT-CONTEXT-BUTTON-PRESS-HANDLER>)
    1: (CLIM:STREAM-READ-GESTURE #<CLIMACS-GUI::CLIMACS-MINIBUFFER-PANE ESA:MINIBUFFER {1004E0FC81}> :TIMEOUT NIL :INPUT-WAIT-TEST #<FUNCTION CLIM-INTERNALS::INPUT-CONTEXT-WAIT-TEST> :INPUT-WAIT-HANDLER #<FUNCTION CLIM-INTERNALS::INPUT-CONTEXT-EVENT-HANDLER> :POINTER-BUTTON-PRESS-HANDLER #<FUNCTION CLIM-INTERNALS::INPUT-CONTEXT-BUTTON-PRESS-HANDLER>)
|#

(in-package :info.read-eval-print.climacs.ext)

(progn
  (clim:define-gesture-name :complete :keyboard (:tab))
  (clim:define-gesture-name :complete :keyboard (#\i :control) :unique nil))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; input-editing.lisp
;;;; 補完候補一覧の背景色を変える。
(in-package :clim-internals)

(defun print-possibilities (possibilities possibility-printer stream)
  "Write `possibitilies' to `stream', using
`possibility-printer'. `Possibilities' must be a list of
input-completion possibilities. `Stream' must be an input-editing
stream. Output will be done to its typeout."
  (with-input-editor-typeout (stream :erase t)
    ;; +cornsilk1+ から drei:*background-color* に変更
    (surrounding-output-with-border (stream :shape :drop-shadow :background drei:*background-color*)
      (surrounding-output-with-border (stream :shape :rectangle)
        (let ((ptype `(completion ,possibilities)))
          (format-items possibilities
           :stream stream
           :printer #'(lambda (possibility stream)
                        (funcall possibility-printer
                                 possibility
                                 ptype
                                 stream))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; pwd
(define-command (com-pwd :name t :command-table drei:info-table) ()
  "Print working directory."
  (let ((filepath (or (drei:filepath (drei:buffer (drei:current-view)))
                      *default-pathname-defaults*)))
    (esa:display-message (directory-namestring filepath))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; fuzzy-completions

(in-package :drei-lisp-syntax)

(defmethod fuzzy-completions ((image swank-local-image) symbol-name default-package &optional limit)
  (declare (ignore image))
  (let ((swank::*buffer-package* (package-name default-package)))
    (swank::fuzzy-completions symbol-name swank::*buffer-package* :limit limit)))
