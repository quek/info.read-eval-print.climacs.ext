;;; -*- mode: lisp; indent-tabs: nil -*-

(defsystem :info.read-eval-print.climacs.ext
  :serial t
  ;; add new files to this list:
  :components ((:file "package") (:file "info.read-eval-print.climacs.ext"))
  :depends-on (#+nil :cl-ppcre))
