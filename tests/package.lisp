(defpackage :clcss-tests
  (:use :cl :nst)
  (:use :clcss)
  (:export #:run-all))

(in-package :clcss-tests)

(defun run-all ()
 (let ((results-dir (merge-pathnames (make-pathname :directory '(:relative "tests" "results"))
                                     (asdf:system-source-directory :clcss-tests))))
    ;; Verbosity knob
    ;; (nst-cmd :set :verbose :trace)

    (nst-cmd :run-group all-tests)

    (junit-results-by-group :dir results-dir
                            :if-file-exists :supersede
                            :if-dir-does-not-exist :create)))
