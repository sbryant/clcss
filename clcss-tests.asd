;; -*- mode: Lisp;  -*-
(asdf:defsystem #:clcss-tests
  :serial t
  :depends-on (#:clcss
               #:nst)
  :components ((:module "tests"
                :components ((:file "package")
                             (:file "tests" :depends-on ("package"))))))
