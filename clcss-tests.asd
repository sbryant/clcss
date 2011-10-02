;; -*- mode: Lisp;  -*-
(asdf:defsystem #:clcss-tests
  :depends-on (#:clcss
               #:nst)

  :components ((:module "tests"
                :components ((:file "package")

                             (:file "helpers" :depends-on ("package"))
                             (:file "groups" :depends-on ("package"))

                             (:module "tests" :depends-on ("helpers" "groups") :components
                                      ((:file "parse")))))))
