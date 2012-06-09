;; -*- mode: Lisp;  -*-
(asdf:defsystem #:clcss-tests
  :depends-on (#:clcss
               #:nst)

  :components ((:module "tests"
                :components ((:file "package")

                             (:module "html" :components
                                      ((:html-file "simple")))

                             (:file "helpers" :depends-on ("package"))
                             (:file "fixtures" :depends-on ("html"))
                             (:file "groups" :depends-on ("package" "fixtures"))

                             (:module "tests" :depends-on ("helpers" "groups") :components
                                      ((:file "parse")
                                       (:file "matching")))))))
