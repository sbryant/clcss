(defsystem clcss
  :name "clcss"
  :depends-on ()
  ;;  :in-order-to ((test-op (load-op clcss-test)))
  :components ((:module "src"
                        :components ((:file "package")
                                     (:file "clcss" :depends-on ("package"))))))