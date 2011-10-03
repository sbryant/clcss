(defsystem clcss
  :name "clcss"
  :depends-on (:cl-ppcre :closer-mop)
  :in-order-to ((test-op (load-op clcss-tests)))
  :components ((:module "src"
                        :components ((:file "package")
                                     (:file "clcss" :depends-on ("package"))))))

(defmethod perform ((o asdf:test-op) (c (eql (asdf:find-system :clcss))))
  (funcall (intern (symbol-name :run-all) :clcss-tests)))