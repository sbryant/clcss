(asdf:defsystem clcss
  :name "clcss"
  :depends-on (:cl-ppcre :closer-mop :closure-html)
  :in-order-to ((test-op (load-op :clcss-tests)))
  :components ((:module "src"
                        :components ((:file "package")
                                     (:file "tokenizer" :depends-on ("package"))
                                     (:file "compiler" :depends-on ("tokenizer"))
                                     (:file "clcss" :depends-on ("compiler"))))))

(defmethod perform ((o asdf:test-op) (c (eql (asdf:find-system :clcss))))
  (funcall (intern (symbol-name :run-all) :clcss-tests)))
