(in-package :clcss-tests)

;; Helpers
(defgeneric is-symbol (form)
  (:method ((form list))
    (is-word (car form)))
  (:method ((form symbol))
    (equal form :word)))

;; Criteria

(def-criterion-alias (:symbol value) `(:seq (:equal :symbol)
                                            (:all 
                                             (:predicate symbolp)
                                             (:equal ,value))))

(def-criterion-alias (:descendant value) `(:seq (:equal :descendant)
                                                (:all
                                                 (:predicate listp)
                                                 (:seq ,value))))

(def-criterion-alias (:id value) `(:seq (:equal :id)
                                        (:all
                                         (:predicate symbolp)
                                         (:equal ,value))))

(def-criterion-alias (:class value) `(:seq (:equal :class)
                                           (:all 
                                            (:predicate symbolp)
                                            (:equal ,value))))


