(in-package :clcss-tests)

;; Helpers
(defgeneric is-word (form)
  (:method ((form list))
    (is-word (car form)))
  (:method ((form symbol))
    (equal form :word)))

;; Criteria

(def-criterion-alias (:id value) `(:seq (:equal :id)
                                        (:all
                                         (:predicate symbolp)
                                         (:equal ,value))))

(def-criterion-alias (:class value) `(:seq (:equal :class)
                                           (:all 
                                            (:predicate symbolp)
                                            (:equal ,value))))


