(in-package :clcss-tests)

;; Helpers
(defgeneric is-word (form)
  (:method ((form list))
    (is-word (car form)))
  (:method ((form symbol))
    (equal form :word)))

;; Criteria
(def-criterion-alias (:is-word) '(:predicate is-word))

(def-criterion-alias (:word value) `(:seq :is-word
                                          (:equal ,value)))

(def-criterion-alias (:id value) `(:seq (:equal :id)
                                        (:word ,value)))

(def-criterion-alias (:class value) `(:seq (:equal :class)
                                           (:word ,value)))

