(in-package :clcss-tests)

;; Helpers


;; Criteria
(def-criterion-alias (:css-compound &rest criteria)
  `(:seq (:symbol :compound)
         ,@criteria))

(def-criterion-alias (:css-descendant &rest criteria)
  `(:seq (:symbol :descendant)
         ,@criteria))

(def-criterion-alias (:css-symbol symbol)
    `(:seq (:symbol :symbol)
           (:symbol ,symbol)))

(def-criterion-alias (:css-id id)
    `(:seq (:symbol :id)
           (:symbol ,id)))

(def-criterion-alias (:css-class class)
    `(:seq (:symbol :class)
           (:symbol ,class)))
