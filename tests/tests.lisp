(in-package :clcss-tests)

;; Old test data, using as reference
;; TODO: Remove when cases are covered
(defparameter *css-tests* `("h1" "div span" "div span p#poop" "ul#nav"
                            "#my-id" ".my-class" "div#body" "div.my-class" ".indent p"))

;; Helpers
(defgeneric is-word (form)
  (:method ((form list))
    (is-word (car form)))
  (:method ((form symbol))
    (equal form :word)))

(def-test-group parse-tests ()
  (:documentation "Tests basics of CSS string parsing"))

;; Root
(def-test-group all-tests ()
  (:documentation "Root of the test tree")
  (:include-groups parse-tests))


;; Tests
(def-test (can-parse-nothing-to-nothing :group parse-tests)
    (:not :true) (read-css ""))

(def-test (can-parse-simple-element :group parse-tests)
    (:all (:apply caar (:equal :word))
          (:apply cadar (:equal :h1)))
  (read-css "h1"))

(def-test (can-parse-nested-elements :group parse-tests
                                     :fixtures parse-helpers)
    (:all (:seq (:seq (:predicate is-word) (:equal :div))
                (:seq (:predicate is-word) (:equal :span))))
  (read-css "div span"))
