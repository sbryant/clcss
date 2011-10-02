(in-package :clcss-tests)

(defparameter *css-tests* `("h1" "div span" "div span p#poop" "ul#nav"
                            "#my-id" ".my-class" "div#body" "div.my-class" ".indent p"))

;; Suites
(def-test-group parse-tests ()
  (:documentation "Tests basics of CSS string parsing"))

;; Root
(def-test-group all-tests ()
  (:documentation "Root of the test tree")
  (:include-groups parse-tests))


;; Tests
(def-test (can-parse-nothing-to-nothing :group parse-tests)
    (assert-null (read-css "")))
