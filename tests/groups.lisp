(in-package :clcss-tests)

;; Groups
(def-test-group parse-tests ()
  (:documentation "Tests basics of CSS string parsing"))

;; Root Group
(def-test-group all-tests ()
  (:documentation "Root of the test tree")
  (:include-groups parse-tests))


