(in-package :clcss-tests)

;; Groups
(def-test-group parse-tests ()
  (:documentation "Tests basics of CSS string parsing"))

(def-test-group matching-tests ()
  (:documentation "Test for matching of compiled expressions go in this group."))

;; Root Group
(def-test-group all-tests ()
  (:documentation "Root of the test tree")
  (:include-groups parse-tests matching-tests))


