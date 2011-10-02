(defpackage :clcss-tests
  (:use :cl :clcss))

(in-package :clcss-tests)

(defparameter *css* `("h1" "#my-id" ".my-class" "div#body" "div.my-class" ".indent p"))