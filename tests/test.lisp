(defpackage :clcss-tests
  (:export
   #:*css-tests*)
  (:use :cl :clcss))

(in-package :clcss)

(defparameter *css-tests* `("h1" "div span" "div span p#poop" "ul#nav"
                            "#my-id" ".my-class" "div#body" "div.my-class" ".indent p"))


(defun try ()
  (mapcar #'read-css
          (subseq *css-tests* 0 5)))
