(in-package :clcss-tests)

(defparameter *css-tests* `("h1" "div span" "div span p#poop" "ul#nav"
                            "#my-id" ".my-class" "div#body" "div.my-class" ".indent p"))

(defun try ()
  (mapcar #'read-css
          *css-tests*))
