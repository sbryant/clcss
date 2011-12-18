(in-package :clcss-tests)

;; Old test data, using as reference
;; TODO: Remove when cases are covered
(defparameter *css-tests* `("h1" "div span" "div span p#poop" "ul#nav"
                            "#my-id" ".my-class" "div#body" "div.my-class" ".indent p"))

;; Tests
(def-test (can-parse-nothing-to-nothing :group parse-tests)
    (:not :true)
  (read-css ""))

(def-test (can-parse-simple-element :group parse-tests)
    (:seq (:symbol :symbol)
          (:symbol :h1))
  (read-css "h1"))

(def-test (can-parse-nested-elements :group parse-tests)
    (:seq (:symbol :compound)
          (:seq (:symbol :symbol) (:symbol :div))
          (:seq (:symbol :descendant)
                (:seq (:symbol :symbol)
                      (:symbol :span))))
   (read-css "div span"))

(def-test (can-parse-compound-tag-with-id :group parse-tests)
    (:seq (:symbol :compound)
          (:seq (:symbol :symbol)
                (:symbol :ul))
          (:seq (:symbol :id)
                (:symbol :nav)))
  (read-css "ul#nav"))

(def-test (can-parse-just-id :group parse-tests)
    (:seq (:symbol :id) (:symbol :my-id))
  (read-css "#my-id"))

(def-test (can-parse-just-class :group parse-tests)
    (:seq (:symbol :class)
          (:symbol :my-class))
  (read-css ".my-class"))

(def-test (can-parse-tag-with-id-and-class :group parse-tests)
    (:seq (:symbol :compound)
          (:seq (:symbol :symbol) (:symbol :div))
          (:seq (:symbol  :id) (:symbol :wrap))
          (:seq (:symbol  :class) (:symbol :main)))
  (read-css "div#wrap.main"))
