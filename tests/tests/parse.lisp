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
    (:seq (:word :h1))
  (read-css "h1"))

(def-test (can-parse-nested-elements :group parse-tests)
    (:seq (:word :div)
          (:word :span))
  (read-css "div span"))

(def-test (can-parse-compound-tag-with-id :group parse-tests)
    (:seq (:seq (:equal :compound)
                (:word :ul)
                (:id :nav)))
  (read-css "ul#nav"))

(def-test (can-parse-just-id :group parse-tests)
    (:seq (:id :my-id))
  (read-css "#my-id"))

(def-test (can-parse-just-class :group parse-tests)
    (:seq (:class :my-class))
  (read-css ".my-class"))

(def-test (can-parse-tag-with-id-and-class :group parse-tests)
    (:seq (:seq (:equal :compound)
                (:word :div)
                (:id :wrap)
                (:class :main)))
  (read-css "div#wrap.main"))
