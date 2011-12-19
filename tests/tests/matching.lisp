(in-package :clcss-tests)

(def-test (matches-single-element :group matching-tests :fixtures (simple-html-fixture))
    :true
  (matches-p (make-path-matcher "div") html-tree))

(def-test (matches-compound-expression :group matching-tests :fixtures (simple-html-fixture))
    :true
  (matches-p (make-path-matcher "div#main") html-tree))

(def-test (matches-complex-compound-expression :group matching-tests :fixtures (simple-html-fixture))
    :true
  (matches-p (make-path-matcher "div#main.wrap") html-tree))

(def-test (matches-descendant-path :group matching-tests :fixtures (simple-html-fixture))
    :true
  (matches-p (make-path-matcher "div#main.wrap p") html-tree))

(def-test (matches-descendant-with-compound-expression-path :group matching-tests :fixtures (simple-html-fixture))
    :true
  (matches-p (make-path-matcher "div#main.wrap p#content.article") html-tree))


