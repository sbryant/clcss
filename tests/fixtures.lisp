(in-package :clcss-tests)

(def-fixtures simple-html-fixture (:cache t)
  (builder (chtml:make-lhtml-builder))
  (html-file (merge-pathnames #p"tests/html/simple.html" (asdf:system-source-directory :clcss-tests)))
  (html-tree (chtml:parse html-file builder)))
