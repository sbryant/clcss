(in-package :clcss)

(defun css-class-macro-character (stream char)
  (declare (ignorable char))
  `(:class ,(read stream)))

(defun css-id-macro-character (stream char)
  (declare (ignorable char))
  `(:id ,(read stream)))

(defun read-css% (path &key (start 0) acc)
  (let ((*readtable* (copy-readtable nil)))
    (set-macro-character #\# #'css-id-macro-character t)
    (set-macro-character #\! #'css-class-macro-character t)
    (multiple-value-bind (data offset) (read-from-string path nil nil :start start :preserve-whitespace t)
      (cond
        ((null data) (reverse acc))
        (t (read-css% path :start offset :acc (cons data acc)))))))

(defun transform-css-path (path)
  (flet ((paren-syms (s)
           "Replace all symbols as CSS sees them with (:word symbol) to separate compund statements"
                  (ppcre:regex-replace-all "([\\w_-]+)" s
                                           "(:word \\1)"))

         (group-compound (s)
           "Wrap groups of lists joined by a non-space into lists. e.g.:
             (:word p)#(:word my-p) => (:compound (:word p)#(:word my-p))"
           (ppcre:regex-replace-all "(\\([^\\(]+?\\)[^\\w_-\\s]\\([^\\(]+?\\))+" s
                                    "(:compound \\1)"))
         (dot-to-bang (s)
           "Convert . to ! for lazy parsing"
           (ppcre:regex-replace-all "\\." s
                                    "!")))

  (reduce #'(lambda (res proc) (funcall proc res))
          (list #'paren-syms #'group-compound #'dot-to-bang)
          :initial-value path)))


(defun read-css (path)
  (read-css% (transform-css-path path)))

