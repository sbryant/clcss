(in-package :clcss)

(defun css-id-macro-character (stream char)
  (declare (ignorable char))
  `(:id ,(read stream)))

(defun read-css (path &key (start 0) acc)
  (let ((*readtable* (copy-readtable nil)))
    (set-macro-character #\# #'css-id-macro-character t)
    (multiple-value-bind (data offset) (read-from-string path nil nil :start start :preserve-whitespace t)
      (cond
        ((null data) (reverse acc))
        (t (read-css path :start offset :acc (cons data acc)))))))
