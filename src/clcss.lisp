(in-package :clcss)

(defun read-css (path &key (start 0) acc)
   (multiple-value-bind (data offset) (read-from-string path nil nil :start start)
     (cond
       ((null data) (reverse acc))
       (t (read-css path :start offset :acc (cons data acc))))))
