(in-package :clcss)

(defun css-id-macro-character (stream char)
  (declare (ignorable char))
  `(:id ,(read stream)))

(defun read-css (path &key (start 0) acc)
  (flet ((read-next () 
           (multiple-value-bind (data offset) (read-from-string path nil nil :start start)
                          (cond
                            ((null data) (reverse acc))
                            (t (read-css path :start offset :acc (cons data acc)))))))
    (let ((old-read-table (copy-readtable))
          result)
      (unwind-protect 
           (progn 
             (set-macro-character #\# #'css-id-macro-character t)
             (setf result (read-next))))
      (copy-readtable old-read-table *readtable*)
      result)))
