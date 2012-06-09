(in-package :clcss)

(defun read-css (path)
  (tokens-to-tree (token-list (path-to-tokens path))))

(defun next-node (node)
  (let ((head (when (listp node) (car node))))
    (cond ((listp head)
           (or (cadddr head) (caddr head)))
          (t node)))) ;; text nodes n stuff

(defun matches-p (matcher node)
  (let* ((next-node (next-node node))
         (matched-nodes (funcall matcher next-node)))
    (format t "matcher ~A node ~A~%" matcher node)
    (format t "nn: ~A~%" next-node)
    (format t "mn: ~A~%" matched-nodes)
    (cond
     ((null node) nil)
     (matched-nodes next-node)
     ((stringp node) nil)
     ((and (not (null node)) (listp node))
      (or (matches-p matcher (caddr node))
          (matches-p matcher (cdddr node)))))))




