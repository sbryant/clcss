(in-package :clcss)

(defun tokens-to-tree (tokens)
  (labels ((compound (tokens list)
                     (cond
                      ((null tokens)
                       (format t "returning list ~A ~%" list)
                       list)
                      ((equal (first tokens) :descendant)
                       (format t "found descendant ~A tokens while compouding with list ~A~%" tokens list)
                       `(,@list ,(compound-or-descend tokens)))
                      (t
                       (format t "compounding tokens ~A and list ~A.~%" tokens list)
                       (compound (cdr tokens) (append list (list (first tokens)))))))
           (descend (tokens)
                    (format t "descending with tokens ~A.~%" tokens)
                    (let ((tree (tokens-to-tree tokens)))
                      `(:descendant ,@(if (= 1 (length tree))
                                          tree
                                        (list tree)))))
           (compound-or-descend (tokens)
                                (cond
                                 ((equal (first tokens) :descendant)
                                  (format t "descendant branch: tokens ~A.~%" tokens)
                                  (descend (cdr tokens)))
                                 (t
                                  (format t "compound branch: tokens ~A.~%" tokens)
                                  `(:compound ,@(compound tokens nil))))))
    (format t "tokens ~A~%" tokens)
    (cond
     ((null tokens) nil)
     ((= (length tokens) 1) (car tokens))
     (t (compound-or-descend tokens)))))

(defun make-path-matcher (path)
  (compile-tree (read-css path)))

(defun make-compound-matcher (predicates)
  ; (format t "Predicate checker ~A ~%" predicates)
  (let ((compiled-predicates (mapcar #'(lambda (pred)
                                         (compile-tree pred)) predicates)))
    (format t "Compiled preds: ~A~%" compiled-predicates)
    (lambda (data)
      (format t "compound matcher~%")
      (every #'(lambda (pred) (funcall pred data)) compiled-predicates))))

(defun make-descendant-matcher (predicates)
  (format t "descendant matcher ~A ~%" predicates)
  (let ((compiled-predicates (mapcar #'(lambda (pred)
                                         (compile-tree pred)) predicates)))
      (labels ((match-with-predicates (data predicates)
                                      (format t "Descendant matcher with data ~A with preds ~A~%" data predicates)
                                      (when (and (not (null data)) (listp data))
                                        (let ((head-descendant (car data))
                                              (tail-descendants (cdr data)))
                                          (format t "hd: ~A td: ~A~%" head-descendant tail-descendants)
                                          (cond ((every #'(lambda (pred) (funcall pred head-descendant)) predicates) t)
                                                (t (match-with-predicates tail-descendants predicates))))))
               (make-matcher ()
                             (lambda (data)
                               (match-with-predicates data compiled-predicates))))
        (make-matcher))))

(defun make-symbol-matcher (symbol)
  `(lambda (data)
    (format t "Symbol matching ~A with data ~A~%" ,symbol data)
    (when (and (listp data) (equalp ,symbol (car data)))
      (format t "We got symbols~%")
      t)))

(defun make-class-matcher (symbol)
  (lambda (data)
    (format t "Class matching ~A with data ~A~%" symbol data)
    (if (listp data)
        (when (string-equal (symbol-name symbol) (cadr (assoc :class (second data))))
            t))))

(defun make-id-matcher (symbol)
  (lambda (data)
    (format t "Id matching ~A with data ~A~%" symbol data)
    (if (listp data)
        (when (string-equal (symbol-name symbol) (cadr (assoc :id (second data))))
          t))))

(defun compile-tree (tree)
  "Take in a tree of css and spit on a function that operates on node
reference. Usually this will be be a css expression matcher."
  (format t "Got Tree ~A~%Expanding...~%" tree)
  (eval
   `(macrolet ((:compound (&rest predicates)
                 (format t "We are in the :compound ~A ~%" predicates)
                 (make-compound-matcher predicates))
               (:symbol (symbol)
                 (format t "We are in the :symbol (~A) ~%" symbol)
                 (make-symbol-matcher symbol))
               (:descendant (&rest predicates)
                 (format t "We are in the :descendant ~A ~%" predicates)
                 (make-descendant-matcher predicates))
               (:id (symbol)
                 (format t "We are in the :id (~A)~%" symbol)
                 (make-id-matcher symbol))
               (:class (symbol)
                 (format t "We are in the :class (~A)~%" symbol)
                 (make-class-matcher symbol)))
      ,@(list tree))))
