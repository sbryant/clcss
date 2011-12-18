(in-package :clcss)

(defclass fsm (c2mop:funcallable-standard-object)
  ((state :initarg :state :accessor state :initform nil)
   (token-list :initarg :token-list :accessor token-list :initform nil)
   (current-token :initform nil :accessor current-token))
  (:metaclass c2mop:funcallable-standard-class))

(defmethod initialize-instance :before ((fsm fsm) &key)
  (c2mop:set-funcallable-instance-function
   fsm
   #'(lambda (event)
       (setf (state fsm) (funcall (state fsm) fsm event))
       fsm)))

(defun make-token-fsm ()
  (make-instance 'fsm :state 'read-symbol))

(defmethod print-object ((fsm fsm) s)
  (format s "#<FSM state: ~A>" (state fsm)))

(defmethod read-symbol ((fsm fsm) event)
  (cond
    ((null event)
     (emit-token fsm event) 'stop)
    ((ppcre:scan "[\\w-]" (string event))
     (append-to-token fsm event) 'read-symbol)
    ((equal event #\Space)
     (emit-token fsm event) 'deciding-descendant)
    ((equal event #\.)
     (emit-token fsm event) 'read-class)
    ((equal event #\#)
     (emit-token fsm event) 'read-id)))

(defmethod emit-class-token ((fsm fsm) event)
  (setf (token-list fsm)
        (append (token-list fsm)
                (list `(:class ,(intern (string-upcase
                                          (coerce (current-token fsm) 'string))
                                         (find-package :keyword))))))
  (setf (current-token fsm) nil)
  (read-symbol fsm event))

(defmethod emit-id-token ((fsm fsm) event)
  (setf (token-list fsm)
        (append (token-list fsm)
                (list `(:id ,(intern (string-upcase
                                      (coerce (current-token fsm) 'string))
                                     (find-package :keyword))))))
  (setf (current-token fsm) nil)
  (read-symbol fsm event))

(defmethod emit-child-token ((fsm fsm))
  (setf (token-list fsm)
        (append (token-list fsm)
                (list :immediate-child))))

(defmethod emit-descendant-token ((fsm fsm))
  (setf (token-list fsm)
        (append (token-list fsm)
                (list :descendant))))

(defmethod emit-adjacent-sibling-token ((fsm fsm))
  (setf (token-list fsm)
        (append (token-list fsm)
                (list :adjacent-sibling))))

(defmethod emit-token ((fsm fsm) event)
  (unless (null (current-token fsm))
    (setf (token-list fsm)
          (append (token-list fsm)
                  (list `(:symbol ,(intern (string-upcase
                                 (coerce (current-token fsm) 'string))
                                (find-package :keyword)))))
          (current-token fsm) nil)))

(defmethod append-to-token ((fsm fsm) c)
  (setf (current-token fsm) (append (current-token fsm) (list c))))

(defmethod read-class ((fsm fsm) event)
  (cond
    ((null event) (emit-class-token fsm event))
    ((ppcre:scan "[\\w-]" (string event))
     (append-to-token fsm event)
     'read-class)
    (t (emit-class-token fsm event))))

(defmethod deciding-descendant ((fsm fsm) event)
  (cond
    ((equal event #\Space)
     'deciding-descendant)
    ((equal event #\>)
     (emit-child-token fsm)
     'read-space)
    ((equal event #\+)
     'read-adjacent-sibling)
    (t (emit-descendant-token fsm)
       (read-symbol fsm event))))

(defmethod read-adjacent-sibling ((fsm fsm) event)
  (cond
    ((equal event #\Space)
     'read-adjacent-sibling)
    ((ppcre:scan "[\\w-]" (string event))
     (emit-adjacent-sibling-token fsm)
     (read-symbol fsm event))
    (t 'read-symbol)))

(defmethod read-space ((fsm fsm) event)
  (if (equal event #\Space)
      'read-space
      (read-symbol fsm event)))

(defmethod read-id ((fsm fsm) event)
  (cond
    ((null event) (emit-id-token fsm event))
    ((ppcre:scan "[\\w-]" (string event))
     (append-to-token fsm event)
     'read-id)
    (t (emit-id-token fsm event))))

(defun path-to-tokens (path)
  (tokenize-stream (make-string-input-stream path)))

(defun tokenize-stream (stream)
  (do ((c (read-char stream nil nil) (read-char stream nil nil))
       (fsm (make-token-fsm) (funcall fsm c)))
      ((or (null fsm) (equal (state fsm) 'stop)) fsm)
    (format t "c: ~A fsm: ~A~%" c fsm)))

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

