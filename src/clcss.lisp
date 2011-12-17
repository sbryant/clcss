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
             (format t "compound tokens ~A and list ~A.~%" tokens list)
             (cond
               ((null tokens) (reverse list))
               ((equal (first tokens) :descendant)
                (reverse (cons (descend (rest tokens)) (reverse list))))
               (t
                (compound (cdr tokens) (cons (first tokens) list)))))
           (descend (tokens)
             (format t "descendants tokens ~A.~%" tokens)
             (cons :descendant (tokens-to-tree tokens)))
           (compound-or-descend (tokens action)
             (let ((token (first tokens)))
               (cond ((equal action 'descend)
                      (format t "descend branch: tokens ~A.~%" tokens)
                      (descend tokens))
                     ((equal action 'compound)
                      (format t "compound tokens branch: tokens ~A.~%" tokens)
                        (list (cons :compound (compound tokens nil))))
                     ((equal (second tokens) :descendant)
                      (list token (descend (cddr tokens))))
                     (t
                      (format t "decided on compound branch: tokens ~A.~%" tokens)
                      (compound-or-descend tokens 'compound))))))
    (format t "tokens ~A~%" tokens)
    (cond
      ((null tokens) nil)
      ((= (length tokens) 1) tokens)
      (t (compound-or-descend tokens 'decide)))))

(defun read-css (path)
  (tokens-to-tree (token-list (path-to-tokens path))))

(defun matches-p (predicate html)
  (if (funcall predicate html)
      html
      nil))

(defun make-compound-matcher (predicates)
  (format t "Predicate checker ~A ~%" predicates)
  (let ((compiled-predicates (mapcar #'(lambda (pred)
                                         (compile-tree (list pred))) predicates)))
    (format t "Compiled preds: ~A~%" compiled-predicates)
    (lambda (data)
      (format t "compound matcher~%")
      (every #'(lambda (pred) (funcall pred data)) compiled-predicates))))

(defun make-descendant-matcher (predicates)
  (format t "descendant matcher ~A ~%" predicates)
  (lambda (data)
    (format t "Descendant matcher with data ~A" data)))

(defun make-symbol-matcher (symbol)
  (lambda (data)
    (format t "Symbol matching ~A with data ~A~%" symbol data)
    (if (equalp symbol (car data))
        t
      nil)))

(defun make-class-matcher (symbol)
  (lambda (data)
    (format t "Class matching ~A with data ~A~%" symbol data)
    (if (equalp symbol (getf (second data) :class))
        t
      nil)))

(defun compile-tree (tree)
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
                 (format t "We are in the :id (~A)~%" symbol))
               (:class (symbol)
                 (format t "We are in the :class (~A)~%" symbol)
                 (make-class-matcher symbol)))
      ,@tree)))
