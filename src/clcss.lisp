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
      ;; (format t "state: ~A~%" (state fsm))
      ;; (format t "event: ~A~%" event)
       (setf (state fsm) (funcall (state fsm) fsm event))
       fsm)))
       
(defun make-token-fsm ()
  (make-instance 'fsm :state 'read-symbol))

(defmethod print-object ((fsm fsm) s)
  (format s "#<FSM state: ~A>" (state fsm)))

(defmethod read-symbol ((fsm fsm) event)
  (cond 
    ((null event)
     'emit-token)
    ((ppcre:scan "[\\w-]" (string event))
     (append-to-token fsm event)
     'read-symbol)
    ((equal event #\Space)
     (emit-token fsm event 'read-space))
    ((equal event #\.)
     (emit-token fsm event 'read-class))
    ((equal event #\#)
     (emit-token fsm event 'read-id))))

(defmethod emit-token ((fsm fsm) event &optional (next-event 'stop))
  (let ((prefix (cond 
                  ((equal 'read-id (state fsm)) :id)
                  ((equal 'read-class (state fsm)) :class)
                  (t :word))))
    ;;(format t "Emitting token: ~A~%" (current-token fsm))
    (setf (token-list fsm) 
          (append (token-list fsm) 
                  (list `(,prefix ,(coerce (current-token fsm) 'string)))))
    (setf (current-token fsm) nil)
    next-event))

(defmethod append-to-token ((fsm fsm) c)
  (setf (current-token fsm) (append (current-token fsm) (list c))))

(defmethod read-class ((fsm fsm) event)
  (cond 
    ((null event) (read-symbol fsm event))
    ((ppcre:scan "[\\w-]" (string event))
     (append-to-token fsm event)
     'read-class)
    (t (read-symbol fsm event))))
  
(defmethod read-space ((fsm fsm) event)
  (if (equal event #\Space)
      'read-space
      (read-symbol fsm event)))

(defmethod read-id ((fsm fsm) event)
  (cond 
    ((ppcre:scan "[\\w-]" (string event))
     (append-to-token fsm event)
     'read-id)
    (t (read-symbol fsm event))))

(defun path-to-tokens (path)
  (tokenize-stream (make-string-input-stream path)))

(defun tokenize-stream (stream)
  (do ((c (read-char stream nil nil) (read-char stream nil nil))
       (fsm (make-token-fsm) (funcall fsm c)))
      ((or (null fsm) (equal (state fsm) 'stop)) fsm)
    (format t "c: ~A fsm: ~A~%" c fsm)))
