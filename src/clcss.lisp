(in-package :clcss)

(defun css-class-macro-character (stream char)
  (declare (ignorable char))
  `(:class ,(read stream)))

(defun css-id-macro-character (stream char)
  (declare (ignorable char))
  `(:id ,(read stream)))


(defun read-css% (path &key (start 0) acc)
  (let ((*readtable* (copy-readtable nil))
        (*package* (find-package :keyword)))
    (set-macro-character #\# #'css-id-macro-character t)
    (set-macro-character #\! #'css-class-macro-character t)
    (multiple-value-bind (data offset) (read-from-string path nil nil :start start :preserve-whitespace t)
      (cond
        ((null data) (reverse acc))
        (t (read-css% path :start offset))))))

(defun read-css (path)
  (read-css% path))

(defclass fsm (c2mop:funcallable-standard-object)
  ((state :initarg :state :accessor state))
  (:metaclass c2mop:funcallable-standard-class))

(defmethod initialize-instance :before ((fsm fsm) &key)
  (c2mop:set-funcallable-instance-function 
   fsm 
   #'(lambda (event)
       (format t "state: ~A~%" (state fsm))
       (format t "event: ~A~%" event)
       (setf (state fsm) (funcall (state fsm) fsm event))
       fsm)))
       
(defun make-token-fsm ()
  (make-instance 'fsm :state 'read-symbol))

(defmethod print-object ((fsm fsm) s)
  (format s "#<FSM state: ~A>" (state fsm)))

(defmethod read-symbol ((fsm fsm) event)
  (cond 
    ((null event) nil)
    ((ppcre:scan "[\\w-]" (string event))
     'read-symbol)
    ((equal event #\Space)
     'read-space)))

(defmethod read-space ((fsm fsm) event)
  (if (equal event #\Space)
      'read-space
      (read-symbol fsm event)))

(defun path-to-tokens (path)
  (tokenize-stream (make-string-input-stream path)))

(defun tokenize-stream (stream)
  (do ((c (read-char stream nil nil) (read-char stream nil nil))
       (fsm (make-token-fsm) (funcall fsm c)))
      ((or (null c) (null fsm)) fsm)
    (format t "c: ~A fsm: ~A~%" c fsm)))
    
    

              
