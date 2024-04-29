(in-package :pipeline)

(define-condition pipeline-result ()
  ((result :initarg :result :reader .result)
   (name :initarg :name :reader .name :initform nil)))

(define-condition pipeline-fold (pipeline-result)
  ())

(defun signal-result (value)
  (with-simple-restart (continue "Stop propagating signal")
    (signal 'pipeline-result :result value)))

(defun signal-fold (key value)
  (with-simple-restart (continue "Stop propagating signal")
    (signal 'pipeline-fold :result value :name key)))

(defun signal-named-result (name value)
  (with-simple-restart (continue "Stop propagating signal")
    (signal 'pipeline-result :name name :result value)))

