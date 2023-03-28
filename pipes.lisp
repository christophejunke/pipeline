(in-package :pipeline.pipes)

(defun ensure-stream-closed/no-error (stream)
  (cond
    ((null stream))
    ((not (typep stream 'stream))
     (warn "Not a stream: ~a" stream))
    ((open-stream-p stream)
     (close stream))))

(defstruct (pipe (:constructor make-pipe% (in out)))
  (in  nil :read-only t)
  (out nil :read-only t))

(defun clean-pipe (pipe)
  (prog1 nil
    (ensure-stream-closed/no-error (pipe-in pipe))
    (ensure-stream-closed/no-error (pipe-out pipe))))

(defun clean-pipes (pipes)
  (map-into pipes #'clean-pipe pipes))

(defun make-pipe ()
  (multiple-value-bind (read-fd write-fd) (unix-pipe)
    (if read-fd
        (let (in-stream
              out-stream)
          (handler-case
              (make-pipe%
               (setf in-stream (make-fd-stream read-fd :input t))
               (setf out-stream (make-fd-stream write-fd :output t)))
            (error (e)
              (ensure-stream-closed/no-error in-stream)
              (ensure-stream-closed/no-error out-stream)
              (unix-close/warn-on-error read-fd)
              (unix-close/warn-on-error write-fd)
              (error e))))
        ;; In case of error, write-fd holds the error code
        (error "Unix pipe create error: ~s" write-fd))))

(defun make-pipes (count)
  (let ((pipes (make-array count :fill-pointer 0)))
    (unwind-protect
         (loop
           repeat count
           do (vector-push (make-pipe) pipes))
      (unless (= (length pipes) count)
        (clean-pipes pipes)
        (setf (fill-pointer pipes) 0)))
    (coerce pipes `(simple-vector ,count))))

(defmacro with-pipes% ((pipes count) &body body)
  `(let ((,pipes (make-pipes ,count)))
     (declare (type (simple-vector ,count) ,pipes))
     (unwind-protect (progn ,@body)
       (clean-pipes ,pipes))))

(defmacro with-pipes ((pipes count) &body body)
  (with-gensyms (internal)
    `(with-pipes% (internal ,count)
       (let ((,pipes (copy-seq ,internal)))
         (declare (type (simple-vector ,count) ,pipes))
         ,@body))))

