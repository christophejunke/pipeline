(defpackage :pipeline.utils
  (:use :cl)
  (:export #:reservoir-sampling
           #:call-with-reservoir-sampling
           #:with-reservoir-sampling
           #:write-vector-as-lines))

(in-package :pipeline.utils)

(defun reservoir-sampling (size &aux (counter 0))
  (check-type size (integer 1))
  (let ((sample (make-array size :initial-element nil :fill-pointer 0)))
    (values sample
            (lambda (value)
              (prog1 sample
                (incf counter)
                (cond
                  ((<= counter size)
                   (vector-push value sample))
                  ((< (random counter) size)
                   (setf (aref sample (random size)) value))))))))

(defun call-with-reservoir-sampling (size function)
  (multiple-value-bind (sample collector) (reservoir-sampling size)
    (funcall function sample collector)))

(defmacro with-reservoir-sampling ((sample &optional (collector sample))
                                   size &body body)
  (alexandria:with-gensyms (v c)
    `(call-with-reservoir-sampling 
      ,size
      (lambda (,sample ,c)
        (flet ((,collector (,v) (funcall ,c ,v)))
          ,@body)))))

(defun write-vector-as-lines (vector)
  (princ (length vector))
  (terpri)
  (map () (lambda (x) (princ x) (terpri)) vector))
