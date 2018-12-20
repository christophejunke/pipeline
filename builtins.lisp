(in-package :pipeline)

(defun tee (&rest streams)
  (lambda ()
    (uiop:copy-stream-to-stream
     *standard-input*
     (apply #'make-broadcast-stream *standard-output* streams))))

(defun tee/error ()
  (tee *error-output*))

(export
 (defun map-lines (function)
   (loop
      for line = (read-line *standard-input* nil)
      while line
      do (funcall function line))))

(defmacro with-read-loop ((value read-fn) &body body)
  (alexandria:with-gensyms (sentinel)
    `(loop
       named ,(gensym)
       with ,sentinel = (gensym)
       for ,value = (,(ecase read-fn ((read read-line) read-fn))
                     *standard-input*
                     nil
                     ,sentinel)
       until (eq ,value ,sentinel)
       do (progn ,@body))))

(defmacro lambda-line ((line) &body body)
  `(lambda ()
     (with-read-loop (,line read-line)
       (locally ,@body))))

(defmacro lambda-line-print ((line) &body body)
  (alexandria:with-gensyms (transform)
    `(lambda ()
       (with-read-loop (,line read-line)
         (let ((,transform (locally ,@body)))
           (when ,transform
             (princ ,transform)
             (terpri)))))))

(export
 (defun each-line (function)
   (lambda-line (line)
     (funcall function line))))

(export
 (defmacro lambda-form ((form) &body body)
   (alexandria:with-gensyms (value)
     `(lambda ()
        (with-read-loop (,form read)
          (let ((,value (progn ,@body)))
            (write ,value :escape t)
            (terpri)))))))

(export
 (defun each-form (function)
   (lambda-form (form)
     (funcall function form))))

(export
 (defun feed (sequence &optional (separator #\newline))
   (lambda ()
     (map ()
          (lambda (u)
            (write u :escape nil)
            (write separator :escape nil))
          sequence))))

(export
 (defun read-form ()
   (lambda-form (form) (throw :pipeline form))))

(export
 (defun from-file (file)
   (lambda ()
     (with-open-file (*standard-input* file)
       (with-read-loop (line read-line)
         (write-sequence line *standard-output*)
         (terpri))))))

(export
 (defun to-file (file)
   (lambda ()
     (with-open-file (*standard-output*
                      file
                      :direction :output
                      :if-exists :supersede)
       (with-read-loop (line read-line)
         (write-sequence line *standard-output*)
         (terpri))
       (pathname *standard-output*)))))

(export
 (defun line-collector (&optional (type 'simple-vector))
   (lambda ()
     (let ((vector (make-array 256 :fill-pointer 0 :adjustable t)))
       (with-read-loop (line read-line)
         (vector-push-extend line vector (array-total-size vector)))
       (coerce vector type)))))

