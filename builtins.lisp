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
  (alexandria:with-gensyms (transform)
    `(lambda ()
       (with-read-loop (,line read-line)
         (let ((,transform (progn ,@body)))
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
