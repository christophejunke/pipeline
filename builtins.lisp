(in-package :pipeline)

;; (defun tee (&rest streams)
;;   (lambda ()
;;     (uiop:copy-stream-to-stream *standard-input*
;;                                 (apply #'make-broadcast-stream
;;                                        *standard-output*
;;                                        streams)
;;      :linewise t)))

(defun tee/error ()
  (tee *error-output*))

(defmacro broadcast (&rest stream-forms)
  "Build a function that broadcasts *STANDARD-INPUT* to all STREAM-FORMS.

STREAM-FORMS is a list of forms evaluating to streams, at
function call time (not at function creation time).

In STREAM-FORMS, some symbols are treated specially:

- :OUTPUT and T designate *STANDARD-OUTPUT*
- :ERROR designates *ERROR-OUTPUT*"
  (let ((stream-forms
          (remove nil
                  (sublis '((t . *standard-output*)
                            (:output . *standard-output*)
                            (:error . *error-output*))
                          stream-forms))))
    (with-gensyms (stream rebind)
      `(bricabrac.sdl2.event-loop:with-captured-bindings
           (,rebind
            ,@(set-difference
               (remove-if-not #'symbolp
                              stream-forms)
               '(*standard-output* *error-output*)))
         (lambda ()
           (,rebind
            (let ((,stream (make-broadcast-stream ,@stream-forms)))
              (unwind-protect (uiop:copy-stream-to-stream *standard-input*
                                                          ,stream
                                                          :linewise t)
                (close ,stream)))))))))

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
       for ,value = (,(ecase read-fn ((read read-line read-char) read-fn))
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

(defmacro do-lines ((line) &body body)
  `(with-read-loop (,line read-line)
     (locally ,@body)))

(export
 (defun each-line (function)
   (lambda-line (line)
     (write-line (funcall function line))
     (finish-output))))

(export
 (defmacro lambda-form ((form) &body body)
   `(lambda ()
      (with-read-loop (,form read)
        (locally ,@body)))))

(export
 (defmacro lambda-form-print ((form) &body body)
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

(defun keep-regex (regex)
  (let ((scanner (ppcre:create-scanner regex)))
    (lambda-line (line)
      (when (ppcre:scan scanner line)
        (write-line line)))))

(export
 (defun reservoir-sampling (n)
   (let ((sample (make-array n :initial-element nil)) (i 0))
     (lambda (item)
       (unless (eq item sample)
         (if (<= (incf i) n)
             (setf (aref sample (1- i)) item)
             (when (< (random i) n)
               (setf (aref sample (random n)) item))))
       sample))))

(export
 (defun reservoir-sampling-pipe (size &optional (on-result #'print))
   (check-type size (integer 1))
   (check-type on-result (or function symbol))
   (let ((sampler (reservoir-sampling size))
         (callback (etypecase on-result
                     (symbol (symbol-function on-result))
                     (function on-result))))
     (check-type callback function)
     (lambda ()
       (with-read-loop (line read-line)
         (funcall sampler line))
       (funcall callback (funcall sampler sampler))))))

(export
 (defun print-sequence (sequence)
   (map () #'print sequence)))

(export
 (defun sequence-emitter (&optional (separator #\newline))
   (lambda (&aux (first t))
     (let ((sequence (read)))
       (map
        ()
        (lambda (v)
          (cond
            (first (setq first nil))
            (t (princ separator)))
          (princ v))
        sequence)))))

(export
 (defun uniq (&key (key #'identity) (test #'equalp) (stop-after 10) (on-result #'print))
   (let ((hash (make-hash-table :test test)))
     (lambda (&aux (counter stop-after))
       (block nil
         (with-read-loop (line read-line)
           (let ((value (funcall key line)))
             (multiple-value-bind (_ e) (gethash value hash)
               (declare (ignore _))
               (cond
                 (e
                  (when (<= (decf counter) 0)
                    (return)))
                 (t
                  (setf counter stop-after)
                  (setf (gethash value hash) line)))))))
       (funcall on-result (alexandria:hash-table-values hash))))))
