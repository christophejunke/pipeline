;; Use bordeaux-threads, osicat, etc.

(defpackage :pipeline
  (:use :cl)
  (:import-from #:sb-ext
                #:process-alive-p)
  (:import-from #:sb-thread
                #:make-thread
                #:join-thread)
  (:import-from #:alexandria
                #:with-gensyms
                #:once-only))

(in-package :pipeline)

(defmacro with-unix-pipe ((read-fd write-fd) &body body)
  (with-gensyms (first second error)
    `(multiple-value-bind (,first ,second) (sb-unix:unix-pipe)
       (if ,first
           (unwind-protect
                (multiple-value-bind (,read-fd ,write-fd)
                    (values ,first ,second)
                  ,@body)
             (handler-bind ((error (lambda (,error)
                                     (warn "Unix close error: ~s" ,error))))
               (sb-unix:unix-close ,first)
               (sb-unix:unix-close ,second)))
           (error "Unix pipe error: ~s" ,second)))))

(defmacro with-fd-stream% ((var fd direction buffering) &body body)
  (check-type direction (member :output :input))
  (check-type buffering symbol)
  (with-gensyms (in%)
    `(let ((,in% (sb-sys:make-fd-stream ,fd
                                        :buffering ,buffering
                                        ,direction t)))
       (declare (dynamic-extent ,in%))
       (unwind-protect (let ((,var ,in%))
                         (declare (dynamic-extent ,var))
                         ,@body)
         (close ,in%)))))

(defmacro with-pipe-streams (((in rfd) (out wfd) &key (buffering :full))
                             &body body)
  (once-only (buffering)
    `(with-fd-stream% (,in ,rfd :input ,buffering)
       (with-fd-stream% (,out ,wfd :output ,buffering)
         ,@body))))

(defun reader (in out)
  (loop
     for line = (read-line in nil nil)
     while line
     do (format out "~&>>> ~a~%" line))
  (close in)
  (close out))

(defun writer (out)
  (dotimes (i 100)
    (print i out))
  (close out))

(defmacro with-unix-pipe-streams ((in out) &body body)
  (with-gensyms (rfd wfd)
    `(with-unix-pipe (,rfd ,wfd)
       (with-pipe-streams ((,in ,rfd) (,out ,wfd))
         ,@body))))

(defun closer (&rest streams)
  (lambda (process)
    (unless (process-alive-p process)
      (map () #'close streams))))
