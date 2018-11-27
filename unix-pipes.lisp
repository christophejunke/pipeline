(in-package :pipeline.unix-pipes)

(defun unix-close/warn-on-error (file-descriptor)
  (multiple-value-bind (status error) (unix-close file-descriptor)
    (prog1 status
      (unless (eql error 0)
        (warn "Unix close error: ~S" error)))))

(defmacro with-unix-pipe ((read-fd write-fd) &body body)
  (with-gensyms (first second)
    `(multiple-value-bind (,first ,second) (unix-pipe)
       (if ,first
           (unwind-protect
                (multiple-value-bind (,read-fd ,write-fd)
                    (values ,first ,second)
                  ,@body)
             (unix-close/warn-on-error ,first)
             (unix-close/warn-on-error ,second))
           (error "Unix pipe error: ~s" ,second)))))

(defmacro with-fd-stream% ((var fd direction &rest fd-args) &body body)
  (check-type direction (member :output :input))
  (with-gensyms (in%)
    `(let ((,in% (make-fd-stream ,fd ,direction t ,@fd-args)))
       (unwind-protect (let ((,var ,in%))
                         (declare (dynamic-extent ,var))
                         ,@body)
         (close ,in%)))))

(defmacro with-fd-streams (((in read-fd &rest read-args)
                            (out write-fd &rest write-args))
                           &body body)
  `(with-fd-stream% (,in ,read-fd :input ,@read-args)
     (with-fd-stream% (,out ,write-fd :output ,@write-args)
       ,@body)))

(defmacro with-unix-pipe-streams ((in &rest read-args)
                                  (out &rest write-args)
                                  &body body)
  (with-gensyms (rfd wfd)
    `(with-unix-pipe (,rfd ,wfd)
       (with-pipe-streams ((,in ,rfd ,@read-args)
                           (,out ,wfd ,@write-args))
         ,@body))))
