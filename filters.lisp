(in-package :pipeline.filters)

(defgeneric spawn (command &key pipeline input output error wait first last)
  (:documentation
   "Create the program or thread that executes the given COMMAND.

INPUT, OUTPUT and ERROR are streams that are given to the COMMAND and
should be closed once the task terminates.

WAIT indicates whether the current thread should wait for COMMAND to
terminate.  In a pipe, only the last command is given :WAIT T by
default.

The returned value is passed to CLEAN once all commands in a pipe
finish."))

(defgeneric clean/tag (tag filter))

(defgeneric clean (filter)
  (:method ((filter cons))
    (destructuring-bind (tag value) filter
      (clean/tag tag value))))

(defvar *environment*
  '("LANG_ALL=C" "USE_COLORS=0"
    (:inherit :ssh_auth_sock)))

(defun call-with-augmented-environment (environment function)
  (flet ((preprocess (item)
           (typecase item
             (string item)
             ((cons atom atom) (format nil "~a=~a" (car item) (cdr item)))
             (t item))))
    (let ((*environment*
            (append (delete nil (mapcar #'preprocess environment))
                    *environment*)))
      (funcall function))))

(defmacro with-augmented-environment ((&rest values) &body body)
  `(call-with-augmented-environment
    (list ,@(mapcar (lambda (v)
                      (typecase v
                        (keyword `(list :inherit ,v))
                        ((cons (eql :-) t)
                         (cdr v))
                        ((cons atom (not null))
                         (destructuring-bind (key . value) v
                           `(cons ',key ,value)))
                        (t v)))
                    values))
    (lambda () ,@body)))

(defclass program ()
  ((name :initarg :name)
   (environment :initform *environment*)
   (arguments :initarg :arguments)
   (directory :initform t :initarg :directory :accessor directory-of)
   (search :initarg :search :initform t)
   (error :initarg :error :initform nil :accessor error-of)))

(defmethod initialize-instance :after ((p program) &key &allow-other-keys)
  (when (eq t (slot-value p 'directory))
    (setf (slot-value p 'directory) *default-pathname-defaults*)))

(defun program* (name args &rest extra-args)
  (apply #'make-instance 'program :name name :arguments args extra-args))

(defun program (name &rest args)
  (program* name (mapcar #'princ-to-string args)))

(defun command (string)
  "Split string by spaces, if escaping is a problem use PROGRAM"
  (apply #'program (ppcre:split #\space string)))

(define-compiler-macro command (&whole whole string)
  (if (stringp string)
      `(program ,@(ppcre:split #\space string))
      whole))

(defun error-to-output (program)
  (prog1 program
    (setf (error-of program) :output)))

(defmacro warn-on-errors ((&key (type 'error)) &body body)
  `(handler-case (progn ,@body)
     (,type (e) (warn "caught error ~s" e))))

(defun make-hook/on-death-close-streams (in out err)
  (lambda (process)
    (unless (process-alive-p process)
      ;; flush process fd-handlers
      (sb-sys:serve-all-events 0)
      ;; close streams so that other processes/events in pipeline see EOF
      (ensure-stream-closed/no-error (process-input process))
      (ensure-stream-closed/no-error in)
      (ensure-stream-closed/no-error (process-output process))
      (ensure-stream-closed/no-error out)
      (ensure-stream-closed/no-error (process-error process))
      (ensure-stream-closed/no-error err))))

(defun compute-environment (list)
  (let ((projection (make-hash-table))
        (raw-strings nil))
    (flet ((getenv (key)
             (osicat-posix:getenv (string key)))
           (checked-set (key value)
             (let ((current (gethash key projection)))
               (if current
                   (warn "ignore ~s = ~s" key value)
                   (setf (gethash key projection) value))))
           (insert (key value)
             (let ((v (gethash key projection)))
               (setf (gethash key projection)
                     (list* value
                            (etypecase v
                              (list v)
                              (t (list v))))))))
      (dolist (item list)
        (typecase item
          (string (push item raw-strings))
          (cons (destructuring-bind (token . args) item
                  (ecase token
                    (:inherit
                     (dolist (key args)
                       (checked-set key (getenv key))))
                    (:push
                     (destructuring-bind (where what) args
                       (insert where what))))))))
      (with-hash-table-iterator (next projection)
        (loop
          (multiple-value-bind (vp key value) (next)
            (unless vp (return))
            (push (format nil
                          "~a=~{~a~^:~}"
                          key
                          (alexandria:ensure-list value))
                  raw-strings))))
      (nreverse raw-strings))))

(defmethod spawn ((program program) &key input output error wait first last &allow-other-keys)
  (with-slots (name arguments search (program-error error) environment) program
    (let* ((input (if first
                      (pipeline::register-resource
                       (make-concatenated-stream input))
                      input))
           (output (if last
                       (pipeline::register-resource
                        (make-broadcast-stream output))
                       output))
           (error (pipeline::%pipe-arg-error
                   (or program-error
                       (pipeline::register-resource
                        (make-broadcast-stream error)))
                   output)))
      `(:process
        ,(apply #'
          run-program
          name
          arguments
          :search search
          :wait wait
          :input input
          :output output
          :directory (or (slot-value program 'directory)
                         *default-pathname-defaults*)
          :error error
          :status-hook (unless wait
                         (make-hook/on-death-close-streams
                          input output error))
          (when environment
            (list :environment (compute-environment
                                environment))))))))

(defmethod clean/tag ((tag (eql :process)) process)
  (when (process-alive-p process)
    (process-wait process)))

(declaim (inline channel-to))

(defun channel-to (channel function)
  (check-type channel trivial-channels:channel)
  (pipeline:redirecting-result-to channel function))

(defmethod spawn ((function function) &key pipeline input output error wait first last)
  (let ((input (if first (pipeline::register-resource
                          (make-concatenated-stream input))
                   input))
        (output (if last
                    (pipeline::register-resource
                     (make-broadcast-stream output))
                    output))
        (error (pipeline::register-resource
                (make-broadcast-stream error))))
    (flet ((wrapped (&aux (channel (pipeline:channel-of pipeline)))
             (let ((wrapped (if channel (channel-to channel function) function))
                   (*standard-input* input)
                   (*standard-output* output)
                   (*error-output* error))
               (unwind-protect (funcall wrapped)
                 (ensure-stream-closed/no-error input)
                 (ensure-stream-closed/no-error output)
                 (ensure-stream-closed/no-error error)))))
      (if wait
          `(:funcall ,(wrapped))
          `(:thread ,(make-thread #'wrapped))))))

(defmethod clean/tag ((tag (eql :funcall)) call-result)
  call-result)

(defmethod clean/tag ((tag (eql :thread)) thread)
  (join-thread thread))

