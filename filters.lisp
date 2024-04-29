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
  '("LANG_ALL=C" "USE_COLORS=0" (:inherit :ssh_auth_sock)))

(defun call-with-augmented-environment (environment function)
  (flet ((preprocess (item)
           (typecase item
             (string item)
             ((cons atom atom) (format nil "~a=~a" (car item) (cdr item)))
             (t item))))
    (let ((*environment* (append (delete nil (mapcar #'preprocess environment))
                                 *environment*)))
      (funcall function))))

(defmacro with-augmented-environment ((&rest values) &body body)
  `(call-with-augmented-environment
    (list ,@(mapcar (lambda (v)
                      (typecase v
                        (keyword `(list :inherit ,v))
                        ((cons atom (and atom (not null)))
                         (destructuring-bind (key . value) v
                           `(cons ',key ,value)))
                        (t v)))
                    values))
    (lambda () ,@body)))

(defclass program ()
  ((name :initarg :name)
   (environment :initform *environment*)
   (arguments :initarg :arguments)
   (search :initarg :search :initform t)
   (error :initarg :error :initform nil :accessor error-of)))

(defun program* (name args)
  (make-instance 'program :name name :arguments args))

(defun program (name &rest args)
  (program* name (mapcar #'princ-to-string args)))

(defun error-to-output (program)
  (prog1 program
    (setf (error-of program) :output)))

(defmacro warn-on-errors ((&key (type 'error)) &body body)
  `(handler-case (progn ,@body)
     (,type (e) (warn "caught error ~s" e))))

(defun make-hook/on-death-close-streams (in out err)
  (lambda (process)
    (unless (process-alive-p process)
      (ensure-stream-closed/no-error (process-input process))
      (ensure-stream-closed/no-error in)
      (ensure-stream-closed/no-error (process-output process))
      (ensure-stream-closed/no-error out)
      (ensure-stream-closed/no-error (process-error process))
      (ensure-stream-closed/no-error err))))

(defun compute-environment (list)
  (labels ((inherit (key)
             (alexandria:when-let (value (osicat-posix:getenv (string key)))
               (format nil "~a=~a" key value)))
           (stringify (item)
             (etypecase item
               (string (list item))
               ((cons (eql :inherit) list)
                (delete nil (mapcar #'inherit (rest item)))))))
    (mapcan #'stringify list)))

(defmethod spawn ((program program) &key pipeline input output error wait first last)
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
          :directory *default-pathname-defaults*
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
    (flet ((wrapped (&aux (warn-stream *error-output*)
                          (channel (pipeline:channel-of pipeline)))
             (let ((wrapped (if channel (channel-to channel function) function))
                   (*standard-input* input)
                   (*standard-output* output)
                   (*error-output* error))
               (unwind-protect (handler-case (funcall wrapped)
                                 (error (e)
                                   (let ((*error-output* warn-stream))
                                     (warn "caught error ~s" e))))
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

