(in-package :pipeline.filters)

(defgeneric spawn (command &key input output error wait)
  (:documentation "Create the program or thread that executes the given COMMAND.

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

(defclass program ()
  ((name :initarg :name)
   (arguments :initarg :arguments)
   (search :initarg :search :initform t)
   (error :initarg :error :initform :output)))

(defun program (name &rest args)
  (make-instance 'program :name name :arguments args))

(defun make-hook/on-death-close-streams (in out err)
  (lambda (process)
    (unless (process-alive-p process)
      (ensure-stream-closed/no-error (process-input process))
      (ensure-stream-closed/no-error (process-output process))
      (ensure-stream-closed/no-error (process-error process))
      (ensure-stream-closed/no-error in)
      (ensure-stream-closed/no-error out)
      (ensure-stream-closed/no-error err))))

(defvar *unix-environment* '("LANG_ALL=C"))

(defmethod spawn ((program program) &key input output error wait)
  (with-slots (name arguments search (program-error error)) program
    `(:process
      ,(run-program
        name
        arguments
        :environment *unix-environment*
        :search search
        :wait wait
        :input input
        :output output
        :directory *default-pathname-defaults*
        :error (or program-error (make-broadcast-stream error))
        :status-hook (make-hook/on-death-close-streams input output error )))))

(defmethod clean/tag ((tag (eql :process)) process)
  (process-wait process))

(defmethod spawn ((function function) &key input output error wait)
  (flet ((wrapped ()
           (let ((*standard-input* input)
                 (*standard-output* output)
                 (*error-output* error))
             (unwind-protect (funcall function)
               (ensure-stream-closed/no-error input)
               (ensure-stream-closed/no-error output)
               (ensure-stream-closed/no-error error)))))
    (if wait
        `(:funcall ,(wrapped))
        `(:thread ,(make-thread #'wrapped)))))

(defmethod clean/tag ((tag (eql :funcall)) call-result)
  call-result)

(defmethod clean/tag ((tag (eql :thread)) thread)
  (join-thread thread))

