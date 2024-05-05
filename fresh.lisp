(defpackage :proc
  (:use :cl :alexandria :iolib.os :trivial-signal
        :iolib.multiplex))

(in-package :proc)

(defclass io-redirection ()
  ((inp :reader .inp :initarg :input)
   (out :reader .out :initarg :output)
   (err :reader .err :initarg :error)))

(defclass node (io-redirection)
  ((name :reader .name :initarg :name)
   (next :reader .next :initarg :next)
   (prev :reader .prev :initarg :prev)))

(defclass program (node)
  ((command :reader .command :initarg :command)
   (environment :reader .environment :initarg :environment)))

(defun on-sig/call (&optional f)
  (lambda (c)
    (print (list :signal c) *error-output*)
    (finish-output)
    (when f
      (funcall f))))

(signal-handler-bind ((:cld (lambda (c) (print c))))
  (run-program "/bin/ls"))



(with-event-base (base)
  )

(defun status (s)
  (cond
    ((isys:wifexited s)
     (list :exited (isys:wexitstatus s)))
    ((isys:wifsignaled s)
     (list :signaled 
           (isys:wtermsig* s)
           (isys:wcoredump s)))
    ((isys:wifstopped s)
     :stopped)
    ((isys:wifcontinued s)
     :continued)
    (t :unknown)))


(defun tst ()
  (let ((p (iolib.os:create-process '("/usr/bin/sleep" "4")))
        (o *standard-output*)
        (e (cons nil nil)))
    (setf (cdr (setf *watcher-thread* e))
          (bt:make-thread (lambda (&aux (pid (iolib.os:process-pid p)))
                            (ignore-errors
                             (loop
                               until (car e)
                               while (iolib.os:process-activep p)
                               do
                                  (multiple-value-bind (_ s)
                                      (iolib.syscalls:waitpid 
                                       pid
                                       (logior isys:wuntraced
                                               isys:wcontinued))
                                    (declare (ignore _))
                                    (print (list :status s (status s))
                                           o)
                                    (finish-output o)))))))
    (print :sleep)
    (sleep 1)
    (print :int)
    (iolib.syscalls:kill (iolib.os:process-pid p) iolib.syscalls:sigint)
    (print :stop)
    (iolib.syscalls:kill (iolib.os:process-pid p) iolib.syscalls:sigstop)
    (sleep 1)
    (print :cont)
    (iolib.syscalls:kill (iolib.os:process-pid p) iolib.syscalls:sigcont)    
    (iolib.os:process-status p :wait t)
    (iolib.common-lisp:close p))
  (print :done))

