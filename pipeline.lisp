(in-package :pipeline)

(defgeneric ensure-stream (stream direction output-if-error)
  (:method ((s stream) d o) s)
  (:method ((s (eql nil)) d o)
    (ecase d
      (:input  (make-concatenated-stream))
      ((:error :output) (make-broadcast-stream))))
  (:method ((s (eql t)) direction o)
    (ecase direction
      (:input  *standard-input*)
      (:output *standard-output*)
      (:error  *error-output*)))
  (:method ((s (eql :output)) (d (eql :error)) output)
    output))

(defvar *handler*)
(defgeneric exit-handler (handler))
(defgeneric handler-register-resource (handler resource))
(defun register-resource (resource)
  (handler-register-resource *handler* resource))
(defmacro with-resource-handler ((handler) &body body)
  (metatilities:once-only (handler)
    `(let ((*handler* ,handler))
       (unwind-protect (progn ,@body)
         (exit-handler ,handler)))))

(defgeneric cleanup-resource (handler resource))

(defclass simple-handler-mixin ()
  ((resources :initform nil :accessor .resources)))

(defmethod handler-register-resource ((h simple-handler-mixin) resource)
  (prog1 resource
    (pushnew resource (.resources h))))

(defmethod exit-handler ((handler simple-handler-mixin))
  (with-simple-restart (ignore "Ignore all remaining resources")
    (loop
      while (.resources handler)
      for resource = (pop (.resources handler))
      do (with-simple-restart (accept "Treat current resource as cleaned up")
           (cleanup-resource handler resource)))))

(defclass pipeline ()
  ((is :initarg :input :reader .input)
   (os :initarg :output :reader .output)
   (es :initarg :error :reader .error)
   (filters :initarg :filters :reader .filters)))

(defun %pipe-arg-input (input)
  (etypecase input
    ((eql nil) (make-concatenated-stream))
    ((eql t) *standard-input*)
    (stream input)))

(defun %pipe-arg-output (output)
  (etypecase output
    ((eql nil) (make-broadcast-stream))
    ((eql t) *standard-output*)
    (stream output)))

(defun %pipe-arg-error (error-stream output-stream)
  (etypecase error-stream
    ((eql :output) output-stream)
    ((eql nil) (make-broadcast-stream))
    ((eql t) *error-output*)
    (stream error-stream)))

(defun make-pipeline (input output error filters)
  (let* ((inp (%pipe-arg-input input))
         (out (%pipe-arg-output output))
         (err (%pipe-arg-error error out)))
    (make-instance 'pipeline
                   :input inp
                   :output out
                   :error err
                   :filters (coerce filters 'simple-vector))))

(defclass active-pipeline (simple-handler-mixin)
  ((pipeline :reader .pipeline :initarg :pipeline)
   (open :initform nil :accessor .open)))

(defun make-active-pipeline (pipeline)
  (make-instance 'active-pipeline :pipeline pipeline))

(defgeneric open-pipeline (pipeline)
  (:method-combination progn :most-specific-last)
  (:method progn (pipeline) pipeline)
  (:method :around ((pipeline active-pipeline))
    (prog2 (assert (not (.open pipeline)))
        (call-next-method)
      (setf (.open pipeline) t))))

(defgeneric close-pipeline (pipeline)
  (:method-combination progn :most-specific-first)
  (:method progn (pipeline) t)
  (:method :around ((pipeline active-pipeline))
    (prog2 (assert (.open pipeline))
        (call-next-method)
      (setf (.open pipeline) nil))))

(defun %execute-pipeline (pipeline)
  (check-type pipeline pipeline)
  (let ((pipeline (make-active-pipeline pipeline)))
    (unwind-protect (with-resource-handler (pipeline)
                      (open-pipeline pipeline)
                      (close-pipeline pipeline)))))

(defun execute-pipeline (pipeline &key on-result)
  (check-type pipeline pipeline)
  (labels ((call (c) (funcall on-result (.result c)))
           (run ()
             (let ((pipeline (make-active-pipeline pipeline)))
               (unwind-protect (with-resource-handler (pipeline)
                                 (open-pipeline pipeline)
                                 (close-pipeline pipeline))))))
    (if on-result
        (handler-bind ((pipeline-result #'call))
          (run))
        (run))))

(defmethod open-pipeline progn ((pipeline active-pipeline))
  (let* ((pipeline (.pipeline pipeline))
         (size (1- (length (.filters pipeline))))
         (pipes (make-pipes size)))
    (declare (type simple-vector pipes))
    (map () #'register-resource pipes)
    (loop
      :for filter :across (.filters pipeline)
      :for p-in = (.input pipeline)
      :then (pipe-in (svref pipes index))
      :for index :upfrom 0
      :for first = t :then nil
      :for last = (= index size)
      :for wait = last
      :for p-out = (if last
                       (.output pipeline)
                       (pipe-out (svref pipes index)))
      ;; may block until done
      :for spawned = (spawn filter
                            :input p-in
                            :output p-out
                            :error (make-broadcast-stream (.error pipeline))
                            :last last
                            :first first
                            :wait wait)
      :do (unless wait
            (register-resource spawned)))))

(defmethod cleanup-resource ((_ active-pipeline) (p pipeline.pipes:pipe))
  (pipeline.pipes:clean-pipe p))

(defmethod cleanup-resource ((_ active-pipeline) (spawned cons))
  (destructuring-bind (tag value) spawned
    (ecase tag
      (:thread (sb-thread:join-thread value))
      (:process (sb-ext:process-wait value)))))

(define-condition pipeline-result ()
  ((result :initarg :result :reader .result)))

(defun signal-result (value)
  (signal 'pipeline-result :result value))

(defun emitter (&key (key #'identity)
                     (test #'identity))
  (lambda-line (line)
    (when (funcall test line)
      (signal-result (funcall key line))
      (write-line line))))

(defparameter *test-pipeline*
  (make-pipeline nil
                 t
                 :output
                 (list (program "/bin/ls" "-l" "/tmp/")
                       (lambda-line (line)
                         (when (find #\a line)
                           (write-line line)))
                       (program "/usr/bin/wc" "-l")
                       (emitter :key #'parse-integer))))

(defmacro push-to (place &aux (v (gensym)))
  `(lambda (,v) (push ,v ,place)))

(defmacro setf-to (place &aux (v (gensym)))
  `(lambda (,v) (setf ,place ,v)))

;; asynchronous results in threads

(let ((result))
  (execute-pipeline *test-pipeline* :on-result (push-to result))
  result)

;; (untrace)
;; (trace cleanup-resource register-resource open-pipeline close-pipeline)

(defvar *%channel%*)

(defmacro with-pipeline ((&key input (output t) (error :output)
                               channel)
                         &body filters)
  (flet ((common-code (&key (redirect nil rp))
           (assert rp () "explicit call only")
           `(%execute-pipeline
             (make-pipeline ,input
                            ,output
                            ,error
                            ,(if redirect
                                 `(let ((*%channel%* ,redirect))
                                    (list ,@filters))
                                 `(list ,@filters))))))
    (etypecase channel
      (null
       (common-code :redirect nil))
      ((eql t)
       (with-gensyms (channel)
         `(let ((,channel (trivial-channels:make-channel)))
            ,(common-code :redirect channel)
            (all-channel-values ,channel))))
      (symbol
       (common-code :redirect channel)))))

(defun redirecting-result-to (channel function &key name)
  (lambda ()
    (handler-bind ((pipeline-result
                     (lambda (c)
                       (let ((v (if name (cons name (.result c)) (.result c))))
                         (trivial-channels:sendmsg channel v)))))
      (funcall function))))

(defmacro channeling-as (name &body body)
  `(redirecting-result-to *%channel%* (lambda () ,@body) :name ,name))

(defun all-channel-values (channel)
  (loop
    with h = (make-hash-table)
    for m = (trivial-channels:getmsg channel)
    while m
    if (consp m)
    do (destructuring-bind (k . v) m (push v (gethash k h)))
    else collect m into anonymous
    finally
       (return
         (values anonymous
                 (alexandria:hash-table-plist h)))))

(defun keep-regex (regex)
  (let ((scanner (ppcre:create-scanner regex)))
    (lambda-line (line)
      (when (ppcre:scan scanner line)
        (write-line line)))))

(defun find-suspicious (directory)
  (with-pipeline (:error nil)
    (program "find" (namestring (truename directory)))
    (keep-regex '(:sequence #\( (:regex "\\d+") #\)))
    (lambda ()
      (write-line "<suspicious>")
      (do-lines (line)
        (alexandria:when-let (f (probe-file line))
          (format t
                  "~&  <file name=~s/>"
                  (namestring f))))
      (fresh-line)
      (write-line "</suspicious>"))
    (program "tidy" "-xml" "-i")))

;; (find-suspicious "~/download/")

;; (defmacro with-pipeline ((&key input (output t) (error :output))
;;                          &body processors)
;;   (flet ((make-spawn (com in out err wait first last)
;;            `(spawn ,com
;;                    :input ,in
;;                    :output ,out
;;                    :error ,err
;;                    :wait ,wait
;;                    :first ,first
;;                    :last ,last)))
;;     (let ((size (length processors)))
;;       (with-gensyms (input% output% error%)
;;         `(catch :pipeline
;;            (symbol-macrolet ((*in* *standard-input*)
;;                              (*out* *standard-output*))
;;              (macrolet ((% (&rest f) `(lambda () ,@f)))
;;                (block nil
;;                  (let* ((,input%  (ensure-stream ,input  :input  nil))
;;                         (,output% (ensure-stream ,output :output nil))
;;                         (,error%  (ensure-stream ,error  :error  ,output%)))
;;                    ,(case size
;;                       (0 nil)
;;                       (1 (make-spawn (first processors) input% output% error% t t t))
;;                       (t
;;                        (with-gensyms (pipes)
;;                          (let ((bindings (loop
;;                                             for p in processors
;;                                             collect (list (gensym) p))))
;;                            `(let ,bindings
;;                               (with-pipes% (,pipes ,(1- size))
;;                                 (alexandria:lastcar
;;                                  (mapcar
;;                                   #'clean
;;                                   (list
;;                                    ,@(loop
;;                                         for p-in = input% then `(pipe-in
;;                                                                  (svref ,pipes
;;                                                                         ,index))
;;                                         for index from 0
;;                                         for (var . rest) on (mapcar #'first bindings)
;;                                         for lastp = (not rest)
;;                                         for p-out = (if lastp output%
;;                                                         `(pipe-out
;;                                                           (svref ,pipes
;;                                                                  ,index)))
;;                                         collect (make-spawn var
;;                                                             p-in
;;                                                             p-out
;;                                                             error%
;;                                                             lastp
;;                                                             (= index 0)
;;                                                             lastp))))))))))))))))))))

(defun execute (&rest args)
  (destructuring-bind (keyword process)
      (with-pipeline (:error *error-output*)
        (apply #'program args))
    (assert (eq :process keyword))
    (zerop (sb-ext:process-exit-code process))))
