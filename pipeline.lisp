(in-package :pipeline)

(defgeneric ensure-stream (stream direction output-if-error)
  (:method ((s stream) d o) s)
  (:method ((s (eql nil)) d o)
    (ecase d
      (:input (register-resource
               (make-concatenated-stream)))
      ((:error :output) (register-resource
                         (make-broadcast-stream)))))
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
(defmacro with-resource-handler ((&optional handler) &body body)
  (unless handler
    (setf handler '(make-instance 'simple-handler-mixin)))
  (alexandria:once-only (handler)
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
  ((is :initarg :input :accessor .input)
   (os :initarg :output :accessor .output)
   (es :initarg :error :accessor .error)
   (filters :initarg :filters :reader .filters)))

(defun fresh-string-stream ()
  (let ((stream (make-string-output-stream)))
    (prog1 stream
      (handler-bind ((error (lambda (c)
                              (declare (ignore c))
                              (close stream))))
        (register-resource `(:stream ,stream))))))

(defun %pipe-arg-input (input)
  (etypecase input
    ((eql nil) (make-concatenated-stream))
    ((eql t) *standard-input*)
    (stream input)))

(defun %pipe-arg-output (output)
  (etypecase output
    ((eql :string) (fresh-string-stream))
    ((eql nil) (register-resource
                (make-broadcast-stream)))
    ((eql t) *standard-output*)
    (stream output)))

(defun %pipe-arg-error (error-stream output-stream)
  (etypecase error-stream
    ((eql :string) (fresh-string-stream))
    ((eql :output) output-stream)
    ((eql nil) (register-resource
                (make-broadcast-stream)))
    ((eql t) *error-output*)
    (stream error-stream)))

(defun make-pipeline (input output error filters)
  (make-instance 'pipeline
                 :input input
                 :output output
                 :error error
                 :filters (coerce filters 'simple-vector)))

(defclass active-pipeline (simple-handler-mixin pipeline)
  ((pipeline :reader .pipeline :initarg :pipeline)
   (open :initform nil :accessor .open)))

(defmethod print-object ((p active-pipeline) stream)
  (print-unreadable-object (p stream :type t :identity t)
    (format stream "~:[CLOSED~;OPEN~]" (.open p))))

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

(defun get-output-string (pipeline)
  (get-output-stream-string (.output pipeline)))

(defun %execute-pipeline (pipeline)
  (check-type pipeline pipeline)
  (let ((active (make-instance 'active-pipeline 
                               :pipeline pipeline
                               :input (.input pipeline)
                               :output (.output pipeline)
                               :error (.error pipeline)
                               :filters (.filters pipeline))))
    (prog1 active
      (with-resource-handler (active)
        (open-pipeline active)
        (close-pipeline active)))))

(defmethod open-pipeline progn ((pipeline active-pipeline)
                                &aux (length (length (.filters pipeline))))
  (setf (.input pipeline)
        (%pipe-arg-input (.input pipeline)))
  (setf (.output pipeline)
        (%pipe-arg-output (.output pipeline)))
  (setf (.error pipeline)
        (%pipe-arg-error (.error pipeline)
                         (.output pipeline)))
  (case length
    (0)
    (1 (spawn (aref (.filters pipeline) 0)
              :input (.input pipeline)
              :output (.output pipeline)
              :error (register-resource
                      (make-broadcast-stream
                       (.error pipeline)))
              :last t
              :first t
              :wait t))
    (t 
     (let* ((size (1- length))
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
                               :error (register-resource
                                       (make-broadcast-stream
                                        (.error pipeline)))
                               :last last
                               :first first
                               :wait wait)
         :do (register-resource spawned))))))

(defmethod cleanup-resource (_ (p pipeline.pipes:pipe))
  (pipeline.pipes:clean-pipe p))

(defmethod cleanup-resource (_ (s stream))
  (handler-case (close s :abort t)
    (error (e) (warn "error while closing stream ~s: ~a" s e))))

(defgeneric cleanup-resource-tag (pipeline tag value))

(defmethod cleanup-resource (pipeline (tagged cons))
  (destructuring-bind (tag value) tagged
    (cleanup-resource-tag pipeline tag value)))

(defmethod cleanup-resource-tag (_p (_t (eql :thread)) v)
  (sb-thread:join-thread v :default nil))

(defmethod cleanup-resource-tag (_p (_t (eql :process)) v)
  (when (sb-ext:process-alive-p v)
    (sb-ext:process-wait v))
  (sb-ext:process-close v))

(defmethod cleanup-resource-tag (_p (_t (eql :funcall)) v))

(defmethod cleanup-resource-tag (p (_t (eql :stream)) v)
  (close v :abort t))

(defmacro with-pipeline ((&key input (output t) (error :output)
                               channel (trace nil) (env nil))
                         &body filters)
  "#<WITH-PIPELINE DOCUMENTATION>

Set CHANNEL to T to capture results reported by SIGNAL-RESULT and return them
eventually as a list.
"
  (flet ((common-code (&key (redirect nil rp))
           (assert rp () "explicit call only")
           `(%execute-pipeline
             (make-pipeline ,input
                            ,output
                            ,error
                            ,(if redirect
                                 `(let ((*%channel%* ,redirect))
                                    (list ,@filters))
                                 `(list ,@filters)))))
         (maybe-trace (expr)
           (if trace
               (let ((traced (if (consp trace)
                                 trace
                                 '(spawn cleanup-resource-tag cleanup-resource
                                   make-pipe open-pipeline close-pipeline
                                   ensure-stream))))
                 `(prog2
                      (trace ,@traced)
                      (unwind-protect ,expr
                        (untrace ,@traced))))
               expr)))
    (maybe-trace
     `(block nil
        (with-augmented-environment ,env
          ,(etypecase channel
             (null
              (common-code :redirect nil))
             ((eql t)
              (with-gensyms (channel condition)
                `(let ((,channel (trivial-channels:make-channel)))
                   (handler-bind ((pipeline-result 
                                    (lambda (,condition)
                                      (trivial-channels:sendmsg
                                       ,channel ,condition)
                                      (invoke-restart 'continue))))
                     ,(common-code :redirect channel))
                   (all-channel-values ,channel))))
             (symbol
              (common-code :redirect channel))))))))

;; (trace cleanup-resource register-resource open-pipeline close-pipeline)

(defun redirecting-result-to (channel function &key name)
  (lambda ()
    (handler-bind ((pipeline-result
                     (lambda (c)
                       (trivial-channels:sendmsg
                        channel
                        (make-condition 'pipeline-result
                                        :name (or (.name c) name)
                                        :result (.result c)))
                       (invoke-restart 'continue))))
      (funcall function))))

(defvar *%channel%*)

(defmacro channeling-as (name &body body)
  `(redirecting-result-to *%channel%* (lambda () ,@body) :name ,name))

(defun all-channel-values (channel &aux (h (make-hash-table)) (anonymous nil))
  (flet ((visit (name result)
           (if name
               (push result (gethash name h))
               (push result anonymous))))
    (loop
      for m = (trivial-channels:getmsg channel)
      while m
      do (typecase m
           (pipeline-result
            (visit (.name m) (.result m)))
           (t (visit nil m)))
      finally
         (return
           (values (nreverse anonymous)
                   (alexandria:hash-table-plist h))))))

(defun execute (&rest args)
  (destructuring-bind (keyword process)
      (with-pipeline (:error *error-output*)
        (apply #'program args))
    (assert (eq :process keyword))
    (zerop (sb-ext:process-exit-code process))))
