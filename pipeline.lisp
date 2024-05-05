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
   (mapping :initarg :mapping :accessor .mapping)
   (channel :initarg :channel :accessor channel-of :initform nil)
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

(defun make-pipeline (input output error redirect mapping filters)
  (make-instance 'pipeline
                 :input input
                 :output output
                 :mapping mapping
                 :error error
                 :channel redirect
                 :filters (coerce filters 'simple-vector)))

(defclass active-pipeline (simple-handler-mixin pipeline)
  ((pipeline :reader .pipeline :initarg :pipeline)
   (open :initform nil :accessor .open)
   (foldenv :initform nil :accessor foldenv)
   (named-results :initform (make-hash-table :test #'equalp)
                  :accessor named-results)
   (results :initform nil :accessor results)
   (id :initform (gensym (string '#:PIPELINE)) :reader pipeline-id)))

(defmethod print-object ((p active-pipeline) stream)
  (print-unreadable-object (p stream :type t :identity t)
    (format stream "~A ~:[CLOSED~;OPEN~]" (pipeline-id p) (.open p))))

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
      (setf (.open pipeline) nil)
      (setf (channel-of pipeline) nil))))

(defun get-output-string (pipeline)
  (get-output-stream-string (.output pipeline)))

(defun %execute-pipeline (pipeline)
  (check-type pipeline pipeline)
  (let ((active (make-instance 'active-pipeline
                               :channel (channel-of pipeline)
                               :mapping (.mapping pipeline)
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

  (make-channel-collector pipeline)

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
              :pipeline pipeline
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
                               :pipeline pipeline
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

(defmethod cleanup-resource-tag (p (_t (eql :channel-thread)) v)
  (destructuring-bind (&key channel thread) v
    (trivial-channels:sendmsg channel :done)
    (cleanup-resource-tag p :thread thread)))

(defun make-channel-collector (pipeline
                               &aux
                               (p pipeline)
                               (channel (channel-of p)))
  (when channel
    (flet ((event-loop ()
             (loop
               for m = (trivial-channels:recvmsg channel)
               while m
               do (typecase m
                    ((eql :done)
                     (return))
                    (pipeline-fold
                     (setf (foldenv p)
                           (foldenv:fold-environments%
                            (foldenv p)
                            (list (.name m) (.result m))
                            (.mapping p))))
                    (pipeline-result
                     (if (.name m)
                         (push (.result m) (gethash (.name m) (named-results p)))
                         (push (.result m) (results p))))
                    (t (push m (results p)))))))
      (let ((name (format nil "channel-collector-~a" (pipeline-id pipeline))))
        (register-resource
         `(:channel-thread (:thread ,(bt:make-thread #'event-loop :name name)
                            :channel ,channel)))))))

(defmacro with-pipeline ((&key input (output t) (error :output)
                               (channel t) (trace nil) (env nil)
                               (mapping nil))
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
                            ,redirect
                            ,mapping
                            (list ,@filters))))
         (maybe-trace (expr)
           (if trace
               (let ((traced (if (consp trace)
                                 trace
                                 '(spawn cleanup-resource-tag cleanup-resource
                                   register-resource make-pipe open-pipeline close-pipeline
                                   ensure-stream %pipe-arg-error %pipe-arg-output
                                   %pipe-arg-input))))
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
              (with-gensyms (channel)
                `(let ((,channel (trivial-channels:make-channel)))
                    ,(common-code :redirect channel))))
             (symbol
              (common-code :redirect channel))))))))

;; (trace cleanup-resource register-resource open-pipeline close-pipeline)

(defun redirecting-result-to (channel function)
  (lambda ()
    (handler-bind ((pipeline-result
                     (lambda (c)
                       (trivial-channels:sendmsg channel c)
                       (invoke-restart 'continue))))
      (funcall function))))

(defvar *%channel%*)

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
           (list* t (nreverse anonymous)
                  (alexandria:hash-table-plist h))))))

(defun execute (&rest args)
  (destructuring-bind (keyword process)
      (with-pipeline (:error *error-output*)
        (apply #'program args))
    (assert (eq :process keyword))
    (zerop (sb-ext:process-exit-code process))))
