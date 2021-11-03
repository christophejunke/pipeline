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
    ((eql nil) (make-broadcast-stream))
    ((eql t) *standard-output*)
    (stream output)))

(defun %pipe-arg-error (error-stream output-stream)
  (etypecase error-stream
    ((eql :string) (fresh-string-stream))
    ((eql :output) output-stream)
    ((eql nil) (make-broadcast-stream))
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
              :error (make-broadcast-stream (.error pipeline))
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
                               :error (make-broadcast-stream
                                       (.error pipeline))
                               :last last
                               :first first
                               :wait last)
         :do (unless wait
               (register-resource spawned)))))))

(defmethod cleanup-resource (_ (p pipeline.pipes:pipe))
  (pipeline.pipes:clean-pipe p))

(defgeneric cleanup-resource-tag (pipeline tag value))

(defmethod cleanup-resource (pipeline (tagged cons))
  (destructuring-bind (tag value) tagged
    (cleanup-resource-tag pipeline tag value)))

(defmethod cleanup-resource-tag (_p (_t (eql :thread)) v)
  (sb-thread:join-thread v :default nil))

(defmethod cleanup-resource-tag (_p (_t (eql :process)) v)
  (when (sb-ext:process-alive-p v)
    (sb-ext:process-wait v)))

(defmethod cleanup-resource-tag (p (_t (eql :stream)) v)
  (close v))

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
    `(block nil
       ,(etypecase channel
          (null
           (common-code :redirect nil))
          ((eql t)
           (with-gensyms (channel condition)
             `(let ((,channel (trivial-channels:make-channel)))
                (handler-bind ((pipeline-result 
                                 (lambda (,condition)
                                   (trivial-channels:sendmsg
                                    ,channel (.result ,condition))
                                   (invoke-restart 'continue))))
                  ,(common-code :redirect channel))
                (all-channel-values ,channel))))
          (symbol
           (common-code :redirect channel))))))

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
               (setf (gethash name h) result)
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
