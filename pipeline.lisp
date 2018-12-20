(in-package :pipeline)

(defgeneric ensure-stream (stream direction output-if-error)
  (:method ((s stream) d o) s)
  (:method ((s (eql nil)) d o) (make-broadcast-stream))
  (:method ((s (eql t)) direction o)
    (ecase direction
      (:input  *standard-input*)
      (:output *standard-output*)
      (:error  *error-output*)))
  (:method ((s (eql :output)) (d (eql :error)) output)
    output))

(defmacro with-pipeline ((&key input (output t) (error :output))
                         &body processors)
  (flet ((make-spawn (com in out err wait)
           `(spawn ,com
                   :input ,in
                   :output ,out
                   :error ,err
                   :wait ,wait)))
    (let ((size (length processors)))
      (with-gensyms (input% output% error%)
        `(catch :pipeline
           (block nil
             (let* ((,input%  (ensure-stream ,input  :input  nil))
                    (,output% (ensure-stream ,output :output nil))
                    (,error%  (ensure-stream ,error  :error  ,output%)))
               ,(case size
                  (0 nil)
                  (1 (make-spawn (first processors) input% output% error% t))
                  (t
                   (with-gensyms (pipes)
                     (let ((bindings (loop
                                       for p in processors
                                       collect (list (gensym) p))))
                       `(let ,bindings
                          (with-pipes% (,pipes ,(1- size))
                            (alexandria:lastcar
                             (mapcar
                              #'clean
                              (list
                               ,@(loop
                                   for p-in = input% then `(pipe-in
                                                            (svref ,pipes
                                                                   ,index))
                                   for index from 0
                                   for (var . rest) on (mapcar #'first bindings)
                                   for lastp = (not rest)
                                   for p-out = (if lastp output%
                                                   `(pipe-out
                                                     (svref ,pipes
                                                            ,index)))
                                   collect (make-spawn var
                                                       p-in
                                                       p-out
                                                       error%
                                                       lastp))))))))))))))))))

(defun execute (&rest args)
  (destructuring-bind (keyword process)
      (with-pipeline (:error *error-output*)
        (apply #'program args))
    (assert (eq :process keyword))
    (zerop (sb-ext:process-exit-code process))))
