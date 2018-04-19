(in-package :pipeline)

(with-unix-pipe (rfd wfd)
  (with-pipe-streams ((in rfd) (out wfd))
    (mapc #'join-thread
          (list (make-thread #'reader :arguments (list in *standard-output*))
                (make-thread #'writer :arguments (list out))))))

(with-unix-pipe-streams (in0 out0)
  (with-unix-pipe-streams (in1 out1)
    (let ((threads
           (list (make-thread #'writer :arguments (list out0))
                 (make-thread #'reader :arguments (list in1 *standard-output*)))))
      (sb-ext:run-program "sed" '("s/5/<cinq>/g")
                          :search t
                          :input in0
                          :output out1
                          :wait nil
                          :status-hook (closer in0 out1))
      (mapc #'join-thread threads))))
