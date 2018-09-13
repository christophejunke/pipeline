(defpackage :pipeline.unix-pipes
  (:documentation "")
  (:use :cl)
  (:import-from #:sb-unix
                #:unix-pipe
                #:unix-close)
  (:import-from #:sb-sys
                #:make-fd-stream)
  (:import-from #:alexandria
                #:with-gensyms)

  (:export #:with-fd-streams
           #:with-unix-pipe))

(defpackage :pipeline.pipes
  (:documentation "")
  (:use :cl)
  (:import-from #:sb-unix
                #:unix-pipe
                #:unix-close)

  (:import-from #:sb-sys
                #:make-fd-stream)

  (:import-from #:alexandria
                #:with-gensyms)

  (:export #:make-pipe 
           #:make-pipes
           #:clean-pipe
           #:clean-pipes
           #:with-pipes
           #:pipe-in
           #:pipe-out
           #:ensure-stream-closed/no-error
           #:with-auto-closing-streams))

(defpackage :pipeline.filters
  (:documentation "")
  (:use :cl)
  (:import-from #:pipeline.pipes
                #:ensure-stream-closed/no-error
                #:with-auto-closing-streams)
  (:import-from #:sb-thread
                #:make-thread
                #:join-thread)
  (:import-from #:sb-ext
                #:run-program
                #:process-wait
                #:process-alive-p
                #:process-input
                #:process-output
                #:process-error)
  (:export #:spawn
           #:clean
           #:clean/tag
           #:program))

(defpackage :pipeline
  (:documentation "")
  (:use :cl :pipeline.pipes :pipeline.filters)
  (:import-from #:alexandria
                #:with-gensyms
                #:once-only)
  (:export #:with-pipeline
           #:program))

(defpackage pipeline.builtins
  (:documentation "")
  (:use :cl :pipeline.filters)
  (:export #:tee
           #:tee/error))

(defpackage :pipeline.tests
  (:use :cl :pipeline :pipeline.builtins)
  (:import-from #:sb-thread
                #:make-thread
                #:join-thread)
  (:import-from #:sb-ext
                #:run-program))

