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
           #:with-pipes%
           #:with-pipes
           #:pipe
           #:pipe-in
           #:pipe-out
           #:ensure-stream-closed/no-error))

(defpackage :pipeline.filters
  (:documentation "")
  (:use :cl)
  (:import-from #:pipeline.pipes
                #:ensure-stream-closed/no-error)
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
           #:program
           #:program*
           #:error-to-output
           #:*environment*
           #:call-with-augmented-environment
           #:with-augmented-environment))

(defpackage :pipeline
  (:documentation "")
  (:use #:cl
        #:pipeline.pipes
        #:pipeline.filters)
  (:import-from #:alexandria
                #:with-gensyms
                #:once-only)
  (:export #:program
           #:*in*
           #:*out*
           #:%
           #:*environment*
           #:call-with-augmented-environment
           #:with-augmented-environment
           #:program*
           #:redirecting-result-to
           #:with-pipeline
           #:signal-fold
           #:signal-result
           #:signal-named-result
           #:channel-of
           #:program
           #:keep-regex
           #:tee
           #:with-read-loop
           #:map-lines
           #:each-line
           #:do-lines
           #:lambda-line
           #:lambda-line-print
           #:lambda-form-print
           #:each-form
           #:lambda-form
           #:tee/error
           #:broadcast
           #:read-form
           #:feed
           #:from-file
           #:to-file
           #:line-collector
           #:reservoir-sampling
           #:uniq
           #:print-sequence
           #:signal-each-line
           #:reservoir-sampling-pipe
           #:sequence-emitter
           
           #:foldenv
           #:named-results
           #:results))

(defpackage :pipeline.tests
  (:use :cl :pipeline)
  (:import-from #:sb-thread
                #:make-thread
                #:join-thread)
  (:import-from #:sb-ext
                #:run-program))
