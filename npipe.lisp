(ql:quickload :iolib)
(delete-package :npipe)
(defpackage :npipe (:use :cl :iolib))
(in-package :npipe)

(ql:quickload :iolib/os)

;; create process

;; BLUEPRINT

;; process-group
;;
;; a <- fifo
;; b <- fifo
;; x <- closure
;;
;; e <- process(:stdin, a.w)
;; f <- process/gid(e.pid, a.r, b.w)
;; g <- thread(x, b.r, :stdout)
;;
;; waitpid loop : waitpid(-gid)
;; observe statuses: call status hooks
;; watch(e)
;; wait(g)

(defparameter *p* (iolib.os:create-process '("/bin/ls") :stdin nil :stdout :pipe :stderr nil))
(defparameter *q* (iolib.os:create-process '("/usr/bin/wc") :stdin (iolib/os:process-stdout *p*) :stdout :pipe :stderr nil))

(defparameter *q* nil)

(iolib.os::slurp-char-stream (iolib.os:process-stdout *q*))

(iolib/os:process-kill *p*)

(setf *q* (iolib/os:create-process '("/usr/bin/wc" "-l") :stdin (iolib/os:process-stdin *p*) :stdout #P"/tmp/test"))

(iolib/os:run-program )
(get-output-stream-string out)

