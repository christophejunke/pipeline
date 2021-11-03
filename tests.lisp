(in-package :pipeline.tests)

(ql:quickload :cl-interpol)
(cl-interpol:enable-interpol-syntax)

(defun home ()
  (sb-posix:getenv "HOME"))

(with-pipeline (:error nil)
  (program "ls" (namestring (merge-pathnames "bin/" (user-homedir-pathname))))
  (program "sed" "s/a/aaaa/")
  (tee/error)
  (program "wc" "-c")
  #'read)

#+cl-ppcre
(defun program* (string)
  (apply #'program (ppcre:split '(:greedy-repetition 1 nil #\space))))


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

;; (let ((result))
;;   (execute-pipeline *test-pipeline* :on-result (push-to result))
;;   result)

;; (untrace)

;; (with-pipeline (:error :string)
;;   (program "ls" "/tmp")
;;   (program "sort" "-Q")
;;   (lambda-line (line)
;;     (write-line line *error-output*)))

(let ((res (make-array 256
                       :initial-element 0 
                       :element-type '(unsigned-byte 8)
                       :fill-pointer 0)))
  (prog1 res
    (with-pipeline (:output nil)
      (program "head" "-c" (array-total-size res) "/dev/urandom")
      (program "hexdump" "-v" "-e" "/1 \"%02X\\n\"")
      (lambda-line (line)
        (vector-push (parse-integer line :radix 16) res)))))
