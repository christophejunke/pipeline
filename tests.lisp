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
