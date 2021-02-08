(in-package :pipeline)

(defun program-reader (end-delimiter)
  (lambda (stream char)
    (declare (ignore char))
    (list* 'program
           (ppcre:split '(:alternation
                          :start-anchor
                          :end-anchor
                          (:greedy-repetition 1 nil #\space))
                        (coerce (loop
                                   for c = (read-char stream t nil t)
                                   until (char= end-delimiter c)
                                   collect c)
                                'string)))))

(defun enable-program-syntax
    (&optional
       (start-delimiter #\&)
       (end-delimiter #\#)
       (readtable *readtable*))
  (set-macro-character start-delimiter
                       (program-reader end-delimiter)
                       nil
                       readtable))
