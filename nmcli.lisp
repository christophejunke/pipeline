(defpackage #:pipeline.nmcli
  (:use #:cl #:alexandria #:cl-ppcre #:pipeline)
  (:export #:connections))

(in-package #:pipeline.nmcli)

(defstruct (connection (:constructor connection)
                       (:conc-name))
  name uuid device-type)

(defvar *device-types*
  (plist-hash-table
   '("802-11-wireless" :wifi
     "802-3-ethernet" :ethernet)
   :test #'equal))

(defun parse-nmcli-triplet (line)
  (flet ((unspace (s) (string-trim " " s)))
    (multiple-value-bind (start end) (scan "[0-9a-f-]{36}" line)
      (when start
        (connection :name (unspace (subseq line 0 start))
                    :uuid (subseq line start end)
                    :device-type (let ((tt (unspace (subseq line end))))
                                   (gethash tt *device-types* tt)))))))

(defun connections ()
  (let ((connections) (*unix-environment* '("LANG_ALL=C")))
    (with-pipeline ()
      (program "nmcli" "--fields" "name,uuid,type" "connection")
      (lambda-line (line)
        (prog1 nil
          (when-let (connection (parse-nmcli-triplet line))
            (push connection connections)))))
    (coerce (nreverse connections) 'simple-vector)))
