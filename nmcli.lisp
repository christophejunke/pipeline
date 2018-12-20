(defpackage #:pipeline.nmcli
  (:use #:cl #:alexandria #:cl-ppcre #:pipeline)
  (:nicknames :nmcli)
  (:export #:connections
           #:device-type
           #:uuid
           #:name))

(in-package #:pipeline.nmcli)

(defstruct
    (connection
      (:constructor connection (&key ((:type device-type)) name uuid))
      (:conc-name))
  name uuid device-type)

(defvar *device-types*
  (plist-hash-table
   '("802-11-wireless" :wifi
     "wifi"            :wifi
     "802-3-ethernet"  :ethernet
     "ethernet"        :ethernet)
   :test #'equal))

(declaim (inline unspaced-slice))

(defun unspaced-slice (string start end)
  (setf end (or end (length string)))
  (flet ((non-space-position (from-end)
           (position #\space string
                     :from-end from-end
                     :test-not #'char=
                     :start start
                     :end end)))
    (let* ((start (non-space-position nil))
           (end (and start (non-space-position t))))
      (if end (make-array (- (1+ end) start)
                          :element-type
                          (array-element-type string)
                          :displaced-to string
                          :displaced-index-offset start)
          ""))))

(defun connections (&aux (result (make-array 64 :fill-pointer 0 :adjustable t)))
  (prog1 result
    (let ((*unix-environment* '("LANG_ALL=C")))
      (with-pipeline ()
        (program "nmcli" "--fields" "name,uuid,type" "connection")
        (lambda-line (line)
          (multiple-value-bind (uuid-beg uuid-end) (scan "[0-9a-f-]{36}" line)
            (when uuid-beg
              (vector-push-extend
               (connection :name (unspaced-slice line 0 uuid-beg)
                           :uuid (unspaced-slice line uuid-beg uuid-end)
                           :type (let ((dt (unspaced-slice line uuid-end nil)))
                                   (gethash dt *device-types* dt)))
               result
               (array-total-size result))))))))))
