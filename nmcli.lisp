(defpackage #:pipeline.nmcli
  (:use #:pipeline)
  (:use #:cl-ppcre)
  (:use #:alexandria)
  (:use #:cl)
  (:nicknames :nmcli)
  (:export #:connections
           #:device-type
           #:uuid
           #:name))

(in-package #:pipeline.nmcli)

(defstruct (connection
            (:constructor connection (&key ((:type device-type)) name uuid))
            (:constructor connection% (name uuid device-type))
            (:conc-name))
  name uuid device-type)

(defmethod print-object ((o connection) stream)
  (if *print-readably*
      (call-next-method)
      (format stream "#<~a connection ~a>" (device-type o) (name o))))

(define-parse-tree-synonym uuid (:regex "[0-9a-f-]{36}"))

(define-parse-tree-synonym separator #\COLON)

(define-parse-tree-synonym nmcli-splitter
    (:sequence separator (:register uuid) separator))

(defun connections ()
  "List of all connections known to network-manager"
  (let ((scanner (load-time-value (create-scanner 'nmcli-splitter))))
    (with-pipeline (:return :results)
      (command "nmcli --terse --fields name,uuid,type connection")
      (lambda-line (line)
        (let ((split (split scanner line :sharedp t :with-registers-p t)))
          (destructuring-bind (name uuid type) split
            (signal-result (connection% name uuid type))))))))

