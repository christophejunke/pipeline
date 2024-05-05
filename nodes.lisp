(defpackage :nodes (:use :cl))
(in-package :nodes)

(defclass has-tags () 
  ((.tags :initarg :tags :initform nil :accessor .tags)))

(defclass has-name ()
  ((.name :initargs :port :initform nil :accessor .name)))

(defclass has-ports ()
  ((.ports :initargs :port :initform nil :accessor .ports)))

(defclass has-options ()
  ((.options :initargs :port :initform nil :accessor .options)))

;; options



;; tagging things

(defgeneric add-tags (object &rest tags)
  (:method ((o has-tags) &rest tags)
    (setf (.tags o) (delete-duplicates (union (.tags o) tags)))))

(defgeneric del-tags (object &rest tags)
  (:method ((o has-tags) &rest tags)
    (setf (.tags o) (delete-duplicates (set-difference (.tags o) tags)))))

;; ports

(defclass port (has-name 
                has-tags
                has-options) 
  ())

;; tubes

(defclass link (has-name
                has-tags
                has-options) 
  ())

;; nodes

(defclass node (has-name
                has-tags
                has-ports
                has-options)
  ())



