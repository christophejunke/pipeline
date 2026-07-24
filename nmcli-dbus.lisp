(defpackage :nmcli-dbus
  (:export #:connections)
  (:import-from #:pipeline.nmcli #:connection%)
  (:use :foldenv.local-keywords)
  (:use :foldenv)
  (:use :access)
  (:use :ppcre)
  (:use :dbus)
  (:use :cl))

(in-package :nmcli-dbus)

(defvar *dbus* nil)

(defun extend (string delimiter old)
  (let ((split (split delimiter string)))
    (cond
      ((not split) old)
      ((string= "" (car split)) (reverse (rest split)))
      (t (append (reverse split) old)))))

(define-local-keyword .address
  (:documentation "D-Bus address :session, :system, or an address")
  (:variables address))

(define-local-keyword .interface
  (:documentation "D-Bus interface (defaults to the service name)")
  (:variables interface))

(define-local-keyword .bus
  (:documentation "D-Bus bus instance")
  (:variables bus))

(define-local-keyword .service
  (:documentation "Dot-separated string naming the destination service")
  (:variables service))

(define-local-keyword .path
  (:documentation "Slash-separated path (defaults to path derived from service)")
  (:variables path))

(defun .service ()
  (env *dbus* .service))

(defun .bus ()
  (env *dbus* .bus))

(defun service-components (&optional env)
  "Components of a dot-separated string in reverse order"
  (extend (env env .service) #\. nil))

(defmethod fold-for-key ((key (eql .interface)) old new)
  (etypecase new
    (string (extend new #\. (service-components)))
    (null old)))

(defmethod fold-for-key ((key (eql .path)) old new)
  (etypecase new
    ((eql .service)
     (service-components))
    (string (extend new #\/ (or old (service-components))))
    (null old)))

(defun .path (&optional string)
  (with-environment *dbus* (.path string)
    (environment-bind (path) *dbus*
      (format nil
              "/~{~a~^/~}"
              (reverse (or path (service-components *dbus*)))))))

(defun .interface (&optional string)
  (with-environment *dbus* (.interface string)
    (environment-bind (interface service) *dbus*
      (format nil "~{~a~^.~}" (reverse (or interface service))))))

(defun resolve-bus-address (address)
  (ecase address
    (:system (system-server-addresses))
    (:session (session-server-addresses))
    (t (parse-server-addresses-string address))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro invoke ((interface name) &rest args)
    (let ((o (gensym)))
      `(with-introspected-object (,o (.bus) (.path) (.service))
         (,o (.interface ,interface) ,name ,@args))))
  (defmacro with-dbus-options ((&rest options) &body body)
    `(with-environment *dbus* ,options ,@body))
  (defmacro with-open-dbus (address (&rest options) &body body)
    (let (($bus (gensym)) ($address (gensym)))
      `(let ((,$address (resolve-bus-address ,address)))
         (with-open-bus (,$bus ,$address)
           (with-dbus-options (.address ,$address .bus ,$bus ,@options)
             ,@body))))))

(defun nm-list-connections (&optional path)
  (with-dbus-options (.path path)
    (invoke ("Settings" "ListConnections"))))

(defun nm-connection-settings (&optional path)
  (with-dbus-options (.path path)
    (invoke ("Settings.Connection" "GetSettings"))))

(defun @ (dbus-alist name)
  (accesses dbus-alist name #'first))

(defun connections ()
  (with-open-dbus :system (.service "org.freedesktop.NetworkManager")
    (loop
      :for path :in (nm-list-connections "Settings")
      :for connection := (@ (nm-connection-settings path) "connection")
      :collect (connection% (@ connection "id")
                            (@ connection "uuid")
                            (@ connection "type")))))

(defun gt/create-instance ()
  (with-dbus-options (.path "Factory0")
    (invoke ("Factory0" "CreateInstance") '())))

(defun gt/screen-exec (screen-path)
  (with-dbus-options (.path screen-path)
    (invoke ("Terminal0" "Exec")
            '(("shell" ((:boolean) t)))
            (list))))

(defun gnome-terminal ()
  (with-open-dbus :session (.service "org.gnome.Terminal")
    (gt/screen-exec (gt/create-instance))))
