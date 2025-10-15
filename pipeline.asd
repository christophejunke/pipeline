(defsystem :pipeline
  :depends-on ("alexandria"
               "foldenv"
               "access"
               "dbus"
               "trivial-channels"
               "cl-ppcre"
               "osicat")
  :components ((:file "packages")
               (:file "unix-pipes")
               (:file "signals")
               (:file "pipes")
               (:file "filters")
               (:file "pipeline")
               (:file "builtins")
               (:file "nmcli")
               (:file "nmcli-dbus"))
  :serial t)


