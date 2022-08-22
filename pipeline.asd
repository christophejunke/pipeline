(defsystem :pipeline
  :depends-on (:alexandria :bricabrac :trivial-channels :cl-ppcre)
  :components ((:file "packages")
               (:file "unix-pipes")
               (:file "signals")
               (:file "pipes")
               (:file "filters")
               (:file "pipeline")
               (:file "builtins")
               (:file "nmcli"))
  :serial t)


