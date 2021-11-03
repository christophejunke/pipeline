(defsystem :pipeline
  :depends-on (:alexandria :trivial-channels :cl-ppcre)
  :components ((:file "packages")
               (:file "unix-pipes")
               (:file "pipes")
               (:file "filters")
               (:file "pipeline")
               (:file "builtins")
               (:file "nmcli"))
  :serial t)


