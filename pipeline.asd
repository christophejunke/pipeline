(defsystem :pipeline
  :depends-on (:alexandria
               :foldenv
               :trivial-channels
               :cl-ppcre
               :osicat
               )
  :components ((:file "packages")
               (:file "unix-pipes")
               (:file "signals")
               (:file "pipes")
               (:file "filters")
               (:file "pipeline")
               (:file "utils")
               (:file "builtins")
               (:file "nmcli"))
  :serial t)


