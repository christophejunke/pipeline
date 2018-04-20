(defsystem :pipeline
  :depends-on (:alexandria)
  :components ((:file "packages")
               (:file "unix-pipes")
               (:file "pipes")
               (:file "pipeline")
               (:file "filters")
               (:file "builtins")))


