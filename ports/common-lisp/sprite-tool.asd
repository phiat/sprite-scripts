(defsystem :sprite-tool
  :version "0.1.0"
  :depends-on (:sb-posix)
  :serial t
  :components ((:module "src"
                :components ((:file "package")
                             (:file "config")
                             (:file "sprite")
                             (:file "push")
                             (:file "pull")
                             (:file "watch")
                             (:file "launch")
                             (:file "main")))))
