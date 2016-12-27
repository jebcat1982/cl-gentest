(asdf:defsystem :cl-gentest
  :description "Simple generative testing for Common Lisp."

  :author "Steve Losh <steve@stevelosh.com>"

  :license "MIT/X11"
  :version "1.0.0"

  :depends-on ()

  :serial t
  :components ((:module "vendor" :serial t
                :components ((:file "quickutils-package")
                             (:file "quickutils")))
               (:module "src" :serial t
                :components ((:file "package")
                             (:file "gentest")
                             (:file "generators")))))
