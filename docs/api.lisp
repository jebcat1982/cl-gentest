(ql:quickload "cl-d-api")

(defparameter *header*
  "The following is a list of all user-facing parts of cl-gentest.

If there are backwards-incompatible changes to anything listed here, they will
be noted in the changelog and the author will feel bad.

Anything not listed here is subject to change at any time with no warning, so
don't touch it.

")

(d-api:generate-documentation
  :cl-gentest
  #p"docs/03-reference.markdown"
  (list "GENTEST")
  *header*
  :title "API Reference")

