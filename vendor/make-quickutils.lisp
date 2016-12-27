(ql:quickload 'quickutil)

(qtlc:save-utils-as
  "quickutils.lisp"
  :utilities '(

               :compose
               :curry
               :map-product
               :parse-ordinary-lambda-list
               :once-only
               :rcurry
               :symb
               :with-gensyms

               )
  :package "GENTEST.QUICKUTILS")
