;;;; This file was automatically generated by Quickutil.
;;;; See http://quickutil.org for details.

;;;; To regenerate:
;;;; (qtlc:save-utils-as "quickutils.lisp" :utilities '(:COMPOSE :CURRY :MAP-PRODUCT :PARSE-ORDINARY-LAMBDA-LIST :ONCE-ONLY :RCURRY :SYMB :WITH-GENSYMS) :ensure-package T :package "GENTEST.QUICKUTILS")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package "GENTEST.QUICKUTILS")
    (defpackage "GENTEST.QUICKUTILS"
      (:documentation "Package that contains Quickutil utility functions.")
      (:use #:cl))))

(in-package "GENTEST.QUICKUTILS")

(when (boundp '*utilities*)
  (setf *utilities* (union *utilities* '(:MAKE-GENSYM-LIST :ENSURE-FUNCTION
                                         :COMPOSE :CURRY :MAPPEND :MAP-PRODUCT
                                         :SIMPLE-PROGRAM-ERROR :MAKE-KEYWORD
                                         :ENSURE-LIST
                                         :PARSE-ORDINARY-LAMBDA-LIST :ONCE-ONLY
                                         :RCURRY :MKSTR :SYMB
                                         :STRING-DESIGNATOR :WITH-GENSYMS))))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-gensym-list (length &optional (x "G"))
    "Returns a list of `length` gensyms, each generated as if with a call to `make-gensym`,
using the second (optional, defaulting to `\"G\"`) argument."
    (let ((g (if (typep x '(integer 0)) x (string x))))
      (loop repeat length
            collect (gensym g))))
  )                                        ; eval-when
(eval-when (:compile-toplevel :load-toplevel :execute)
  ;;; To propagate return type and allow the compiler to eliminate the IF when
  ;;; it is known if the argument is function or not.
  (declaim (inline ensure-function))

  (declaim (ftype (function (t) (values function &optional))
                  ensure-function))
  (defun ensure-function (function-designator)
    "Returns the function designated by `function-designator`:
if `function-designator` is a function, it is returned, otherwise
it must be a function name and its `fdefinition` is returned."
    (if (functionp function-designator)
        function-designator
        (fdefinition function-designator)))
  )                                        ; eval-when

  (defun compose (function &rest more-functions)
    "Returns a function composed of `function` and `more-functions` that applies its ;
arguments to to each in turn, starting from the rightmost of `more-functions`,
and then calling the next one with the primary value of the last."
    (declare (optimize (speed 3) (safety 1) (debug 1)))
    (reduce (lambda (f g)
              (let ((f (ensure-function f))
                    (g (ensure-function g)))
                (lambda (&rest arguments)
                  (declare (dynamic-extent arguments))
                  (funcall f (apply g arguments)))))
            more-functions
            :initial-value function))

  (define-compiler-macro compose (function &rest more-functions)
    (labels ((compose-1 (funs)
               (if (cdr funs)
                   `(funcall ,(car funs) ,(compose-1 (cdr funs)))
                   `(apply ,(car funs) arguments))))
      (let* ((args (cons function more-functions))
             (funs (make-gensym-list (length args) "COMPOSE")))
        `(let ,(loop for f in funs for arg in args
                     collect `(,f (ensure-function ,arg)))
           (declare (optimize (speed 3) (safety 1) (debug 1)))
           (lambda (&rest arguments)
             (declare (dynamic-extent arguments))
             ,(compose-1 funs))))))
  

  (defun curry (function &rest arguments)
    "Returns a function that applies `arguments` and the arguments
it is called with to `function`."
    (declare (optimize (speed 3) (safety 1) (debug 1)))
    (let ((fn (ensure-function function)))
      (lambda (&rest more)
        (declare (dynamic-extent more))
        ;; Using M-V-C we don't need to append the arguments.
        (multiple-value-call fn (values-list arguments) (values-list more)))))

  (define-compiler-macro curry (function &rest arguments)
    (let ((curries (make-gensym-list (length arguments) "CURRY"))
          (fun (gensym "FUN")))
      `(let ((,fun (ensure-function ,function))
             ,@(mapcar #'list curries arguments))
         (declare (optimize (speed 3) (safety 1) (debug 1)))
         (lambda (&rest more)
           (apply ,fun ,@curries more)))))
  

  (defun mappend (function &rest lists)
    "Applies `function` to respective element(s) of each `list`, appending all the
all the result list to a single list. `function` must return a list."
    (loop for results in (apply #'mapcar function lists)
          append results))
  

  (defun map-product (function list &rest more-lists)
    "Returns a list containing the results of calling `function` with one argument
from `list`, and one from each of `more-lists` for each combination of arguments.
In other words, returns the product of `list` and `more-lists` using `function`.

Example:

    (map-product 'list '(1 2) '(3 4) '(5 6))
     => ((1 3 5) (1 3 6) (1 4 5) (1 4 6)
         (2 3 5) (2 3 6) (2 4 5) (2 4 6))"
    (labels ((%map-product (f lists)
               (let ((more (cdr lists))
                     (one (car lists)))
                 (if (not more)
                     (mapcar f one)
                     (mappend (lambda (x)
                                (%map-product (curry f x) more))
                              one)))))
      (%map-product (ensure-function function) (cons list more-lists))))
  

  (define-condition simple-program-error (simple-error program-error)
    ())

  (defun simple-program-error (message &rest args)
    (error 'simple-program-error
           :format-control message
           :format-arguments args))
  

  (defun make-keyword (name)
    "Interns the string designated by `name` in the `keyword` package."
    (intern (string name) :keyword))
  

  (defun ensure-list (list)
    "If `list` is a list, it is returned. Otherwise returns the list designated by `list`."
    (if (listp list)
        list
        (list list)))
  

  (defun parse-ordinary-lambda-list (lambda-list &key (normalize t)
                                                      allow-specializers
                                                      (normalize-optional normalize)
                                                      (normalize-keyword normalize)
                                                      (normalize-auxilary normalize))
    "Parses an ordinary lambda-list, returning as multiple values:

1. Required parameters.

2. Optional parameter specifications, normalized into form:

   `(name init suppliedp)`

3. Name of the rest parameter, or `nil`.

4. Keyword parameter specifications, normalized into form:

   `((keyword-name name) init suppliedp)`

5. Boolean indicating `&allow-other-keys` presence.

6. `&aux` parameter specifications, normalized into form

   `(name init)`.

7. Existence of `&key` in the `lambda-list`.

Signals a `program-error` if `lambda-list` is malformed."
    (let ((state :required)
          (allow-other-keys nil)
          (auxp nil)
          (required nil)
          (optional nil)
          (rest nil)
          (keys nil)
          (keyp nil)
          (aux nil))
      (labels ((fail (elt)
                 (simple-program-error "Misplaced ~S in ordinary lambda-list:~%  ~S"
                                       elt lambda-list))
               (check-variable (elt what &optional (allow-specializers allow-specializers))
                 (unless (and (or (symbolp elt)
                                  (and allow-specializers
                                       (consp elt) (= 2 (length elt)) (symbolp (first elt))))
                              (not (constantp elt)))
                   (simple-program-error "Invalid ~A ~S in ordinary lambda-list:~%  ~S"
                                         what elt lambda-list)))
               (check-spec (spec what)
                 (destructuring-bind (init suppliedp) spec
                   (declare (ignore init))
                   (check-variable suppliedp what nil))))
        (dolist (elt lambda-list)
          (case elt
            (&optional
             (if (eq state :required)
                 (setf state elt)
                 (fail elt)))
            (&rest
             (if (member state '(:required &optional))
                 (setf state elt)
                 (fail elt)))
            (&key
             (if (member state '(:required &optional :after-rest))
                 (setf state elt)
                 (fail elt))
             (setf keyp t))
            (&allow-other-keys
             (if (eq state '&key)
                 (setf allow-other-keys t
                       state elt)
                 (fail elt)))
            (&aux
             (cond ((eq state '&rest)
                    (fail elt))
                   (auxp
                    (simple-program-error "Multiple ~S in ordinary lambda-list:~%  ~S"
                                          elt lambda-list))
                   (t
                    (setf auxp t
                          state elt))
                   ))
            (otherwise
             (when (member elt '#.(set-difference lambda-list-keywords
                                                  '(&optional &rest &key &allow-other-keys &aux)))
               (simple-program-error
                "Bad lambda-list keyword ~S in ordinary lambda-list:~%  ~S"
                elt lambda-list))
             (case state
               (:required
                (check-variable elt "required parameter")
                (push elt required))
               (&optional
                (cond ((consp elt)
                       (destructuring-bind (name &rest tail) elt
                         (check-variable name "optional parameter")
                         (cond ((cdr tail)
                                (check-spec tail "optional-supplied-p parameter"))
                               (normalize-optional
                                (setf elt (append elt '(nil)))))))
                      (t
                       (check-variable elt "optional parameter")
                       (when normalize-optional
                         (setf elt (cons elt '(nil nil))))))
                (push (ensure-list elt) optional))
               (&rest
                (check-variable elt "rest parameter")
                (setf rest elt
                      state :after-rest))
               (&key
                (cond ((consp elt)
                       (destructuring-bind (var-or-kv &rest tail) elt
                         (cond ((consp var-or-kv)
                                (destructuring-bind (keyword var) var-or-kv
                                  (unless (symbolp keyword)
                                    (simple-program-error "Invalid keyword name ~S in ordinary ~
                                                         lambda-list:~%  ~S"
                                                          keyword lambda-list))
                                  (check-variable var "keyword parameter")))
                               (t
                                (check-variable var-or-kv "keyword parameter")
                                (when normalize-keyword
                                  (setf var-or-kv (list (make-keyword var-or-kv) var-or-kv)))))
                         (if (cdr tail)
                             (check-spec tail "keyword-supplied-p parameter")
                             (when normalize-keyword
                               (setf tail (append tail '(nil)))))
                         (setf elt (cons var-or-kv tail))))
                      (t
                       (check-variable elt "keyword parameter")
                       (setf elt (if normalize-keyword
                                     (list (list (make-keyword elt) elt) nil nil)
                                     elt))))
                (push elt keys))
               (&aux
                (if (consp elt)
                    (destructuring-bind (var &optional init) elt
                      (declare (ignore init))
                      (check-variable var "&aux parameter"))
                    (progn
                      (check-variable elt "&aux parameter")
                      (setf elt (list* elt (when normalize-auxilary
                                             '(nil))))))
                (push elt aux))
               (t
                (simple-program-error "Invalid ordinary lambda-list:~%  ~S" lambda-list)))))))
      (values (nreverse required) (nreverse optional) rest (nreverse keys)
              allow-other-keys (nreverse aux) keyp)))
  

  (defmacro once-only (specs &body forms)
    "Evaluates `forms` with symbols specified in `specs` rebound to temporary
variables, ensuring that each initform is evaluated only once.

Each of `specs` must either be a symbol naming the variable to be rebound, or of
the form:

    (symbol initform)

Bare symbols in `specs` are equivalent to

    (symbol symbol)

Example:

    (defmacro cons1 (x) (once-only (x) `(cons ,x ,x)))
      (let ((y 0)) (cons1 (incf y))) => (1 . 1)"
    (let ((gensyms (make-gensym-list (length specs) "ONCE-ONLY"))
          (names-and-forms (mapcar (lambda (spec)
                                     (etypecase spec
                                       (list
                                        (destructuring-bind (name form) spec
                                          (cons name form)))
                                       (symbol
                                        (cons spec spec))))
                                   specs)))
      ;; bind in user-macro
      `(let ,(mapcar (lambda (g n) (list g `(gensym ,(string (car n)))))
              gensyms names-and-forms)
         ;; bind in final expansion
         `(let (,,@(mapcar (lambda (g n)
                             ``(,,g ,,(cdr n)))
                           gensyms names-and-forms))
            ;; bind in user-macro
            ,(let ,(mapcar (lambda (n g) (list (car n) g))
                    names-and-forms gensyms)
               ,@forms)))))
  

  (defun rcurry (function &rest arguments)
    "Returns a function that applies the arguments it is called
with and `arguments` to `function`."
    (declare (optimize (speed 3) (safety 1) (debug 1)))
    (let ((fn (ensure-function function)))
      (lambda (&rest more)
        (declare (dynamic-extent more))
        (multiple-value-call fn (values-list more) (values-list arguments)))))
  

  (defun mkstr (&rest args)
    "Receives any number of objects (string, symbol, keyword, char, number), extracts all printed representations, and concatenates them all into one string.

Extracted from _On Lisp_, chapter 4."
    (with-output-to-string (s)
      (dolist (a args) (princ a s))))
  

  (defun symb (&rest args)
    "Receives any number of objects, concatenates all into one string with `#'mkstr` and converts them to symbol.

Extracted from _On Lisp_, chapter 4.

See also: `symbolicate`"
    (values (intern (apply #'mkstr args))))
  

  (deftype string-designator ()
    "A string designator type. A string designator is either a string, a symbol,
or a character."
    `(or symbol string character))
  

  (defmacro with-gensyms (names &body forms)
    "Binds each variable named by a symbol in `names` to a unique symbol around
`forms`. Each of `names` must either be either a symbol, or of the form:

    (symbol string-designator)

Bare symbols appearing in `names` are equivalent to:

    (symbol symbol)

The string-designator is used as the argument to `gensym` when constructing the
unique symbol the named variable will be bound to."
    `(let ,(mapcar (lambda (name)
                     (multiple-value-bind (symbol string)
                         (etypecase name
                           (symbol
                            (values name (symbol-name name)))
                           ((cons symbol (cons string-designator null))
                            (values (first name) (string (second name)))))
                       `(,symbol (gensym ,string))))
            names)
       ,@forms))

  (defmacro with-unique-names (names &body forms)
    "Binds each variable named by a symbol in `names` to a unique symbol around
`forms`. Each of `names` must either be either a symbol, or of the form:

    (symbol string-designator)

Bare symbols appearing in `names` are equivalent to:

    (symbol symbol)

The string-designator is used as the argument to `gensym` when constructing the
unique symbol the named variable will be bound to."
    `(with-gensyms ,names ,@forms))
  
(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(compose curry map-product parse-ordinary-lambda-list once-only
            rcurry symb with-gensyms with-unique-names)))

;;;; END OF quickutils.lisp ;;;;
