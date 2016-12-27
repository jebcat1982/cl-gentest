(in-package :gentest)

;;;; Utils --------------------------------------------------------------------
(defun argument-names (lambda-list)
  (multiple-value-bind
      (reqs opts rest-name keys aok? auxs key?)
      (parse-ordinary-lambda-list lambda-list)
    (declare (ignore aok? key?))
    (append reqs
            (mapcar #'car opts)
            (if rest-name (list rest-name) nil)
            (mapcar #'cadar keys)
            (mapcar #'car auxs))))

(defmacro lambda-ignoreable (lambda-list &body body)
  `(lambda ,lambda-list
     (declare (ignorable ,@(argument-names lambda-list)))
     ,@body))


;;;; Generators ---------------------------------------------------------------
(defstruct generator
  name
  generate
  edge-cases
  validp
  shrink)

(defvar *generators* (make-hash-table))

(defun clear-generators ()
  (clrhash *generators*))

(defun find-generator (name)
  (gethash name *generators*))


(defun define-generator% (name &key generate edge-cases shrink validp)
  (setf (gethash name *generators*)
        (make-generator :name name
                        :generate generate
                        :edge-cases edge-cases
                        :shrink shrink
                        :validp validp)))


(defmacro define-generator (name lambda-list &key
                            generate
                            (edge-cases '())
                            (shrink '())
                            (validp t))
  `(progn (define-generator%
            ',name
            :generate (lambda-ignoreable (,@lambda-list) ,generate)
            :edge-cases (lambda-ignoreable (,@lambda-list) ,edge-cases)
            :shrink (lambda-ignoreable (value ,@lambda-list) ,shrink)
            :validp (lambda-ignoreable (value ,@lambda-list) ,validp))
          ',name))


;;;; Generator Designators ----------------------------------------------------
(defun normalize-gd (generator-designator)
  (etypecase generator-designator
    (symbol (list generator-designator))
    (cons generator-designator)))

(defun gd-generator (generator-designator)
  (find-generator (first generator-designator)))

(defun gd-arguments (generator-designator)
  (rest generator-designator))


;;;; Generator Aliases --------------------------------------------------------
(defvar *aliases* (make-hash-table :test 'eq))

(defmacro defgen (name lambda-list &body body)
  `(progn
    (setf (gethash ',name *aliases*)
          (lambda ,lambda-list ,@body))
    ',name))

(defun resolve-aliases (generator-designator)
  (let ((alias-function (gethash (gd-name generator-designator) *aliases*)))
    (if alias-function
      (apply alias-function (gd-arguments generator-designator))
      generator-designator)))


;;;; Generator API ------------------------------------------------------------
(defmacro with-generator ((gen args) generator-designator &body body)
  (with-gensyms (gd)
    `(let* ((,gd (resolve-aliases (normalize-gd ,generator-designator)))
            (,gen (gd-generator ,gd))
            (,args (gd-arguments ,gd)))
      ,@body)))

(defun filter-valid (gen args values)
  (remove-if-not (lambda (value)
                   (apply (generator-validp gen) value args))
                 values))


(defun generate (generator-designator)
  "Return a single random object."
  ;; todo: bail after too many tries
  (with-generator (gen args) generator-designator
    (loop :for val = (apply (generator-generate gen) args)
          :until (apply (generator-validp gen) val args)
          :finally (return val))))

(defun edge-cases (generator-designator)
  "Return a list of edge case objects."
  (with-generator (gen args) generator-designator
    (filter-valid gen args (apply (generator-edge-cases gen) args))))

(defun shrink (generator-designator object)
  "Return a list of shrunken versions of `object`."
  (with-generator (gen args) generator-designator
    (filter-valid gen args (apply (generator-shrink gen) object args))))

(defun validp (generator-designator object)
  "Return whether `object` is a valid value for the generator."
  (with-generator (gen args) generator-designator
    (apply (generator-validp gen) object args)))


;;;; Checks -------------------------------------------------------------------
(defvar *checks* (make-hash-table))

(defstruct data symbol generator-designator)
(defstruct check name data function)

(defun find-check (name)
  (gethash name *checks*))

(defun add-check (name data function)
  (setf (gethash name *checks*)
        (make-check :name name
                    :data (mapcar (lambda (d)
                                    (destructuring-bind (symbol gd) d
                                      (make-data :symbol symbol
                                                 :generator-designator gd)))
                                  data)
                    :function function)))

(defun clear-checks ()
  (clrhash *checks*))


;;;; Running Tests ------------------------------------------------------------
(defparameter *test-count* 100)
(defparameter *current-args* nil)
(defparameter *current-data* nil)

(defun generate-data (data)
  (append
    ;; Include all combinations of edge cases
    (apply (curry #'map-product #'list)
           (mapcar (lambda (d)
                     (edge-cases (data-generator-designator d)))
                   data))
    ;; And then generate random data
    (loop :repeat *test-count*
          :collect (loop :for d :in data
                         :collect (generate (data-generator-designator d))))))


(define-condition check-violated (error)
  ((form :initarg :form)
   (value :initarg :value)))


(defmethod print-object ((object check-violated) stream)
  (format stream
          "Test failed: ~S returned null for data:~{~%  ~S = ~S~}"
          (slot-value object 'form)
          (mapcan #'list *current-args* *current-data*)))


(defun check-succeeds-p (check data)
  (handler-case (progn (apply (check-function check) data)
                       t)
    (check-violated () nil)))

(defun remove-successes (check candidates)
  (remove-if (curry #'check-succeeds-p check)
             candidates))

(defun shrink-each (check data)
  ; please kill me
  (loop :for g :in (mapcar #'data-generator-designator (check-data check))
        :for (d . remaining) :on data
        :append (mapcar (lambda (v)
                          (append prefix (list v) remaining))
                        (shrink g d)) :into result
        :collect d :into prefix
        :finally (return result)))

(defun shrink-failure (check candidates)
  (let* ((next (remove-successes check (mapcan (curry #'shrink-each check)
                                               candidates))))
    (if next
      (shrink-failure check next)
      (first candidates))))


(defun run-check% (check)
  (write (check-name check))
  (loop :with function = (check-function check)
        :with data = (check-data check)
        :with args = (mapcar #'data-symbol data)
        :for i :from 1
        :for values :in (generate-data (check-data check))
        :do (handler-case (apply function values)
              (check-violated
                ()
                (let* ((final-failing-values (shrink-failure check (list values)))
                       (*current-args* args)
                       (*current-data* final-failing-values))
                  (apply function final-failing-values))))
        :do (write-string ".")
        :finally (format t " ~D tests passed~%" i))
  nil)

(defun run-check (name)
  (let ((check (find-check name)))
    (assert (not (null check)) () "No check with name ~S found." name)
    (run-check% check)))

(defun run ()
  (loop :for name :being :the hash-keys :of *checks*
        :do (run-check name)))


;;;; Testing API --------------------------------------------------------------
(defun parse-data (data)
  (values (mapcar (lambda (d)
                    (destructuring-bind (name &rest more) d
                      `(list ',name ,@more)))
                  data)
          (mapcar #'car data)))

(defmacro check (name data &body body)
  (multiple-value-bind (data args) (parse-data data)
    `(add-check ',name
      (list ,@data)
      (lambda ,args
        (declare (optimize (debug 3)))
        ,@body))))


(defmacro holds (form)
  (with-gensyms (result)
    `(let ((,result ,form))
      (if (not ,result)
        (restart-case
          (error 'check-violated
                 :form ',form
                 :value ,result)
          (ignore () :report "Ignore this failure and continue testing."))))))
