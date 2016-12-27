(in-package :gentest)

;;;; Utils --------------------------------------------------------------------
(defun random-range (min max)
  (+ min (random (- (1+ max) min))))

(defun random-element (sequence)
  (elt sequence (random (length sequence))))


;;;; Integers -----------------------------------------------------------------
(define-generator integer (&key (min (* 2 most-negative-fixnum))
                                (max (* 2 most-positive-fixnum)))
  :validp (<= min value max)
  :generate (random-range min max)

  :edge-cases `(0 1 -1
                ,most-positive-fixnum
                ,most-negative-fixnum
                ,(1+ most-positive-fixnum)
                ,(1- most-negative-fixnum))

  :shrink (cond ((> (abs value) 100) (list (truncate value 10)))
                ((plusp value) (list (1- value)))
                ((minusp value) (list (1+ value)))
                ((zerop value) nil)))


(defgen positive-integer (&key (max (* 2 most-positive-fixnum)))
  `(integer :min 1 :max ,max))

(defgen non-negative-integer (&key (max (* 2 most-positive-fixnum)))
  `(integer :min 0 :max ,max))

(defgen negative-integer (&key (min (* 2 most-negative-fixnum)))
  `(integer :max -1 :min ,min))

(defgen non-positive-integer (&key (min (* 2 most-negative-fixnum)))
  `(integer :max 0 :min ,min))


;;;; Floats -------------------------------------------------------------------
(define-generator float (&key (min most-negative-long-float)
                              (max most-positive-long-float))
  :validp
  (<= min value max)

  :generate
  (coerce (random-range min max)
          (random-element #(single-float double-float short-float long-float)))

  ;; todo: shrink

  :edge-cases
  (remove-duplicates `(0.0f0 1.0f0 -1.0f0
                       0.0s0 1.0s0 -1.0s0
                       0.0l0 1.0l0 -1.0l0
                       0.0d0 1.0d0 -1.0d0
                       ,most-positive-short-float
                       ,most-negative-short-float
                       ,most-positive-long-float
                       ,most-negative-long-float
                       ,most-positive-single-float
                       ,most-negative-single-float
                       ,most-positive-double-float
                       ,most-negative-double-float
                       ,least-positive-short-float
                       ,least-negative-short-float
                       ,least-positive-long-float
                       ,least-negative-long-float
                       ,least-positive-single-float
                       ,least-negative-single-float
                       ,least-positive-double-float
                       ,least-negative-double-float)
                     :test #'=))


(defgen positive-float (&key (max most-positive-long-float))
  `(float :min ,least-positive-long-float :max ,max))

(defgen non-negative-float (&key (max most-positive-long-float))
  `(float :min 0 :max ,max))

(defgen negative-float (&key (min most-negative-long-float))
  `(float :max ,least-negative-long-float :min ,min))

(defgen non-positive-float (&key (min most-negative-long-float))
  `(float :max 0 :min ,min))


;;;; Symbols ------------------------------------------------------------------
(define-generator symbol ()
  :generate (gensym)
  :edge-cases (list nil t :a-keyword-symbol))


;;;; Lists --------------------------------------------------------------------
(defun remove-one (list)
  (if (null (cdr list))
    '(nil)
    (cons (cdr list)
          (mapcar (curry #'cons (car list))
                  (remove-one (cdr list))))))

(define-generator list (element-generator &key (min-size 0) (max-size 10))
  :validp (<= min-size (length value) max-size)

  :generate (loop :repeat (random-range min-size (1+ max-size))
                  :collect (generate element-generator))

  :edge-cases `(() ; empty list
                ,@(mapcar #'list ; unit lists of underlying edge cases
                          (edge-cases element-generator)))

  :shrink (remove-one value)) ; todo shrink the elements


(defgen non-empty-list (&key (max-size 10))
  `(list :min-size 1 :max-size ,max-size))


;;;; Conses -------------------------------------------------------------------
(define-generator cons (car-generator cdr-generator)
  :generate (cons (generate car-generator)
                  (generate cdr-generator))
  :edge-cases (map-product #'cons
                           (edge-cases car-generator)
                           (edge-cases cdr-generator))
  :shrink (destructuring-bind (a . b) value
            (append (mapcar (lambda (s) (cons s b))
                            (shrink car-generator a))
                    (mapcar (lambda (s) (cons a s))
                            (shrink car-generator b)))))


