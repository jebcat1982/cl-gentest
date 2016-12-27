(in-package :gentest)


(let ((type 'integer))
  (check basic-addition
      ((x 'positive-integer)
       (y type))
    ; (print (cons x y))
    (assert (integerp (+ x y)))
    ; (assert (plusp (+ x y)))
    ))



