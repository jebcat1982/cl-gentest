(in-package :gentest)


(let ((type 'integer))
  (check basic-addition
      ((x 'positive-integer)
       (y type))
    ; (print (cons x y))
    (assert (integerp (+ x y)))
    ; (assert (plusp (+ x y)))
    ))



(clear-checks)
(check wat ((c '(list integer)))
  (holds (< (length c) 3))
  )
