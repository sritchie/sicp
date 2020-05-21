#lang sicp

;; Exercise 1.12

(define pasc
  (lambda (row col)
    (cond
      ((= col 1) 1)
      ((> col row) 0)
      ((< col 1) 0)
      (else (+ (pasc (sub1 row) (sub1 col))
               (pasc (sub1 row) col))))))

(define pascrow
  (lambda (row)
    (define (pascmake n)
      (cond
        ((zero? n) (quote ()))
        (else (cons (pasc row (- row (sub1 n)))
                    (pascmake (sub1 n))))))
    (pascmake row)))

(pascrow 1)
(pascrow 2)
(pascrow 3)
(pascrow 4)
(pascrow 5)
