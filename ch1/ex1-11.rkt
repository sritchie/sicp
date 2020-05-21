#lang sicp

;; Exercise 1.11

;; recursive version
(define (frec n)
  (cond
    ((< n 3) n)
    (else (+ (frec (- n 1)) (* 2 (frec (- n 2))) (* 3 (frec (- n 3)))))))

;; iterative version.
(define (fit n)
  (define (fit-iter a b c n)
    (cond
      ((= n 0) a)
      ((< n 3) (fit-iter b c 0 (sub1 n)))
      (else (fit-iter b c (+ (* 3 a) (* 2 b) c) (sub1 n)))))
  (fit-iter 0 1 2 n))
