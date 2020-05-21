#lang sicp

;; Exercise 1.3
;;
;; Nice example here of building up primitives with no help!

(define (ex1.3 a b c)

  (define (sum-of-squares l r)
    (+ (* l l) (* r r)))

  (cond ((and (>= b a) (>= c a)) (sum-of-squares b c))
        ((and (>= a b) (>= c b)) (sum-of-squares a c))
        (else (sum-of-squares a b))))
