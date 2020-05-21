#lang sicp

;; Exercise 2.1

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (make-rat n d)
  (define (make-rat-reduce n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))

  (let ((flipped (or (and (< n 0) (< d 0))
                     (and (> n 0) (< d 0)))))
    (cond
      ((= d 0) (error "can't divide by zero!"))
      ((= n 0) 0)
      (flipped (make-rat-reduce (- n) (- d)))
      (else (make-rat-reduce n d)))))
