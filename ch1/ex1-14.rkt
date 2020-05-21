#lang sicp

;; Exercise 1.15

(define (cube x) (* x x x))

(define (p x) (- (* 3 x) (* 4 (cube x))))

(define (sinecount counter angle)
  (if (not (> (abs angle) 0.1))
      counter
      (sinecount (add1 counter) (/ angle 3.0))))

(define (sine angle)
  (if (not (> (abs angle) 0.1))
      angle
      (p (sine (/ angle 3.0)))))


(define subst
  (lambda (new old l)
    (cond
      ((null? l) (quote ()))
      ((atom? (car l))
       (cond
         ((eq? (car l) old) (cons new
                                  (subst new old (cdr l))))
         (else (cons (car l)
                     (subst new old (cdr l))))))
      (else (cons (subst new old (car l))
                  (subst new old (cdr l)))))))

(eq? 4 4)
