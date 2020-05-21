#lang sicp

;; Exercise 1.24

;; This solution shows that prime search for n * 1000 takes about twice the time
;; as does the search for n itself.
;;

(define (fast-prime-search n found start-time)
  (cond
    ((= found 3) (- (system-time) start-time))
    ((even? n) (prime-search (+ n 1) found start-time))
    ((fast-prime? n) (prime-search (+ n 1) (+ found 1) start-time))
    (else (prime-search (+ n 1) found start-time))))

(define (fast-prime? n times)
  (cond ((= times 0) #t)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (expmod base exp m)
  (cond
    ((= exp 0) 1)
    ((even? exp) (remainder (square (expmod base (/ exp 2) m)) m))
    (else (remainder (* base (expmod base (- exp 1) m)) m))
    ))
