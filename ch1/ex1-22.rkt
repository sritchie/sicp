#lang sicp

;; Exercise 1.22

(define (time-primes first second)
  (/ (search-for-primes first)
     (search-for-primes second)))

(define (search-for-primes min)
  (newline)
  (display "searching...")
  (prime-search min 0 (system-time)))

(define (prime-search n found start-time)
  (cond
    ((= found 3) (- (system-time) start-time))
    ((even? n) (prime-search (+ n 1) found start-time))
    ((prime? n) (prime-search (+ n 1) (+ found 1) start-time))
    (else (prime-search (+ n 1) found start-time))))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (system-time)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (system-time)
  (internal-time/ticks->seconds (real-time-clock)))
