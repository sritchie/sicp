#lang sicp

;; Thus concludes chapter 1!!!

;; Exercise 1.46: write iterative-improve, redefine sqrt and fixed-point

;; There, definitely prettier when we get to add two arguments to the
;; good-enough?

(define Y
  (lambda (le)
    ((lambda (f) (f f))
     (lambda (f)
       (le (lambda (x) ((f f) x)))))))

(Y (lambda (plus)
     (lambda (x y)
       (if (= x 0)
           y
           (plus (dec x) (inc y))))))

(define (iterative-improve good-enough? improve)
  (lambda (guess)
    (define (try guess)
      (let ((improved (improve guess)))
        (if (good-enough? guess improved)
            improved
            (try improved))))
    (try guess)))

(define (close-enough? v1 v2)
  (let ((tolerance 0.00001))
    (< (abs (- v1 v2)) tolerance)))

(define (fixed-point f first-guess)
  ((iterative-improve close-enough?
                      f)
   first-guess))

(define (sqrt x)
  (define (average v1 v2)
    (/ (+ v1 v2) 2))
  ((iterative-improve close-enough?
                      (lambda (guess)
                        (average guess (/ x guess))))
   1.0))

;; First, as below, I did this with good-enough? taking one argument...
;; I like it better with two args, as above.

(define (iterative-improve good-enough? improve)
  (lambda (guess)
    (define (try guess)
      (if (good-enough? guess)
          guess
          (try (improve guess))))
    (try guess)))

(define (fixed-point f first-guess)
  ((iterative-improve (lambda (guess)
                        (let ((tolerance 0.00001))
                          (< (abs (- guess (f guess))) tolerance)))
                      f)
   first-guess))

(define (sqrt x)
  (define (average v1 v2)
    (/ (+ v1 v2) 2))
  ((iterative-improve (lambda (guess)
                        (< (abs (- (square guess) x)) 0.0001))
                      (lambda (guess)
                        (average guess (/ x guess))))
   1.0))

;; Exercise 1.45: nth roots, using average damping that
;; tunes itself for the various roots.

;; think about the below functions.

;; Floor or ceiling, doesn't really matter.

(define (dampen-root n)
  (define (log2 x) (/ (log x) (log 2)))
  (repeated average-damp (ceiling (log2 n))))

(define (n-root x n)
  (fixed-point-of-transform (lambda (y) (/ x (expt y (- n 1))))
                            (dampen-root n)
                            1.0))

;; now, we can redefine square roots, to take advantage of the more general
;; abstractions!

(define (sqrt x)
  (n-root x 2))

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess count)
    (let ((next (f guess)))
      (print-line count guess)
      (if (close-enough? guess next)
          next
          (try next (+ 1 count)))))
  (try first-guess 0))

(define (print-line count guess)
  (newline)
  (display count)
  (display "   ")
  (display guess))

(define (average-damp f)
  (define (average v1 v2)
    (/ (+ v1 v2) 2))
  (lambda (x)
    (average x (f x))))

;; Exercise 1.44: Smoothing!

;; We're creating, for example, (smooth (smooth f)), for n = 2

(define (repeated-smooth f n)
  ((repeated smooth n) f))

(define dx 0.00001)

(define (smooth f)
  (define (average v1 v2 v3)
    (/ (+ v1 v2 v3) 3))
  (lambda (x)
    (average (f (- x dx))
             (f x)
             (f (+ x dx)))))

;; Exercise 1.43

(define (repeated f n)
  (define (identity x) x)
  (if (= n 0)
      identity
      (compose f (repeated f (sub1 n)))))

;; Or, for the linear iterative solution,

(define (repeated f n)
  (define (identity x) x)
  (define (iter n result)
    (if (zero? n)
        result
        (iter (sub1 n) (compose f result))))
  (iter n identity))

;; Exercise 1.42

(define (compose f g)
  (lambda (x)
    (f (g x))))

;; Exercise 1.41

(define (double f)
  (lambda (x)
    (f (f x))))

;; (((double (double double)) add1) 5) returns 21;
;; the (double double) creates quad, basically, and applying double to quad
;; results in
;; (define (sixteen-times f)
;;   (lambda (f)
;;     (quad (quad f))))
;; or, when applied to add1, add16.

;; Exercise 1.40

(define (cubic a b c)
  (lambda (x)
    (+ (cube x)
       (* a (square x))
       (* b x)
       c)))

;; Exercise 1.39

(define (tan-cf x k)
  (define (square x) (* x x))
  (cont-frac (lambda (i)
               (if (= i 1)
                   x
                   (- (square x))))
             (lambda (i)
               (- (* i 2) 1))
             k))

;; Exercise 1.38

(define (euler-cont k)
  (+ 2
     (cont-frac (lambda (i) 1.0)
                (lambda (i)
                  (if (= (remainder i 3) 2)
                      (- i (quotient i 3))
                      1))
                k)))

;; Exercise 1.37

;; looks like we need k of about 13 to get within four decimal places!

;; Recursive version

(define (cont-frac n d k)
  (define (loop i)
    (if (= i k)
        0
        (/ (n i)
           (+ (d i)
              (loop (add1 i))))))
  (loop 1))

;; Iterative version

(define (cont-frac n d k)
  (define (iter k result)
    (if (zero? k)
        result
        (iter (- k 1)
              (/ (n k)
                 (+ (d k)
                    result)))))
  (iter k 0))

(define (how-many-tries f first-try)
  (define (close-enough? v1 v2)
    (let ((tolerance 0.0001))
      (< (abs (- v1 v2)) tolerance)))
  (define (try k-value)
    (let ((this (f k-value))
          (next (f (add1 k-value))))
      (if (close-enough? this next)
          (add1 k-value)
          (try (add1 k-value)))))
  (try first-try))

(define golden-inverse-tries
  (how-many-tries (lambda (k)  (cont-frac (lambda (i) 1.0)
                                          (lambda (i) 1.0)
                                          k))
                  1))

;; Exercise 1.36

(define (without-damp first-guess)
  (fixed-point (lambda (x) (/ (log 1000) (log x)))
               first-guess))

(define (with-damp first-guess)
  (fixed-point (lambda (x) (average x
                                    (/ (log 1000) (log x))))
               first-guess))

(define (average v1 v2)
  (/ (+ v1 v2) 2.0))

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess count)
    (let ((next (f guess)))
      (print-line count guess)
      (if (close-enough? guess next)
          next
          (try next (+ 1 count)))))
  (try first-guess 0))

(define (print-line count guess)
  (newline)
  (display count)
  (display "   ")
  (display guess))

;; Exercise 1.35

(define (golden-ratio first-guess)
  (fixed-point (lambda (x) (+ 1 (/ 1 x)))
               first-guess))

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

;; Exercise 1.34

(define (f g)
  (g 2))

;; (f 2) results in "the object 2 is not applicable".

;; Exercise 1.33

(define (filtered-accumulate filter combiner null-value term a next b)
  (cond
   ((> a b) null-value)
   ((filter a) (combiner (term a)
                         (filtered-accumulate filter combiner null-value
                                              term (next a) next b)))
   (else (filtered-accumulate filter combiner null-value
                              term (next a) next b))))

(define (sum-prime-squares a b)
  (filtered-accumulate prime? + 0 square a add1 b))

(define (prod-rel-prime n)
  (define (add1 x) (+ x 1))
  (define (identity x) x)
  (define (rel-prime? x) (= (gcd x n) 1))
  (filtered-accumulate rel-prime? * 1 identity 1 add1 n))

;; Exercise 1.32:
;; iterative and recursive accumulators, plus sum and product redefined.

(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner (term a) result))))
  (iter a null-value))

(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term (next a) next b))))

(define (product term a next b)
  (accumulate * 1 term a next b))

(define (sum term a next b)
  (accumulate + 0 term a next b))

;; Exercise 1.31

(define (pi-guess n)
  (define (square x) (* x x))
  (define (pi-inc x) (+ x 2))
  (define (pi-term x)
    (/ (* (+ x 1)
          (+ x 3))
       (square (+ x 2))))
  (* (product pi-term 1.0 pi-inc n)
     4))

(define (factorial n)
  (define (identity x) x)
  (define (add1 x) (+ x 1))
  (product identity 1 add1 n))

(define (product term a next b)
  (if (> a b)
      1
      (* (term a) (product term (next a) next b))))

(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* (term a) result))))
  (iter a 1))

;; Exercise 1.30

(define (sum-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ (term a) result))))
  (iter a 0))

;; Exercise 1.29

(define (simp f a b n)
  (define (round-even x)
    (if (even? x)
        x
        (add1 x)))
  (let ((rounded-n (round-even n))
        (h (/ (- b a)
              (round-even n))))
    (define (func x)
      (f (+ a
            (* x h))))
    (define (simp-term x)
      (cond
       ((or (zero? x) (= x rounded-n)) (func x))
       ((even? x) (* 2 (func x)))
       (else (* 4 (func x)))))
    (* (sum simp-term 0 add1 rounded-n)
       (/ h 3))))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

;; Exercise 1.27

(define (miller-rabin-expmod base exp m)
  (define (squaremod-with-check x)
    (define (check-nontrivial-sqrt1 x square)
      (if (and (= square 1)
               (not (= x 1))
               (not (= x (- m 1))))
          0
          square))
    (check-nontrivial-sqrt1 x (remainder (square (remainder x m)) m)))
  (cond ((= exp 0) 1)
        ((even? exp) (squaremod-with-check
                      (miller-rabin-expmod base (/ exp 2) m)))
        (else
         (remainder (* base (miller-rabin-expmod base (- exp 1) m))
                    m))))

(define (miller-rabin-test n)
  (define (try-it a)
    (define (check-it x)
      (and (not (= x 0)) (= x 1)))
    (check-it (miller-rabin-expmod a (- n 1) n)))
  (try-it (+ 1 (random (- n 1)))))

(define (miller-fast-prime? n times)
  (cond ((= times 0) true)
        ((miller-rabin-test n) (miller-fast-prime? n (- times 1)))
        (else false)))

;; Carmichael Numbers!

(define (pass-fermat? n)
  (fermat-iter 1 n))

(define (fermat-iter a n)
  (cond ((= a n) #t)
        ((= (expmod a n n) a) (fermat-iter (+ a 1) n))
        (else #f)))


;; Exercise 1.24

;; This solution shows that prime search for n * 1000 takes about twice the time as does the
;; search for n itself.
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

;; Exercise 1.23

;; I get a ratio between the two of about 1.5 (run (search-for-primes n) on each system,
;; and all becomes clear.)
;; I'm attributing the extra .5 to the overhead of calling the next function.

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


;; Exercise 1.21

(define (prime-tests)
  (display (prime? 199))
  (display (prime? 1999))
  (display (prime? 19999))
  )

(define (prime? n)
  (= (smallest-divisor n) n))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond
   ((> (square test-divisor) n) n)
   ((divides? test-divisor n) test-divisor)
   (else (find-divisor n (add1 test-divisor)))))

(define (next n)
  (if (= n 2)
      3
      (+ n 2)))

(define (divides? a b)
  (= (remainder b a) 0))

;; Exercise 1.14

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

;; Change counting example.

(define (count-change amount)
  (cc amount 5))

(define (cc amount kinds-of-coins)
  (cond
   ((= amount 0) 1)
   ((< amount 0) 0)
   ((= kinds-of-coins 0) 0)
   (else (+ (cc amount (sub1 kinds-of-coins))
            (cc (- amount (first-denomination kinds-of-coins)) kinds-of-coins)))))

(define (first-denomination kinds-of-coins)
  (cond
   ((= kinds-of-coins 1) 1)
   ((= kinds-of-coins 2) 5)
   ((= kinds-of-coins 3) 10)
   ((= kinds-of-coins 4) 25)
   ((= kinds-of-coins 5) 50)))

;; Exercises before

(define (mk-square)
  (define error-threshold 0.001)

  (define square
    (lambda (x)
      (* x x)))

  (define abs
    (lambda (x)
      (if (< x 0) (- x) x)))

  (define cube-root
    (lambda (x)
      (try 1.0 0.0 x)))

  (define try
    (lambda (guess prev-guess x)
      (cond
       ((good-enough? guess prev-guess x) guess)
       (else (try (improve guess x) guess x)))))

  (define good-enough?
    (lambda (guess prev-guess x)
      (< (abs-diff guess prev-guess) (abs (* guess error-threshold)))))

  (define abs-diff
    (lambda (x y)
      (abs (- x y))))

  ;; (define improve
  ;;   (lambda (guess x)
  ;;     (average guess (/ x guess))))

  (define improve
    (lambda (guess x)
      (average3 (/ x (square guess)) guess guess)))

  (define average3
    (lambda (x y z)
      (/ (+ x y z) 3)))

  (define average
    (lambda (x y)
      (/ (+ x y) 2))))
