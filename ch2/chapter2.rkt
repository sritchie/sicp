#lang sicp

;; Exercise 2.28

(define (deep-reverse l)
  (cond
   ((null? l) (quote ()))
   ((atom? (car l)) (append (deep-reverse (cdr l))
                            (list (car l))))
   (else (append (deep-reverse (cdr l))
                 (list (deep-reverse (car l)))))))

;; Exercise 2.23

(define (for-each proc list)
  (cond
   ((null? list) #t)
   (else (proc (car list))
         (for-each proc (cdr list)))))

;; Exercise 2.18

(define (reverse l)
  (if (null? l)
      (quote ())
      (append (reverse (cdr l))
              (list (car l)))))

;; Exercise 2.17

;; As Eli Bendersky pointed out in his great blog, and as I explained in the
;; answer to 2.16, we've got some issues with how we treat intervals versus real
;; numbers. I can't devise a package like that! But, again, as Eli points out,
;; if we had some sort of identity that we could use for maintaining intervals
;; through these sorts of transformations, we'd be on the right track.

;; Exercise 2.16

;; I'm going to agree that par2 is a better program. When you divide an interval
;; by itself, based on my tests below, you lose precision. (compare (percent
;; (div-interval test1 test1)) with (percent test1)). So, best to stick with the
;; original identity that DOESN'T require one to do this sort of transformation.

;; Exercise 2.15

;; So, I missed this, but we have failure here for the reason that they point
;; out in the text. Time to wait until I'm less tired to conquer these! When you
;; define an interval with, say, (define test-int (make-interval-width 5 0.1)),
;; and then take (div-interval test-int test-int), you get a result with
;; different error than 0.1. So, the improper assumption is that dividing an
;; uncertain number by itself produces that same number back again. This is a
;; false assumption stemming from DIFFERENT types of numbers, rational numbers,
;; and these dudes even hinted at the proper answer earlier, By asking about the
;; problems with multiplication and division. Silly me!

;; Exercise 2.14

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

(define (par3 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
                  (div-interval (add-interval r1 r2)
                                (mul-interval r1 r2)))))

;; Exercise 2.13

;; Represent this as (x + dx) * (y + dy) = (z + dz).
;; x*y + (y*dx + x*dy) = z + dz
;; (y*dx / x*y) + (x*dy / x*y) = dz / z
;; (dx/x + dy/y = dz/z
;; So, the tolerance of a multiplication is the sum of the component tolerances.

;; Exercise 2.12

(define (make-center-percent c p)
  (let ((w (abs (* p
                   (/ c 100)))))
    (make-center-width c w)))

(define (percent i)
  (let ((w (width i))
        (c (abs (center i))))
    (* 100 (/ w c))))

(define (make-center-width c w)
  (make-interval (- c w)
                 (+ c w)))

(define (center i)
  (/ (+ (lower-bound i)
        (upper-bound i))
     2))

(define (width i)
  (abs (/ (- (lower-bound i)
             (upper-bound i))
          2)))

;; Exercise 2.11

;; woah, ugly!
(define (mul-interval x y)
  (define (above-zero? num) (>= num 0))
  (define (below-zero? num) (< num 0))
  (let ((xl (lower-bound x))
        (xu (upper-bound x))
        (yl (lower-bound y))
        (yu (upper-bound y)))
    (cond
     ((and (above-zero? xl) (above-zero? xu)
           (above-zero? yl) (above-zero? yu))
      (make-interval (* xl yl)
                     (* xu yu)))
     ((and (above-zero? xl) (above-zero? xu)
           (below-zero? yl) (above-zero? yu))
      (make-interval (* xu yl)
                     (* xu yu)))
     ((and (above-zero? xl) (above-zero? xu)
           (below-zero? yl) (below-zero? yu))
      (make-interval (* xu yl)
                     (* xl yu)))
     ((and (below-zero? xl) (above-zero? xu)
           (above-zero? yl) (above-zero? yu))
      (make-interval (* xl yu)
                     (* xu yu)))
     ((and (below-zero? xl) (above-zero? xu)
           (below-zero? yl) (above-zero? yu))
      ;; we can't tell here, since both span zero, so we need to do it the old way
      (let ((p1 (* xl yl))
            (p2 (* xl yu))
            (p3 (* xu yl))
            (p4 (* xu yu)))
        (make-interval (min p1 p2 p3 p4)
                       (max p1 p2 p3 p4))))
     ((and (below-zero? xl) (above-zero? xu)
           (below-zero? yl) (below-zero? yu))
      (make-interval (* xu yl)
                     (* xl yl)))
     ((and (below-zero? xl) (below-zero? xu)
           (above-zero? yl) (above-zero? yu))
      (make-interval (* xl yu)
                     (* xu yl)))
     ((and (below-zero? xl) (below-zero? xu)
           (below-zero? yl) (above-zero? yu))
      (make-interval (* xl yu)
                     (* xl yl)))
     (else
      (make-interval (* xu yu)
                     (* xl yl))))))

;; Exercise 2.10

(define (div-interval x y)
  (define (spans-zero? interval)
    (and (negative? (lower-bound interval))
         (positive? (upper-bound interval))))
  (if (spans-zero? y)
      (error "the second interval spans zero!")
      (mul-interval x
                    (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y))))))

;; Exercise 2.9

;; Did this mostly off book. It's because addition and subtraction do translation of the widths,
;; while multiplication and division do scaling. Try using two inputs of width 1, with different ranges,
;; to mul-interval, then compare the final widths. You don't get a one-to-one map, like you do when you work out
;; Addition and subtraction.

;; Exercise 2.8

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

;; Exercise 2.7

(define (div-interval x y)
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (make-interval lower upper) (cons lower upper))
(define (lower-bound interval) (car interval))
(define (upper-bound interval) (cdr interval))

;; Exercise 2.6

(define zero
  (lambda (f)
    (lambda (x) x)))

(define (add-1 n)
  (lambda (f)
    (lambda (x)
      (f ((n f) x)))))

(define one
  (lambda (f)
    (lambda (x)
      (f x))))

(define two
  (lambda (f)
    (lambda (x)
      (f (f x)))))

;; so, y mind was blasting out to repeated on this; git, think of every application of f as increasing zero by one.
;; add-1 actually gets us up from zero to n, by the application of (n f) to x. remember,
;; x by itself is zero (sitting in the middle of the function nest, of course.
;; so, if add-1 can be represented by taking in a number, applying it to f, applying that to x, and then applying
;; f to the whole business, we can just skip that process by applying (a f) to the whole business, right?

(define (plus a b)
  (lambda (f)
    (lambda (x)
      ((a f) ((b f) x)))))

;; Exercise 2.5

;; This is totally bizarre. Of course it works, since 2 and three are relatively prime... but, really weird.
(define (cons a b)
  (* (expt 2 a)
     (expt 3 b)))

(define (car z)
  (degree-of-factor 2 z))

(define (cdr z)
  (degree-of-factor 3 z))

(define (degree-of-factor n z)
  (define (divides? a num)
    (= (remainder num a) 0))
  (define (degree-iter a z)
    (if (divides? n z)
        (degree-iter (add1 a) (/ z n))
        a))
  (degree-iter 0 z))

;; Exercise 2.4

(define (cdr z)
  (z (lambda (p q) q)))

(define (car z)
  (z (lambda (p q) p)))

(define (cons p q)
  (lambda (m) (m p q)))

;; Exercise 2.3

;; Constructor - (make-rect topleft bottomright)
;; selectors - (rect-width rect), (rect-height rect)

(define (rect-perimeter rect)
  (define (double x) (* x 2))
  (+ (double (rect-width rect))
     (double (rect-height rect))))

(define (rect-area rect)
  (* (rect-width rect)
     (rect-height rect)))

;; First implementation...

(define (make-rect topleft bottomright)
  (cons topleft bottomright))

(define (rect-width rect)
  (abs (- (x-point (car rect))
          (x-point (cdr rect)))))

(define (rect-height rect)
  (abs (- (y-point (car rect))
          (y-point (cdr rect)))))

;; Second implementation!
;; stores the diagonal. This is exactly what we've done before,
;; as make-segment is defined as cons.

(define (make-rect topleft bottomright)
  (make-segment topleft bottomright))

(define (rect-width rect)
  (let ((start (start-segment rect))
        (end (end-segment rect)))
    (abs (- (x-point start)
            (x-point end)))))

(define (rect-height rect)
  (let ((start (start-segment rect))
        (end (end-segment rect)))
    (abs (- (y-point start)
            (y-point end)))))

;; third would be, store all four points -- but then we have to validate!

;; Exercise 2.2

(define (midpoint-segment segment)
  (define (average d1 d2)
    (/ (+ d1 d2) 2.0))
  (let ((segstart (start-segment segment))
        (segend (end-segment segment)))
    (make-point (average (x-point segstart)
                         (x-point segend))
                (average (y-point segstart)
                         (y-point segend)))))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ", ")
  (display (y-point p))
  (display ")"))

(define (make-segment start end)
  (cons start end))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))

(define (make-point x y)
  (cons x y))

(define (x-point point)
  (car point))

(define (y-point point)
  (cdr point))
