(define tolerance 0.000001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (display next)
      (newline)
      (if (close-enough? guess next)
        next
        (try next))))
  (display first-guess)
  (newline)
  (try first-guess))


(define (cont-frac n d k)
  (define (next n d k acc)
    (if (zero? k)
      acc
      (next n d (- k 1) (/ (n k) (+ (d k) acc)))))
  (next n d k 0))
(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           12)

(define (eulnd n)
  (define (3n-1 n)
    (if (zero? (remainder (+ n 1) 3))
      (/ (+ n 1) 3)
      0))
  (let ((k (3n-1 n)))
    (if (positive? k)
      (* 2 k)
      1)))

(exact->inexact (cont-frac (lambda (i) 1) eulnd 1000))

(define (tan-cf x k)
  (define (n k)
    (if (= k 1)
      x
      (- (* x x))))
  (define (d k)
    (- (* 2 k) 1))
  (cont-frac n d k))
(exact->inexact (tan-cf 2 100))


(define (average a b) (/ (+ a b) 2))
(define (thing x) (/ (log 1000) (log x)))
(define (average-damp f)
  (lambda (x) (average x (f x))))

(define x (fixed-point thing 2))
(expt x x)
(define x (fixed-point (average-damp thing) 2))
(expt x x)
