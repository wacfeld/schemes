(define (deriv g)
  (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))
(define dx 0.00000001)
(define (cube x) (* x x x))

(define (newton-transform g)
  (lambda (x) (- x (/ (g x) ((deriv g) x)))))
(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (close-enough? v1 v2)
  (< (abs (- v1 v2))
     tolerance))
(define tolerance 0.00000001)
(define (fixed-point f first-guess)
  (define (try guess)
    (let ((next (f guess)))
      ;(display (exact->inexact next))
      (display "a")
      (if (close-enough? guess next)
        next
        (try (exact->inexact next)))))
  ;(display first-guess)
  (display "a")
  (try first-guess))

(define (cubic a b c)
  (lambda (x)
    (+ (expt x 3) (* a (expt x 2)) (* b x) (* c))))

(define (average a b) (/ (+ a b) 2))
(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (double f) (lambda (x) (f (f x))))
(((double (double double)) 1+ ) 5)
(define (compose f g) (lambda (x) (f (g x))))
(define (repeated f n)
  (if (zero? n)
    (lambda (x) x)
    (lambda (x) (f ((repeated f (- n 1) ) x)))))

(define (smooth f)
  (lambda (x)
    (/ (+
         (f (+ x dx))
         (f x)
         (f (- x dx)))
       3)))

(define (comp x n) (lambda (y) (/ x (expt y (- n 1)))))

(define (fpot g transform guess)
  (exact->inexact (newfp (transform g) guess)))

;1.45: nth root requires floor(lg(n)) repetitions
(define (log2 x) (/ (log x) (log 2)))
(define (numrep n) (floor (log2 n)))
(define (nroot n) (fpot (comp x n) (repeated average-damp (numrep n)) 1))

(define x 10)
(define n 58)
(define r 4)
(comp x 4)
;(define res (fpot (comp x n) (repeated average-damp r) 1))
(define res2 (nroot n))
;x
;;res
;res2
;;(expt res n)
;(expt res2 n)

(define (iter-imp good? improve)
  (define (try guess)
    (if (good? guess)
      guess
      (try (improve (exact->inexact guess)))))
  (lambda (x)
    (try x)))

(define (newsqrt x) ((iter-imp (lambda (guess) (close-enough? (* guess guess) x)) (lambda (guess) (average guess (/ x guess)))) 1))
(newsqrt 10)
(define (newfp f first) ((iter-imp (lambda (x) (close-enough? x (f x))) f) first))
x
;res
res2
;(expt res n)
(expt res2 n)

