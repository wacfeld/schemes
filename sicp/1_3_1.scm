(define (sum term a next b)
  (if (> a b)
    0
    (+ (term a)
       (sum term (next a) next b))))

(define (prod term a next b)
  (define (iter a res)
    (if (> a b)
      res
      (iter (next a) (* res (term a)))))
  (iter a 1))

(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (combiner (term a) result))))
  (iter a null-value))

(define (facc comb nullv term a next b filter)
  (define (newterm n)
    (if (filter n) 
      (term n)
      nullv))
  (accumulate comb nullv newterm a next b))


(define (prime? n)
  (= n (smallest-divisor n)))
(define (smallest-divisor n) (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b) (= (remainder b a) 0))

(define (square x) (* x x))
(facc + 0 square 10 1+ 30 prime?)
(define n 10)
(define (relprime? a)
  (= (gcd a n) 1))
(facc * 1 ident 1 1+ n relprime?)

#|
(define (sum2 term a next b)
  (accumulate + 0 term a next b))
(define (prod2 term a next b)
  (accumulate * 1 term a next b))

(define (simpson f a b n)
  (define h (/ (- b a) n))
  (define (term k)
    (cond ((zero? k) (f a))
          ((= n k) (f b))
          ((even? k) (* 2 (f (+ a (* k h)))))
          (else (* 4 (f (+ a (* k h)))))))
  (* (sum term 0 1+ n) (/ h 3)))
(define (cube n) (* n n n))
(simpson (lambda (x) (+ (cube x) x)) 0 1 2)

(define (integral f a b dx)
  (define (add-dx x)
    (+ x dx))
  (* (sum-iter f (+ a (/ dx 2.0)) add-dx b)
     dx))

(define (sum-iter term a next b) 
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (+ result (term a)))))
  (iter a 0))


(define (topterm n)
  (cond ((even? n) (+ n 2))
        (else (+ n 3))))
(define (botterm n)
  (cond ((even? n) (+ n 3))
        (else (+ n 2))))
(define (ident x) x)
(define (top/bot n)
  (/ (topterm n) (botterm n)))
(define (pi/4 n)
  (exact->inexact (prod top/bot 0 1+ n)))

(sum2 (lambda (x) (* x x)) 1 1+ 10)
(sum (lambda (x) (* x x)) 1 1+ 10)
|#
