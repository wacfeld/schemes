;section 1.2, 1.3
(define (fast-expt b n a)
  (cond
    ((= n 0) a)
    ((even? n) (fast-expt (square b) (/ n 2) a))
    (else (fast-expt b (- n 1) (* a b )))))
(define (slow-expt b n a)
  (cond
    ((= n 0) a)
    (else (slow-expt b (- n 1) (* a b)))))

(define (fast-mult b n)
  (cond
    ((= n 0) 0)
    ((even? n) (* 2 (fast-mult b (/ n 2))))
    (else (+ b (fast-mult b (- n 1))))))
(define (fib n)
  (fib-iter 1 0 0 1 n))
(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (* p p) (* q q))
                   (+ (* q q) (* 2 p q))
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))
