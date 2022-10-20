; regular implementation
(define (gcdd a b acc1 acc2)
  (cond
    ((or (< a acc2) (< b acc2)) acc1)
    ((and (integer? (/ a acc2)) (integer? (/ b acc2)))
     (gcdd (/ a acc2) (/ b acc2) (* acc1 acc2) 2))
    (else (gcdd a b acc1 (+ 1 acc2)))))


(define (gcda a b)
  (gcdd a b 1 2))


; euclidean algorithm
(define (gcd-eu a b)
  (cond
    ((= a b) a)
    (else (gcd-eu (min a b) (- (max a b) (min a b))))))

;better euclidean algorithm
(define (gcd-best a b)
  (cond
    ((or (integer? (/ a b)) (integer? (/ b a))) (min a b))
    (else (gcd-best (min a b) (- (max a b) (* (quotient (max a b) (min a b)) (min a b)))))))


(define (gcd-bester a b)
  (cond
    ((zero? b) a)
    ;((integer? (/ a b)) b)
    (else (gcd-bester b (remainder a b)))))

