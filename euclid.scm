(define (euclid m n)
  (cond
    ((zero? n) m)
    (else (euclid n (modulo m n)))))

(define (ext-euclid m n)
  (cond
    ((zero? n) (list 1 0))
    (else (let* ((res (ext-euclid n (modulo m n)))
                 (a (car res))
                 (b (cadr res)))
            (list b (- a (* b (quotient m n))))))))
  
