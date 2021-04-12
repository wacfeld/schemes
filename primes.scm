(define undiv
  (lambda (n p)
    (cond
      ((null? p) #t)
      ((integer? (/ n (car p))) #f)
      (else (undiv n (cdr p))))))
(define nundiv
  (lambda (n p)
    (cond
      ((undiv n p) n)
      (else (nundiv (+ n 1) p)))))

(define str
  (lambda (n p)
    (let ((a (nundiv n p)))
      (cons
        (lambda () a)
        (lambda () (str a (cons a p)))))))
(define next (lambda (s) ((cdr s))))
(define now (lambda (s) ((car s))))
(let d ((n 100) (s (str 2 ())))
  (if (zero? n)
    ()
    (cons (now s) (d (- n 1) (next s)))))
    
