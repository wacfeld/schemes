(define (frontier str n)
  (cond
    ((zero? n) ())
    (else (cons (car str) (frontier ((cadr str)) (- n 1))))))

(define (Q str n)
  (cond
    ((zero? (remainder (car str) n))
     (Q ((cadr str)) n))
    (else (list (car str)
                (lambda ()
                  (Q ((cadr str)) n))))))

(define (P str)
  (list (car str) (lambda () (P (Q str (car str))))))

(define (strmake next n)
  (list n (lambda () (strmake next (next n)))))

(define myint (strmake 1+ 0))

(frontier (P ((cadr ((cadr myint))))) 10)
