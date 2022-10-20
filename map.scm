(define (add1lst lst)
  (cond
    ((null? lst) '())
    (else (cons (+ 1 (car lst)) (add1lst (cdr lst))))))

(define (mul2lst lst)
  (cond
    ((null? lst) '())
    (else (cons (* 2 (car lst)) (mul2lst (cdr lst))))))

(define (dofun f n)
  (f n))


(define (mklstfun f)
  (letrec ((lstfun (lambda (lst)
                     (cond
                       ((null? lst) '())
                       (else (cons (f (car lst)) (lstfun (cdr lst)))))))) lstfun))

(define (mymap f lst)
  ((mklstfun f) lst))
