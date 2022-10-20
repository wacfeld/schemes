(load "rowreduce.scm")
(load "io.scm")

(define (eval-poly p x)
  (if (null? p)
    0
    (+ (car p) (* x (eval-poly (cdr p) x)))))


(define randlim 5)


(define (encode secret k)
  (if (= 1 k)
    (cons secret '())
    (cons (random randlim) (encode secret (- k 1)))))

(define (distribute p n)
  (define (iter p n)
    (if (zero? n)
      '()
      (cons (eval-poly p n) (iter p (- n 1)))))
  (reverse (iter p n)))

(define (lhs k x)
  (define (iter k x l)
    (if (zero? k)
      l
      (iter (- k 1) x (cons (expt x (- k 1)) l))))
  (iter k x '()))

(define (mat k exes whys)
  (define (iter k exes whys m)
    (if (null? exes)
      m
      (iter k
            (cdr exes)
            (cdr whys)
            (cons (append (lhs k (car exes)) (cons (car whys) '())) m))))
  (reverse (iter k exes whys '())))

(define (pointmat k points)
  (mat k (map car points) (map cdr points)))

(define (oneto k)
  (define (iter k l)
    (if (zero? k)
      l
      (iter (- k 1) (cons k l))))
  (iter k ()))

(define (first-n l n)
  (if (zero? n)
    ()
    (cons (car l) (first-n (cdr l) (- n 1)))))

(define (augval m k)
  (first-n (map last m) k))

(define (trysolve points)
  (augval (rowred (pointmat k points)) k))
(define (fullsolve points)
  (rowred (pointmat k points)))


;(define secret 3)
;(define n 10)
;(define k 6)

;(define secret (prompt "secret number: "))
;(define n (prompt "number of people: "))
;(define k (prompt "required people: "))
;
;(define p (encode secret k))
;(define d (distribute p n))

(define msg '(1 4 6 5 6 4 0))
(define k (length msg) )
(define n (- (* 2 k) 1))
(define p msg)
(define d (distribute p n))

(define points (map cons (oneto n) d))

(define m (mat k (oneto k) d))
(define r (rowred m))
(define a (augval r k))

(say points)

;points
;p
;d
;m
;r
;a
;
;
;
;(define p '(1 -6 4))
;(define d '(1 -1 5 19))
;
;
;(define m (mat 3 '(0 1 2 3) d))
;(augval (rowred m) 3)
;p
;
;
;p
;d
;
;
;(define mat
;  '((0 0 1 1)
;    (1 1 1 -1)
;    (4 2 1 5)))
;(rowred mat)
