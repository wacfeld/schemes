(define (sop tup acc)
  (cond
    ((null? tup) '())
    (else (cons (+ (car tup) acc) (sop (cdr tup) (+ (car tup) acc))))))

(define multirember
  (lambda (a lat)
    ((Y (lambda (mr)
          (lambda (lat)
            (cond
              ((null? lat) (quote ()))
              ((eq? a (car lat))
               (mr (cdr lat)))
              (else (cons (car lat)
                          (mr (cdr lat))))))))
     lat)))

(define atom? (lambda (x)
(and (not (pair? x)) (not (null? x)))))

(define-syntax letcc
  (syntax-rules ()
    ((_ h M) (call/cc (lambda (h) M)))))


(define-syntax fall
  (syntax-rules ()
    ((_) #f)
    ((_ (c1 B1) (c2 B2) ...)
     (if c1
       (begin B1 B2 ...)
       (fall (c2 B2) ...)))))



(define rm
  (lambda (a l oh)
    (cond
      ((null? l) (oh (quote no)))
      ((atom? (car l))
       (if (eq? (car l) a)
         (cdr l)
         (cons (car l)
               (rm a (cdr l) oh))))
      (else
        (if (atom?
              (letcc oh (rm a (car l) oh)))
          (cons (car l)
                (rm a (cdr l) oh))
          (cons (rm a (car l) 0)
                (cdr l)))))))

(define rm2
  (lambda (a l)
    (cond
      ((null? l) 'aaaaa)
      ((atom? (car l))
       (if (eq? (car l) a)
         (cdr l)
         (cons (car l)
               (rm2 a (cdr l)))))
      (else
        (let ((t (rm2 a (car l))))
          (if (atom? t)
            (cons (car l)
                  (rm2 a (cdr l)))
            (cons t (cdr l))))))))

; (define-syntax try
;   (syntax-rules ()
;     ((_ x a b)
;      (letcc _success
;             (letcc x a)
;             b))))
