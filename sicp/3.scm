;; 3.17

; (define (count-pairs x acc)
;   (cond
;     ((not (null? (filter (lambda (p) (eq? p x)) acc))) 0)
;     ((not (pair? x)) 0)
;     (else
;       (+ (count-pairs (car x) (cons x acc))
;          (count-pairs (cdr x) (cons x acc))
;          1))))

(define gcp
  (let ((acc '(())))
    (lambda (x)
      (cond
        ((not (null? (filter (lambda (p) (eq? p x)) acc))) 0)
        ((not (pair? x)) 0)
        (else
          (append! acc (list x))
          (+ (gcp (car x))
             (gcp (cdr x))
             1))))))

(define (bcp x)
  (if (not (pair? x))
    0
    (+ (bcp (car x))
       (bcp (cdr x))
       1)))

(define (eqmem? x l)
  (not (null? (filter (lambda (p) (eq? p x)) l))))

;; 3.18
(define (cycle? l acc)
  (cond
    ((null? l) false)
    ((eqmem? (car l) acc) true)
    (else (cycle? (cdr l) (cons (car l) acc)))))

;; 3.19

;(incorrect)
(define (c2? l)
  (cond
    ((null? l) false)
    ((or (eq? l (cdr l)) (eqmem? l (cdr l))) true)
    (else (c2? (cdr l)))))
