(define sort2
  (lambda (l)
    (if (> (car l) (cadr l))
      (cons (cadr l) (cons (car l) ()))
      l)))

(define qsort
  (lambda (l)
    (cond
      ((<= (length l) 1) l)
      ((= (length l) 2) (sort2 l))
      (else (let ((p (part l '() '())))
              (append (qsort (car p)) (cadr p) (qsort (cddr p))))))))

(define part
  (lambda (l p1 p2)
    (cond
      ((= 1 (length l)) (cons p1 (cons l p2)))
      (else (if (< (cadr l) (car l))
              (part (cons (car l) (cddr l)) (cons (cadr l) p1) p2)
              (part (cons (car l) (cddr l)) p1 (cons (cadr l) p2)))))))

(define mymin
  (lambda (l m)
    (cond
      ((null? l) m)
      ((< (car l) m) (mymin (cdr l) (car l)))
      (else (mymin (cdr l) m)))))

(define rem
  (lambda (l m)
    (cond
      ((null? l) ())
      ((= (car l) m) (cdr l))
      (else (cons (car l) (rem (cdr l) m))))))

(define insort
  (lambda (l)
    (cond
      ((null? l) ())
      (else
        (let ((m (mymin l 1000)))
          (cons m (insort (rem l m))))))))


(define mylist '(49 50 27 90 48 93 52 36 51 25 11 12 46 98 8 28 9 6 74 78 72 53 31 82 39 7 99 60 92 26 34 3 61 81 62 73 100 37 58 56 89 59 33 76 87 16 40 97 13 85 29 32 44 41 10 54 70 75 95 65 22 14 57 91 88 30 84 86 94 1 64 83 80 68 20 43 96 21 77 2 55 17 15 69 42 45 63 47 19 18 79 24 35 66 38 5 4 71 67 23))

;(let loop ((n 100))
;  (cond
;    ((zero? n) #t)
;    (else (begin
;            (qsort mylist)
;            (loop (- n 1))))))

(define myloop
  (lambda (n)
    (cond
      ((zero? n) #t)
      (else (begin
              (qsort mylist)

              (myloop (- n 1)))))))

(display (qsort mylist))
