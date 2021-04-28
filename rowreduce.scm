(define (getrow m i)
  (if (= i 1)
    (car m)
    (getrow (cdr m) (- i 1))))

(define (setrow m i r)
  (if (= i 1)
    (cons r (cdr m))
    (cons  (car m) (setrow (cdr m) (- i 1) r))))

(define (swaprow m i j)
  (let ((t (getrow m i)))
    (setrow
      (setrow m i (getrow m j))
      j
      t)))

(define (mulrow m i c)
  (setrow m i
          (map (lambda (x) (* x c))
               (getrow m i))))

(define (addrow m i j)
  (setrow m j
          (map +
               (getrow m i)
               (getrow m j))))
