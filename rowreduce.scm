(load "io.scm")

(define (getrow m i)
  (if (= i 1)
    (car m)
    (getrow (cdr m) (- i 1))))

(define (getelem m i j)
  (getrow (getrow m i) j))

(define (getdiag m i)
  (getrow (getrow m i) i))

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

(define numrow length)
(define (numcol m)
  (length (car m)))

(define (non0row m i j)
  (define (iter i)
    (cond
      ((> i (numrow m)) 0)
      ((zero? (getelem m i j)) (iter (1+ i)))
      (else i)))
  (iter (1+ i)))

(define (coliso m i)
  (if (zero? (getdiag m i))
    (let ((r (non0row m i i)))
      (if (zero? r)
        m
        (swaprow m i r)))
    m))

(define (norm m i)
  (let ((e (getdiag m i)))
    (if (zero? e)
      m
      (mulrow m i (/ 1 (getdiag m i))))))

;ironic how the clean function is very messy because i refused to implement r2 + c*r1 in a separate function
(define (clean m r1 r2)
  (let ((c r1))
    (let ((v1 (getelem m r1 c)) (v2 (getelem m r2 c)))
      (if (zero? v1)
        m
        (let ((scale (- (/ v2 v1))))
          (let ((row (map (lambda (x) (* scale x)) (getrow m r1))))
            (setrow m r2 (map + (getrow m r2) row))))))))
;(addrow (mulrow m r1 (- (/ v2 v1))) r1 r2)))))

(define (colclean m i)
  (define (iter m j)
    (cond
      ((> j (numrow m)) m)
      ((= i j) (iter m (1+ j)))
      (else (iter (clean m i j) (1+ j)))))
  (iter m 1))

(define (rowred m)
  (define (iter m i)
    (if (or (> i (numcol m)) (> i (numrow m)))
      m
      (iter (colclean (norm (coliso m i) i) i) (1+ i))))
  (iter m 1))

; (define mat
;   '((0 0 1 1)
;     (1 1 1 -1)
;     (4 2 1 5)))
(define mat
  '((3 1 6 6)
    (4 2 6 3)
    (2 6 2 1)))
(rowred mat)
