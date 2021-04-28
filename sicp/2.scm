(define (make-rat n d)
  (let ((g (gcd n d)))
    (if (negative? d)
      (cons (/ (- n) g) (/ (- d) g))
      (cons (/ n g) (/ d g)))))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (average a b) (/ (+ a b) 2))

(define make-point cons)
(define start-segment car)
(define end-segment cdr)
(define x-point car)
(define y-point cdr)
(define make-segment cons)
(define (midpoint-segment p q)
  (make-point (average (x-point p) (x-point q)) (average (y-point p) (y-point q))))
(print-point (midpoint-segment '(5 . 3) '(3 . 7)))

(define (make-pair a b)
  (* (expt 2 a) (expt 3 b)))
(define (mycar p)
  (let ((try (/ p 2)))
    (if (integer? try)
     (+ 1 (mycar try))
     0)))
(define (mycdr p)
  (let ((try (/ p 3)))
    (if (integer? try)
      (+ 1 (mycdr try))
      0)))

(define zero (lambda (f) (lambda (x) x)))
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))
(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))
(define (add m n)
  (lambda (f) (lambda (x) ((m f) ((n f) x)))))



(define make-interval cons)
(define upper-bound cdr)
(define lower-bound car)
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))
(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))
(define (div-interval x y)
  (if (and (<= (lower-bound y) 0) (>= (upper-bound y) 0) )
    (display "error")
    (mul-interval
      x
      (make-interval (/ 1.0 (upper-bound y))
                     (/ 1.0 (lower-bound y))))))
(define (sub-interval x y)
  (add-interval
    x
    (make-interval (- (upper-bound y))
                   (- (lower-bound y)))))

;2.11 the special case is -+ -+. i will not write them out.
(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))
(define (make-center-percent c p)
  (let ((p/100 (/ p 100))) (make-interval (- c (* c p/100)) (+ c (* c p/100)))))
(define (percent x) (/ (width x) (center x)))

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))
(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval
      one (add-interval (div-interval one r1)
                        (div-interval one r2)))))

(define A '(99.9 . 100.1))
(define B '(89.9 . 90.1))
(define (print-int x)
  (display (center x))
  (newline)
  (display (percent x))
  (newline))
;2.15: looking at an equation as a whole, with n variables (no duplicates), try all 2^n combinations of max and man values?

;2.2
(define (last-pair l)
  (cond
    ((null? l) (display "error"))
    ((null? (cdr l)) l)
    (else (last-pair (cdr l)))))
(define (rev l)
  (define (go l r)
    (if (null? l)
      r
      (go (cdr l) (cons (car l) r))))
  (go l ()))

(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 50 100 20 10 5 2 1 0.5))
(define (count-change amount) (cc amount 5))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
          (+ (cc amount
                 (except-first-denomination
                   coin-values))
             (cc (- amount
                    (first-denomination
                      coin-values))
                 coin-values)))))
(define first-denomination car)
(define except-first-denomination cdr)
(define no-more? null?)

(define (same-parity w . z)
  (define (iter w z)
    (cond
      ((null? z) ())
      ((even? (+ w (car z))) (cons (car z) (iter w (cdr z))))
      (else (iter w (cdr z)))))
  (cons w (iter w z)))


(define (square x) (* x x))
(define (square-list items)
  (if (null? items)
    ()
    (cons (square (car items)) (square-list (cdr items)))))
(define (square-list2 items)
  (map square items))

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
      answer
      (iter (cdr things)
            (cons answer
                  (square (car things))))))
  (iter items ()))

(define (myforeach f l)
  (if (null? l)
    true
    (begin
      (f (car l))
      (myforeach f (cdr l)))))

(define (deep-reverse l)
  (define (iter l r)
    (cond
      ((null? l) r)
      ((not (pair? l)) l)
      (else (iter (cdr l) (cons (deep-reverse (car l)) r)))))
  (iter l ()))

(deep-reverse '(1 (2 3) (4 ((5 6) 7)) 8))

(define (fringe l)
  (cond
    ((null? l) ())
    ((not (pair? l)) (list l))
    (else (append (fringe (car l)) (fringe (cdr l))))))
(fringe '(1 (2 3) (4 ((5 6) 7)) 8))
(fringe ())
(fringe '(1))

(define (make-branch length structure)
  (list length structure))
(define (make-mobile left right)
  (list left right))

(define left-branch car)
(define right-branch cdr)
(define branch-length car)
(define branch-structure cdr)

(define (total-weight mob)
  (if (number? mob)
    mob
    (+ (total-weight (branch-structure (left-branch mob)))
       (total-weight (branch-structure (right-branch mob))))))
;(total-weight '(1 (1 (1 (2 3)))))

(define (balanced? mob)
  (if (number? mob)
    mob
    (let ((b1 (balanced? (branch-structure (left-branch mob))))
          (b2 (balanced? (branch-structure (right-branch mob)))))
      (if (and b1
               b2
               (= (* (branch-length (left-branch mob)) b1)
                  (* (branch-length (right-branch mob)) b2)))
        (+ b1 b2)
        #f))))

(define (make-mobile left right) (cons left right))
(define (make-branch length structure)
(cons length structure))
(balanced? '((5 . 6) . (6 . 5)))


(define (tree-map f t)
  (cond
    ((null? t) ())
    ((pair? t) (cons (tree-map f (car t)) (tree-map f (cdr t))))
    (else (f t))))
(define (square-tree t) (tree-map square t))
(square-tree
  (list 1
        (list 2 (list 3 4) 5)
        (list 6 7)))


(define (subsets s)
  (cond
    ((null? s) '(()))
    (else (let ((rest (subsets (cdr s))))
      (append rest (map (lambda (x) (cons (car s) x)) rest))))))
(subsets '(1 2 3))


(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence)
        (accumulate op initial (cdr sequence)))))

(define (mymap p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) () sequence))

(define (myappend s1 s2)
  (accumulate cons s2 s1))

(define (mylength l)
  (accumulate (lambda (x y) (1+ y)) 0 l))

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
                 (+ (* x higher-terms) this-coeff))
              0
              coefficient-sequence))

(define (count-leaves t)
  (accumulate (lambda (x y) (+ x y)) 0
              (map (lambda (e)
                     (if (pair? e)
                       (count-leaves e)
                       1))
                   t)))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
    ()
    (cons (accumulate op init (map car seqs))
          (accumulate-n op init (map cdr seqs )))))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))
(define (matrix-*-vector m v)
  (map (lambda (w) (dot-product v w)) m))

(define (transpose mat)
  (accumulate-n cons () mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (transpose (map (lambda (v) (matrix-*-vector m v)) cols))))
(fold-right / 1 (list 1 2 3))

(fold-right cons () (list 1 2 3))
(fold-left cons () (list 1 2 3))

(define (myrev seq)
  (fold-right (lambda (x y) (append y (list x))) () seq))
(define (myrev2 seq)
  (fold-left (lambda (x y) (cons y x)) () seq))

(define nil ())
(define (enumerate-interval low high)
  (if (> low high)
    nil
    (cons low (enumerate-interval (+ low 1) high))))
(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))
(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence)
        (accumulate op initial (cdr sequence)))))

(define (unique-pairs n)
  (accumulate
    append nil (map (lambda (i)
                      (map (lambda (j) (list j i))
                           (enumerate-interval 1 (- i 1))))
                    (enumerate-interval 1 n))))

(define (prime? n)
  (= n (smallest-divisor n)))
(define (smallest-divisor n) (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b) (= (remainder b a) 0))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))
(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum? (unique-pairs n))))


(define (permutations s)
  (if (null? s)
     ; empty set?
     (list nil)
      ; sequence containing empty set
      (flatmap (lambda (x)
                 (map (lambda (p) (cons x p))
                      (permutations (remove x s))))
               s)))
(define (remove item sequence)
  (filter (lambda (x) (not (= x item)))
          sequence))

(define (triples n)
  (flatmap (lambda (i)
             (flatmap (lambda (j)
                        (flatmap (lambda (k) (list (list k j i)))
                                 (enumerate-interval 1 (- j 1))))
                      (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

(define (sumton n)
  (accumulate append nil (map permutations (filter (lambda (a) (= (+ (car a) (cadr a) (caddr a)) n)) (triples n)))))



