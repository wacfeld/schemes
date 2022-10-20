;; this works, but it still uses a list indexing function
;; therefore it's simpler to just index the middle element(s)
;; an actual solution would permute the list without any indexing

;; g: get median of sorted list
(define (median sl)
  (local
    ((define len (length sl))
    (define pl (foldl cons empty (permute sl (backforth len)))))
    (cond
      ((even? len) (/ (+ (first pl) (second pl)) 2))
      ((odd? len) (first pl)))))


;; sg: permute based on list of indices
(define (permute lst perm)
  (map (lambda (n) (index lst n)) perm))

;; ssg: indexing function
(define (index lst n)
  (foldr (lambda (x y z) (cond ((= y n) x) (else z)))
         false
         lst
         (build-list (length lst) (lambda (x) (add1 x)))))

;; ssg: create (5 1 4 2 3) (using build-list)

(define (backforth n)
  (build-list n
              (lambda (x)
                (cond
              ((even? x) (- n (/ x 2)))
              (else (add1 (/ (sub1 x) 2)))))))
