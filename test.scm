
(define (prime? n)
  (= n (smallest-divisor n)))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
    (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))
(define (smallest-divisor n) (find-divisor n 2))

(define (next n) (if (= n 2) 3 (+ n 2)))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))
(define (divides? a b) (= (remainder b a) 0))

(define (search-for-primes fr to) (if (> fr to) 1 (begin (timed-prime-test fr) (search-for-primes (+ fr 1) to))))


(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder
           (square (expmod base (/ exp 2) m))
           m))
        (else
          (remainder
            (* base (expmod base (- exp 1) m))
            m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

; 1000000007
; 1000000009
; 1000000021
; 1000000033
; 1000000087
; 1000000093
;
; 1000000097      *** 2.0000000000000018e-2
; 10000000019     *** .06999999999999984
; 10000000033     *** .06999999999999984
; 10000000061     *** .06999999999999984
; 10000000069     *** .07000000000000028
; 10000000097     *** .07000000000000028
; 100000000003    *** .20999999999999996
; 100000000019    *** .20000000000000018
; 100000000057    *** .20999999999999996
; 100000000063    *** .20999999999999996
; 100000000069    *** .20000000000000018
; 100000000073    *** .20999999999999996
; 100000000091    *** .20999999999999996
; 1000000000039   *** .6399999999999997
; 1000000000061   *** .6299999999999999
; 1000000000063   *** .6400000000000006
; 1000000000091   *** .6299999999999999
; 10000000000037  *** 2.
; 10000000000051  *** 2.
; 10000000000099  *** 1.9900000000000002
; 100000000000031 *** 6.32
; 100000000000067 *** 6.280000000000001
; 100000000000097 *** 6.340000000000003
; 100000000000099 *** 6.32
(timed-prime-test 1000000097     )
(timed-prime-test 10000000019    )
(timed-prime-test 10000000033    )
(timed-prime-test 10000000061    )
(timed-prime-test 10000000069    )
(timed-prime-test 10000000097    )
(timed-prime-test 100000000003   )
(timed-prime-test 100000000019   )
(timed-prime-test 100000000057   )
(timed-prime-test 100000000063   )
(timed-prime-test 100000000069   )
(timed-prime-test 100000000073   )
(timed-prime-test 100000000091   )
(timed-prime-test 1000000000039  )
(timed-prime-test 1000000000061  )
(timed-prime-test 1000000000063  )
(timed-prime-test 1000000000091  )
(timed-prime-test 10000000000037 )
(timed-prime-test 10000000000051 )
(timed-prime-test 10000000000099 )
(timed-prime-test 100000000000031)
(timed-prime-test 100000000000067)
(timed-prime-test 100000000000097)
(timed-prime-test 100000000000099)
