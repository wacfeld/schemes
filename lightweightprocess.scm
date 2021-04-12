;; setting up data structures
(define make-queue
  (lambda ()
    (let ((end (cons 'ignored '())))
      (cons end end))))
(define putq!
  (lambda (q v)
    (let ((end (cons 'ignored '())))
      (set-car! (cdr q) v)
      (set-cdr! (cdr q) end)
      (set-cdr! q end))))
(define getq
  (lambda (q)
    (car (car q))))
(define delq!
  (lambda (q)
    (set-car! q (cdr (car q)))))
(define empty?
  (lambda (q)
    (= (length (car q)) 1)))

(define make-stack
  (lambda ()
    (let ((ls '()))
      (lambda (msg . args)
        (cond
          ((eqv? msg 'empty?) (null? ls))
          ((eqv? msg 'push!) (set! ls (cons (car args) ls)))
          ((eqv? msg 'top) (car ls))
          ((eqv? msg 'pop!) (set! ls (cdr ls)))
          ((eqv? msg 'len) (length ls))
          (else "oops"))))))

;; the stack holds continuations for (quit) to make use of
(define stacktrace (make-stack))

(define lwp-queue (make-queue))
(define quit-k #f)
(define lwp
  (lambda (thunk)
    (putq! lwp-queue thunk)))
(define next
  (lambda ()
    (let ((p (getq lwp-queue)))
      (display (stacktrace 'len))
      (delq! lwp-queue)
      (call/cc
        (lambda (k)
          (stacktrace 'push! k)
          (p)))
      (stacktrace 'pop!)
      (if (empty? lwp-queue)
        (exit)
        (next)))))
(define start
  (lambda ()
    (call/cc
      (lambda (k)
        (set! quit-k k)
        (next)))))
; the problem is that, with my stack system, it just keeps growing. i think the solution 
; would be to, instead of having pause call next, pause calls a continuation in next, and
; then the same next process handles the queue directly. then, quit would simply return
; control to next without putting itself at the end of the queue. this eliminates the need
; for a stack, instead having just one continuation leading back to whatever next was just doing.
; so we have a queue which is  filled with thunks and continuations of those thunks, and at ony
; moment we have a single separate continuation leading back to the current next, getting 
; replaced as next moves along.
(define pause
  (lambda ()
    (call/cc
      (lambda (k)
        (lwp (lambda () (k #f)))
        (next)))))
(define quit
  (lambda ()
    (let ((k (stacktrace 'top)))
      (stacktrace 'pop!)
      (k #f))))

;; the two methods of leaving a process are calling (quit) and simply dropping off without recursing. the second and third functions below display that.
(lwp (lambda () (let f () (pause) (display "h") (f))))
(lwp (lambda () (let f () (quit) (display "e") (f))))
(lwp (lambda () (let f () (pause) (display "y") )))
(lwp (lambda () (let f () (pause) (display "!") (f))))
(lwp (lambda () (let f () (pause) (newline)     (f))))
(start)
