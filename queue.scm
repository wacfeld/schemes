(define make-queue
  (lambda ()
    (cons () ())))

(define putq!
  (lambda (q v)
    (let ((end (cons v ())))
      (if (null? (car q))
        (begin
          (set-car! q end)
          (set-cdr! q end))
        (begin
          (set-cdr! (cdr q) end)
          (set-cdr! q end))))))
      

(define getq
  (lambda (q)
    (car (car q))))

(define delq!
  (lambda (q)
    (set-car! q (cdar q))
    (if (null? (car q))
      (set-cdr! q ()))))
