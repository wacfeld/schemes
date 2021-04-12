(define l?
  (lambda (s)
    (cond
      ((null? s) #t)
      ((pair? s)
       (help s (cdr s)))
      (else #f))))

(define help
  (lambda (tort hare)
    (cond
      ((eqv? tort hare) #f) ;cycle
      ((null? hare) #t)
      ((not (pair? hare)) #f)
      ((null? (cdr hare)) #t)
      ((not (pair? (cdr hare))) #f)
      (else (help (cdr tort) (cddr hare))))))


