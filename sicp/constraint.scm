; set-value!
; get-value
; add-prop!
; forget-value!
; probe
; make-connector
; multiplier
; adder
; constant


(define (make-connector)
  (let ((val false) (props ()) (const false))
    (define (set-value! v before)
      (cond
       ((false? val) (set! val v) 'ok)
       ((false? v)
        (cond
         (const 'ok)
         (else (set! val v) 'ok)))
       ((eq? val v) 'ok)
       (else (error "contradiction")))
      (if (not const)
          (for-each
           (lambda (p)
             (if (not (member p before))
                 (begin (display "hi") (p (cons p before)))))
           props)))

    (define (get-value) val)

    (define (set-const!) (set! const true))
    ;; (define (const?) const)

    (define (get-props) props)

    ;; (define (add-poll! p)
    ;;   (set! polls (cons p polls)))
    (define (add-prop! p)
      (set! props (cons p props)))
    
    (define (dispatch m)
      (cond
       ((eq? m 'set-value!) set-value!)
       ((eq? m 'get-value) get-value)
       ;; ((eq? m 'add-poll!) add-poll!)
       ((eq? m 'add-prop!) add-prop!)
       ((eq? m 'get-props) get-props)
       ((eq? m 'const?) const)
       ((eq? m 'set-const!) set-const!)))
    dispatch))

(define (set-value! con val before)
  ((con 'set-value!) val before))

(define (forget-value! con)
  (set-value! con false ()))

(define (add-prop! con prop)
  ((con 'add-prop!) prop))

(define (probe connect)
  (add-prop! connect
             (lambda (before) (display (get-value connect)))))

(define (const? connect)
  (connect 'const?))

(define (get-props con)
  ((con 'get-props)))

;; (define (add-poll! con poll)
;;   ((con 'add-poll!) con poll))

(define (get-value con)
  ((con 'get-value)))


(define (constant const connect)
  ;; ((connect 'set-const!))
  (define (prop before)
    (if (and (get-value connect) (not (eq? (get-value connect) const)))
        (error "constant: contradiction" connect const (get-value connect))))
  (add-prop! connect prop)
  (set-value! connect const ())
  ((connect 'set-const!)))

(define (div a b sym)
  (if (zero? b)
      (error "division by zero" a b sym)
      (/ a b)))

(define (adder a b c)
  (define (prop-a before)
    (let ((a-val (get-value a))
          (b-val (get-value b))
          (c-val (get-value c)))
      (cond
       ((false? a-val)
        (set-value! b false (cons prop-a before))
        (set-value! c false (cons prop-a before)))
       (b-val (set-value! c (+ a-val b-val) (cons prop-a before)))
       (c-val (set-value! b (- c-val a-val) (cons prop-a before))))))
  (add-prop! a prop-a)

  (define (prop-b before)
    (let ((a-val (get-value a))
          (b-val (get-value b))
          (c-val (get-value c)))
      (cond
       ((false? b-val)
        (set-value! a false (cons prop-b before))
        (set-value! c false (cons prop-b before)))
       (a-val (set-value! c (+ a-val b-val) (cons prop-b before)))
       (c-val (set-value! a (- c-val b-val) (cons prop-b before))))))
  (add-prop! b prop-b)

  (define (prop-c before)
    (let ((a-val (get-value a))
          (b-val (get-value b))
          (c-val (get-value c)))
      (cond
       ((false? c-val)
        (set-value! a false (cons prop-c before))
        (set-value! b false (cons prop-c before)))
       (a-val (set-value! b (- c-val a-val) (cons prop-c before)))
       (b-val (set-value! a (- c-val b-val) (cons prop-c before))))))
  (add-prop! c prop-c))

(define (multiplier a b c)
  (define (prop-a before)
    (let ((a-val (get-value a))
          (b-val (get-value b))
          (c-val (get-value c)))
      (cond
       ((false? a-val)
        (set-value! b false (cons prop-a before))
        (set-value! c false (cons prop-a before)))
       (b-val (set-value! c (* a-val b-val) (cons prop-a before)))
       (c-val (if (not (zero? a-val)) (set-value! b (div c-val a-val 'here) (cons prop-a before)))))))
  (add-prop! a prop-a)

  (define (prop-b before)
    (let ((a-val (get-value a))
          (b-val (get-value b))
          (c-val (get-value c)))
      (cond
       ((false? b-val)
        (set-value! a false (cons prop-b before))
        (set-value! c false (cons prop-b before)))
       (a-val (set-value! c (* a-val b-val) (cons prop-b before)))
       (c-val (if (not (zero? b-val)) (set-value! a (div c-val b-val 'there) (cons prop-b before)))))))
  (add-prop! b prop-b)

  (define (prop-c before)
    (let ((a-val (get-value a))
          (b-val (get-value b))
          (c-val (get-value c)))
      (cond
       ((false? c-val)
        (set-value! a false (cons prop-c before))
        (set-value! b false (cons prop-c before)))
       (a-val (if (not (zero? a-val)) (set-value! b (div c-val a-val 'any) (cons prop-c before))))
       (b-val (if (not (zero? b-val)) (set-value! a (div c-val b-val 'where) (cons prop-c before)))))))
  (add-prop! c prop-c))

;; (define a (make-connector))
;; (define b (make-connector))
;; (define c (make-connector))
(define c (make-connector))
(define f (make-connector))

(define u (make-connector))
(define v (make-connector))
(define w (make-connector))
(define x (make-connector))
(define y (make-connector))

(multiplier c w u)
(multiplier v x u)
(adder v y f)
(constant 9 w)
(constant 5 x)
(constant 32 y)

