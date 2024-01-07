#lang Racket

;SIMULATION OF OBJECT-ORIENTED PROGRAMMING THROUGH CLOSURES

(define (calculator)
  (let ((data 0))

    (define (get-data)
      data)

    (define (set-data value)
      (set! data value))
    
    (define (add-one)
      (set! data (+ 1 data)))

    (define (dec-one)
      (set! data (- 1 data)))

    (define (add x)
      (set! data (+ x data)))

    (define (dec x)
      (set! data (- data x)))

    ; dispatcher, aka what is returned from the closure.
    (lambda (message . args)
      (case message
        ((get-data) (get-data))
        ((set-data) (apply set-data args))
        ((add-one) (add-one))
        ((dec-one) (dec-one))
        ((add) (apply add args))
        ((dec) (apply dec args))))))

(define first-obj (calculator))
(first-obj 'add-one)
(first-obj 'add-one)
(first-obj 'get-data)
(first-obj 'dec 3)
(first-obj 'get-data)


; INHERITANCE THROUGH DELEGATION

(define (advanced-calculator)
  (let ((parent-calc (calculator)))

    (define (prod x)
      (let ((current (parent-calc 'get-data)))
        (set! current (* x current))
        (parent-calc 'set-data current)))

    ; dispatcher with delegation.
    (lambda (message . args)
      (case message
        ((prod) (apply prod args))
        ((get-data) (parent-calc 'get-data))
        ((set-data) (apply parent-calc (cons 'set-data args)))
        ((add-one) (parent-calc 'add-one))
        ((dec-one) (parent-calc 'dec-one))
        ((add) (apply parent-calc (cons 'add args)))
        ((dec) (apply parent-calc (cons 'dec args)))))))

(define ad-calc (advanced-calculator))
(ad-calc 'set-data 5)
(ad-calc 'get-data)
(ad-calc 'prod 6)
(ad-calc 'get-data)