#lang Racket

#|
Define a list-to-compose pure function, which takes a list containing functions of one argument and
returns their composition.

E.g.
(list-to-compose (list f g h)) is the function f(g(h(x)). 
|#

; We're gonna implement this using a closure, because it is necessary to return a function which
; is parametrized based on the external environment which is the list passed as parameter

(define (list-to-compose l)
  (unless (null? l)
  (lambda (x)
    (let loop ((h (car l))
               (t (cdr l)))
      (if (null? t)
          (h x)
          (h (loop (car t) (cdr t))))))))

; We create two generic functions to compose
(define (double x)
  (* 2 x))
(define (pw x)
  (* x x))

; Let's use the function list-to-compose to compose the functions
(define composition (list-to-compose (list double pw)))

; Let's apply the composition on some value
(composition 3)

; The unless check inside the function makes sure that also an empty list won't cause any error
(list-to-compose '())


#|
A variant of the previous exercise would be the same function (doing the same things), which takes a series of
functions as separated arguments (and not as a list of elements).
|#

; Notice that in this definition it is mandatory to pass at least one function
(define (list-to-compose2 f1 . rest)
  (lambda (x)
    (if (null? rest)
        (f1 x)
        (f1 (let loop ((h (car rest))
                       (t (cdr rest)))
              (if (null? t)
                  (h x)
                  (h (loop (car t) (cdr t)))))))))
         

; Let's use the function list-to-compose2 to compose the functions
(define composition2 (list-to-compose2 double pw))

; Let's apply the composition on some value
(composition2 3)



   