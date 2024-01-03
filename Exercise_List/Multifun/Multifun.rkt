#lang Racket

#| Design a construct to define multiple functions with the same number of arguments at the same time.
The proposed syntax is the following:

(multifun <list of function names> <list of parameters> <list of bodies>).

E.g.  (multifun (f g) (x) ((+ x x x) (* x x)))

defines the two functions f with body (+ x x x) and g with body (* x x), respectively.|#

(define-syntax multifun
  (syntax-rules ()

    ((_ (fun) (var ...) (body))
     (define (fun var ...) body))

    ((_ (fun . fun-rest) (var ...) (body . rest-body))
     (begin                      ; begin clause fundamental when multiple expansion operations are needed!
     (define (fun var ...) body)
     (multifun fun-rest (var ...) rest-body)))))

(multifun (f) () (4))
(display (f))
(newline)

(multifun (t g) (x) ((+ x x x) (* x x)))
(display (t 5))
(newline)
(display (g 8))
(newline)