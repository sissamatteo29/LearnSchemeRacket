#lang Racket

#|
Define a let** construct that behaves like the standard let*, but gives to variables provided without a binding
the value of the last defined variable.
It also contains a default value, stated by a special keyword def:,
to be used if the first variable is given without binding.

For example:

(let** def: #f (a (b 1) (c (+ b 1)) d (e (+ d 1)) f)
(list a b c d e f))

should return '(#f 1 2 2 3 3), because a assumes the default value #f, while d = c and f = e.
|#

(define-syntax let**
  (syntax-rules (def:)

    ((_ def: last ((var val)) body ...)
     (let* ((var val)) body ...))

    ((_ def: last ((var)) body ...)
     (let* ((var last)) body ...))

    ((_ def: last ((var val) . rest) body ...)
      (begin
        (let* ((var val))
          (let** def: val rest body ...))))

    ((_ def: last (var . rest) body ...)
     (begin
       (let* ((var last))
         (let** def: last rest body ...))))
  ))

(let** def: #f (e (b 3) (c 5)) (display b) (display e))



