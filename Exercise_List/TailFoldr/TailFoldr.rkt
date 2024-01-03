#lang Racket

#|
Create a procedure that implements the foldr function in a tail-recursive manner.
|#

(define (tail-foldr fun base list)
  (define (tail-foldr-helper fun base list acc)
    (if (null? list)
        (acc base)
        (tail-foldr-helper fun base (cdr list) (lambda (x) (acc (fun (car list) x))))))
  (tail-foldr-helper fun base list (lambda (x) x)))

(let ((my-string (tail-foldr string-append "" '("hello" "Stranger"))))
  (display my-string) (newline))

#|
The concept here is to create a helper function that has an additional parameter acc. This is an accumulator that stores all the future computation
for the recursive calls. So, instead of calling the function f externally (which would make the foldr not tail-recursive), the recursive calls on the f
function are stored in a lambda function, which is then called at the end of the reading process for the list parameter.
|#


#|
Since the previous procedure is tail-recursive, it is possible to optimize it with a loop.
|#

(define (loop-foldr fun base list)
  (let loop ((f fun)
             (b base)
             (l list)
             (acc (lambda (x) x)))
    (if (null? l)
        (acc b)
        (loop f b (cdr l) (lambda (x) (acc (f (car l) x)))))))

(let ((my-string (loop-foldr string-append "" '("hello" "Stranger"))))
  (display my-string) (newline))