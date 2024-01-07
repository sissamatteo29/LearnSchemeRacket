#lang Racket

#|
The purpose of this code is to simulate Object Oriented Programming in the form of prototypes.
Classes do not exist and objects are created by cloning other objects existing in the environment.
In order to simulate this, hash tables are employed (to represent objects). Hash tables associate a key with a value, and in this case the
key becomes the message that the object receives, while the value is what the object returns. The value can either be an actual value or a "method",
so a lambda function.
|#

(define (make-obj)
  (make-hash))

(define (clone obj)
  (hash-copy obj))

(define (add-value obj msg value)
  (hash-set! obj msg value))

(define (take-value obj msg)
  (hash-ref obj msg))

; This version of hash-ref takes a default value that is returned in the case the key is not present in the hash table.
(define (take-value-def obj msg default)
  (hash-ref obj msg default))

(define (run-method obj msg . rest)
  (if (not (hash-has-key? obj msg))
      (displayln "no such method")
      (apply (take-value obj msg) rest)))

; Testing these basic methods
(define my-object (make-obj))
(add-value my-object 'name "Marco")
(add-value my-object 'fun (lambda () (displayln "Hello everybody")))
(add-value my-object 'func (lambda (x) (displayln (* x x))))
(take-value my-object 'name)
((take-value my-object 'fun))
(run-method my-object 'func 3)

; Now we can implement inheritance. In order to model inheritance in the easiest possible way, all objects that have a
; parent have to show the parent object under the 'parent key. If the 'parent key doesn't exist, the object has no parent.
; With this model for inheritance, in a prototype OOP it is necessary to redefine the way objects are cloned (deep clone).

(define (deep-copy obj)
  (if (not (hash-has-key? obj 'parent))
      (clone obj)
      (let* ((cl (clone obj))
             (cl-parent (deep-copy (take-value obj 'parent))))
        (add-value cl 'parent cl-parent)
        cl)))

; A simple example application in which a copy of two son-parent objects is performed.
(define son (make-obj))
(define parent (make-obj))
(add-value son 'parent parent)
(define d-copy (deep-copy son))
(add-value son 'hello (lambda () (displayln "hello")))
(run-method son 'hello)
(run-method d-copy 'hello)

; Now it is possible to create a dispatcher method that checks if the message is available on the current object,
; otherwise it delegates to the parent.

(define (dispatch obj msg . rest)
  (if (hash-has-key? obj msg)
      
      (if (procedure? (take-value obj msg))
          (apply (take-value obj msg) rest)
          (take-value obj msg))
      
      (if (hash-has-key? obj 'parent)
          (let ((par (take-value obj 'parent)))
            (apply dispatch (cons par (cons msg rest))))
          (displayln "no such key"))))

; Trying the dispatcher
(add-value parent 'double (lambda (x) (* 2 x)))
(add-value parent 'data "user@email.com")
(dispatch son 'double 2)
(dispatch son 'data)

            