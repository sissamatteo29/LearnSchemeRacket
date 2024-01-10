#lang Racket

#|
Define a new syntax form to simulate a try-catch block.
The try block takes a series of expressions and evaluates them. If all the expressions are executed without errors,
the value of the last expression is returned.
The catch block starts with a string describing the error that is caught and then there is a series of expressions to
evaluate in case the error is raised.
If an error which is not described in the catch is raised, a delegation mechanism has to be implemented, so that
the catch blocks of the other functions (in higher positions of the control flow graph) can be executed.
|#


(define queue '())

(define (push-handler h)
  (set! queue (cons h queue)))

(define (pop-handler)
  (if (not (null? queue))
      (let ((h (car queue))
            (t (cdr queue)))
        (set! queue t)
        h)
      (error "No handler found")))

(define (throw err)
  (let ((h (pop-handler)))
    (h err)))

; Simple testing
(push-handler 4)
(pop-handler)

(define-syntax try
  (syntax-rules (catch)
    ((_ expr-try ... (catch err expr-catch ...))
     (call/cc (lambda (exit)
                (push-handler (lambda (catch-error)
                                 (if (eqv? err catch-error)
                                     (exit (begin expr-catch ...))
                                     (throw catch-error))))
                 (let ((res (begin expr-try ...)))
                   (pop-handler)
                   res))))))

(define (try-division x y )
(try
  (if (= y 0)
      (throw "DivisionByZero")
      (/ x y))
  (catch "DivisionByZero"
         (displayln "Sorry, division by zero encountered"))))

(try-division 10 5)
(try-division 10 0)