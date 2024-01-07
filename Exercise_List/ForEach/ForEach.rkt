#lang Racket

#|
We want to implement a for-each/cc procedure which takes a condition, a list and a body and performs a for-each. 
The main difference is that, when the condition holds for the current value, the continuation of the body is stored in a global queue of continuations. 
We also need an auxiliary procedure, called use-cc, which extracts and call the oldest stored continuation in the global queue, discarding it. 

E.g. if we run: 
(for-each/cc odd?              
		'(1 2 3 4)              
		(lambda (x) (displayln x))) 
two continuations corresponding to the values 1 and 3 will be stored in the global queue. 

Then, if we run: 
(use-scc)
we will get on screen: 2 3 4
|#

(define queue (list))

(define (add_queue cont)
  (set! queue (cons cont queue)))

(define (use-cc)
  (if (pair? queue)
  (let loop ((initial queue) (final '()))
    (let ((first (car initial))
          (rest (cdr initial)))
      (if (pair? rest)
      (loop rest (append final (list first)))
      ;last element of the list
      (begin
        (set! queue final)
        (first)))))
  (displayln "no element")))
    
  

(define (for-each/cc condition L body)
  (for-each (lambda (x)
              (call/cc (lambda (cont)
                         (when (condition x)
                           (add_queue cont))
                         (body x))))
            L))

(for-each/cc odd?
             '(1 2 3 4)
             (lambda (x) (displayln x))
(use-cc)
(use-cc)
             
  
