#lang Racket

#|
Consider the following For construct:

(define-syntax For  
  (syntax-rules (from to break: do)    
  ((_ var from min to max break: break-sym         
      do body ...)     
   (let* ((min1 min)      
          (max1 max)            
          (inc (if (< min1 max1) + -)))       
   (call/cc (lambda (break-sym)                  
      (let loop ((var min1))                    
      body ...                    
      (unless (= var max1)                      
      (loop (inc var 1)))))))))) 


 Define a fix to the above definition, to avoid to introduce in the macro definition the special break symbol break-sym, 
 by providing a construct called break. 

 E.g. (For i from 1 to 10 do      
      (displayln i)     
      (when (= i 5)         
      (break #t))) 
will return #t after displaying the numbers from 1 to 5.
|#


(define break-list (list))

(define (push-break cont)
  (set! break-list (cons cont break-list)))

(define (break v)
  ((car break-list) v ))

(define-syntax For
  (syntax-rules (from to do)
    ((_ var from min to max do expr ...)
     (let* ((min1 min)
            (max1 max)
            (inc (if (< min1 max1) + -)))
       (call/cc (lambda (break)
             (push-break break)
             (let loop ((var min1))
               (unless (= var max1)
                 expr ...
                 (loop (inc var 1))))))))))

(For i from 1 to 10 do      
      (displayln i)     
      (when (= i 5)         
      (break #t))) 
             
