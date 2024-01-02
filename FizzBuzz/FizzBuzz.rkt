#lang Racket

(define (fizz-buzz)
  (let loop ((count 0))
    (when (<= count 100)
    (cond
      ((and (= 0 (modulo count 3)) (= 0 (modulo count 5))) (display "FizzBuzz") (newline))
      ((= 0 (modulo count 3)) (display "Fizz") (newline))
      ((= 0 (modulo count 5)) (display "Buzz") (newline))
      (else (display count) (newline))))
    (loop (+ 1 count))))

(fizz-buzz)