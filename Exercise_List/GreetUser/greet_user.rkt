#lang Racket

(define (greet-user)
  (display "Hello, type in your name please: ") (newline)
  (let ((name (read-line)))
    (display (string-append "Welcome " name "!")) (newline)))

(greet-user)

(define (greet-Alice-Bob)
  (display "Hello, enter your name please: ") (newline)
  (let ((name (read-line)))
    (if (equal? name "Bob") (display "Hello, Bob!") ; "equal?" fundamental for comparison of strings in this case
        (if (equal? name "Alice") (display "Hello, Alice")
            (begin
            (display "Error, unacceptable name!")
            (newline))))))

(greet-Alice-Bob)