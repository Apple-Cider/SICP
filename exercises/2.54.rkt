#lang racket

(define (equal? a b)
  (cond ((eq? a b) true)
        ((and (pair? a) (pair? b)) (and (equal? (car a) (car b))
                                        (equal? (cdr a) (cdr b))))
        (else false)))

(newline) (display "expected: #t") (newline) (display "computed: ")
(equal? (list 1 2 (list 3) 4) (list 1 2 (list 3) 4))

(newline) (display "expected: #f") (newline) (display "computed: ")
(equal? (list 1 2 4) (list 1 2 3))

(define a (list 5 6 7))
(define b 'a)
(newline) (display "expected: #f") (newline) (display "computed: ")
(equal? a b)

(newline) (display "expected: #t") (newline) (display "computed: ")
(equal? 'a b)