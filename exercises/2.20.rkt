#lang racket

(define (same-parity-i x . z)
  (define (parity-iter result remaining)
    (cond ((null? remaining) result)
          ((= (remainder (car result) 2)
              (remainder (car remaining) 2))
           (parity-iter (append result
                                (list (car remaining)))
                        (cdr remaining)))
          (else (parity-iter result (cdr remaining)))))
  (parity-iter (list x) z))

(define (same-parity-r x . z)
  (define (recurser remaining)
    (cond ((null? remaining) '())
          ((= (remainder x 2)
              (remainder (car remaining) 2))
           (cons (car remaining) (recurser (cdr remaining))))
          (else (recurser (cdr remaining)))))
  (cons x (recurser z)))


(display "expected:  '(1 13 25 37)")
(newline)
(display "iterative: ")
(same-parity-i 1 12 13 24 25 36 37)
(display "resursive: ")
(same-parity-r 1 12 13 24 25 36 37)

(newline)
(display "expected:  '(2 4 6 8)")
(newline)
(display "iterative: ")
(same-parity-i 2 3 4 5 6 7 8)
(display "resursive: ")
(same-parity-r 2 3 4 5 6 7 8)
