#lang racket
(define (same-parity x . z)
  (display "z: ")
  (display z)
  (newline)
  (let ((parity (remainder x 2)))
    (define (parity-iter result remaining)
      (cond ((null? remaining) result)
            ((= parity (remainder (car remaining) 2))
             (display "found parity match: ")
             (display (car remaining))
             (newline)
             (parity-iter (append result (list (car remaining))) (cdr remaining)))
            (else (parity-iter result (cdr remaining)))))
    (parity-iter (list x) z)))
    

(same-parity 1 2 3 4 5 6 7)

(define (list-check x . z)
  (display "      z: ")
  (display z)
  (newline)
  (display "(car z): ")
  (display (car z))
  (newline))

;(list-check 1 2 3 4 5)
;(list-check 1 '(2 3 4 5))

(define (cond-check)
  (cond ((= 2 2) (newline) (display "2nd statement") (newline) (display "4th statement"))
        (else (newline))))