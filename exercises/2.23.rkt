#lang racket

(define (for-each proc items)
  (cond ((null? items) (void))
        (else (proc (car items))
              (for-each proc (cdr items)))))


(define (print-halved x)
  (display (/ x 2.0))
  (newline))

(for-each print-halved (list 1 2 3 4))