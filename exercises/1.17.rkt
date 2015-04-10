#lang racket

(define (double x) (+ x x))
(define (halve x) (/ x 2))
(define (even? x) (= (remainder x 2) 0))

(define (fast-multy a b)
  (cond ((= b 0) 0)
        ((even? b) (double (fast-multy a (halve b))))
        (else (+ a (fast-multy a (- b 1))))))