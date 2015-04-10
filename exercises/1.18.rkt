#lang racket

(define (double x) (+ x x))
(define (halve x) (/ x 2))
(define (even? x) (= (remainder x 2) 0))

(define (fast-multy a b)
  (define (multy-iter x a b)
    (cond ((= b 0) x)
          ((even? b) (multy-iter x (double a) (halve b)))
          (else (multy-iter (+ x a) a (- b 1)))))
  (multy-iter 0 a b))