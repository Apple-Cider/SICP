#lang racket

(define (square x) (* x x))
(define (even? x) (= (remainder x 2) 0))

(define (expt b n)
  (define (expt-iter a b n)
    (cond ((= n 0) a)
          ((even? n) (expt-iter a (square b) (/ n 2)))
          (else (expt-iter (* a b) b (- n 1)))))
  (expt-iter 1 b n))