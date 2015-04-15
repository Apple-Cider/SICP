#lang racket

(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

;for testing accuracy...
(define (identity x) x)
(define (inc x) (+ x 1))
(define (sum-ints a b)
  (sum identity a inc b))
(define (cube x) (* x x x))
(define (sum-cubes a b)
  (sum cube a inc b))
