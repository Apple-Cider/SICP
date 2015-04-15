#lang racket

(define (product term a next b)
  (if (> a b)
      1
      (* (term a) (product term (next a) next b))))


(define (inc x) (+ x 1))
(define (identity x) x)

(define (factorial n)
  (product identity 1 inc n))

(define (product-i term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))

(define (factorial-i n)
  (product-i identity 1 inc n))

;for testing accuracy
(factorial 4)
(factorial-i 4)