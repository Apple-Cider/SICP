#lang racket

(define (identity x) x)
(define (inc x) (+ x 1))


(define (accumulate combiner null-value term a next b)
  (if (> a b)
       null-value
       (combiner (term a)
                 (accumulate combiner null-value term (next a) next b))))


(define (sum term a next b)
  (accumulate + 0 term a next b))

(define (add-integers a b)
  (sum identity a inc b))


(define (product term a next b)
  (accumulate * 1 term a next b))

(define (factorial n)
  (product identity 1 inc n))


(define (accumulate-i combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner (term a) result))))
  (iter a null-value))


(define (sum-i term a next b)
  (accumulate-i + 0 term a next b))

(define (add-integers-i a b)
  (sum-i identity a inc b))


(define (product-i term a next b)
  (accumulate-i * 1 term a next b))

(define (factorial-i n)
  (product-i identity 1 inc n))

;tests
(+ 4 5 6 7 8 9)
(add-integers 4 9)
(add-integers-i 4 9)
(newline)
(* 1 2 3 4 5 6)
(factorial 6)
(factorial-i 6)