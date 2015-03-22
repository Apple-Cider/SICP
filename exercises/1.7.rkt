#lang racket

(define (square x) (* x x))

(define (abs x)
  (if (< x 0) (- x) x))

(define (relative-difference x y)
  (/ (abs (- x y))
     x))

(define (good-enough? old-guess guess)
  (< (relative-difference old-guess guess) 0.001))

(define (avg x y) (/ (+ x y) 2))

(define (improve guess x)
  (avg guess (/ x guess)))

(define (sqrt-iter old-guess guess x)
  (if (good-enough? old-guess guess)
      guess
      (sqrt-iter guess (improve guess x) x)))

(define (sqrt x)
  (sqrt-iter 1.0 (improve 1.0 x) x))