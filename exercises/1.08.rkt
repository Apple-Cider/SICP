#lang racket

(define (square x) (* x x))

(define (abs x)
  (if (< x 0) (- x) x))

(define (relative-difference x y)
  (/ (abs (- x y))
     x))

(define (good-enough? old-guess guess)
  (< (relative-difference old-guess guess) 0.001))

(define (improve guess x)
  (/ (+ (/ x (square guess))
        (* guess 2))
     3))

(define (cbrt-iter old-guess guess x)
  (if (good-enough? old-guess guess)
      guess
      (cbrt-iter guess (improve guess x) x)))

(define (cbrt x)
  (cbrt-iter 1.0 (improve 1.0 x) x))