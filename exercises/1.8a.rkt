#lang racket

(define (square x) (* x x))

(define (abs x)
  (if (< x 0) (- x) x))

(define (relative-difference x y)
  (/ (abs (- x y))
     x))

(define (cbrt x)
  (define tolerance 0.001)
  (define (cbrt-iter guess)
    (define new-guess
      (/ (+ (/ x (square guess))
            (* guess 2))
         3))
    (define (good-enough?)
      (< (relative-difference guess new-guess) tolerance))
    (if (good-enough?)
        new-guess
        (cbrt-iter new-guess)))
  (cbrt-iter 1.0))