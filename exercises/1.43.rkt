#lang racket

(define (square x) (* x x))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (lambda (x)
    (if (= n 1)
        (f x)
        ((compose f (repeated f (- n 1))) x))))

((repeated square 2) 5)