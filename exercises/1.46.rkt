#lang racket

(define (iterative-improve test improve)
  (lambda (guess)
    (if (test guess)
        guess
        ((iterative-improve test improve) (improve guess)))))


(define (average x y) (/ (+ x y) 2))

(define (sqrt x)
  (define (test guess)
    (< (abs (- (* guess guess) x)) 0.0001))
  (define (improve guess)
    (average guess (/ x guess)))
  ((iterative-improve test improve) 1.0))

4
(sqrt 16)

(define (fixed-point f first-guess)
  (define (test guess)
    (< (abs (- guess (f guess))) 0.00001))
  ((iterative-improve test f) first-guess))

0.7390822985224023
(fixed-point cos 1.0)
