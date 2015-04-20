#lang racket

(define zero
  (lambda (f)
    (lambda (x)
      x)))

(define (add-1 n)
  (lambda (f)
    (lambda (x)
      (f ((n f) x)))))

;((lambda (x) (* x x)) 8)
(define (inc x) (+ x 1))

(define confusing
  (lambda (f)
    (lambda (x)
      (f x))))

(define donfusing (confusing inc))
;(donfusing 2)

(((add-1 zero) inc) 2)
(((add-1 (add-1 zero)) inc) 2)

;zero: f doesn't get applied to x
;add-1(zero): f gets applied to x

(define one
  (lambda (f)
    (lambda (x)
      (f x))))