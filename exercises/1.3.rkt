#lang racket

(define (square x)
  (* x x))

(define (sum-squares x y)
  (+ (square x) (square y)))

(define (least-of-three x y z)
  (cond ((< x y z) x)
        ((< y x z) y)
        (else z)))

(define (sum-largest-squares x y z)
  (define least (least-of-three x y z))
  (cond ((= least x) (sum-squares y z))
        ((= least y) (sum-squares x z))
        (else (sum-squares x y))))