#lang racket

(define (cons a b)
  (cond ((> a 0) (* 2 (cons (- a 1) b)))
        ((> b 0) (* 3 (cons a (- b 1))))
        (else 1)))

(define (times-divides x d)
  (if (= (remainder x d) 0)
      (+ 1 (times-divides (/ x d) d))
      0))

(define (car x)
  (times-divides x 2))

(define (cdr x)
  (times-divides x 3))

(define pair-a (cons 6 10))
(define pair-b (cons 0 2))

(car pair-a)
(cdr pair-a)
(car pair-b)
(cdr pair-b)
