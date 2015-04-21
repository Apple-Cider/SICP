#lang racket

(define zero
  (lambda (f)
    (lambda (x)
      x)))

(define (add-1 n)
  (lambda (f)
    (lambda (x)
      (f ((n f) x)))))

;(define (inc x) (+ x 1))

;(((add-1 zero) inc) 2)
;(((add-1 (add-1 zero)) inc) 2)

;zero: f doesn't get applied to x
;add-1(zero): f gets applied to x

(define one
  (lambda (f)
    (lambda (x)
      (f x))))

(define two
  (lambda (f)
    (lambda (x)
      (f (f x)))))

(define three
  (lambda (f)
    (lambda (x)
      (f (f (f x))))))

;((two inc) 31)

; (+ zero one) -> one
; (+ one one)  -> two
; (((+ one two) inc)) 4) -> 7

(define (+ a b)
  (lambda (f)
    (lambda (x)
      ((a (b f)) x))))

(define (dec x) (- x 1))

(((+ two two) dec) 17)
(((+ two three) dec) 17)