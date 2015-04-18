#lang racket

(define tolerance 0.00001)
(define dx 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (average x y) (/ (+ x y) 2))

(define (avg-dmp f)
  (lambda (x) (average (f x) x)))

(define (deriv f)
  (lambda (x) (/ (- (f (+ x dx)) (f x))
                 dx)))


(define (newton-method g guess)
  (fixed-point (lambda (x) (- x
                              (/ (g x)
                                 ((deriv g) x))))
               guess))

;(define (sqrt-newton x)
;  (newton-method (lambda (y) (- (* y y) x))
;                 1.0))

;(sqrt-newton 16)


(define (cube x) (* x x x))
(define (square x) (* x x))

(define (cubic a b c)
  (lambda (x) (+ (cube x)
                 (* a (square x))
                 (* b x)
                 c)))

(newton-method (cubic 2 8 3) 1)
(newton-method (cubic 0 0 -8) 1)