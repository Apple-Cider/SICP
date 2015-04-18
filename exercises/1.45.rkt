#lang racket

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (lambda (x)
    (if (= n 1)
        (f x)
        ((compose f (repeated f (- n 1))) x))))

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

(define (sqrt x)
  (fixed-point (avg-dmp (lambda (y) (/ x y)))
               1.0))


(define (fourth-rt x)
  (define (g y) (/ x (cube y)))
  (fixed-point (avg-dmp (avg-dmp g))
               1.0))


(define (fourth-rt-repeat x)
  (define (g y) (/ x (cube y)))
  (fixed-point ((repeated avg-dmp 2) g)
               1.0))


;(fourth-rt (* 5 5 5 5))
;(fourth-rt-repeat (* 5 5 5 5))

(define (even? x) (= (remainder x 2) 0))
(define (pow-k k)
  (lambda (base)
    ((repeated (lambda (x) (* x base))
               (- k 1)) 
     base)))

(define (square x) ((pow-k 2) x))
(define (cube x) ((pow-k 3) x))

(* 12 12)
(square 12)
(* 7 7 7)
(cube 7)
(* 3 3 3 3)
((pow-k 4) 3)