#lang racket

(define (make-point x y) (cons x y))
(define (x-point point) (car point))
(define (y-point point) (cdr point))

(define (make-segment a b) (cons a b))
(define (start-segment s) (car s))
(define (end-segment s) (cdr s))

(define (average x y) (/ (+ x y) 2))

(define (mid-point s)
  (let ((x (average (x-point (start-segment s))
                    (x-point (end-segment s))))
        (y (average (y-point (start-segment s))
                    (y-point (end-segment s)))))
    (make-point x y)))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define origin (make-point 0 0))
(define point-a (make-point 10 5))
(define segment-a (make-segment origin point-a))
(define point-middle (mid-point segment-a))
(print-point point-middle)