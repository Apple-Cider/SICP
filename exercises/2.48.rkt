#lang racket

(define (make-vect x y) (list x y))
(define (xcor-vect vect) (car vect))
(define (ycor-vect vect) (cadr vect))

(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1) (xcor-vect v2))
             (+ (ycor-vect v1) (ycor-vect v2))))

(define (scale-vect v s)
  (make-vect (* (xcor-vect v) s)
             (* (ycor-vect v) s)))

(define (sub-vect v1 v2)
  (add-vect v1 (scale-vect v2 -1)))


(define (make-segment v1 v2) (list v1 v2))
(define (start-segment s) (car s))
(define (end-segment s) (cadr s))


(define (print-vect v)
  (display "(")
  (display (xcor-vect v))
  (display ",")
  (display (ycor-vect v))
  (display ")")
  (newline))

(define (print-seg s)
  (display "start: ")
  (print-vect (start-segment s))
  (display "end:   ")
  (print-vect (end-segment s)))

(define vect-a (make-vect 2 5))
(define vect-b (make-vect -3 12))
(define segment (make-segment vect-a vect-b))

(display "segment with start at (2,5), end at (-3,12):")
(newline)
(print-seg segment)