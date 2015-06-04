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



(define (print-vect v)
  (display "(")
  (display (xcor-vect v))
  (display ",")
  (display (ycor-vect v))
  (display ")")
  (newline))

(define vect-a (make-vect 2 5))
(define vect-b (make-vect -3 12))

(display "adding vect-a and vect-b...")
(newline)
(display "expected: (-1,17)")
(newline)
(display "computed: ")
(print-vect (add-vect vect-a vect-b))

(display "subtracting vect-a from vect-b...")
(newline)
(display "expected: (-5,7)")
(newline)
(display "computed: ")
(print-vect (sub-vect vect-b vect-a))

(display "multiplying vect-a by -2...")
(newline)
(display "expected: (-4,-10)")
(newline)
(display "computed: ")
(print-vect (scale-vect vect-a -2))

