#lang racket

(define (make-interval a b) (cons (min a b) (max a b)))

(define (lower-bound interval) (car interval))
(define (upper-bound interval) (cdr interval))

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x 
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

(define (equals? x y)
  (and (= (lower-bound x) (lower-bound y))
       (= (upper-bound x) (upper-bound y))))

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (make-center-percent c t)
  (make-center-width c (/ (* c t) 100)))
(define (percent i)
  (/ (* 100 (width i)) (center i)))

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))
(define (par2 r1 r2)
  (let ((one (make-interval 1 1))) 
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

(define (print-center-percent interval)
  (newline)
  (display (center interval))
  (display " ±")
  (display (percent interval))
  (display "%"))

(define i-a (make-center-percent 120000 .001))
(define i-b (make-center-percent 240000 .001))

(define self-quotient (div-interval i-a i-a))
(define quotient (div-interval i-a i-b))
(print-center-percent self-quotient)
(print-center-percent quotient)

(newline)
(define i-par1 (par1 i-a i-b))
(define i-par2 (par2 i-a i-b))
(print-center-percent i-par1)
(print-center-percent i-par2)

(newline)
(print-center-percent (sub-interval i-par1 i-par2))

(define i-expr0 (div-interval (sub-interval i-b i-a)
                              (add-interval i-b i-a)))
(define i-expr1 (sub-interval (div-interval i-b (add-interval i-b i-a))
                              (div-interval i-a (add-interval i-b i-a))))

(newline)
(print-center-percent i-expr0)
(print-center-percent i-expr1)
(newline)
(print-center-percent (sub-interval i-expr0 i-expr1))
             