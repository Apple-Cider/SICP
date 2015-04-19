#lang racket

(define (make-point x y) (cons x y))
(define (x-point point) (car point))
(define (y-point point) (cdr point))

(define (make-segment a b) (cons a b))
(define (start-segment s) (car s))
(define (end-segment s) (cdr s))

(define (make-rectangle-a corner-a corner-b)
  (let ((length (abs (- (x-point corner-a)
                        (x-point corner-b))))
        (width (abs (- (y-point corner-a)
                       (y-point corner-b)))))
    (let ((dimensions (cons length width))
          (delimiters (cons corner-a corner-b)))
      (cons dimensions delimiters))))

(define (make-rectangle-b segment height)
  (let ((length height)
        (width (abs (- (x-point (start-segment segment))
                        (x-point (end-segment segment))))))
    (let ((dimensions (cons length width))
          (delimiters (cons segment height)))
      (cons dimensions delimiters))))

(define (perimeter rectangle)
  (let ((length (car (car rectangle)))
        (width (cdr (car rectangle))))
    (+ (* length 2)
       (* width 2))))

(define (area rectangle)
  (let ((length (car (car rectangle)))
        (width (cdr (car rectangle))))
    (* length width)))

(define (print-rectangle-aspects rectangle)
  (newline)
  (display "perimeter: ")
  (display (perimeter rectangle))
  (newline)
  (display "area: ")
  (display (area rectangle)))

(define point-a (make-point 6 13))
(define point-b (make-point 12 5))
(define rect-a (make-rectangle-a point-a point-b))

(define point-c (make-point 2 6))
(define point-d (make-point 8 6))
(define seg-a (make-segment point-c point-d))
(define rect-b (make-rectangle-b seg-a 4))

(print-rectangle-aspects rect-a)
(print-rectangle-aspects rect-b)