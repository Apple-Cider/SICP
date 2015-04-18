#lang racket

(define dx 0.001)

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (lambda (x)
    (if (= n 1)
        (f x)
        ((compose f (repeated f (- n 1))) x))))


(define (avg-3 x y z) (/ (+ x y z) 3))

(define (smooth f)
  (lambda (x)
    (avg-3 (f (- x dx))
           (f x)
           (f (+ x dx)))))

(define (n-fold-smooth f n)
  (lambda (x)
    (((repeated smooth n) f) x)))

(define my-num 5.34019)
(define (cube x) (* x x x))

(display "5.34019^3: ")
(newline)
(cube my-num)

(display "5.34019^3 1-smoothed: ")
(newline)

(avg-3 (cube (- my-num dx))
       (cube my-num)
       (cube (+ my-num dx)))
((smooth cube) my-num)
(((repeated smooth 1) cube) my-num)
((n-fold-smooth cube 1) my-num)

(display "5.34019^3 3-smoothed: ")
(newline)

(avg-3 (cube (avg-3 (- my-num dx dx)
                    (- my-num dx)
                    my-num))
       (cube (avg-3 (- my-num dx)
                    my-num
                    (+ my-num dx)))
       (cube (avg-3 my-num
                    (+ my-num dx)
                    (+ my-num dx dx))))
(((repeated smooth 3) cube) my-num)
((n-fold-smooth cube 3) my-num)