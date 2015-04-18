#lang racket

(define tolerance 0.0000001)

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (lambda (x)
    (if (= n 1)
        (f x)
        ((compose f (repeated f (- n 1))) x))))

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

(define (even? x) (= (remainder x 2) 0))

(define (pow-k k)
  (lambda (base)
    ((repeated (lambda (x) (* x base))
               (- k 1)) 
     base)))

(define (log-two x)
  (define (log-two-iter x i)
    (if (not (< x 2))
        (log-two-iter (/ x 2) (+ i 1))
        i))
  (log-two-iter x 0))


(define (nth-root root x)
  (define num-avgs (log-two root))
  (define (g y) (/ x 
                   ((pow-k (- root 1)) y)))
  ;(display "num-avgs: ") (display num-avgs) (newline)
  ;(display "root: ") (display root) (newline)
  ;(display "radicand: ") (display x) (newline)
  (fixed-point ((repeated avg-dmp num-avgs) g)
               1.0))

(nth-root 36 (* 3 3 3 3 3 3 3 3 3
                3 3 3 3 3 3 3 3 3
                3 3 3 3 3 3 3 3 3
                3 3 3 3 3 3 3 3 3))