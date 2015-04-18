#lang racket

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (lambda (x)
    (if (= n 1)
        (f x)
        ((compose f (repeated f (- n 1))) x))))

(define tolerance 0.0000001)

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




(define (even? x) (= (remainder x 2) 0))
(define (pow-k k)
  (lambda (base)
    ((repeated (lambda (x) (* x base))
               (- k 1)) 
     base)))


;(define (fourth-rt x)
;  (define (g y) (/ x (cube y)))
;  (fixed-point (avg-dmp (avg-dmp g))
;               1.0))


;(define (fourth-rt-repeat x)
;  (define (g y) (/ x (cube y)))
;  (fixed-point ((repeated avg-dmp 2) g)
;               1.0))

(define (fourth-rt-repeat-pow-k x)
  (define (g y) (/ x ((pow-k 3) y)))
  (fixed-point ((repeated avg-dmp 2) g)
               1.0))


;(fourth-rt (* 5 5 5 5))
;(fourth-rt-repeat (* 5 5 5 5))
;(fourth-rt-repeat-pow-k (* 5 5 5 5))

;(define (square x) ((pow-k 2) x))
;(define (cube x) ((pow-k 3) x))

;(* 12 12)
;(square 12)
;(* 7 7 7)
;(cube 7)
;(* 3 3 3 3)
;((pow-k 4) 3)

(define (twlv-rt x)
  (define (g y) (/ x ((pow-k 11) y)))
  (fixed-point ((repeated avg-dmp 3) g)
               1.0))
;(twlv-rt (* 3 3 3 3 3 3 3 3 3 3 3 3))

(define (twentyfourth-rt x)
  (define (g y) (/ x ((pow-k 30) y)))
  (fixed-point ((repeated avg-dmp 4) g)
               1.0))

;(twentyfourth-rt (* 3 3 3 3 3 3 3 3
;                    3 3 3 3 3 3 3 3
;                    3 3 3 3 3 3 3 3
;                    3 3 3 3 3 3 3))

(define (log-two x)
  (define (log-two-iter x i)
    (if (not (< x 2))
        (log-two-iter (/ x 2) (+ i 1))
        i))
  (log-two-iter x 0))

;(log-two 32)

(define (nth-root root x)
  (define num-avgs (log-two root))
  (define (g y) (/ x 
                   ((pow-k (- root 1)) y)))
  (display "num-avgs: ") (display num-avgs) (newline)
  (display "root: ") (display root) (newline)
  (display "radicand: ") (display x) (newline)
  (fixed-point ((repeated avg-dmp num-avgs) g)
               1.0))

(nth-root 36 (* 3 3 3 3 3 3 3 3 3
                3 3 3 3 3 3 3 3 3
                3 3 3 3 3 3 3 3 3
                3 3 3 3 3 3 3 3 3))