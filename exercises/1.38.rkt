#lang racket

(define (cont-frac n d k)
  (define (cont-frac-recursor i)
    (if (= i k)
        (/ (n i) (d i))
        (/ (n i) (+ (d i)
                    (cont-frac-recursor (+ i 1))))))
  (cont-frac-recursor 1))

(define (e k)
  (define (d i)
    (if (= (remainder (+ i 1) 3) 0)
        (/ (* 2 (+ i 1)) 3)
        1.0))
  (+ (cont-frac (lambda (i) i) d k)
     2))

(e 1000)