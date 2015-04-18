#lang racket

(define (cont-frac n d k)
  (define (cont-frac-recursor i)
    (if (= i k)
        (/ (n i) (d i))
        (/ (n i) (+ (d i)
                    (cont-frac-recursor (+ i 1))))))
  (cont-frac-recursor 1))

(define (tan-cf x k)
  (define (n i)
    (if (= i 1)
        x
        (- (* x x))))
  (define (d i)
    (- (* i 2.0) 1))
  (cont-frac n d k))

(tan-cf 2 1000)