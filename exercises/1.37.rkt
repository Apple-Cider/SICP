#lang racket

(define (cont-frac n d k)
  (define (cont-frac-recursor i)
    (if (= i k)
        (/ (n i) (d i))
        (/ (n i) (+ (d i)
                    (cont-frac-recursor (+ i 1))))))
  (cont-frac-recursor 1))

(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           100)

(define (cont-frac-i n d k)
  (define (cont-frac-iter result k)
    (if (= k 0)
        result
        (cont-frac-iter (/ (n k)
                           (+ (d k) result)) (- k 1))))
  (cont-frac-iter 0 k))

(cont-frac-i (lambda (i) 1.0)
             (lambda (i) 1.0)
             100)