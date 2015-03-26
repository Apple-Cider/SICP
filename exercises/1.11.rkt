#lang racket

(define (f-r n)
  (if (< n 3) n
      (+ (f-r (- n 1))
         (* (f-r (- n 2)) 2)
         (* (f-r (- n 3)) 3))))

(define (f-i n)
  (define (f-iter i a b c)
    (if (= i n) c
        (f-iter (+ i 1)
                b
                c
                (+ c (* 2 b) (* 3 a)))))
    (if (< n 3) n
        (f-iter 3 1 2 4)))