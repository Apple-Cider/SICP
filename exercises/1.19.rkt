#lang racket

(define (even? x) (= (remainder x 2) 0))
(define (sq x) (* x x))

(define (fib n)
  (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (/ (* b q p) a)
                      (* q p)
                      (sq p))
                   (+ (/ (* b (sq q)) a)
                      (sq q)
                      (* p q))
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))