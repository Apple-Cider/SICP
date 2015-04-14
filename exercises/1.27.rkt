#lang racket
(define (square x) (* x x))
(define (even? x) (= (remainder x 2) 0))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (exhaustive-fermat-test n)
  (define (test-iter a)
    (cond ((= a 0) #t)
          ((= (expmod a n n) a) (test-iter (- a 1)))
          (else #f)))
  (test-iter (- n 1)))

(exhaustive-fermat-test 561)
(exhaustive-fermat-test 1105)
(exhaustive-fermat-test 1729)
(exhaustive-fermat-test 2465)
(exhaustive-fermat-test 2821)
(exhaustive-fermat-test 6601)