#lang racket
(define (square x) (* x x))
(define (even? x) (= (remainder x 2) 0))

(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (define (next divisor)
    (if (= divisor 2) 3 (+ divisor 2)))
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))
(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))


(define (timed-prime-test n)
  (start-prime-test n (current-milliseconds)))
(define (start-prime-test n start-time)
  (cond ((prime? n)
      (report-prime n (- (current-milliseconds) start-time))
      #t)
        (else #f)))
(define (report-prime n elapsed-time)
  (newline)
  (display n)
  (display " ms: ")
  (display elapsed-time))


(define (search-for-primes k min)
  (define (prime-iter k min)
    (cond ((> k 0) 
           (if (timed-prime-test min)
               (prime-iter (- k 1) (+ min 2))
               (prime-iter k (+ min 2))))))
  (if (even? min)
      (prime-iter k (+ min 1))
      (prime-iter k (+ min 2))))


(define (do-exercise)
  (begin
    (search-for-primes 3 100000000000)
    (newline)
    (search-for-primes 3 1000000000000)
    (newline)
    (search-for-primes 3 10000000000000)
    (newline)
    (search-for-primes 3 100000000000000)))