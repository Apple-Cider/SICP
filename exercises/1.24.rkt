#lang racket
(define (square x) (* x x))


(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) #t)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else #f)))



(define (timed-prime-test n)
  (start-prime-test n (current-milliseconds)))
(define (start-prime-test n start-time)
  (cond ((fast-prime? n)
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