#lang racket
(define (square x) (* x x))
(define (even? x) (= (remainder x 2) 0))

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (new-expmod base exp m)
  (remainder (fast-expt base exp) m))

(define (fermat-test n)
  (define (try-it a)
    (= (new-expmod a n n) a))
  (if (> n 4294967087)
      (try-it (+ 1 (random (- 10000 1))))
      (try-it (+ 1 (random (- n 1))))))

(define (fast-prime? n times)
  (cond ((= times 0) #t)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else #f)))



(define (timed-prime-test n)
  (start-prime-test n (current-milliseconds)))
(define (start-prime-test n start-time)
  (cond ((fast-prime? n 1000)
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


(define (do-exercise k)
  (begin
    (search-for-primes k 100000000000)
    (newline)
    (search-for-primes k 1000000000000)
    (newline)
    (search-for-primes k 10000000000000)
    (newline)
    (search-for-primes k 100000000000000)
    (newline)
    (search-for-primes k 1000000000000000)))