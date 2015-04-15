#lang racket
(define (square x) (* x x))
(define (even? x) (= (remainder x 2) 0))

;(define (expmod base exp m)
;  (cond ((= exp 0) 1)
;        ((even? exp)
;         (remainder (square (expmod base (/ exp 2) m))
;                    m))
;        (else
;         (remainder (* base (expmod base (- exp 1) m))
;                    m))))

(define (expmod base exp m)
  (define (nt-sqrt? x)
    (if (or (= x (- m 1)) (= x 1))
        #f
        #t))
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (miller-rabin-test n)
  (define (initial)
    (if (even? n)
        (/ n 2)
        (+ (/ n 2) 1)))
  (define (test-iter a)
    (cond ((= a 0) #t)
          ((= (expmod a (- n 1) n) 0) #f)
          (else (test-iter (- a 1)))))
  (test-iter (initial)))

;(miller-rabin-test 561)
;(miller-rabin-test 1105)
;(miller-rabin-test 1729)
;(miller-rabin-test 2465)
;(miller-rabin-test 2821)
;(miller-rabin-test 6601)