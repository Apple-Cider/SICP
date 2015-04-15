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


(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))


(define (identity x) x)
(define (inc x) (+ x 1))


(define (filtered-accumulate combiner filter null-value term a next b)
  (cond ((> a b) null-value)
        ((filter a) (combiner (term a)
                              (filtered-accumulate combiner filter null-value term (next a) next b)))
        (else (filtered-accumulate combiner filter null-value term (next a) next b))))


;(define (sum-odds term a next b)
;  (define (odd? x) (not (even? x)))
;  (filtered-accumulate + odd? 0 term a next b))

;(define (add-odd-integers a b)
;  (sum-odds identity a inc b))

;(+ 3 5 7 9 11)
;(add-odd-integers 2 11)


(define (sum-prime-squares a b)
  (define prime? exhaustive-fermat-test)
  (filtered-accumulate + prime? 0 square a inc b))


(define (product-relative-primes n)
  (define (relatively-prime? x)
    (= (gcd n x) 1))
  (filtered-accumulate * relatively-prime? 1 identity 2 inc n))

;tests
(+ 25 49 121 169)
(sum-prime-squares 4 16)
(* 17 13 11 7 5)
(product-relative-primes 18)