#lang racket

(define (cube x) (* x x x))
(define (even? x) (= (remainder x 2) 0))
(define (inc x) (+ x 1))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

(define (simpson-integral f a b n)
  (define h (/ (- b a) n))
  (define (coefficient k)
    (cond ((or (= k 0) (= k n)) 1)
          ((even? k) 2)
          (else 4)))
  (define (term k)
    (* (coefficient k)
       (f (+ a (* k h)))))
  (* (/ h 3)
     (sum term 0 inc n)))

;used to check because even at low, low values for n,
;simpson-integral seems completely accurate
(define (my-fn x)
  (- (* 6 (cube x)) (* x x)))


(display "integral: ")
(integral cube 0 1 0.01)
(display "simpson: ")
(simpson-integral cube 0 1 100)
(display "integral: ")
(integral cube 0 1 0.001)
(display "simpson: ")
(simpson-integral cube 0 1 1000)