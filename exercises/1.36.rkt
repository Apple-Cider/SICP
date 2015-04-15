#lang racket

(define (avg x y) (/ (+ x y) 2))

(define tolerance 0.00001)

(define (fixed-point dampened? f first-guess)
  (define (print-guess g i)
    (display i)
    (display ": ")
    (display g)
    (newline))
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess i)
    (let ((next (if dampened?
                    (avg guess (f guess))
                    (f guess))))
      (print-guess next i)
      (if (close-enough? guess next)
          next
          (try next (+ i 1)))))
  (print-guess first-guess 1)
  (try first-guess 2))

(define (fixed-point-d f first-guess)
  (define (print-guess g i)
    (display i)
    (display ": ")
    (display g)
    (newline))
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess i)
    (let ((next (f guess)))
      (print-guess next i)
      (if (close-enough? guess next)
          next
          (try next (+ i 1)))))
  (print-guess first-guess 1)
  (try first-guess 2))


(define non-dampened
  (fixed-point #f (lambda (x) (/ (log 1000) (log x))) 2))

(define dampened
  (fixed-point #t (lambda (x) (/ (log 1000) (log x))) 2))

non-dampened
dampened