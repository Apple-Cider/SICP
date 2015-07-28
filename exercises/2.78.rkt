#lang racket

; to ignore error of undefined put procedure
(define (put op type item) null)

(define (attach-tag type-tag contents) ; unsure if this modification was what they're looking for
  (if (eq? type-tag 'number)
      contents ; does not pair 'number to contents if somewhere a number is accidentally tagged as such
      (cons type-tag contents)))

(define (type-tag datum)
  (cond ((pair? datum) (car datum))
        ((number? datum) 'number)
        (else (error "Bad tagged datum -- TYPE-TAG" datum))))

(define (contents datum)
  (cond ((pair? datum) (cdr datum))
        ((number? datum) datum)
        (else (error "Bad tagged datum -- CONTENTS" datum))))

(define (install-number-package)
  (put 'add '(number number)
       (lambda (x y) (+ x y)))
  (put 'sub '(number number)
       (lambda (x y) (- x y)))
  (put 'mul '(number number)
       (lambda (x y) (* x y)))
  (put 'div '(number number)
       (lambda (x y) / x y))
  'done)