#lang racket

; reduced installation packages to the equ? implementation and supporting functions


; === general procedures ===

; to ignore error of undefined put and get procedures
(define (put op types item) null)
(define (get op types) null)

(define (attach-tag type-tag contents)
  (if (eq? type-tag 'number)
      contents
      (cons type-tag contents)))

(define (type-tag datum)
  (cond ((pair? datum) (car datum))
        ((number? datum) 'number)
        (else (error "Bad tagged datum -- TYPE-TAG" datum))))

(define (contents datum)
  (cond ((pair? datum) (cdr datum))
        ((number? datum) datum)
        (else (error "Bad tagged datum -- CONTENTS" datum))))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error
            "No method for these types -- APPLY-GENERIC"
            (list op type-tags))))))


; === generic arithmetic package ===
(define (=zero? x) (apply-generic '=zero? x))


; === ordinary numbers package ===
(define (install-number-package)
  (put '=zero? '(number)
       (lambda (x) (= x 0))
  'done))


; === rational numbers package ===
(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (=zero? x) (= (numer x) 0))
  ;; interface to rest of the system
  (put '=zero? '(rational) =zero?)
  'done)


; === rectangular-complex numbers package ===
(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (=zero? z)
    (and (= (real-part z) 0)
         (= (imag-part z) 0)))
  ;; interface to the rest of the system
  (put '=zero? '(rectangular) =zero?)
  'done)


; === polar-complex numbers package ===
(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (=zero? z)
    (= (magnitude z) 0))
  ;; interface to the rest of the system
  (put '=zero? '(polar) =zero?)
  'done)


; === generic complex numbers package ===
(define (install-complex-package)
  ;; interface to rest of the system
  (put '=zero? '(complex) =zero?)
  'done)