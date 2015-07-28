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
(define (equ? x y) (apply-generic 'equ? x y))


; === ordinary numbers package ===
(define (install-number-package)
  (put 'equ? '(number number)
       (lambda (x y) (= x y))
  'done))


; === rational numbers package ===
(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (equal-rat? x y)
    (= (* (numer x) (denom y))
       (* (numer y) (denom x))))
  ;; interface to rest of the system
  (put 'equ? '(rational rational) equal-rat?)
  'done)


; === rectangular-complex numbers package ===
(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  ;; interface to the rest of the system
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  'done)


; === polar-complex numbers package ===
(define (install-polar-package)
  ;; internal procedures
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  ;; interface to the rest of the system
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  'done)


; === generic complex numbers package ===
(define (install-complex-package)
  ;; internal procedures
  (define (equ-complex? z1 z2)
    (and (= (real-part z1) (real-part z2))
         (= (imag-part z1) (imag-part z2))))
  ;; interface to rest of the system
  (put 'equ? '(complex complex) equ-complex?)
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  'done)

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))