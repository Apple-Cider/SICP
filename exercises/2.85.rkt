#lang racket

; === GENERAL TYPE PROCEDURES ===

; OP-TABLE PROCEDURES (undefined!)
(define (put op types item) true)
(define (get op types) true)
(define (put-coercion type1 type2 coercion) true)
(define (get-coercion type1 type2) true)

; TYPE GETTERS/SETTERS
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

; === GENERAL COERCION PROCEDURES ===

; UPWARD COERCIONS
(define (raise-integer integer)
  ((get-coercion 'integer 'rational) integer))
	
(define (raise-rational rational)
  ((get-coercion 'rational 'real) rational))

(define (raise-real real)
  ((get-coercion 'real 'complex) real))

(put 'raise 'integer raise-integer)
(put 'raise 'rational raise-rational)
(put 'raise 'real raise-real)

(define (raise number) (apply-generic 'raise number))

(define (distance-from-top number)
  (define (tower-climber height-climbed arg)
    (let ((raise-op (get 'raise (type-tag arg))))
      (if raise-op
          (tower-climber (+ height-climbed 1) (raise-op arg))
          height-climbed)))
  (tower-climber 0 number))

; true when n1 is closer to top
(define (higher? n1 n2)
  (< (distance-from-top n1) (distance-from-top n2)))

(define (raise-to-same-height number target)
  (if (eq? (type-tag number) (type-tag target))
      number
      (raise-to-same-height (raise number) target)))

; DOWNWARD COERCIONS
(define (project-complex complex)
  ((get-coercion 'complex 'real) complex))

(define (project-real real)
  ((get-coercion 'real 'integer) real))

(define (project-rational rational)
  ((get-coercion 'rational 'integer) rational))

(put 'project 'complex project-complex)
(put 'project 'real project-real)
(put 'project 'rational project-rational)

(define (project number) (apply-generic 'project number))

; seems to violate a type of generality principle,
;   given how (project number) abstracts getting the appropriate
;   coercion. but the getting must be checked, and apply-generic,
;   called through (project number), would simply signal an error.
(define (drop number)
  (let ((project-proc (get 'project (type-tag number))))
    (if project-proc
        (let ((projected-number (project-proc number)))
          (let ((raised-projected-number (raise projected-number)))
            (if (equ? raised-projected-number number)
                (drop projected-number)
                number)))
        number)))

(define (simplify-answer answer)
  (cond ((pair? answer)
         (cons (drop (car answer))
               (simplify-answer (cdr answer))))
        ((null? answer) '())
        (else (drop answer))))

; GENERIC PROCEDURE HANDLING
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (simplify-answer (apply proc (map contents args)))
          (if (= (length args) 2)
              (let ((a1 (car args))
                    (a2 (cadr args)))
                (cond ((higher? a1 a2)
                       (simplify-answer (apply-generic op
                                                       a1
                                                       (raise-to-same-height a2 a1))))
                      ((higher? a2 a1)
                       (simplify-answer (apply-generic op
                                                       (raise-to-same-height a1 a2)
                                                       a2)))
                      (else
                       (error "No method for these types"
                              (list op type-tags)))))
              (error "No method for these types"
                     (list op type-tags)))))))


; === GENERIC ARITHMETIC PACKAGE ===
(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (equ? x y) (apply-generic 'equ? x y))


; === ORDINARY NUMBERS PACKAGE ===
; *** how should 'real type fit into system?
;     shoehorning it in here is at least not ideal,
;     if not a cause of errors
(define (install-number-package)
  ;; internal procedures
  
  (define (real->integer real)
    (round real))
    
  ;; interface to rest of the system
  (put 'add '(number number) +)
  (put 'sub '(number number) -)
  (put 'mul '(number number) *)
  (put 'div '(number number) /)
  (put 'equ? '(number number) =)
  
  (put-coercion 'real 'integer real->integer)
  'done)


; === RATIONAL NUMBERS PACKAGE ===
(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
  (define (equal-rat? x y)
    (= (* (numer x) (denom y))
       (* (numer y) (denom x))))
  
  (define (rational->integer x)
    (round (/ (numer x)
              (denom x))))
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'equ? '(rational rational) equal-rat?)
  
  (put-coercion 'rational 'integer rational->integer)

  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  'done)

(define (make-rational n d)
  ((get 'make 'rational) n d))


; === RECTANGULAR-COMPLEX NUMBERS PACKAGE ===
(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt (+ (sqr (real-part z))
             (sqr (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a) 
    (cons (* r (cos a)) (* r (sin a))))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular 
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular 
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'rectangular) x y))


; === POLAR-COMPLEX NUMBERS PACKAGE ===
(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y) 
    (cons (sqrt (+ (sqr x) (sqr y)))
          (atan y x)))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar 
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 'polar) r a))


; === GENERIC COMPLEX NUMBERS PACKAGE ===
(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

(define (install-complex-package)
  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))
  (define (equ-complex? z1 z2)
    (and (= (real-part z1) (real-part z2))
         (= (imag-part z1) (imag-part z2))))
  
  (define (complex->real z)
    (real-part (contents z)))
  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'equ? '(complex complex) equ-complex?)
  
  (put-coercion 'complex 'real complex->real)
  
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)