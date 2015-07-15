#lang racket

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

;returns x s.t. (car x) = elements pasing predicate, (cdr x) = elements failing)
(define (gate predicate sequence)
  (define (iter trues falses remaining)
    (cond ((null? remaining) (cons trues falses))
          ((predicate (car remaining))
           (iter (append trues (list (car remaining)))
                 falses
                 (cdr remaining)))
          (else (iter trues
                      (append falses (list (car remaining)))
                      (cdr remaining)))))
  (iter '() '() sequence))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

;(define (make-sum a1 a2)
;  (cond ((=number? a1 0) a2)
;        ((=number? a2 0) a1)
;        ((and (number? a1) (number? a2)) (+ a1 a2))
;        (else (list '+ a1 a2))))

;(define (make-sum . a)
;  (if (< (length a) 2)
;      (if (= (length a) 1)
;          a
;          (error "no arguments -- make-sum"))
;      (let ((filtered (gate number? a)))
;        (let ((numbers (car filtered))
;              (symbols (cdr filtered)))
;          (let ((numeral-result (accumulate + 0 numbers))
;                (symbol-list (accumulate cons null symbols)))
;            (if (= numeral-result initial) ;
;                (cons '+ symbol-list))
;                (cons '+ (cons numeral-result symbol-list)))))))

;(define (make-sum a1 . a)
;  (cond ((null? a) a1)
;        ((=number? a1 0) (make-sum (car a) (cdr a)))
;        ((=number? (car a) 0) (make-sum a1 (cdr a)))
;        ((and (number? a1) (number? (car a))) (make-sum (+ a1 (car a)) (cdr a)))
;        (else (list '+ a1 a2))))

;(define (make-product m1 m2)
;  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
;        ((=number? m1 1) m2)
;        ((=number? m2 1) m1)
;        ((and (number? m1) (number? m2)) (* m1 m2))
;        (else (list '* m1 m2))))

(define (arithmetic-combiner quoted-op op initial)
  (define (operands-1-condition operands)
    (display "error-ish... operands length = 1")
    (newline)
    (display "operands: ")
    (display operands)
    (newline)
    (car operands))
  (lambda (operands)
    (if (< (length operands) 2)
        (if (= (length operands) 1)
            (operands-1-condition operands)
            ;(car operands) ;perhaps an error condition
            (error "no arguments -- arithmetic-combiner" quoted-op))
        (let ((filtered (gate number? operands)))
          (let ((numbers (car filtered))
                (symbols (cdr filtered)))
            (let ((numeral-result (accumulate op initial numbers))
                  (symbol-list (accumulate cons null symbols)))
              (if (= numeral-result initial)
                  (cons quoted-op symbol-list)
                  (cons quoted-op (cons numeral-result symbol-list)))))))))

(define (make-sum . a)
  ((arithmetic-combiner '+ + 0) a))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))

;(define (augend s) (caddr s))
(define (augend s) (make-sum (cddr s)))

(define (make-product . m)
  ((arithmetic-combiner '* * 1) m))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))

;(define (multiplicand p) (caddr p))
(define (multiplicand p) (make-product (cddr p)))

(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))

(define (base x) (cadr x))

(define (exponent x) (caddr x))

(define (make-exponentiation base exponent)
  (cond ((=number? exponent 0) 1)
        ((=number? exponent 1) base)
        ((and (number? base) (number? exponent)) (expt base exponent))
        (else (list '** base exponent))))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
        ((exponentiation? exp)
         (make-product 
          (make-product (exponent exp)
                        (make-exponentiation (base exp)
                                             (make-sum -1 (exponent exp))))
          (deriv (base exp) var)))
        (else
         (error "unknown expression type -- DERIV" exp))))

(define test-sum (make-sum 1 2 3 4 'x 'y 'z 5 'a))
(define test-product (make-product 1 2 3 4 'x 'y 'z 5 'a))

test-sum
(addend test-sum)
(augend test-sum)

(cddr test-sum)
(make-sum (cddr test-sum))
