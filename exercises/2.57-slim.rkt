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

(define (augend s) (make-sum (cddr s)))


(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        (else
         (error "unknown expression type -- DERIV" exp))))

(define test-sum (make-sum 1 2 3 4 'x 'y 'z 5 'a))

test-sum
(addend test-sum)
(augend test-sum)

(cddr test-sum)
(make-sum (cddr test-sum))
