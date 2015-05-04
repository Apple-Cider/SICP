#lang racket

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (car (cdr mobile)))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (car (cdr branch)))

(define (total-weight mobile)
  (define (branch-weight branch)
    (let ((structure (branch-structure branch)))
      (if (pair? structure)
          (total-weight structure) ;structure is a mobile
          structure)))             ;structure is a weight
  (+ (branch-weight (left-branch mobile))
     (branch-weight (right-branch mobile))))


(define (torque mobile)
  (define (branch-torque branch)
    (let ((structure (branch-structure branch))
          (length (branch-length branch)))
      (if (pair? structure)
          (* length (total-weight structure))
          (* length structure))))
  (- (branch-torque (left-branch mobile))
     (branch-torque (right-branch mobile))))

(define (balanced? mobile)
  (= 0 (torque mobile)))

; should reuse print-test

(define (print-test f f-name input input-name output)
  (define (print-results)
    (display f-name)
    (display " test failed for ")
    (display input-name)
    (newline)
    (display "  expected: ")
    (display output)
    (newline)
    (display "  computed: ")
    (display (f input)))
  (if (equal? output (f input))
      (display "test passed!")
      (print-results))
  (newline))

(define (weight-test mobile mobile-name weight)
  (print-test total-weight "weight" mobile mobile-name weight))

(define (balance-test mobile mobile-name balance)
  (print-test balanced? "balance" mobile mobile-name balance))



(define br-l-l (make-branch 4 2))
(define br-l-r (make-branch 8 1))
(define br-l (make-branch 5 (make-mobile br-l-l br-l-r)))
(define br-r (make-branch 3 5))
(define b-mobile-0 (make-mobile br-l br-r))
(define b-mobile-1 (make-mobile (make-branch 2 b-mobile-0)
                                (make-branch 4 4)))
(define u-mobile-0 (make-mobile (make-branch 2 b-mobile-0)
                                (make-branch 4 16)))

(weight-test b-mobile-0 "b-mobile-0" (+ 2 1 5))
(weight-test b-mobile-1 "b-mobile-1" (+ 2 1 5 4))
(weight-test u-mobile-0 "u-mobile-0" (+ 2 1 5 16))


(balance-test b-mobile-0 "b-mobile-0" #t)
(balance-test b-mobile-1 "b-mobile-1" #t)
(balance-test u-mobile-0 "u-mobile-0" #f)