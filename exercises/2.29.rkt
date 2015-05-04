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
      ;(newline)
      ;(display "structure: ")
      ;(display structure)
      (if (pair? structure)
          (total-weight structure) ;structure is a mobile
          structure)))             ;structure is a weight
  (+ (branch-weight (left-branch mobile))
     (branch-weight (right-branch mobile))))

(define br-l-l (make-branch 4 2))
(define br-l-r (make-branch 8 1))
(define br-l (make-branch 5 (make-mobile br-l-l br-l-r)))
(define br-r (make-branch 3 5))
(define b-mobile-0 (make-mobile br-l br-r))
(define b-mobile-1 (make-mobile (make-branch 2 b-mobile-0)
                                (make-branch 4 4)))
(define u-mobile-0 (make-mobile (make-branch 2 b-mobile-0)
                                (make-branch 4 16)))

;(display "branch br-1-1: ")
;br-l-l
;(display "its structure: ")
;(branch-structure br-l-l)

;(display "branch br-1: ")
;br-l
;(display "its structure: ")
;(branch-structure br-l)

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


(display "expected weight of b-mobile 0: ")
(+ 2 1 5)
(display "computed weight of b-mobile 0: ")
(total-weight b-mobile-0)

(balanced? b-mobile-0)
(balanced? b-mobile-1)
(balanced? u-mobile-0)