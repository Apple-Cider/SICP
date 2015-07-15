#lang racket

(define (adjoin-set x set)
  (cond ((or (null? set) (< x (car set)))
         (cons x set))
        ((> x (car set))
         (cons (car set) (adjoin-set x (cdr set))))
        (else set)))

(adjoin-set 5 '(1 2 3 4 6 7 8 9 10))
(adjoin-set 5 '(1 2 3 4 5 6 7 8 9 10))
(adjoin-set 0 '(1 2 3 4 5 6 7 8 9 10))
(adjoin-set 11 '(1 2 3 4 5 6 7 8 9 10))


;easter egg! first solution which did not halve the number of steps:
(define (append-in-reverse list-1 list-2)
  (if (null? list-1)
      list-2
      (append-in-reverse (cdr list-1)
                         (cons (car list-1) list-2))))

(define (adjoin-set-bad-iterative-version x set)
  (define (adjoin-set-search less remaining)
    (cond ((or (null? remaining) (< x (car remaining)))
           (append-in-reverse less (cons x remaining)))
          ((> x (car remaining))
           (adjoin-set-search (cons (car remaining) less) (cdr remaining)))
          (else set)))
  (adjoin-set-search '() set))