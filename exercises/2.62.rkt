#lang racket

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else (let ((x1 (car set1)) (x2 (car set2)))
                (cond ((= x1 x2) (cons x1 (union-set (cdr set1) (cdr set2))))
                      ((< x1 x2) (cons x1 (union-set (cdr set1) set2)))
                      (else (cons x2 (union-set set1 (cdr set2)))))))))

(define set-odds '(1 3 5 7 9))
(define set-evens '(0 2 4 6 8))
(define set-high '(2 11))
(define set-low '(-2 7))

(union-set set-odds set-evens)
(union-set set-evens set-high)
(union-set set-low set-odds)
(union-set set-evens set-evens)
(union-set set-odds '())