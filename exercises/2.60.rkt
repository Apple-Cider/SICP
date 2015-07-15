#lang racket

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set) (cons x set))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)        
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))


(define (union-set set1 set2)
  (if (null? set1)
      set2
      (append set1 set2)))

(define set-a '(a b c 1 2 3))
(define set-b '(c d e 3 4 5))
(define set-c '(0 1 2 3 4 5 6))

(define union-a-b (union-set set-a set-b))
(define intersection-a-b (intersection-set set-a set-b))
(define intersection-ab-c (intersection-set union-a-b set-c))

union-a-b
intersection-a-b
(newline)
intersection-ab-c