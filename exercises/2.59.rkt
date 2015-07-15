#lang racket

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)        
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

;reliant on adjoin-set for eliminating duplication of elements
(define (union-set-simple set1 set2)
  (if (null? set1)
      set2
      (union-set-simple (cdr set1)
                        (adjoin-set (car set1) set2))))

;guarantees uniqueness of resultant set and efficiently handles
;  case where set2 is empty
(define (union-set set1 set2)
  (define (union-set-iter remaining united)
    (cond ((null? remaining) united)
          ((element-of-set? (car remaining) united)
           (union-set-iter (cdr remaining) united))
          (else (union-set-iter (cdr remaining) (cons (car remaining) united)))))
  (if (null? set2)
      set1
      (union-set-iter set1 set2)))


(define set-a '(a b c 1 2 3))
(define set-b '(c d e 3 4 5))
(define set-c '(0 1 2 3 4 5 6))

(define union-a-b (union-set set-a set-b))
(define unionsimple-a-b (union-set-simple set-a set-b))
(define union-ab-c (union-set union-a-b set-c))
(define unionsimple-ab-c (union-set-simple unionsimple-a-b set-c))

union-a-b
unionsimple-a-b
(newline)
union-ab-c
unionsimple-ab-c