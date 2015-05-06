#lang racket

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (map-a p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) null sequence))

(define list-0 (list 1 2 3 4))
(define list-1 (list 5 6 7 8))
(define (add-1 x) (+ x 1))

(map add-1 list-0)
(map-a add-1 list-0)

(define (append-a seq1 seq2)
  (accumulate cons seq2 seq1))

(append list-0 list-1)
(append-a list-0 list-1)


(define (length-a sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))

(length list-0)
(length-a list-0)