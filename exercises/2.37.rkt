#lang racket

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      null
      (cons (accumulate op init (map (lambda (seq) (car seq)) seqs))
            (accumulate-n op init (map (lambda (seq) (cdr seq)) seqs)))))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))


(define (matrix-*-vector m v)
  (map (lambda (x) (dot-product v x)) m))


(define (transpose mat)
  (accumulate-n cons null mat))


(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (x) (accumulate-n (lambda (y z) (+ (* x y) z)) 0 cols)) m)))

(define m-0 (list (list 1 -1 2) (list 0 -3 1)))
(define v-0 (list 2 1 0))
(define m-0-X-v-0 (list 1 -3))
m-0-X-v-0
(matrix-*-vector m-0 v-0)

(define m-0-T (list (list 1 0) (list -1 -3) (list 2 1)))
m-0-T
(transpose m-0)


(define m-1 (list (list 0 4 -2) (list -4 -3 0)))
(define m-2 (list (list 0 1) (list 1 -1) (list 2 3)))
(define m-1-X-m-2 (list (list 0 -10) (list -3 -1)))
m-1-X-m-2
(matrix-*-matrix m-1 m-2)

