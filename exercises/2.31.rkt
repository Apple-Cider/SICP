#lang racket

(define (square x) (* x x))

(define (tree-map f tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map f sub-tree)
             (if (null? sub-tree)
                 '()
                 (f sub-tree))))
       tree))

(define (square-tree tree)
  (tree-map square tree))

(define my-tree (list 1 2 3 4 (list 5 6 7) (list 8 9 10) 11 (list (list 12 13) 14) 15))
(define expected (list 1 4 9 16 (list 25 36 49) (list 64 81 100) 121 (list (list 144 169) 196) 225))

expected
(square-tree my-tree)