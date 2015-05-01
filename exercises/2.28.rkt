#lang racket

(define (fringe items)
  (if (not (pair? items))
      items
      (if (pair? (car items))
          (append (fringe (car items)) (fringe (cdr items)))
          (cons (car items) (fringe (cdr items))))))


(define x (list (list 1 (list 2 3 4) (list 5 6)) (list 7 8)))

x
(fringe x)