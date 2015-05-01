#lang racket

(define (deep-reverse items)
  (define (reverse-iter items reversed)
    (if (null? items)
        reversed
        (if (pair? (car items))
            (reverse-iter (cdr items)
                          (cons (deep-reverse (car items))
                                reversed))
            (reverse-iter (cdr items)
                          (cons (car items)
                                reversed)))))
          
  (reverse-iter items '()))

(define x (list (list 1 (list 2 3 4) (list 5 6)) (list 7 8)))
x
(deep-reverse x)