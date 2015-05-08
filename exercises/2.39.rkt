#lang racket

(define (fold-right op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (fold-right op initial (cdr sequence)))))

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(define (reverse-original items)
  (define (reverse-iter items reversed)
    (if (null? items)
        reversed
        (reverse-iter (cdr items)
                      (cons (car items) reversed))))
  (reverse-iter items '()))

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

(define (reverse-right sequence)
  (fold-right (lambda (x y) 
                (if (null? y)
                    (list x)
                    (append y (list x))))
                null sequence))

(define (reverse-left sequence)
  (fold-left (lambda (x y)
               (if (null? x)
                   (list y)
                   (cons y x)))
             null sequence))

(reverse-original (list 1 2 3))
(reverse-right (list 1 2 3))
(reverse-left (list 1 2 3))