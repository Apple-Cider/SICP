#lang racket

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (count-leaves-a t)
  (accumulate (lambda (x y)
                (+ y
                   (if (pair? x)
                       (count-leaves-a x)
                       1)))
              0
              t))

(define (count-leaves-original x)
  (cond ((null? x) 0)  
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

(define (count-leaves-b t)
  (accumulate +
              0
              (map (lambda (x)
                     (if (pair? x)
                         (count-leaves-b x)
                         1)) t)))



(define my-tree (list 1 (list 2 (list 3 4) (list 5 6 7))))
my-tree
(count-leaves-original my-tree)
(count-leaves-a my-tree) ;first answer. did not follow book's answer's pattern
(count-leaves-b my-tree) ;second answer. does fit book's answer's pattern