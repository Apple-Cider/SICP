#lang racket

(define (filter predicate sequence)
  (cond ((null? sequence) null)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      null
      (cons low (enumerate-interval (+ low 1) high))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append null (map proc seq)))

;exercise:

(define (unique-pairs n)
  (flatmap
   (lambda (i)
     (map (lambda (j) (list i j))
          (enumerate-interval 1 (- i 1))))
   (enumerate-interval 1 n)))

;(define (prime-sum-pairs n)
;  (define (prime-sum? pair)
;    (prime? (+ (car pair) (cadr pair))))
;  (define (make-pair-sum pair)
;    (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))
;  (map make-pair-sum
;       (filter prime-sum? (unique-pairs n))))

(define (unique-triples n)
  (flatmap
   (lambda (i)
     (flatmap
      (lambda (j)
        (map (lambda (k) (list i j k))
        (enumerate-interval 1 (- j 1))))
      (enumerate-interval 1 (- i 1))))
   (enumerate-interval 1 n)))

(define (triple-addends n s)
  (define (correct-sum? triple)
    (= s (+ (car triple) (cadr triple) (caddr triple))))
  (filter correct-sum? (unique-triples n)))

(define answer (list (list 5 3 2) (list 5 4 1) (list 6 3 1)))
(define check (triple-addends 6 10))
answer
check

;(define (ordered-k-sequences low high k)