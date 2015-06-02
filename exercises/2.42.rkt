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

(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))

(define (queens board-size)
  (define (make-position column row) (list column row))
  (define (column position) (car position))
  (define (row position) (cadr position))
  
  (define empty-board (list))
  
  (define (adjoin-position new-row k rest-of-queens)
    (append (make-position k new-row) rest-of-queens))
  
  (define (safe? k positions)
    (safe-iter (car positions) (cdr positions)))
  (define (safe-iter position positions)
    (cond ((null? positions) true)
          ((attacks? position (car positions)) false)
          (else (safe-iter position (cdr positions)))))
  (define (attacks? pos1 pos2)
    (or (= (row pos1) (row pos2))
        (= (abs (- (column pos1) (column pos2)))
           (abs (- (row pos1) (row pos2))))))
  
  (define (queen-cols k)  
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))