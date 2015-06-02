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


(define (queens board-size)
  (define (make-position column row) (list column row))
  (define (column position) (car position))
  (define (row position) (cadr position))
  ;(define (print-position position) (newline) (display "col: ") (display (car position)) (display " row: ") (display (cadr position)))
  ;(define (print-board board) (map (lambda (position) (print-position position)) board))
  
  (define empty-board (list))
  
  (define (adjoin-position new-row k rest-of-queens)
    ;(newline) (display "new position: ") (print-position (make-position k new-row))
    (cons (make-position k new-row) rest-of-queens))
  
  (define (safe? k positions)
    ;(newline) (display "is this board safe? ") (print-board positions) (newline)
    (safe-iter (car positions) (cdr positions)))
  (define (safe-iter position positions)
    (cond ((null? positions) true)
          ((attacks? position (car positions)) false)
          (else (safe-iter position (cdr positions)))))
  (define (attacks? pos1 pos2)
    ;(newline) (display "attacks? :") (print-position pos1) (print-position pos2)
    (or (= (row pos1) (row pos2))
        (= (abs (- (column pos1) (column pos2)))
           (abs (- (row pos1) (row pos2))))))
  
  (define (queen-cols k)
    ;(newline) (display " running queen-cols; k: ") (display k)
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

(display "92 solutions to the 8 queen puzzle should be found.")
(newline)
(display "computed solution count: ")
(length (queens 8))