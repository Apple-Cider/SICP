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

(define my-seqs (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))
(define answer (list 22 26 30))

answer
(accumulate-n + 0 my-seqs)