#lang racket

(define (p pos row)
  (cond ((= pos 0) 1)
        ((= pos row) 1)
        ((< row pos) 0)
        (else (+ (p (- pos 1) (- row 1))
                 (p pos (- row 1))))))